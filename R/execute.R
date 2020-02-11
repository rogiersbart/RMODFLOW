#' Run a MODFLOW model
#' 
#' \code{rmf_execute} runs a MODFLOW model.
#' 
#' @param path path to name file; typically '*.nam'
#' @param code name of the MODFLOW variant to use
#' @param par vector of parameter value file parameter values to run the model with
#' @param convergence character denoting the message in the terminal output used to check for convergence
#' @rdname rmf_execute
#' @method rmf_execute character
#' @export
#' @seealso \code{\link{rmf_install}}
rmf_execute.character <- function(
  path,
  code = 2005,
  par = NULL,
  convergence = "Normal termination"
) {
  
  code <- rmfi_find(code)
  
  # get directory and filename
    dir <- dirname(path)
    file <- basename(path)
    
    # set initial parameters if provided  
    if(!is.null(par)) {
      nam <- rmf_read_nam(path)
      if('PVAL' %in% nam$ftype) {
        pval <- rmf_read_pval(file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
        
        # copy and write original pval
        rmf_write_pval(pval, file = file.path(dir, paste('original', nam$fname[which(nam$ftype=='PVAL')], sep = '_')))
        
        pval$parval <- par
        rmf_write_pval(pval, file = file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
        
      } else {
        ui_warn('Model does not have pval input file. Ignoring par argument')
      }
    }
    
  # run modflow
    mf_stdout <- ! getOption("RMODFLOW.ui") == "none"
    out <- processx::run(code, file, wd = dir,
                         stdout_line_callback = if (mf_stdout) rmfi_line_callback else NULL)
    cvg <- grepl(convergence, out$stdout)
    invisible(cvg)
}

#' Run a MODFLOW model
#' 
#' \code{execute} runs a MODFLOW model.
#' 
#' @param modflow modflow object
#' @param code name of the MODFLOW variant to use
#' @param par vector of parameter value file parameter values to run the model with
#' 
#' @rdname rmf_execute
#' @method rmf_execute modflow
#' @export
rmf_execute.modflow <- function(
  modflow,
  code = 2005,
  par = NULL
) {
  
  # TODO change class and top-level S3 to "rmf_model" instead of modflow
  code <- rmfi_find(code)
  
  # temporary directory
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  
  # write all files
  rmf_write_nam(modflow$nam, file = 'input.nam')
  for(i in 1:nrow(modflow$nam)) {
    if(modflow$nam$ftype[i] %in% c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF')) {
      object_class <- c('hob','pval','dis','zon','mlt','bas','huf','oc','wel','ghb','pcg','kdep','lpf')[which(c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF') == modflow$nam$ftype[i])]
      if(object_class %in% 'lpf') {
        get(paste0('rmf_write_',object_class))(modflow[[object_class]], file = modflow$nam$fname[i], dis = modflow$dis)
      } else {
        get(paste0('rmf_write_',object_class))(modflow[[object_class]], file = modflow$nam$fname[i])  
      }
    }
  }
  
  # run modflow
  rmf_execute('input.nam', code = code, par = par)
  
  # read all output
  
}

#' Generic function to execute a MODFLOW model
#' 
#' @rdname rmf_execute
#' @export
rmf_execute <- function(...) {
  UseMethod('rmf_execute')
}

rmfi_line_callback <- function(line, process) {
  line <- stringr::str_squish(line)
  if (line == "") return(invisible())
  if (line %in% c("MODFLOW-2005", "MODFLOW-LGR2", "MODFLOW-NWT-SWR1", "OWHM 1.0")) {
    ui_title(line)
    return(invisible())
  }
  if (grepl("Normal termination of simulation", line)) {
    ui_done(line)
    return(invisible())
  }
  # TODO add if no convergence!
  ui_info(line)
  invisible()
}

#' Run a MODFLOW model optimization, based on the parameter value file and the head predictions output file
#' 
#' \code{rmf_optimize} runs a MODFLOW optimization.
#' 
#' @param path path to name file; typically '*.nam'
#' @param code name of the MODFLOW variant to use
#' @param par initial parameter values (for all or only included parameters); current parameter value file values are used if par is not provided (default)
#' @param include logical vector indicating which parameters in the parameter value file to include in the optimization
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param control list of control arguments
#' @param logtrans logical vector indicating which parameters should be logtransformed
#' @param cost character denoting which statistic should be used to compute the cost. Possible values are those returned by \code{\link{rmf_performance}}. Defaults to 'ssq'
#' @param out_file optional file path to write cost value, convergence and parameter values to for each iteration
#' @param convergence character denoting the message in the terminal output used to check for convergence
#' @param ... further arguments provided to \code{optim}
#'
#' @details Only works with models using MODFLOW parameters and having a head predictions output file as defined in the HOB object
#'          The only method currently available is "L-BFGS-B". See \code{\link{optim}}
#' @return \code{optim} results with the full list of parameters
#' @export
#' @seealso \code{\link{optim}}, \code{\link{rmf_execute}}, \code{\link{rmf_create_pval}} & \code{\link{rmf_create_hob}} 
rmf_optimize <- function(
  path,
  code = 2005,
  par = NULL,
  include = NULL,
  logtrans = NULL,
  lower = 0, # TODO think about this default value ...
  upper = Inf,
  cost = 'ssq',
  out_file = NULL,
  control = list(),
  convergence = 'Normal termination',
  ...
) {
  code <- rmfi_find(code)
  dir <- dirname(path)
  nam <- rmf_read_nam(path)
  if(!('PVAL' %in% nam$ftype) || !('HOB' %in% nam$ftype)) ui_stop('rmf_optimize only works with models having pval and HOB input')
  pval <- rmf_read_pval(file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(file.path(dir, nam$fname[which(nam$ftype=='HOB')]))
  if(hob$iuhobsv == 0 || toupper(nam$ftype[which(nam$nunit == hob$iuhobsv)]) != 'DATA') {
    ui_stop('rmf_optimize only works with models having a head predictions output file. This can be created by setting the iuhobsv argument of the HOB file to a non-zero value')
  }
  
  # copy and write original pval
  rmf_write_pval(pval, file = file.path(dir, paste('original', nam$fname[which(nam$ftype=='PVAL')], sep = '_')))
  
  if(is.null(par)) par <- pval$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  
  if(is.infinite(lower)) lower <- rep(lower,pval$np)
  if(is.infinite(upper)) upper <- rep(upper,pval$np)
  if(!is.null(logtrans)) {
    par[which(logtrans)] <- log(par[which(logtrans)])
    lower[which(logtrans)] <- log(lower[which(logtrans)])
    upper[which(logtrans)] <- log(upper[which(logtrans)])
    if('parscale' %in% names(control)) {
      control$parscale[which(trans[include])] <- log(control$parscale[which(trans[include])])
    }
  }

  # if par only has values for include = TRUE
  if(length(par) != length(pval$parval)) {
    par2 <- par
    par <- pval$parval
    par[which(include)] <- par2
  }
  
  optim_modflow <- function(par_include) {
    # adjust values
    pval$parval <- par
    pval$parval[which(include)] <- par_include
    if(!is.null(logtrans)) pval$parval[which(logtrans)] <- exp(pval$parval[which(logtrans)])
    
    # write values
    rmf_write_pval(pval, file = file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
    
    # run modflow
    cvg <- rmf_execute(file = file, executable = executable, version = version, convergence = convergence)
    
    # get cost
    if(cvg) {
      hpr <- rmf_read_hpr(file.path(dir, nam$fname[which(nam$nunit==hob$iuhobsv)]))
      cost_value <- rmf_performance(hpr)[cost]
    } else { # return large cost when not converging
      cost_value <- ifelse(!is.null(control$fnscale) && control$fnscale < 0, -1e100, 1e100) # maximization/minimization
    }
    
    # print 
    cat(paste(cost, '=', format(cost_value,scientific=TRUE,digits=4), 'converged =', as.character(cvg), 'parval =', paste(format(pval$parval[include],scientific=TRUE,digits=4), collapse=' '),'\n'))
    if(!is.null(out_file)) cat(paste(cost, '=', format(cost_value,scientific=TRUE,digits=4), 'converged =', as.character(cvg), 'parval =', paste(format(pval$parval[include],scientific=TRUE,digits=4), collapse=' '),'\n'), file = out_file, append = TRUE)
    
    return(cost_value)
  }
  
  # optimize; L-BGFS-B only
  opt <- optim(par[which(include)], optim_modflow, method = 'L-BFGS-B', lower = lower[which(include)], upper = upper[which(include)], control = control, ...)
  opt$included <- include
  return(opt)
}

#' Run a MODFLOW model sensitivity analysis, based on the parameter value file and the head predictions output file
#' 
#' \code{rmf_analyze} performs a MODFLOW model sensitivity analysis.
#' 
#' @param path path to name file; typically '*.nam'
#' @param code name of the MODFLOW variant to use
#' @param par central parameter values (for all or only included parameters); parameter value file values are used if par is not provided (default)
#' @param include logical vector indicating which parameters in the parameter value file to include in the sensitivity analysis
#' @param ... optional arguments passed to \code{rmf_execute}
#' @details Only works with models using MODFLOW parameters and having a head predictions output file as defined in the HOB object
#' @return object of class \code{sen} which is a list with dimensionless scaled sensitivities (dss) and composite scaled sensitivies (css)
#' @export
#' @seealso \code{\link{rmf_execute}}, \code{\link{rmf_create_pval}} & \code{\link{rmf_create_hob}} 
rmf_analyze <- function(path,
                        code = "2005",
                        par = NULL,
                        include = NULL,
                        ...) {
  code <- rmfi_find(code)
  dir <- dirname(path)
  nam <- rmf_read_nam(path)
  if(!('PVAL' %in% nam$ftype) || !('HOB' %in% nam$ftype)) ui_stop('rmf_analyze only works with models having pval and HOB input')
  pval <- rmf_read_pval(file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(file.path(dir, nam$fname[which(nam$ftype=='HOB')]))
  if(hob$iuhobsv == 0 || toupper(nam$ftype[which(nam$nunit == hob$iuhobsv)]) != 'DATA') {
    ui_stop('rmf_analyze only works with models having a head predictions output file. This can be created by setting the iuhobsv argument of the HOB file to a non-zero value')
  }
  pval_org <- pval
  if(is.null(par)) par <- pval$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  
  # if par only has values for include = TRUE
  if(length(par) != length(pval$parval)) {
    par2 <- par
    par <- pval$parval
    par[which(include)] <- par2
  } 

  # copy and write original pval
  rmf_write_pval(pval_org, file = file.path(dir, paste('original', nam$fname[which(nam$ftype=='PVAL')], sep = '_')))
  
  # initial run
  rmf_execute(path = path, code = code, par = par, version = version, ...)
  hpr_orig <- rmf_read_hpr(file.path(dir, nam$fname[which(nam$nunit == hob$iuhobsv)]))

  sens <- list()
  sens$dss <- matrix(NA, nrow = length(hob$obsnam), ncol = length(pval$parval))
  sens$css <- pval$parval*NA
  
  # dss & css
  for(i in which(include)) {
    pval$parval <- par
    pval$parval[i] <- pval$parval[i]*1.01
    rmf_write_pval(pval, file = file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
    rmf_execute(file = file, executable = executable, par = NULL, version = version, ...)
    hpr <- rmf_read_hpr(file.path(dir, nam$fname[which(nam$nunit == hob$iuhobsv)]))
    sens$dss[,i] <- (hpr$simulated - hpr_orig$simulated)/(0.01)
    sens$css[i] <- sqrt(sum(sens$dss[,i]^2)/hob$nh)
  }
  
  #  write original pval and run
  rmf_write_pval(pval_org, file = file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
  rmf_execute(file = file, executable = executable, par = NULL, version = version, ...)
  
  sens$parnam <- pval$parnam
  class(sens) <- 'sen'
  return(sens)
}

#' Find paths to executables
#' 
#' \code{rmfi_find} finds the path to external software executables.
#' 
#' The function first looks for the executable in the current working
#' directory. If not there, it looks in \file{C:/WRDAPP/}, where the
#' software might have been installed by \code{\link{rmf_install}}. If the
#' executable cannot be found, the system path variable is used in an attempt
#' to locate it. If it still cannot be located, you should first install the
#' software or add the folder with the executable to the system path.
#'
#' @param code Character. Name of the code. Currently supported values are 
#' listed in \code{RMODFLOW:::supported_codes}
#' @param precision Character. Can be \code{"single"} or \code{"double"}. Only relevant for MODFLOW-2005.
#'
#' @return Path to the executable.
#'
#' @examples
#' rmfi_find("MODFLOW-2005")
rmfi_find <- function(
  code = "2005",
  precision = "single"
) {
  # TODO automatically select 32 bit executables if available, otherwise throw
  #      error on 32 bit systems
  # TODO check if executable provided in code exists in current wd, not name
  #      of executable that we are using in RMODFLOW!
  if (grepl("2005", code)) {
    if (grepl("dbl", code)) precision <- "double"
    code <- "MODFLOW-2005"
    executable <- ifelse(precision == "single", "mf2005.exe", "mf2005dbl.exe")
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", code, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        ui_stop("Path to {code} executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("owhm", code, ignore.case = TRUE)) {
    code <- "MODFLOW-OWHM"
    executable <- "MF_OWHM.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", code, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        ui_stop("Path to {code} executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("nwt", code, ignore.case = TRUE)) {
    code <- "MODFLOW-NWT"
    executable <- "MODFLOW-NWT_64.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", code, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        ui_stop("Path to {code} executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("lgr", code, ignore.case = TRUE)) {
    code <- "MODFLOW-LGR"
    executable <- "mflgr.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", code, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        ui_stop("Path to {code} executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("cfp", code, ignore.case = TRUE)) {
    code <- "MODFLOW-CFP"
    executable <- "mf2005cfp.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", code, "/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        ui_stop("Path to {code} executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else {
    ui_stop("Finding paths to the executables of software other than MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not supported.")
  }
}
