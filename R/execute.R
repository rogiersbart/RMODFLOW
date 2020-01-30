#' Run a MODFLOW model
#' 
#' \code{execute} runs a MODFLOW model.
#' 
#' @param path path to name file; typically '*.nam'
#' @param code name of the MODFLOW variant to use
#' @param par vector of parameter value file parameter values to run the model with
#' @param verbose logical; should the terminal output be printed to the R console? Defaults to TRUE
#' @param convergence logical; should convergence be checked? If TRUE (default), a logical is returned
#' @param cvg_message character denoting the message in the terminal output used to check for convergence
#' @return if convergence = TRUE, a logical depending on whether the model converged. Otherwise NULL.
#' @rdname rmf_execute
#' @method rmf_execute character
#' @export
rmf_execute.character <- function(
  path,
  code = 2005,
  par = NULL,
  verbose = TRUE,
  convergence = TRUE,
  cvg_message = 'Normal termination'
) {
  
  code <- rmf_find(code)
  
  # get directory and filename
    dir <- dirname(path)
    file <- basename(path)
    
  # set initial parameters if provided  
    if(!is.null(par)) {
      nam <- rmf_read_nam(paste0(dir,'/',file))
      pvl <- rmf_read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
      pvl$parval <- par
      rmf_write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    }
    
  # run modflow
    out <- processx::run(code, file, wd = dir, echo = verbose)
    if(convergence) {
      cvg <- grepl(cvg_message, out$stdout)
      if (verbose) return(invisible(cvg))
      return(cvg)
    }
    return(invisible())
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
  code <- rmf_find(code)
  
  # temporary directory
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  
  # write all files
  rmf_write_nam(modflow$nam, file = 'input.nam')
  for(i in 1:nrow(modflow$nam)) {
    if(modflow$nam$ftype[i] %in% c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF')) {
      object_class <- c('hob','pvl','dis','zon','mlt','bas','huf','oc','wel','ghb','pcg','kdep','lpf')[which(c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF') == modflow$nam$ftype[i])]
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

#' Run a MODFLOW model optimization, based on the parameter value file
#' 
#' \code{rmf_optimize} runs a MODFLOW optimization.
#' 
#' @param path path to name file; typically '*.nam'
#' @param code name of the MODFLOW variant to use
#' @param par initial parameter values (for all or only included parameters); current parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the optimization
#' @param trans vector of transformations; currently only 'log' is supported
#' @param method optimization method: 'Nelder-Mead','BFGS','CG','SANN','Brent','L-BGFS-B'
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param control list of control arguments
#' @param ... further arguments provided to \code{optim}
#' @return \code{optim} results with the full list of parameters
#' @export
rmf_optimize <- function(
  path,
  code = 2005,
  par = NULL,
  include = NULL,
  trans = NULL,
  method = 'Nelder-Mead',
  lower = -Inf,
  upper = Inf,
  control = list(),
  ...
) {
  code <- rmf_find(code)
  dir <- dirname(path)
  file <- basename(path)
  nam <- rmf_read_nam(paste0(dir,'/',file))
  pvl <- rmf_read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(paste0(dir,'/',nam$fname[which(nam$ftype=='HOB')]))
  if(is.null(par)) par <- pvl$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  if(length(par)!=length(pvl$parval)) 
  {
    par2 <- par
    par <- pvl$parval
    par[which(include)] <- par2
  }
  if(is.infinite(lower)) lower <- rep(lower,pvl$np)
  if(is.infinite(upper)) upper <- rep(upper,pvl$np)
  if(!is.null(trans))
  {
    par[which(trans=='log')] <- log(par[which(trans=='log')])
    lower[which(trans=='log')] <- log(lower[which(trans=='log')])
    upper[which(trans=='log')] <- log(upper[which(trans=='log')])
    if('parscale' %in% names(control))
    {
      control$parscale[which(trans[include]=='log')] <- log(control$parscale[which(trans[include]=='log')])
    }
  }
  optim_modflow <- function(par_include)
  {
    pvl$parval <- par
    pvl$parval[which(include)] <- par_include
    if(!is.null(trans)) pvl$parval[which(trans=='log')] <- exp(pvl$parval[which(trans=='log')])
    rmf_write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    rmf_execute(paste0(dir,'/',file),code)
    rmse <- rmf_performance(rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)])))$rmse
    cat(paste('\n RMSE=',format(rmse,scientific=TRUE,digits=4),'parval=',paste(format(pvl$parval[include],scientific=TRUE,digits=4),collapse=' '),'\n')) # file=report, append=T
    return(rmse)
  }
  if(method %in% c('Nelder-Mead','BFGS','CG','SANN','Brent'))
  {
    opt <- optim(par[which(include)],optim_modflow, method=method, control=control, ...)
    #     if(!is.null(control)) opt <- optim(par[which(include)],optim_modflow, method=method, lower=lower, upper=upper, control=control, ...)
    #     if(is.null(control)) opt <- optim(par[which(include)],optim_modflow, method=method, lower=lower, upper=upper, ...)
  } else if(method=='L-BGFS-B')
  {
    opt <- optim(par[which(include)],optim_modflow, method=method, lower=lower[which(include)], upper=upper[which(include)], control=control, ...)
  } else {
    stop(paste('Method',method,'is not supported. Please provide one of the optim methods.'))
  }
  par2 <- opt$par
  opt$par <- par
  opt$par[which(include)] <- par2
  if(!is.null(trans)) opt$par[which(trans=='log')] <- exp(opt$par[which(trans=='log')])
  opt$included <- include
  return(opt)
}

#' Run a MODFLOW model sensitivity analysis, based on the parameter value file
#' 
#' \code{rmf_analyze} performs a MODFLOW model sensitivity analysis.
#' 
#' @param path path to name file; typically '*.nam'
#' @param code name of the MODFLOW variant to use
#' @param par central parameter values (for all or only included parameters); parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the sensitivity analysis
#' @return sensitivity analysis results
#' @export
rmf_analyze <- function(
  path,
  code = 'mf2005',
  par = NULL,
  include = NULL
) {
  code <- rmf_find(code)
  dir <- dirname(path)
  file <- basename(path)
  nam <- rmf_read_nam(paste0(dir,'/',file))
  pvl <- rmf_read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(paste0(dir,'/',nam$fname[which(nam$ftype=='HOB')]))
  if(is.null(par)) par <- pvl$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  if(length(par)!=length(pvl$parval)) 
  {
    par2 <- par
    par <- pvl$parval
    par[which(include)] <- par2
  } 
  rmf_execute(file = paste0(dir,'/',file), code = code,par)
  hpr_orig <- rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)]))
  sens <- list()
  sens$dss <- matrix(NA,nrow=length(hob$obsnam),ncol=length(pvl$parval))
  sens$css <- pvl$parval*NA
  for(i in which(include))
  {
    pvl$parval <- par
    pvl$parval[i] <- pvl$parval[i]*1.01
    rmf_write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    rmf_execute(file = paste0(dir,'/',file), code = code)
    hpr <- rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)]))
    sens$dss[,i] <- (hpr$simulated_equivalent-hpr_orig$simulated_equivalent)/(0.01)
    sens$css[i] <- sqrt(sum(sens$dss[,i]^2)/hob$nh)
  }
  sens$parnam <- pvl$parnam
  class(sens) <- 'sen'
  return(sens)
}

#' Find paths to executables
#' 
#' \code{rmf_find} finds the path to external software executables.
#' 
#' The function first looks for the executable in the current working
#' directory. If not there, it looks in \file{C:/WRDAPP/}, where the
#' software might have been installed by \code{\link{rmf_install}}. If the
#' executable cannot be found, the system path variable is used in an attempt
#' to locate it. If it still cannot be located, you should first install the
#' software or add the folder with the executable to the system path.
#'
#' @param name Character. Name of the software. Currently supported values are \code{"MODFLOW-2005"}, \code{"MODFLOW-OWHM"}, \code{"MODFLOW-NWT"}, \code{"MODFLOW-LGR"} and \code{"MODFLOW-CFP"}.
#' @param precision Character. Can be \code{"single"} or \code{"double"}. Only relevant for MODFLOW-2005.
#'
#' @return Path to the executable.
#' @export
#'
#' @examples
#' rmf_find("MODFLOW-2005")
rmf_find <- function(
  name = "MODFLOW-2005",
  precision = "single"
) {
  # TODO automatically select 32 bit executables if available, otherwise throw
  # error on 32 bit systems
  if (grepl("2005", name)) {
    if (grepl("dbl", name)) precision <- "double"
    name <- "MODFLOW-2005"
    executable <- ifelse(precision == "single", "mf2005.exe", "mf2005dbl.exe")
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("owhm", name, ignore.case = TRUE)) {
    name <- "MODFLOW-OWHM"
    executable <- "MF_OWHM.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("nwt", name, ignore.case = TRUE)) {
    name <- "MODFLOW-NWT"
    executable <- "MODFLOW-NWT_64.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("lgr", name, ignore.case = TRUE)) {
    name <- "MODFLOW-LGR"
    executable <- "mflgr.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (grepl("cfp", name, ignore.case = TRUE)) {
    name <- "MODFLOW-CFP"
    executable <- "mf2005cfp.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0(getOption("RMODFLOW.path"), "/", name, "/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else {
    stop("Finding paths to the executables of software other than MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not supported.")
  }
}
