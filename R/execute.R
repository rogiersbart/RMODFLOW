#' Run a MODFLOW model
#' 
#' \code{run_modflow} runs a MODFLOW model.
#' 
#' @param file path to name file; typically '*.nam'
#' @param version MODFLOW version to use; 2005 (default) is currently the only option
#' @param executable name of the MODFLOW executable to use; if not provided, the executable distributed with RMODFLOW is used, corresponding to version, machine and sysname
#' @param par vector of parameter value file parameter values to run the model with
#' 
#' @rdname rmf_run_modflow
#' @method rmf_run_modflow character
#' @export
rmf_run_modflow.character <- function(file,
                                      version = 2005,
                                      executable = NULL,
                                      par = NULL) {
  
  # add argument intern = FALSE, so it can be set to TRUE for printing MODFLOW terminal output in vignettes
  
  # select appropriate executable
    if(is.null(executable)) {
      if(version == '2005' | version == 2005) {
        executable <- system.file(paste0('bin/MODFLOW-2005_v.1.11.00_',Sys.info()['sysname'],'_',Sys.info()['machine'],'.exe'),package='RMODFLOW')
      }
    }
  
  # get directory and filename
    dir <- dirname(file)
    file <- basename(file)
    
  # set initial parameters if provided  
    if(!is.null(par)) {
      nam <- rmf_read_nam(paste0(dir,'/',file))
      pvl <- rmf_read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
      pvl$parval <- par
      write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    }
    
  # run modflow
    if(Sys.info()['sysname']=='Linux') system(paste('cd',dir,'&',executable,file))
    if(Sys.info()['sysname']=='Windows') shell(paste('cd',dir,'&',executable,file),mustWork=TRUE) # , intern=TRUE
}

#' Run a MODFLOW model
#' 
#' \code{run_modflow} runs a MODFLOW model.
#' 
#' @param modflow modflow object
#' @param version MODFLOW version to use; 2005 (default) is currently the only option
#' @param executable name of the MODFLOW executable to use; if not provided, the executable distributed with RMODFLOW is used, corresponding to version, machine and sysname
#' @param par vector of parameter value file parameter values to run the model with
#' 
#' @rdname rmf_run_modflow
#' @method rmf_run_modflow modflow
#' @export
rmf_run_modflow.modflow <- function(modflow,
                                    version = 2005,
                                    executable = NULL,
                                    par = NULL) {
  
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
  rmf_run_modflow('input.nam', version = version, executable = executable, par = par)
  
  # read all output
  
}

#' Generic function to run a modflow model
#' 
#' @rdname rmf_run_modflow
#' @export
rmf_run_modflow <- function(...) {
  UseMethod('rmf_run_modflow')
}

#' @describeIn rmf_run_modflow Deprecated function name
#' @export
run_modflow <- function(...) {
  .Deprecated(new = "rmf_run_modflow", old = "run_modflow")
  rmf_run_modflow(...)
}

#' Run a MODFLOW model optimization, based on the parameter value file
#' 
#' \code{run_modflow_opt} runs a MODFLOW optimization.
#' 
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use
#' @param par initial parameter values (for all or only included parameters); current parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the optimization
#' @param trans vector of transformations; currently only 'log' is supported
#' @param method optimization method: 'Nelder-Mead','BFGS','CG','SANN','Brent','L-BGFS-B','spso2011','spso2007','ipso','fips','wfips','canonical' or 'DEoptim'
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param control list of control arguments
#' @param ... further arguments provided to \code{optim}, \code{hydroPSO} or \code{DEoptim}
#' @return \code{optim} results with the full list of parameters
#' @importFrom hydroPSO hydroPSO
#' @importFrom DEoptim DEoptim
#' @export
rmf_run_opt <- function(file,executable='mf2005',par=NULL,include=NULL, trans=NULL, method='Nelder-Mead', lower=-Inf, upper=Inf, control=list(), ...)
{
  dir <- dirname(file)
  file <- basename(file)
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
    rmf_run_modflow(paste0(dir,'/',file),executable)
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
  } else if(method %in% c('spso2011','spso2007','ipso','fips','wfips','canonical'))
  {
    opt <- hydroPSO(par[which(include)],optim_modflow, method=method,lower=lower[which(include)],upper=upper[which(include)],control=control, ...)
  } else if(method=='DEoptim')
  {
    opt <- DEoptim(optim_modflow,lower=lower[which(include)],upper=upper[which(include)], control=control, ...)
  } else {
    stop(paste('Method',method,'is not supported. Please provide one of the optim, hydroPSO or DEoptim methods.'))
  }
  if(method!='DEoptim') # make this part compatible with DEoptim as well!
  {
    par2 <- opt$par
    opt$par <- par
    opt$par[which(include)] <- par2
    if(!is.null(trans)) opt$par[which(trans=='log')] <- exp(opt$par[which(trans=='log')])
  }
  opt$included <- include
  return(opt)
}

#' @describeIn rmf_run_opt Deprecated function name
#' @export
run_opt <- function(...) {
  .Deprecated(new = "rmf_run_opt", old = "run_opt")
  rmf_run_opt(...)
}

#' Run a MODFLOW model response surface mapping
#' 
#' \code{run_modflow_rsm} runs a MODFLOW response surface mapping.
#' 
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use
#' @param par central parameter values (for all or only included parameters); current parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the mapping
#' @param trans vector of transformations; currently only 'log' is supported
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param n number of intervals sampled for each parameter
#' @return an rsm object with the full list of parameters and the response value
#' @export
rmf_run_rsm <- function(file,executable='mf2005',par=NULL,include=NULL, trans=NULL, lower, upper, n)
{
  dir <- dirname(file)
  file <- basename(file)
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
  if(!is.null(trans))
  {
    par[which(trans=='log')] <- log(par[which(trans=='log')])
    lower[which(trans=='log')] <- log(lower[which(trans=='log')])
    upper[which(trans=='log')] <- log(upper[which(trans=='log')])
  }
  
  # create parameter sets to run
  parameter_values <- list()
  for(i in 1:sum(include)) parameter_values[[i]] <- seq(lower[include][i],upper[include][i],length=n)
  rsm <- expand.grid(parameter_values)
  rsm$rmse <- NA
  
  # run parameter sets and get RMSE
  rsm_modflow <- function(par_include)
  {
    pvl$parval <- par
    pvl$parval[which(include)] <- as.numeric(par_include)
    if(!is.null(trans)) pvl$parval[which(trans=='log')] <- exp(pvl$parval[which(trans=='log')])
    rmf_write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    rmf_run_modflow(paste0(dir,'/',file),executable)
    rmse <- rmf_performance(rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)])))$rmse
    cat(paste('\n RMSE=',format(rmse,scientific=TRUE,digits=4),'parval=',paste(format(pvl$parval[include],scientific=TRUE,digits=4),collapse=' '),'\n')) # file=report, append=T
    return(rmse)
  }
  for(i in 1:nrow(rsm))
  {
    rsm$rmse[i] <- rsm_modflow(rsm[i,1:sum(include)])
  }
  names(rsm) <- c(paste0(trans,pvl$parnam)[include],'rmse')
  # add attributes later
  class(rsm) <- c('rsm','data.frame')
  return(rsm)
}

#' @describeIn rmf_run_rsm Deprecated function name
#' @export
run_rsm <- function(...) {
  .Deprecated(new = "rmf_run_rsm", old = "run_rsm")
  rmf_run_rsm(...)
}

#' Run a MODFLOW model sensitivity analysis, based on the parameter value file
#' 
#' \code{run_modflow_sen} performs a MODFLOW model sensitivity analysis.
#' 
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use
#' @param par central parameter values (for all or only included parameters); parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the sensitivity analysis
#' @return sensitivity analysis results
#' @export
rmf_run_sen <- function(file,executable='mf2005',par=NULL,include=NULL)
{
  dir <- dirname(file)
  file <- basename(file)
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
  rmf_run_modflow(file = paste0(dir,'/',file), executable = executable,par)
  hpr_orig <- rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)]))
  sens <- list()
  sens$dss <- matrix(NA,nrow=length(hob$obsnam),ncol=length(pvl$parval))
  sens$css <- pvl$parval*NA
  for(i in which(include))
  {
    pvl$parval <- par
    pvl$parval[i] <- pvl$parval[i]*1.01
    rmf_write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    rmf_run_modflow(file = paste0(dir,'/',file), executable = executable)
    hpr <- rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)]))
    sens$dss[,i] <- (hpr$simulated_equivalent-hpr_orig$simulated_equivalent)/(0.01)
    sens$css[i] <- sqrt(sum(sens$dss[,i]^2)/hob$nh)
  }
  sens$parnam <- pvl$parnam
  class(sens) <- 'sen'
  return(sens)
}

#' @describeIn rmf_run_sen Deprecated function name
#' @export
run_sen <- function(...) {
  .Deprecated(new = "rmf_run_sen", old = "run_sen")
  rmf_run_sen(...)
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
rmf_find <- function(name = "MODFLOW-2005",
                     precision = "single") {
  if (name == "MODFLOW-2005") {
    executable <- ifelse(precision == "single", "mf2005.exe", "mf2005dbl.exe")
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-OWHM") {
    executable <- "MF_OWHM.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-NWT") {
    executable <- "MODFLOW-NWT_64.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-LGR") {
    executable <- "mflgr.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-CFP") {
    executable <- "mf2005cfp.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/")
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