#' Run a MODFLOW model
#' 
#' \code{rmf_execute} runs a MODFLOW model.
#' 
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use; if not provided, the executable corresponding to \code{version} is searched in the \code{RMODFLOW} package directory (if installed by \code{\link{rmf_install}})
#' @param version MODFLOW version to use if \code{executable} is missing; defaults to MODFLOW-2005
#' @param par vector of parameter value file parameter values to run the model with
#' @param verbose logical; should the terminal output be printed to the R console? Defaults to TRUE
#' @param convergence logical; should convergence be checked? If TRUE (default), a logical is returned
#' @param cvg_message character denoting the message in the terminal output used to check for convergence
#'
#' @details if \code{executable} is not provided, the executable corresponding to \code{version} is searched in the \code{RMODFLOW} package directory. If no executable is found, an error is returned.
#'          Executables can be installed in the \code{RMODFLOW} package directory using \code{\link{rmf_install}}
#' @return if convergence = TRUE, a logical depending on whether the model converged. Otherwise nothing is returned.
#' @export
#' @seealso \code{\link{rmf_install}}
rmf_execute <- function(file = {cat('Please select nam file ...\n'); file.choose()},
                        executable = NULL,
                        version = c("MODFLOW-2005", "MODFLOW-NWT", "MODFLOW-OWHM", "MODFLOW-LGR", "MODFLOW-CFP"),
                        par = NULL,
                        verbose = TRUE,
                        convergence = TRUE,
                        cvg_message = 'Normal termination') {
  
  # select appropriate executable
    if(is.null(executable)) {
      if(!grepl('MODFLOW', version[1])) version <- paste0('MODFLOW-', version[1])
      version <- match.arg(version)
      exe_name <- switch(version,
                         'MODFLOW-2005' = 'mf2005.exe',
                         'MODFLOW-NWT' = 'MODFLOW-NWT.exe',
                         'MODFLOW-OWHM' = 'MF_OWHM.exe',
                         'MODFLOW-LGR' = 'mflgr.exe',
                         'MODFLOW-CFP' = 'mf2005cfp.exe')
      
      executable <- system.file("exe", version, "bin", exe_name, package = 'RMODFLOW')
      if(!file.exists(executable)) stop(version, ' executable not found. Either specify the executable argument or try calling rmf_install to install in the RMODFLOW package directory.', call. = FALSE)
    }
  
  # get directory
    dir <- dirname(file)

  # set initial parameters if provided  
    if(!is.null(par)) {
      nam <- rmf_read_nam(file)
      if('PVAL' %in% nam$ftype) {
        pvl <- rmf_read_pvl(file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
        
        # copy and write original pvl
        rmf_write_pvl(pvl, file = file.path(dir, paste('original', nam$fname[which(nam$ftype=='PVAL')], sep = '_')))
        
        pvl$parval <- par
        rmf_write_pvl(pvl, file = file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
        
      } else {
        warning('Model does not have PVL input file. Ignoring par argument', call. = FALSE)
      }
    
    }
    
  # run modflow
    out <- processx::run(executable, file, wd = dir, echo = verbose)
    if(convergence) {
      cvg <- grepl(cvg_message, out$stdout)
      return(cvg)
    }
}

#' Run a MODFLOW model optimization, based on the parameter value file and the head predictions output file
#' 
#' \code{rmf_optimize} runs a MODFLOW optimization.
#' 
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use; if not provided, the executable corresponding to \code{version} is searched in the \code{RMODFLOW} package directory (if installed by \code{\link{rmf_install}})
#' @param version MODFLOW version to use if \code{executable} is missing; defaults to MODFLOW-2005
#' @param par initial parameter values (for all or only included parameters); current parameter value file values are used if par is not provided (default)
#' @param include logical vector indicating which parameters in the parameter value file to include in the optimization
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param control list of control arguments
#' @param logtrans logical vector indicating which parameters should be logtransformed
#' @param verbose logical; should the terminal output be printed to the R console? Defaults to TRUE
#' @param cost character denoting which statistic should be used to compute the cost. Possible values are those returned by \code{\link{rmf_performance}}. Defaults to 'ssq'
#' @param out_file optional file path to write cost value, convergence and parameter values to for each iteration
#' @param cvg_message character denoting the message in the terminal output used to check for convergence
#' @param ... further arguments provided to \code{optim}
#'
#' @details Only works with models using MODFLOW parameters and having a head predictions output file as defined in the HOB object
#'          The only method currently available is "L-BFGS-B". See \code{\link{optim}}
#' @return \code{optim} results with the full list of parameters
#' @export
#' @seealso \code{\link{optim}}, \code{\link{rmf_execute}}, \code{\link{rmf_create_pvl}} & \code{\link{rmf_create_hob}} 
rmf_optimize <- function(file = {cat('Please select nam file ...\n'); file.choose()},
                         executable = NULL,
                         version = c("MODFLOW-2005", "MODFLOW-NWT", "MODFLOW-OWHM", "MODFLOW-LGR", "MODFLOW-CFP"),
                         par = NULL,
                         include = NULL,
                         logtrans = NULL,
                         lower = 0,
                         upper = Inf,
                         verbose = TRUE,
                         cost = 'ssq',
                         out_file = NULL,
                         control = list(),
                         cvg_message = 'Normal termination',
                         ...) {
  
  dir <- dirname(file)

  nam <- rmf_read_nam(file)
  if(!('PVAL' %in% nam$ftype) || !('HOB' %in% nam$ftype)) stop('rmf_optimize only works with models having PVL and HOB input', call. = FALSE)
  pvl <- rmf_read_pvl(file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(file.path(dir, nam$fname[which(nam$ftype=='HOB')]))
  if(hob$iuhobsv == 0 || toupper(nam$ftype[which(nam$nunit == hob$iuhobsv)]) != 'DATA') {
    stop('rmf_optimize only works with models having a head predictions output file. This can be created by setting the iuhobsv argument of the HOB file to a non-zero value', call. = FALSE)
  }
  
  # copy and write original pvl
  rmf_write_pvl(pvl, file = file.path(dir, paste('original', nam$fname[which(nam$ftype=='PVAL')], sep = '_')))
  
  if(is.null(par)) par <- pvl$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  
  if(is.infinite(lower)) lower <- rep(lower,pvl$np)
  if(is.infinite(upper)) upper <- rep(upper,pvl$np)
  if(!is.null(logtrans)) {
    par[which(logtrans)] <- log(par[which(logtrans)])
    lower[which(logtrans)] <- log(lower[which(logtrans)])
    upper[which(logtrans)] <- log(upper[which(logtrans)])
    if('parscale' %in% names(control)) {
      control$parscale[which(trans[include])] <- log(control$parscale[which(trans[include])])
    }
  }
  
  # if par only has values for include = TRUE
  if(length(par) != length(pvl$parval)) {
    par2 <- par
    par <- pvl$parval
    par[which(include)] <- par2
  }
  
  optim_modflow <- function(par_include) {
    # adjust values
    pvl$parval <- par
    pvl$parval[which(include)] <- par_include
    if(!is.null(logtrans)) pvl$parval[which(logtrans)] <- exp(pvl$parval[which(logtrans)])
    
    # write values
    rmf_write_pvl(pvl, file = file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
    
    # run modflow
    cvg <- rmf_execute(file = file, executable = executable, version = version, verbose = verbose, cvg_message = cvg_message, convergence = TRUE)
    
    # get cost
    if(cvg) {
      hpr <- rmf_read_hpr(file.path(dir, nam$fname[which(nam$nunit==hob$iuhobsv)]))
      cost_value <- rmf_performance(hpr)[cost]
    } else { # return large cost when not converging
      cost_value <- ifelse(!is.null(control$fnscale) && control$fnscale < 0, -1e100, 1e100) # maximization/minimization
    }
    
    # print 
    if(verbose) cat(paste(cost, '=', format(cost_value,scientific=TRUE,digits=4), 'converged =', as.character(cvg), 'parval =', paste(format(pvl$parval[include],scientific=TRUE,digits=4), collapse=' '),'\n'))
    if(!is.null(out_file)) cat(paste(cost, '=', format(cost_value,scientific=TRUE,digits=4), 'converged =', as.character(cvg), 'parval =', paste(format(pvl$parval[include],scientific=TRUE,digits=4), collapse=' '),'\n'), file = out_file, append = TRUE)
    
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
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use
#' @param version MODFLOW version to use if \code{executable} is missing; defaults to MODFLOW-2005
#' @param par central parameter values (for all or only included parameters); parameter value file values are used if par is not provided (default)
#' @param include logical vector indicating which parameters in the parameter value file to include in the sensitivity analysis
#' @param ... optional arguments passed to \code{rmf_execute}
#' @details Only works with models using MODFLOW parameters and having a head predictions output file as defined in the HOB object
#' @return object of class \code{sen} which is a list with dimensionless scaled sensitivities (dss) and composite scaled sensitivies (css)
#' @export
#' @seealso \code{\link{rmf_execute}}, \code{\link{rmf_create_pvl}} & \code{\link{rmf_create_hob}} 
rmf_analyze <- function(file = {cat('Please select nam file ...\n'); file.choose()},
                        executable = NULL,
                        version = c("MODFLOW-2005", "MODFLOW-NWT", "MODFLOW-OWHM", "MODFLOW-LGR", "MODFLOW-CFP"),
                        par = NULL,
                        include = NULL,
                        ...) {
  dir <- dirname(file)
  nam <- rmf_read_nam(file)
  if(!('PVAL' %in% nam$ftype) || !('HOB' %in% nam$ftype)) stop('rmf_analyze only works with models having PVL and HOB input', call. = FALSE)
  pvl <- rmf_read_pvl(file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(file.path(dir, nam$fname[which(nam$ftype=='HOB')]))
  if(hob$iuhobsv == 0 || toupper(nam$ftype[which(nam$nunit == hob$iuhobsv)]) != 'DATA') {
    stop('rmf_analyze only works with models having a head predictions output file. This can be created by setting the iuhobsv argument of the HOB file to a non-zero value', call. = FALSE)
  }
  pvl_org <- pvl
  
  if(is.null(par)) par <- pvl$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  
  # if par only has values for include = TRUE
  if(length(par) != length(pvl$parval)) {
    par2 <- par
    par <- pvl$parval
    par[which(include)] <- par2
  } 
  
  # copy and write original pvl
  rmf_write_pvl(pvl_org, file = file.path(dir, paste('original', nam$fname[which(nam$ftype=='PVAL')], sep = '_')))
  
  # initial run
  rmf_execute(file = file, executable = executable, par = par, version = version, convergence = FALSE, ...)
  hpr_orig <- rmf_read_hpr(file.path(dir, nam$fname[which(nam$nunit == hob$iuhobsv)]))
  
  sens <- list()
  sens$dss <- matrix(NA, nrow = length(hob$obsnam), ncol = length(pvl$parval))
  sens$css <- pvl$parval*NA
  
  # dss & css
  for(i in which(include)) {
    pvl$parval <- par
    pvl$parval[i] <- pvl$parval[i]*1.01
    rmf_write_pvl(pvl, file = file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
    rmf_execute(file = file, executable = executable, par = NULL, version = version, convergence = FALSE, ...)
    hpr <- rmf_read_hpr(file.path(dir, nam$fname[which(nam$nunit == hob$iuhobsv)]))
    sens$dss[,i] <- (hpr$simulated - hpr_orig$simulated)/(0.01)
    sens$css[i] <- sqrt(sum(sens$dss[,i]^2)/hob$nh)
  }
  
  #  write original pvl and run
  rmf_write_pvl(pvl_org, file = file.path(dir, nam$fname[which(nam$ftype=='PVAL')]))
  rmf_execute(file = file, executable = executable, par = NULL, version = version, convergence = FALSE, ...)
  
  sens$parnam <- pvl$parnam
  class(sens) <- 'sen'
  return(sens)
}
