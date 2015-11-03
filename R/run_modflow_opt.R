#' Run a MODFLOW model optimization, based on the parameter value file
#' 
#' \code{run_modflow_opt} runs a MODFLOW optimization.
#' 
#' @param file path to name file; typically '*.nam'
#' @param modflow_executable name of the MODFLOW executable to use
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
run_modflow_opt <- function(file,modflow_executable='mf2005',par=NULL,include=NULL, trans=NULL, method='Nelder-Mead', lower=-Inf, upper=Inf, control=list(), ...)
{
  dir <- dirname(file)
  file <- basename(file)
  nam <- read_nam(paste0(dir,'/',file))
  pvl <- read_pvl(paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
  hob <- read_hob(paste0(dir,'/',nam$Fname[which(nam$Ftype=='HOB')]))
  if(is.null(par)) par <- pvl$Parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  if(length(par)!=length(pvl$Parval)) 
  {
    par2 <- par
    par <- pvl$Parval
    par[which(include)] <- par2
  }
  if(is.infinite(lower)) lower <- rep(lower,pvl$NP)
  if(is.infinite(upper)) upper <- rep(upper,pvl$NP)
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
    pvl$Parval <- par
    pvl$Parval[which(include)] <- par_include
    if(!is.null(trans)) pvl$Parval[which(trans=='log')] <- exp(pvl$Parval[which(trans=='log')])
    write_pvl(pvl, file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
    run_modflow(paste0(dir,'/',file),modflow_executable)
    rmse <- performance(read_hpr(paste0(dir,'/',nam$Fname[which(nam$Nunit==hob$IUHOBSV)])))$rmse
    cat(paste('\n RMSE=',format(rmse,scientific=TRUE,digits=4),'Parval=',paste(format(pvl$Parval[include],scientific=TRUE,digits=4),collapse=' '),'\n')) # file=report, append=T
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
