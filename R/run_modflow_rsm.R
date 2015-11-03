#' Run a MODFLOW model response surface mapping
#' 
#' \code{run_modflow_rsm} runs a MODFLOW response surface mapping.
#' 
#' @param file path to name file; typically '*.nam'
#' @param modflow_executable name of the MODFLOW executable to use
#' @param par central parameter values (for all or only included parameters); current parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the mapping
#' @param trans vector of transformations; currently only 'log' is supported
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param n number of intervals sampled for each parameter
#' @return an rsm object with the full list of parameters and the response value
#' @export
run_modflow_rsm <- function(file,modflow_executable='mf2005',par=NULL,include=NULL, trans=NULL, lower, upper, n)
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
      pvl$Parval <- par
      pvl$Parval[which(include)] <- as.numeric(par_include)
      if(!is.null(trans)) pvl$Parval[which(trans=='log')] <- exp(pvl$Parval[which(trans=='log')])
      write_pvl(pvl, file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
      run_modflow(paste0(dir,'/',file),modflow_executable)
      rmse <- performance(read_hpr(paste0(dir,'/',nam$Fname[which(nam$Nunit==hob$IUHOBSV)])))$rmse
      cat(paste('\n RMSE=',format(rmse,scientific=TRUE,digits=4),'Parval=',paste(format(pvl$Parval[include],scientific=TRUE,digits=4),collapse=' '),'\n')) # file=report, append=T
      return(rmse)
    }
    for(i in 1:nrow(rsm))
    {
      rsm$rmse[i] <- rsm_modflow(rsm[i,1:sum(include)])
    }
  names(rsm) <- c(paste0(trans,pvl$PARNAM)[include],'rmse')
  # add attributes later
  class(rsm) <- c('rsm','data.frame')
  return(rsm)
}
