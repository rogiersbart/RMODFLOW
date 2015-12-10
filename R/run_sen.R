#' Run a MODFLOW model sensitivity analysis, based on the parameter value file
#' 
#' \code{run_modflow_sen} performs a MODFLOW model sensitivity analysis.
#' 
#' @param file path to name file; typically '*.nam'
#' @param modflow_executable name of the MODFLOW executable to use
#' @param par central parameter values (for all or only included parameters); parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the sensitivity analysis
#' @return sensitivity analysis results
#' @export
run_sen <- function(file,modflow_executable='mf2005',par=NULL,include=NULL)
{
  dir <- dirname(file)
  file <- basename(file)
  nam <- read_nam(paste0(dir,'/',file))
  pvl <- read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
  hob <- read_hob(paste0(dir,'/',nam$fname[which(nam$ftype=='HOB')]))
  if(is.null(par)) par <- pvl$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  if(length(par)!=length(pvl$parval)) 
  {
    par2 <- par
    par <- pvl$parval
    par[which(include)] <- par2
  } 
  run_modflow(paste0(dir,'/',file),modflow_executable,par)
  hpr_orig <- read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)]))
  sens <- list()
  sens$dss <- matrix(NA,nrow=length(hob$obsnam),ncol=length(pvl$parval))
  sens$css <- pvl$parval*NA
  for(i in which(include))
  {
    pvl$parval <- par
    pvl$parval[i] <- pvl$parval[i]*1.01
    write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    run_modflow(paste0(dir,'/',file),modflow_executable)
    hpr <- read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)]))
    sens$dss[,i] <- (hpr$simulated_equivalent-hpr_orig$simulated_equivalent)/(0.01)
    sens$css[i] <- sqrt(sum(sens$dss[,i]^2)/hob$nh)
  }
  sens$parnam <- pvl$parnam
  class(sens) <- 'sen'
  return(sens)
}
