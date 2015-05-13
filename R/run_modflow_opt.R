#' Run a MODFLOW model optimization, based on the parameter value file
#' 
#' \code{run_modflow_optim} runs a MODFLOW optimization.
#' 
#' @param file Name file; typically "*.nam"
#' @param dir Directory of the namefile
#' @param modflow_executable name of the MODFLOW executable to use
#' @param par initial parameter values (for all or only included parameters); parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the optimization
#' @param ... further arguments provided to \code{optim}
#' @return \code{optim} results with the full list of parameters
#' @export
run_modflow_opt <- function(file,dir=getwd(),modflow_executable='mf2005',par=NULL,include=NULL, ...)
{
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
  optim_modflow <- function(par_include)
  {
    pvl$Parval <- par
    pvl$Parval[which(include)] <- par_include
    write_pvl(pvl, file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
    run_modflow(file,dir,modflow_executable)
    rmse <- performance(read_hpr(paste0(dir,'/',nam$Fname[which(nam$Nunit==hob$IUHOBSV)])))$rmse
    cat(paste('RMSE=',format(rmse,scientific=TRUE,digits=4),'Parval=',paste(format(par_include,scientific=TRUE,digits=4),collapse=' '),'\n')) # file=report, append=T
    return(rmse)
  }
  opt <- optim(par[which(include)],optim_modflow, ...)
  par2 <- opt$par
  opt$par <- par
  opt$par[which(include)] <- par2
  opt$included <- include
  return(opt)
}