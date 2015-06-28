#' Run a MODFLOW model
#' 
#' \code{run_modflow} runs a MODFLOW model.
#' 
#' @param file path to name file; typically '*.nam'
#' @param modflow_executable name of the MODFLOW executable to use
#' @param par vector of parameter value file parameter values to run the model with
#' @export
run_modflow <- function(file,modflow_executable='mf2005',par=NULL)
{
  dir <- dirname(file)
  file <- basename(file)
  if(!is.null(par))
  {
    nam <- read_nam(paste0(dir,'/',file))
    pvl <- read_pvl(paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
    pvl$Parval <- par
    write_pvl(pvl, file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
  }
  shell(paste('cd',dir,'&',modflow_executable,file),mustWork=TRUE)
}