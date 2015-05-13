#' Run a MODFLOW model
#' 
#' \code{run_modflow} runs a MODFLOW model.
#' 
#' @param file Name file; typically "*.nam"
#' @param dir Directory of the namefile
#' @param modflow_executable name of the MODFLOW executable to use
#' @export
run_modflow <- function(file,dir=getwd(),modflow_executable='mf2005',par=NULL)
{
  if(!is.null(par))
  {
    nam <- read_nam(paste0(dir,'/',file))
    pvl <- read_pvl(paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
    pvl$Parval <- par
    write_pvl(pvl, file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='PVAL')]))
  }
  shell(paste('cd',dir,'&',modflow_executable,file),mustWork=TRUE)
}