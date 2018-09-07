#' Write a MODFLOW strongly implicit procedure package
#' 
#' \code{rmf_write_sip} writes a MODFLOW strongly implicit procedure file based on an \code{RMODFLOW} sip object
#' 
#' @param sip an \code{RMODFLOW} sip object
#' @param file filename to write to; typically '*.sip'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_sip}}, \code{\link{rmf_create_sip}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?sip.htm}

rmf_write_sip = function(sip, file={cat('Please choose sip file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Strongly Implicit Procedure Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(sip)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(sip$mxiter, sip$nparm, file=file, ...)
  
  # data set 2
  rmfi_write_variables(sip$accl, sip$hclose, sip$ipcalc, sip$wseed, sip$iprsip, file=file, ...)
  
}