#' Write a MODFLOW Newton solver package file
#' 
#' @param nwt an \code{\link{RMODFLOW}} nwt object
#' @param file filename to write to; typically '*.nwt'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_nwt}}, \code{\link{rmf_create_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_write_nwt <- function(nwt,
                          file = {cat('Please select nwt file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Newton solver package File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(nwt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(nwt$headtol, nwt$fluxtol, nwt$maxiterout, nwt$thickfact, nwt$linmeth, nwt$iprnwt, nwt$ibotav, toupper(nwt$options), 
                       rmfi_ifelse0(toupper(nwt$options) == "SPECIFIED", paste(nwt$dbdtheta, nwt$dbdkappa, nwt$dbdgamma, nwt$momfact, nwt$backflag, nwt$maxbackiter, nwt$backtol, nwt$backreduce), '') , file=file)
  
  # data set 2
  if(toupper(nwt$options) == "SPECIFIED") {
    
    if(nwt$linmeth == 1) {
      rmfi_write_variables(nwt$maxitinner, nwt$ilumethod, nwt$levfill, nwt$stoptol, nwt$msdr, file = file)
    } else if(nwt$linmeth == 2) {
      rmfi_write_variables(nwt$iacl, nwt$norder, nwt$level, nwt$north, nwt$iredsys, nwt$rrctols, nwt$idroptol, nwt$epsrn, nwt$hclosexmd, nwt$mxiterxmd, file = file)
    }
  }
  
}
