#' Write a MODFLOW direct solver package
#' 
#' \code{rmf_write_de4} writes a MODFLOW direct solver file based on an \code{RMODFLOW} de4 object
#' 
#' @param de4 an \code{RMODFLOW} de4 object
#' @param file filename to write to; typically '*.de4'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_de4}}, \code{\link{rmf_create_de4}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?de4.htm}

rmf_write_de4 = function(de4, file={cat('Please choose de4 file to overwrite or provide new filename ...\n'); file.choose()} ){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Direct Solver Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(de4)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  write_modflow_variables(de4$itmx, de4$mxup, de4$mxlow, de4$mxbw, file=file)
  
  # data set 2
  write_modflow_variables(de4$ifreq, de4$mutd4, de4$accl, de4$hclose, de4$iprd4, file=file)
  
}