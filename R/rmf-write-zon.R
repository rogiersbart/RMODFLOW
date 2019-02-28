#' Write a MODFLOW zone file
#' 
#' \code{rmf_write_zon} writes a MODFLOW zone file based on a \code{RMODFLOW} zon object
#' 
#' @param zon an \code{RMODFLOW} zon object
#' @param file filename to write to; typically '*.zon'
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_zon}}, \code{\link{rmf_create_zon}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?zone.htm}

rmf_write_zon <-  function(zon, file = {cat('Please choose zon file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Zone File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(zon)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(zon$nzn, file=file)
  
  for (i in 1:zon$nzn){
    
    # data set 2
    rmfi_write_variables(zon$zonnam[i], file=file)
    
    # data set 3
    rmfi_write_array(zon$izon[[i]], file=file, ...)
    
  }
}