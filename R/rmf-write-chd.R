#' Write a MODFLOW time-variant specified-head file
#'
#' \code{rmf_write_chd} writes a MODFLOW time-variant specified-head file based on an \code{RMODFLOW} chd object
#' 
#' @param chd an \code{RMODFLOW} chd object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.chd'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.

#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_chd}}, \code{\link{rmf_create_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}

rmf_write_chd <-  function(chd, dis = rmf_read_dis(), file={cat('Please choose chd file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('shead', 'ehead')
  header <-  'Time-Variant Specified Head Package'
  package <- 'chd'
  partyp <- 'CHD'
  rmfi_write_bc_list(file = file, obj = chd, dis = dis, varnames = vars, header = header, package = package, partyp = partyp, ...) 
  
  
  
}