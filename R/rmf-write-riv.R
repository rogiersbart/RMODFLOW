#' Write a MODFLOW river file
#'
#' \code{rmf_write_riv} writes a MODFLOW river file based on an \code{RMODFLOW} riv object
#' 
#' @param riv an \code{RMODFLOW} riv object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.riv'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_riv}}, \code{\link{rmf_create_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}


rmf_write_riv <-  function(riv, dis = rmf_read_dis(), file={cat('Please choose riv file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('stage', 'cond', 'rbot')
  header <-  'River Package'
  package <- 'riv'
  partyp <- 'RIV'
  rmfi_write_bc_list(file = file, obj = riv, dis = dis, varnames = vars, header = header, package = package, partyp = partyp, ...) 
  
}
