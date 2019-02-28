#' Write a MODFLOW well file
#'
#' \code{rmf_write_wel} writes a MODFLOW well file based on an \code{RMODFLOW} wel object
#' 
#' @param wel an \code{RMODFLOW} wel object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.wel'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.

#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_wel}}, \code{\link{rmf_create_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_write_wel <-  function(wel, dis = rmf_read_dis(), file={cat('Please choose wel file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('q')
  header <-  'Well Package'
  package <- 'wel'
  partyp <- 'Q'
  rmfi_write_bc_list(file = file, obj = wel, dis = dis, varnames = vars, header = header, package = package, partyp = partyp, ...) 
  
}
