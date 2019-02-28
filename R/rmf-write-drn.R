#' Write a MODFLOW drain file
#'
#' \code{rmf_write_drn} writes a MODFLOW drain file based on an \code{RMODFLOW} drn object
#' 
#' @param drn an \code{RMODFLOW} drn object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.drn'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_drn}}, \code{\link{rmf_create_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}

rmf_write_drn <-  function(drn, dis = rmf_read_dis(), file={cat('Please choose drn file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('elevation', 'cond')
  header <-  'Drain Package'
  package <- 'drn'
  partyp <- 'DRN'
  rmfi_write_bc_list(file = file, obj = drn, dis = dis, varnames = vars, header = header, package = package, partyp = partyp, ...) 
  
}
