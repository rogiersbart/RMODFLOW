#' Write a MODFLOW general-head boundary file
#'
#' \code{rmf_write_ghb} writes a MODFLOW general-head boundary file based on an \code{RMODFLOW} ghb object
#' 
#' @param ghb an \code{RMODFLOW} ghb object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.ghb'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.

#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_ghb}}, \code{\link{rmf_create_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}


rmf_write_ghb <-  function(ghb, dis = rmf_read_dis(), file={cat('Please choose ghb file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('bhead', 'cond')
  header <-  'General-Head Boundary Package'
  package <- 'ghb'
  partyp <- 'GHB'
  rmfi_write_bc_list(file = file, obj = ghb, dis = dis, varnames = vars, header = header, package = package, partyp = partyp, ...) 
  
}
