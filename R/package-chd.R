#' Create an \code{RMODFLOW} chd object.
#' 
#' \code{rmf_create_chd} creates an \code{RMODFLOW} chd object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parameter}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parameter}) elements; defines the constant head cells 
#' @param dis dis object
#' @param noprint logical, should the printing of CHD cells to the listing file be suppressed ? Defaults to \code{FALSE}
#' @param aux optional character vector specifying the names of the auxiliary variables. These variables should also be included in the \code{rmf_list} objects that are supplied; defaults to \code{NULL}
#'
#' @return \code{RMODFLOW} chd object
#' @export
#' @seealso \code{\link{rmf_read_chd}}, \code{\link{rmf_write_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}

rmf_create_chd <-  function(..., 
                            dis,
                            noprint = FALSE,
                            aux = NULL
                            
) {
  vars <- c('shead', 'ehead')
  arg <- rmfi_create_bc_list(arg = list(...), dis = dis, varnames = vars, aux = aux)
  
  # create chd object
  obj <- arg[c("np", "mxl", "instances", "mxact", "itmp")]
  obj$ichdcb <- NULL
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$np > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('chd', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW time-variant specified-head file
#' 
#' \code{rmf_read_chd} reads in a MODFLOW time-variant specified-head file and returns it as an \code{RMODFLOW} chd object.
#'
#' @param file filename; typically '*.chd'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} chd object
#' @export
#' @seealso \code{\link{rmf_write_chd}}, \code{\link{rmf_create_chd}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?chd.htm}
rmf_read_chd <-  function(file = {cat('Please select time-variant specified-head file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('shead', 'ehead')
  option <- c('NOPRINT' = FALSE)
  lines <-  readr::read_lines(file)
  
  input <- rmfi_parse_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = c(4,5), ...)
  
  obj <- rmf_create_chd(input$rmf_lists, dis = dis, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
  
}

#' Write a MODFLOW time-variant specified-head file
#'
#' \code{rmf_write_chd} writes a MODFLOW time-variant specified-head file based on an \code{RMODFLOW} chd object
#' 
#' @param chd an \code{RMODFLOW} chd object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.chd'
#' @param ... arguments passed to \code{rmfi_write_variables} and \code{rmfi_write_list} when writing a fixed format file.

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
