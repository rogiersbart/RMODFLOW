#' Create an \code{RMODFLOW} wel object.
#' 
#' \code{rmf_create_wel} creates an \code{RMODFLOW} wel object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parameter}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parameter}) elements; defines the wells. 
#' @param dis dis object
#' @param iwelcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 (cell-by-cell flow terms will not be written)
#' @param noprint logical, should the printing of WEL cells to the listing file be suppressed ? Defaults to \code{FALSE}
#' @param aux optional character vector specifying the names of the auxiliary variables. These variables should also be included in the \code{rmf_list} objects that are supplied; defaults to \code{NULL}
#'
#' @return \code{RMODFLOW} wel object
#' @export
#' @seealso \code{\link{rmf_read_wel}}, \code{\link{rmf_write_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_create_wel <-  function(..., 
                            dis,
                            iwelcb = 0,
                            noprint = FALSE,
                            aux = NULL
                            
) {
  vars <- c('q')
  arg <- rmfi_create_bc_list(arg = list(...), dis = dis, varnames = vars, aux = aux)
  
  # create wel object
  obj <- arg[c("np", "mxl", "instances", "mxact", "itmp")]
  obj$iwelcb <- iwelcb
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$np > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('wel', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW well file
#' 
#' \code{rmf_read_wel} reads in a MODFLOW well file and returns it as an \code{RMODFLOW} wel object.
#'
#' @param file filename; typically '*.wel'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} wel object
#' @export
#' @seealso \code{\link{rmf_write_wel}}, \code{\link{rmf_create_wel}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?wel.htm}

rmf_read_wel <-  function(file = {cat('Please select well file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('q')
  option <- c('NOPRINT' = FALSE)
  lines <-  readr::read_lines(file)
  
  input <- rmfi_parse_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 4, ...)
  
  obj <- rmf_create_wel(input$rmf_lists, dis = dis, iwelcb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}

#' Write a MODFLOW well file
#'
#' \code{rmf_write_wel} writes a MODFLOW well file based on an \code{RMODFLOW} wel object
#' 
#' @param wel an \code{RMODFLOW} wel object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.wel'
#' @param ... arguments passed to \code{rmfi_write_variables} and \code{rmfi_write_list} when writing a fixed format file.

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
