#' Create an \code{RMODFLOW} drn object.
#' 
#' \code{rmf_create_drn} creates an \code{RMODFLOW} drn object
#'
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parm}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parm}) elements; defines the drains. 
#' @param dis dis object
#' @param idrncb flag and unit number for writing cell-by-cell flow terms; defaults to 0 (cell-by-cell flow terms will not be written)
#' @param noprint logical, should the printing of DRN cells to the listing file be suppressed ? Defaults to \code{FALSE}
#' @param aux optional character vector specifying the names of the auxiliary variables. These variables should also be included in the \code{rmf_list} objects that are supplied; defaults to \code{NULL}
#'
#' @return \code{RMODFLOW} drn object
#' @export
#' @seealso \code{\link{rmf_read_drn}}, \code{\link{rmf_write_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}


rmf_create_drn <-  function(..., 
                          dis,
                          idrncb = 0,
                          noprint = FALSE,
                          aux = NULL
                          
) {
  vars <- c('elevation', 'cond')
  arg <- rmfi_create_bc_list(arg = list(...), dis = dis, varnames = vars, aux = aux)
  
  # create drn object
  obj <- list()

  obj$dimensions <- arg$dimensions
  obj$idrncb <- idrncb
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('drn', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW drain file
#' 
#' \code{rmf_read_drn} reads in a MODFLOW drain file and returns it as an \code{RMODFLOW} drn object.
#'
#' @param file filename; typically '*.drn'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} drn object
#' @importFrom readr read_lines
#' @seealso \code{\link{rmf_write_drn}}, \code{\link{rmf_create_drn}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?drn.htm}

rmf_read_drn <-  function(file = {cat('Please select drain file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('elevation', 'cond')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 5, ...)
  
  obj <- rmf_create_drn(input$rmf_lists, dis = dis, idrncb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}


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


