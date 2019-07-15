#' Create an \code{RMODFLOW} riv object.
#' 
#' \code{rmf_create_riv} creates an \code{RMODFLOW} riv object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parameter}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parameter}) elements; defines the rivers. 
#' @param dis dis object
#' @param irivcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 (cell-by-cell flow terms will not be written)
#' @param noprint logical, should the printing of RIV cells to the listing file be suppressed ? Defaults to \code{FALSE}
#' @param aux optional character vector specifying the names of the auxiliary variables. These variables should also be included in the \code{rmf_list} objects that are supplied; defaults to \code{NULL}
#'
#' @return \code{RMODFLOW} riv object
#' @export
#' @seealso \code{\link{rmf_read_riv}}, \code{\link{rmf_write_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}

rmf_create_riv <-  function(..., 
                            dis,
                            irivcb = 0,
                            noprint = FALSE,
                            aux = NULL
                            
) {
  vars <- c('stage', 'cond', 'rbot')
  arg <- rmfi_create_bc_list(arg = list(...), dis = dis, varnames = vars, aux = aux)
  
  # create riv object
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$irivcb <- irivcb
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('riv', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW river file
#' 
#' \code{rmf_read_riv} reads in a MODFLOW river file and returns it as an \code{RMODFLOW} riv object.
#'
#' @param file filename; typically '*.riv'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} riv object
#' @export
#' @seealso \code{\link{rmf_write_riv}}, \code{\link{rmf_create_riv}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?riv.htm}

rmf_read_riv <-  function(file = {cat('Please select river file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('stage', 'cond', 'rbot')
  option <- c('NOPRINT' = FALSE)
  lines <-  readr::read_lines(file)
  
  input <- rmfi_parse_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 5, ...)
  
  obj <- rmf_create_riv(input$rmf_lists, dis = dis, irivcb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}

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

