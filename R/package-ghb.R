#' Create an \code{RMODFLOW} ghb object.
#' 
#' \code{rmf_create_ghb} creates an \code{RMODFLOW} ghb object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parameter}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parameter}) elements; defines the general-head boundary cells. 
#' @param dis dis object
#' @param ighbcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 (cell-by-cell flow terms will not be written)
#' @param noprint logical, should the printing of GHB cells to the listing file be suppressed ? Defaults to \code{FALSE}
#' @param aux optional character vector specifying the names of the auxiliary variables. These variables should also be included in the \code{rmf_list} objects that are supplied; defaults to \code{NULL}
#'
#' @return \code{RMODFLOW} ghb object
#' @export
#' @seealso \code{\link{rmf_read_ghb}}, \code{\link{rmf_write_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}

rmf_create_ghb <-  function(..., 
                            dis,
                            ighbcb = 0,
                            noprint = FALSE,
                            aux = NULL
                            
) {
  vars <- c('bhead', 'cond')
  arg <- rmfi_create_bc_list(arg = list(...), dis = dis, varnames = vars, aux = aux)
  
  # create ghb object
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$ighbcb <- ighbcb
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('ghb', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW general-head boundary file
#' 
#' \code{rmf_read_ghb} reads in a MODFLOW general-head boundary file and returns it as an \code{RMODFLOW} ghb object.
#'
#' @param file filename; typically '*.ghb'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.

#' @return \code{RMODFLOW} ghb object
#' @export
#' @seealso \code{\link{rmf_write_ghb}}, \code{\link{rmf_create_ghb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?ghb.htm}


rmf_read_ghb <-  function(file = {cat('Please select general-head boundary file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('bhead', 'cond')
  option <- c('NOPRINT' = FALSE)
  lines <-  readr::read_lines(file)
  
  input <- rmfi_read_bc_list(lines = lines, dis = dis, varnames = vars, option = option, scalevar = 5, ...)
  
  obj <- rmf_create_ghb(input$rmf_lists, dis = dis, ighbcb = input$icb, noprint = unname(input$option['NOPRINT']), aux = input$aux)
  comment(obj) <- input$comments
  return(obj)
}

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
