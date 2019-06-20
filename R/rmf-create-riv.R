#' Create an \code{RMODFLOW} riv object.
#' 
#' \code{rmf_create_riv} creates an \code{RMODFLOW} riv object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parm}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parm}) elements; defines the rivers. 
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