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
