#' Create an \code{RMODFLOW} chd object.
#' 
#' \code{rmf_create_chd} creates an \code{RMODFLOW} chd object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parm}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parm}) elements; defines the constant head cells 
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
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$ichdcb <- NULL
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('chd', 'rmf_package')
  return(obj)
  
}
