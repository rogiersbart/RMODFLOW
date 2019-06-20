#' Create an \code{RMODFLOW} wel object.
#' 
#' \code{rmf_create_wel} creates an \code{RMODFLOW} wel object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parm}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parm}) elements; defines the wells. 
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
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$iwelcb <- iwelcb
  obj$option <- c('noprint' = noprint)
  obj$aux <- aux
  obj$data <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  class(obj) <- c('wel', 'rmf_package')
  return(obj)
  
}