#' Create an \code{RMODFLOW} modflow object
#' 
#' \code{create_modflow} creates an \code{RMODFLOW} modflow object.
#' 
#' @param ... RMODFLOW objects to be included in the modflow model; if a nam object is not provided, it as added automatically
#' @return Object of class modflow
#' @export
#' @seealso \code{\link{read_modflow}}, \code{\link{write_modflow}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html}
create_modflow <- function(...) {
  
  modflow <- list()
  ellipsis <- list(...)
  for(i in 1:length(ellipsis)) {
    modflow[[class(ellipsis[[i]])[1]]] <- ellipsis[[i]]
  }
  if(!'nam' %in% names(modflow)) {
    modflow$nam <- create_nam(...)
  }

  class(modflow) <- c('modflow')
  return(modflow)
}
