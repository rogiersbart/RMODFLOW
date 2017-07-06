#' Create an \code{RMODFLOW} modflow object
#' 
#' \code{rmf_create} creates an \code{RMODFLOW} modflow object.
#' 
#' @param ... RMODFLOW objects to be included in the modflow model; if a nam object is not provided, it as added automatically
#' @return Object of class modflow
#' @export
#' @seealso \code{\link{rmf_read}}, \code{\link{rmf_write}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html}
rmf_create <- function(...) {
  
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

#' @describeIn rmf_create Deprecated function name
create_modflow <- function(...) {
  .Deprecated(new = "rmf_create", old = "create_modflow")
  rmf_create(...)
}
