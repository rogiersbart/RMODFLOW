#' Generic function to export GIS vector layers from RMODFLOW arrays
#' 
#' @rdname export_shapefile
#' @export
export_vector <- function(...) {
  UseMethod('export_vector')
}
