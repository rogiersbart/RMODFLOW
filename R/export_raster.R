#' Generic function to export GIS raster layers from RMODFLOW arrays
#' 
#' @rdname export_raster
#' @export
export_raster <- function(...) {
  UseMethod('export_raster')
}
