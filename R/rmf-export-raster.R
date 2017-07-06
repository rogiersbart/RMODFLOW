#' Generic function to export GIS raster layers from RMODFLOW arrays
#' 
#' @rdname rmf_export_raster
#' @export
rmf_export_raster <- function(...) {
  UseMethod('rmf_export_raster')
}
