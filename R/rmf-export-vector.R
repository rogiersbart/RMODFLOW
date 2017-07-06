#' Generic function to export GIS vector layers from RMODFLOW arrays
#' 
#' @rdname rmf_export_vector
#' @export
rmf_export_vector <- function(...) {
  UseMethod('rmf_export_vector')
}

#' @describeIn rmf_export_vector Deprecated function name
#' @export
export_vector <- function(...) {
  .Deprecated(new = "rmf_export_vector", old = "export_vector")
  rmf_export_vector(...)
}
