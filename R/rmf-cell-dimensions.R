#' Generic function to get cell dimensions
#' 
#' @rdname rmf_cell_dimensions
#' @export
rmf_cell_dimensions <- function(...) {
  UseMethod('rmf_cell_dimensions')
}

#' @describeIn rmf_cell_dimensions Deprecated function name
cell_dimensions <- function(...) {
  .Deprecated(new = "rmf_cell_dimensions", old = "cell_dimensions")
  rmf_cell_dimensions(...)
}

