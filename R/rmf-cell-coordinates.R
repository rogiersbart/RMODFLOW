#' Generic function to get cell coordinates
#' 
#' @rdname rmf_cell_coordinates
#' @export
rmf_cell_coordinates <- function(...) {
  UseMethod('rmf_cell_coordinates')
}

#' @describeIn rmf_cell_coordinates Deprecated function name
#' @export
cell_coordinates <- function(...) {
  .Deprecated(new = "rmf_cell_coordinates", old = "cell_coordinates")
  rmf_cell_coordinates(...)
}
