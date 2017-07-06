#' Generic function to get information at a certain grid cell
#' 
#' @rdname rmf_cell_info
#' @export
rmf_cell_info <- function(...) {
  UseMethod('rmf_cell_info')
}

#' @describeIn rmf_cell_info Deprecated function name
cell_info <- function(...) {
  .Deprecated(new = "rmf_cell_info", old = "cell_info")
  rmf_cell_info(...)
}
