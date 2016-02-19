#' Generic function to get information at a certain grid cell
#' 
#' @rdname cell_info
#' @export
cell_info <- function(...) {
  UseMethod('cell_info')
}
