#' Generic function to convert rmf_array objects to tibbles
#' 
#' @rdname rmf_as_tibble
#' @export
rmf_as_tibble <- function(...) {
  UseMethod('rmf_as_tibble')
}