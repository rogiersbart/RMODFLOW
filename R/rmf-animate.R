#' Generic function to make animations from RMODFLOW arrays
#' 
#' @rdname rmf_animate
#' @export
rmf_animate <- function(...) {
  UseMethod('rmf_animate')
}
