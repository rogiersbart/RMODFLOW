#' Generic function to export animations from RMODFLOW arrays
#' 
#' @rdname export_animation
#' @export
export_animation <- function(...) {
  UseMethod('export_animation')
}
