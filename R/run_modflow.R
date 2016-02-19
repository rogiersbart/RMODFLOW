#' Generic function to run a modflow model
#' 
#' @rdname run_modflow
#' @export
run_modflow <- function(...) {
  UseMethod('run_modflow')
}
