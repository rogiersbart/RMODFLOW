#' Generic function to run a modflow model
#' 
#' @rdname rmf_run_modflow
#' @export
rmf_run_modflow <- function(...) {
  UseMethod('rmf_run_modflow')
}

#' @describeIn rmf_run_modflow Deprecated function name
#' @export
run_modflow <- function(...) {
  .Deprecated(new = "rmf_run_modflow", old = "run_modflow")
  rmf_run_modflow(...)
}
