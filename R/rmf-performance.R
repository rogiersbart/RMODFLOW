#' Generic function to get model performance measures
#' 
#' @rdname rmf_performance
#' @export
rmf_performance <- function(...) {
  UseMethod('rmf_performance')
}

#' @describeIn rmf_performance Deprecated function name
#' @export
performance <- function(...) {
  .Deprecated(new = "rmf_performance", old = "performance")
  rmf_performance(...)
}
