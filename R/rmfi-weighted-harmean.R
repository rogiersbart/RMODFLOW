#' Calculate a weighted harmonic mean
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{sum}}
#' @seealso \code{\link{rmfi_weighted_geomean}}, \code{\link{weighted.mean}}, \code{\link{rmfi_harmean}} \code{\link{rmfi_geomean}} and \code{\link{mean}}
#' @keywords internal
rmfi_weighted_harmean <- function(x, w, ...) {
  return(sum(w)/(sum(w/x, ...)))
}
