#' Calculate a harmonic mean
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{mean}}
#' @seealso \code{\link{rmfi_geomean}} and \code{\link{mean}}
rmfi_harmean <- function(x, ...) {
  return(1 / (mean(1 / x, ...)))
}
