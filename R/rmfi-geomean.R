#' Calculate a geometric mean
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{rmfi_harmean}} and \code{\link{mean}}
rmfi_geomean <- function(x, ...) {
  return(prod(x, ...) ^ (1 / length(x)))
}
