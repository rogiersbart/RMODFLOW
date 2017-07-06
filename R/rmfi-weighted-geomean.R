#' Calculate a weighted geometric mean
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{rmfi_weighted_harmean}}, \code{\link{weighted.mean}}, \code{\link{rmfi_geomean}}, \code{\link{rmfi_harmean}} and \code{\link{mean}}
rmfi_weighted_geomean <- function(x, w, ...) {
  return(prod(x^w, ...)^(1/sum(w)))
}
