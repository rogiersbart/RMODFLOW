#' Read a MODFLOW head predictions file
#' 
#' \code{rmf_read_hpr} reads in a MODFLOW head predictions file and returns it as an \code{\link{RMODFLOW}} hpr object.
#' 
#' @param file filename; typically '*.hpr'
#' @return object of class hpr
#' @export
rmf_read_hpr <- function(file = {cat('Please select hpr file ...\n'); file.choose()}) {
  hpr <- read.table(file,header=T)
  class(hpr) <- c('hpr','data.frame')
  return(hpr)
}

#' @describeIn rmf_read_hpr Deprecated function name
#' @export
read_hpr <- function(...) {
  .Deprecated(new = "rmf_read_hpr", old = "read_hpr")
  rmf_read_hpr(...)
}

#' @describeIn rmf_read_hpr Compatible with default ModelMuse file extension
#' @export
rmf_read_hob_out <- function(...) {
  rmf_read_hpr(...)
}
