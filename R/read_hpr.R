#' Read a MODFLOW head predictions file
#' 
#' \code{read_hpr} reads in a MODFLOW head predictions file and returns it as an \code{\link{RMODFLOW}} hpr object.
#' 
#' @param file filename; typically '*.hpr'
#' @return object of class hpr
#' @export
read_hpr <- function(file = {cat('Please select hpr file ...\n'); file.choose()}) {
  hpr <- read.table(file,header=T)
  class(hpr) <- c('hpr','data.frame')
  return(hpr)
}
