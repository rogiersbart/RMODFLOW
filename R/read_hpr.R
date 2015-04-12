#' Read a MODFLOW head predictions file
#' 
#' \code{read_hpr} reads in a MODFLOW head predictions file and returns it as an \code{\link{RMODFLOW}} hpr object.
#' 
#' @param file Filename; typically *.hpr
#' @return Object of class hpr
#' @export
read_hpr <- function(file)
{
  hpr <- read.table(file,header=T)
  class(hpr) <- c('data.frame','hpr')
  return(hpr)
}