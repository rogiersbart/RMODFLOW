#' Read a MODFLOW name file
#' 
#' \code{read_nam} reads in a MODFLOW name file and returns it as an \code{\link{RMODFLOW}} nam object.
#' 
#' @param file filename; typically '*.nam'
#' @return object of class nam
#' @export
read_nam <- function(file = {cat('Please select nam file ...\n'); file.choose()}) {
  warning('LSTLVL, Option and comments not implemented yet.')
  nam <- read.table(file)
  names(nam) <- c('ftype','nunit','fname')
  nam$fname <- as.character(nam$fname)
  class(nam) <- c('nam','data.frame')
  return(nam)
}
