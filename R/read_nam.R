#' Read a MODFLOW name file
#' 
#' \code{read_nam} reads in a MODFLOW name file and returns it as an \code{\link{RMODFLOW}} nam object.
#' 
#' @param file Filename; typically "*.nam"
#' @return Object of class nam
#' @export
read_nam <- function(file)
{
  warning('LSTLVL, Option and comments not implemented yet.')
  nam <- read.table(file)
  names(nam) <- c('Ftype','Nunit','Fname')
  nam$Fname <- as.character(nam$Fname)
  class(nam) <- c('nam','data.frame')
  return(nam)
}