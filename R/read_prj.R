#' Read a projection file
#' 
#' \code{read_prj} reads in projection file and returns it as a prj object.
#' 
#' @param file filename; typically '*.prj'
#' @return object of class prj
#' @importFrom readr read_lines
#' @export
read_prj <- function(file)
{
  prj.lines <- read_lines(file)
  prj <- NULL
  prj$projection <- prj.lines[1]
  prj$origin <- as.numeric(remove_empty_strings(strsplit(prj.lines[2],' ')[[1]]))
  prj$rotation <- as.numeric(remove_empty_strings(strsplit(prj.lines[3],' ')[[1]])[1])
  class(prj) <- 'prj'
  return(prj)
}
