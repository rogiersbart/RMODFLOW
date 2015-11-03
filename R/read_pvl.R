#' Read a MODFLOW parameter value file
#' 
#' \code{read_pvl} reads in a MODFLOW parameter value file and returns it as an \code{\link{RMODFLOW}} pvl object.
#' 
#' @param file filename; typically '*.pvl'
#' @param read_all logical, indicating if \code{NP} parameters should be read, or the full parameter table (only relevant if external codes use the pvl file for storing additional parameters).
#' @return object of class pvl
#' @importFrom readr read_lines
#' @export
read_pvl <- function(file, read_all=F)
{
  pvl.lines <- read_lines(file)
  pvl <- NULL
  
  # Data set 0
    comments <- get_comments_from_lines(pvl.lines)
    pvl.lines <- remove_comments_from_lines(pvl.lines)
  
  # Data set 1
    ifelse(read_all, pvl$NP <- length(pvl.lines)-1, pvl$NP <- as.numeric(pvl.lines[1]))
    pvl.lines <- pvl.lines[-1]
  
  # Data set 2
    for(i in 1:pvl$NP)
    {
      pvl$PARNAM[i] <- as.character(strsplit(pvl.lines[1],' ')[[1]][1])
      pvl$Parval[i] <- as.numeric(remove_empty_strings(strsplit(pvl.lines[1],' ')[[1]])[2])
      pvl.lines <- pvl.lines[-1]
    }

  comment(pvl) <- comments
  class(pvl) <- c('pvl','modflow_package')
  return(pvl)
}
