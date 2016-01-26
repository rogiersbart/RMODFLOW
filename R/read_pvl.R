#' Read a MODFLOW parameter value file
#' 
#' \code{read_pvl} reads in a MODFLOW parameter value file and returns it as an \code{\link{RMODFLOW}} pvl object.
#' 
#' @param file filename; typically '*.pvl'
#' @param read_all logical, indicating if \code{np} parameters should be read, or the full parameter table (only relevant if external codes use the pvl file for storing additional parameters).
#' @return object of class pvl
#' @importFrom readr read_lines
#' @export
read_pvl <- function(file = {cat('Please select pvl file ...\n'); file.choose()},
                     read_all=F) {
  
  pvl_lines <- read_lines(file)
  pvl <- list()
  
  # data set 0
    comments <- get_comments_from_lines(pvl_lines)
    pvl_lines <- remove_comments_from_lines(pvl_lines)
  
  # data set 1
    ifelse(read_all, pvl$np <- length(pvl_lines)-1, pvl$np <- as.numeric(pvl_lines[1]))
    pvl_lines <- pvl_lines[-1]
  
  # data set 2
    for(i in 1:pvl$np) {
      pvl$parnam[i] <- as.character(strsplit(pvl_lines[1],' ')[[1]][1])
      pvl$parval[i] <- as.numeric(remove_empty_strings(strsplit(pvl_lines[1],' ')[[1]])[2])
      pvl_lines <- pvl_lines[-1]
    }

  comment(pvl) <- comments
  class(pvl) <- c('pvl','modflow_package')
  return(pvl)
}
