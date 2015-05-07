#' Read a MODFLOW parameter value file
#' 
#' \code{read_pval} reads in a MODFLOW parameter value file and returns it as an \code{\link{RMODFLOW}} pval object.
#' 
#' @param file Filename; typically "*.pval"
#' @param read_all A logical value indicating if \code{NP} parameters should be read, or the full parameter table (only relevant if external codes use the pval file for storing additional parameters).
#' @return Object of class pval
#' @export
read_pval <- function(file, read_all=F)
{
  pval.lines <- read_lines(file)
  pval <- NULL
  
  # Data set 0
    comments <- get_comments_from_lines(pval.lines)
    pval.lines <- remove_comments_from_lines(pval.lines)
  
  # Data set 1
    ifelse(read_all, pval$NP <- length(pval.lines)-1, pval$NP <- as.numeric(pval.lines[1]))
    pval.lines <- pval.lines[-1]
  
  # Data set 2
    for(i in 1:pval$NP)
    {
      pval$PARNAM[i] <- as.character(strsplit(pval.lines[1],' ')[[1]][1])
      pval$Parval[i] <- as.numeric(remove_empty_strings(strsplit(pval.lines[1],' ')[[1]])[2])
      pval.lines <- pval.lines[-1]
    }

  comment(pval) <- comments
  class(pval) <- c('pval','modflow_package')
  return(pval)
}