#' Read a MODFLOW parameter value file
#' 
#' \code{read.pval} reads in a MODFLOW parameter value file and returns it as an \code{\link{RMODFLOW}} pval object.
#' 
#' @param file Filename; typically "*.pval"
#' @param read.all A logical value indicating if \code{NP} parameters should be read, or the full parameter table (only relevant if external codes use the pval file for storing additional parameters).
#' @return Object of class pval
#' @export
read.pval <- function(file, read.all=F)
{
  pval.lines <- scan(file, what=character(), sep='\n')
  pval <- NULL
  
  # Data set 0
    pval.lines <- remove.comments.from.lines(pval.lines)
  
  # Data set 1
    ifelse(read.all, pval$NP <- length(pval.lines)-1, pval$NP <- as.numeric(pval.lines[1]))
    pval.lines <- pval.lines[-1]
  
  # Data set 2
    for(i in 1:pval$NP)
    {
      pval$PARNAM[i] <- as.character(strsplit(pval.lines[1],' ')[[1]][1])
      pval$Parval[i] <- as.numeric(remove.empty.strings(strsplit(pval.lines[1],' ')[[1]])[2])
      pval.lines <- pval.lines[-1]
    }

  class(pval) <- c('pval','mfpackage')
  return(pval)
}