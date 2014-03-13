#' Read a MODFLOW multiplier file
#' 
#' \code{read.mlt} reads in a MODFLOW multiplier file and returns it as an \code{\link{RMODFLOW}} mlt object.
#' 
#' @param file Filename; typically "*.mlt"
#' @return Object of class ba6
#' @export
read.mlt <- function(file,  dis=read.dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')))
{
  mlt <- NULL
  mlt.lines <- scan(file, what=character(), sep='\n')
  
  # Data set 0
    mlt.lines <- remove.comments.from.lines(mlt.lines)
  
  # Data set 1
    mlt$NML <- as.numeric(mlt.lines[1])
    mlt.lines <- mlt.lines[-1]
  
  # Data set 2 + 3
    mlt$RMLT <- list()
    for(i in 1:mlt$NML)
    {
      mlt$MLTNAM[i] <- as.character(strsplit(mlt.lines[1],' ')[1])
      mlt.lines <- mlt.lines[-1]
      dataSet <- get.mfarray(mlt.lines,dis$NROW,dis$NCOL,1)
      mlt.lines <- dataSet$remaining.lines
      mlt$RMLT[[i]] <- dataSet$mfarray
      rm(dataSet)
    }
  
  class(mlt) <- c('mlt','mfpackage')
  return(mlt)
}