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
    if(strsplit(mlt.lines[1],' ')[[1]][1]=='CONSTANT') {mlt$RMLT[[i]] <- as.numeric(strsplit(mlt.lines[1],' ')[[1]][2]);mlt.lines <- mlt.lines[-1]}
    else if(strsplit(mlt.lines[1],' ')[[1]][1]=='INTERNAL')
    {
      mlt.lines <- mlt.lines[-1]
      mlt$RMLT[[i]] <- matrix(nrow=dis$NROW, ncol=dis$NCOL)
      for(j in 1:dis$NROW) 
      {
        mlt$RMLT[[i]][j,] <- as.numeric(strsplit(mlt.lines[1],' ')[[1]])
        mlt.lines <- mlt.lines[-1]
      }    
      class(mlt$RMLT[[i]]) <- 'mf2darray'
    }
  }
  class(mlt) <- 'mlt'
  return(mlt)
}