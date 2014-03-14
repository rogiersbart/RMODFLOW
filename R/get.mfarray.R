#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
get.mfarray <- function(mfarray.lines,NROW,NCOL,NLAY)
{
  # Initialize array object
    mfarray <- array(dim=c(NROW,NCOL,NLAY))
  
  # Read array according to format type if there is anything to be read
    if(prod(dim(mfarray))!=0)
    {
      for(k in 1:NLAY) 
      { 
        # Read in first row with format code
        # If constant and NLAY==1, return constant
        # If constant and NLAY!=1, fill layer with constant (is part of mf3darray!?)
        if(strsplit(mfarray.lines[1],' ')[[1]][1]=='CONSTANT') 
        {
          if(NLAY==1)
          {
            mfarray <- as.numeric(strsplit(mfarray.lines[1],' |\t')[[1]][2])
            mfarray.lines <- mfarray.lines[-1]
            return(list(mfarray=mfarray,remaining.lines=mfarray.lines))
          } else {
            mfarray[,,k] <- matrix(as.numeric(strsplit(mfarray.lines[1],' |\t')[[1]][2]),nrow=NROW,ncol=NCOL)
          }
        }
        else if(strsplit(mfarray.lines[1],' ')[[1]][1]=='INTERNAL')
        {
          mfarray.lines <- mfarray.lines[-1] 
          nPerLine <- length(as.numeric(strsplit(mfarray.lines[1],' |\t')[[1]]))
          nLines <- (NCOL %/% nPerLine + ifelse((NCOL %% nPerLine)==0, 0, 1))*NROW
          mfarray[,,k] <- matrix(as.numeric(strsplit(paste(mfarray.lines[1:nLines],collapse='\n'),' |\t|\n| \n')[[1]]),nrow=NROW,ncol=NCOL,byrow=TRUE)
          mfarray.lines <- mfarray.lines[-c(1:nLines)]
        }
        else if(strsplit(mfarray.lines[1],' ')[[1]][1]=='EXTERNAL')
        {
          stop('Reading EXTERNAL arrays is not implemented yet...')
        }   
        else if(strsplit(mfarray.lines[1],' ')[[1]][1]=='OPEN/CLOSE')
        {
          stop('Reading OPEN/CLOSE arrays is not implemented yet...')
        }   
      }
    }
  
  # Set class of object (2darray; 3darray)
    if(NLAY==1){mfarray <- as.matrix(mfarray[,,1]); class(mfarray) <- 'mf2darray'}
    if(NLAY!=1) class(mfarray) <- 'mf3darray'
  
  # Return output of reading function 
    return(list(mfarray=mfarray,remaining.lines=mfarray.lines))
}