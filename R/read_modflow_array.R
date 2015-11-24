#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
read_modflow_array <- function(remaining_lines,NROW,NCOL,NLAY)
{
  # Initialize array object
    modflow_array <- array(dim=c(NROW,NCOL,NLAY))
  
  # Read array according to format type if there is anything to be read
    if(prod(dim(modflow_array))!=0)
    {
      for(k in 1:NLAY) 
      { 
        # Read in first row with format code
        # If constant and NLAY==1, return constant
        # If constant and NLAY!=1, fill layer with constant (is part of mf3darray!?)
        if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='CONSTANT') 
        {
          if(NLAY==1)
          {
            modflow_array <- as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2])
            remaining_lines <- remaining_lines[-1]
            return(list(modflow_array=modflow_array,remaining_lines=remaining_lines))
          } else {
            modflow_array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2]),nrow=NROW,ncol=NCOL)
            remaining_lines <- remaining_lines[-1]
          }
        }
        else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='INTERNAL')
        {
          remaining_lines <- remaining_lines[-1] 
          nPerLine <- length(as.numeric(remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])))
          nLines <- (NCOL %/% nPerLine + ifelse((NCOL %% nPerLine)==0, 0, 1))*NROW
          modflow_array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]])),nrow=NROW,ncol=NCOL,byrow=TRUE)
          remaining_lines <- remaining_lines[-c(1:nLines)]
        }
        else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='EXTERNAL')
        {
          stop('Reading EXTERNAL arrays is not implemented yet...')
        }   
        else if(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='OPEN/CLOSE')
        {
          warning('Reading OPEN/CLOSE arrays is not fully implemented yet...')
          modflow_array[,,k] <- as.matrix(read.table(file=remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2]))
          remaining_lines <- remaining_lines[-1] 
        } else {
          # in case of output file arrays without INTERNAL in format line
          nPerNum <- substr(remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[9],5,6)
          remaining_lines <- remaining_lines[-1]      
          nPerLine <- length(as.numeric(remove_empty_strings(strsplit((gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ", remaining_lines[1])),' |\t')[[1]])))
          nLines <- (NCOL %/% nPerLine + ifelse((NCOL %% nPerLine)==0, 0, 1))*NROW
          modflow_array[,,k] <- matrix(as.numeric(remove_empty_strings(strsplit(gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ",paste(remaining_lines[1:nLines],collapse='')),' |\t|\n| \n|\n | \t|\t ')[[1]])),nrow=NROW,ncol=NCOL,byrow=TRUE)
          remaining_lines <- remaining_lines[-c(1:nLines)]
        }   
      }
    }
  
  # Set class of object (2darray; 3darray)
    if(NLAY==1){modflow_array <- as.matrix(modflow_array[,,1]); class(modflow_array) <- 'modflow_2d_array'}
    if(NLAY!=1) class(modflow_array) <- 'modflow_3d_array'
  
  # Return output of reading function 
    return(list(modflow_array=modflow_array,remaining_lines=remaining_lines))
}
