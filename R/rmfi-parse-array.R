#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
rmfi_parse_array <- function(remaining_lines,nrow,ncol,nlay, ndim = NULL) {
  # Initialize array object
  array <- array(dim=c(nrow,ncol,nlay))
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(array))!=0)
  {
    for(k in 1:nlay) 
    { 
      if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1] %in% c('CONSTANT', '0')) {
        if(nlay==1) {
          array[1:length(array)] <- as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2])
          remaining_lines <- remaining_lines[-1]
          if(!is.null(ndim)) {
            if(ndim == 1) {
              array <- array(array,dim=nrow*ncol*nlay)
              class(array) <- 'rmf_1d_array'
            } else if(ndim == 2) {
              array <- array(array, dim = c(nrow, ncol))
            }
          }
          return(list(array=array,remaining_lines=remaining_lines))
        } else {
          array[,,k] <- matrix(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])[2]),nrow=nrow,ncol=ncol)
          remaining_lines <- remaining_lines[-1]
        }
      }
      else if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1] %in% c('INTERNAL', '100', '103'))
      {
        remaining_lines <- remaining_lines[-1] 
        nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t')[[1]])))
        nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
        array[,,k] <- matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }
      else if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='EXTERNAL')
      {
        stop('Reading EXTERNAL arrays is not implemented yet...')
      }   
      else if(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]=='OPEN/CLOSE')
      {
        warning('Reading OPEN/CLOSE arrays is not fully implemented yet...')
        array[,,k] <- as.matrix(read.table(file=rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2]))
        remaining_lines <- remaining_lines[-1] 
      } else {
        # in case of output file arrays without INTERNAL in format line
        nPerNum <- substr(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[9],5,6)
        remaining_lines <- remaining_lines[-1]      
        nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit((gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ", remaining_lines[1])),' |\t')[[1]])))
        nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
        array[,,k] <- matrix(as.numeric(rmfi_remove_empty_strings(strsplit(gsub(paste0("([[:alnum:]*[:punct:]*[:space:]]{",nPerNum,"})"), "\\1 ",paste(remaining_lines[1:nLines],collapse='')),' |\t|\n| \n|\n | \t|\t ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }   
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(is.null(ndim)) {
    if(nlay==1){
      array <- as.matrix(array[,,1])
      class(array) <- 'rmf_2d_array'     
    }
    if(nlay!=1) class(array) <- 'rmf_3d_array'
  } else if(ndim == 1) {
    array <- array(array,dim=nrow*ncol*nlay)
    class(array) <- 'rmf_1d_array'
  } else if(ndim == 2) {
    array <- as.matrix(array[,,1])
    class(array) <- 'rmf_2d_array'     
  } else if(ndim == 3) {
    class(array) <- 'rmf_3d_array'
  }
  
  # Return output of reading function 
  return(list(array=array,remaining_lines=remaining_lines))
}
