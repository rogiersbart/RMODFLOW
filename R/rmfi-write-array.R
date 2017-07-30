#' Write modflow array
#' Internal function used in the write_* functions for writing array datasets
#' @keywords internal
rmfi_write_array <- function(array, file, cnstnt=1, iprn=-1, append=TRUE) {
  
  if(is.null(dim(array))) {
    if(prod(c(array)[1] == c(array))==1) {
      cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
    } else {
      cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
      cat(paste(paste(array, collapse=' '), '\n', sep=' '), file=file, append=append)     
    }
  } else if(length(dim(array))==2) {
    if(prod(c(array)[1] == c(array))==1) {
      cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
    } else {
      cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
      if(dim(array)[1] == 1) {
        cat(paste0(paste(array, collapse=' '),'\n'), file=file, append=append)
      } else {
        write.table(array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
      }
    }
  } else {
    for(i in 1:dim(array)[3])
    {
      if(prod(c(array[,,i])[1] == c(array[,,i]))==1) {
        cat(paste('CONSTANT ',cnstnt * c(array[,,i])[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
        if(dim(array)[1] == 1) {
          cat(paste0(paste(array[,,i], collapse=' '),'\n'), file=file, append=append)
        } else write.table(array[,,i], file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
}
