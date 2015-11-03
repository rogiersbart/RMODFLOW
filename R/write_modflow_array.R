#' Write modflow array
#' 
write_modflow_array <- function(modflow_array, file, IPRN=-1, append=TRUE) {
  if(is.null(dim(modflow_array))) {
    cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=append)
    cat(paste(paste(modflow_array, collapse=' '), '\n', sep=' '), file=file, append=append)      
  } else if(length(dim(modflow_array))==2) {
    cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=append)
    if(dim(modflow_array)[1] == 1) {
      cat(paste0(paste(modflow_array, collapse=' '),'\n'), file=file, append=append)
    } else {
      write.table(modflow_array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
    }
  } else {
    for(i in 1:dim(modflow_array)[3])
    {
      cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=append)
      if(dim(modflow_array)[1] == 1) {
        cat(paste0(paste(modflow_array[,,i], collapse=' '),'\n'), file=file, append=append)
      } else write.table(modflow_array[,,i], file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  }
}
