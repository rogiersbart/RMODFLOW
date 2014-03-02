################################################################################
### write.ba6 ##################################################################
################################################################################
write.ba6 <- function(ba6, file, info='No further information provided',IPRN=-1)
{
  # Data set 0
  cat('# MODFLOW Basic Package created in R\n', file=file)
  cat(paste('#', info, '\n'), file=file, append=TRUE)
  
  # Data set 1
  options <- NULL
  if(ba6$XSECTION) options <- paste(options, 'XSECTION ',sep='')
  if(ba6$CHTOCH) options <- paste(options, 'CHTOCH ',sep='')
  if(ba6$FREE) options <- paste(options, 'FREE ',sep='')
  if(ba6$PRINTTIME) options <- paste(options, 'PRINTTIME ',sep='')
  if(ba6$SHOWPROGRESS) options <- paste(options, 'SHOWPROGRESS ',sep='')
  if(ba6$STOPERROR) options <- paste(options,'SHOWPROGRESS ',ba6$STOPER,sep='')
  cat(paste(options, '\n', sep=''), file=file, append=TRUE)
    
  # Data set 2
  if(ba6$XSECTION)
  {
    cat(paste('INTERNAL 1 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
    write.table(ba6$IBOUND, file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE) 
  } else {
    for(i in 1:dim(ba6$IBOUND)[3])
    {
      cat(paste('INTERNAL 1 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
      write.table(ba6$IBOUND[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  }
  
  # Data set 3
  cat(paste(ba6$HNOFLO, '\n', sep=''), file=file, append=TRUE)
  
  # Data set 4
  if(ba6$XSECTION)
  {
    cat(paste('INTERNAL 1 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
    write.table(ba6$STRT, file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE) 
  } else {
    for(i in 1:dim(ba6$STRT)[3])
    {
      cat(paste('INTERNAL 1 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
      write.table(ba6$STRT[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  }
}