#' Write a MODFLOW discretization file
#' 
#' @param dis an \code{\link{RMODFLOW}} dis object
#' @param file filename to write to; typically '*.dis'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_dis <- function(dis, file, IPRN=-1)
{
  # Data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Discretization File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(dis)), sep='\n', file=file, append=TRUE)
  
  # Data set 1
  cat(paste(dis$NLAY,dis$NROW,dis$NCOL,dis$NPER,dis$ITMUNI,dis$LENUNI, '\n', sep=' '), file=file, append=TRUE)
    
  # Data set 2
  cat(paste(paste(dis$LAYCBD, collapse=' '), '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 3
  cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
  cat(paste(paste(dis$DELR, collapse=' '), '\n', sep=' '), file=file, append=TRUE)  
  
  # Data set 4
  cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
  cat(paste(paste(dis$DELC, collapse=' '), '\n', sep=' '), file=file, append=TRUE)  
  
  # Data set 5
  cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
  write.table(dis$TOP, file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE) 
  
  # Data set 6
  for(i in 1:dim(dis$BOTM)[3])
  {
    cat(paste('INTERNAL 1.0 (free) ', IPRN, '\n', sep=''), file=file, append=TRUE)
    write.table(dis$BOTM[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
  }
  
  # Data set 7
  cat(paste(dis$PERLEN,dis$NSTP,dis$TSMULT,dis$SSTR, '\n', sep=' '), file=file, append=TRUE)  
}