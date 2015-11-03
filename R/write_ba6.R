#' Write a MODFLOW basic file
#' 
#' \code{write_ba6} writes a MODFLOW basic file based on an \code{\link{RMODFLOW}} ba6 object.
#' 
#' @param ba6 an \code{\link{RMODFLOW}} ba6 object
#' @param file filename to write to; typically '*.ba6'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_ba6 <- function(ba6, file, IPRN=-1)
{
  # Data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Basic Package created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(ba6)), sep='\n', file=file, append=TRUE)
    
  # Data set 1
    options <- NULL
    if(ba6$XSECTION) options <- paste(options, 'XSECTION ',sep='')
    if(ba6$CHTOCH) options <- paste(options, 'CHTOCH ',sep='')
    if(ba6$FREE) options <- paste(options, 'FREE ',sep='')
    if(ba6$PRINTTIME) options <- paste(options, 'PRINTTIME ',sep='')
    if(ba6$SHOWPROGRESS) options <- paste(options, 'SHOWPROGRESS ',sep='')
    if(ba6$STOPERROR) options <- paste(options,'SHOWPROGRESS ',ba6$STOPER,sep='')
    write_modflow_variables(options, file=file)
      
  # Data set 2
    if(ba6$XSECTION)
    {
      write_modflow_array(ba6$IBOUND, file = file, IPRN = IPRN) # update after changing read_ba6!
    } else {
      write_modflow_array(ba6$IBOUND, file = file, IPRN = IPRN)
    }
  
  # Data set 3
    write_modflow_variables(ba6$HNOFLO, file=file)
  
  # Data set 4
    if(ba6$XSECTION)
    {
      write_modflow_array(ba6$STRT, file = file, IPRN = IPRN) # update after changing read_ba6!
    } else {
      write_modflow_array(ba6$STRT, file = file, IPRN = IPRN)
    }
}
