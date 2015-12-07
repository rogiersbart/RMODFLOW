#' Write a MODFLOW basic file
#' 
#' \code{write_bas} writes a MODFLOW basic file based on an \code{\link{RMODFLOW}} bas object.
#' 
#' @param bas an \code{\link{RMODFLOW}} bas object
#' @param file filename to write to; typically '*.bas'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_bas <- function(bas, file, IPRN=-1)
{
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Basic Package created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(bas)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    options <- NULL
    if(bas$XSECTION) options <- paste(options, 'XSECTION ',sep='')
    if(bas$CHTOCH) options <- paste(options, 'CHTOCH ',sep='')
    if(bas$FREE) options <- paste(options, 'FREE ',sep='')
    if(bas$PRINTTIME) options <- paste(options, 'PRINTTIME ',sep='')
    if(bas$SHOWPROGRESS) options <- paste(options, 'SHOWPROGRESS ',sep='')
    if(bas$STOPERROR) options <- paste(options,'SHOWPROGRESS ',bas$STOPER,sep='')
    write_variables(options, file=file)
      
  # data set 2
    if(bas$XSECTION)
    {
      write_array(bas$IBOUND, file = file, IPRN = IPRN) # update after changing read_bas!
    } else {
      write_array(bas$IBOUND, file = file, IPRN = IPRN)
    }
  
  # data set 3
    write_variables(bas$HNOFLO, file=file)
  
  # data set 4
    if(bas$XSECTION)
    {
      write_array(bas$STRT, file = file, IPRN = IPRN) # update after changing read_bas!
    } else {
      write_array(bas$STRT, file = file, IPRN = IPRN)
    }
}
