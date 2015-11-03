#' Write a MODFLOW discretization file
#' 
#' @param dis an \code{\link{RMODFLOW}} dis object
#' @param file filename to write to; typically '*.dis'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{read_dis}}, \code{\link{create_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
write_dis <- function(dis, file, IPRN=-1)
{
  # Data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Discretization File created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(dis)), sep='\n', file=file, append=TRUE)
  
  # Data set 1
    write_modflow_variables(dis$NLAY,dis$NROW,dis$NCOL,dis$NPER,dis$ITMUNI,dis$LENUNI,file=file)
#  cat(paste(dis$NLAY,dis$NROW,dis$NCOL,dis$NPER,dis$ITMUNI,dis$LENUNI, '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 2
    write_modflow_variables(dis$LAYCBD,file=file)
#  cat(paste(paste(dis$LAYCBD, collapse=' '), '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 3
    write_modflow_array(dis$DELR,file=file,IPRN=IPRN)  
  
  # Data set 4
    write_modflow_array(dis$DELC,file=file,IPRN=IPRN)
  
  # Data set 5
    write_modflow_array(dis$TOP,file=file,IPRN=IPRN)
  
  # Data set 6
    write_modflow_array(dis$BOTM,file=file,IPRN=IPRN)
    
  # Data set 7
    for(i in 1:dis$NPER) write_modflow_variables(dis$PERLEN[i],dis$NSTP[i],dis$TSMULT[i],dis$SSTR[i], file=file)  
}
