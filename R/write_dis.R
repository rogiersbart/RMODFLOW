#' Write a MODFLOW discretization file
#' 
#' @param dis an \code{\link{RMODFLOW}} dis object
#' @param file filename to write to; typically '*.dis'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{read_dis}}, \code{\link{create_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
write_dis <- function(dis, file, iprn=-1) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Discretization File created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(dis)), sep='\n', file=file, append=TRUE)
  
  # data set 1
    write_variables(dis$nlay,dis$nrow,dis$ncol,dis$nper,dis$itmuni,dis$lenuni,file=file)
#  cat(paste(dis$nlay,dis$nrow,dis$ncol,dis$nper,dis$itmuni,dis$lenuni, '\n', sep=' '), file=file, append=TRUE)
  
  # data set 2
    write_variables(dis$laycbd,file=file)
#  cat(paste(paste(dis$laycbd, collapse=' '), '\n', sep=' '), file=file, append=TRUE)
  
  # data set 3
    write_modflow_array(dis$delr,file=file,iprn=iprn)  
  
  # data set 4
    write_modflow_array(dis$delc,file=file,iprn=iprn)
  
  # data set 5
    write_modflow_array(dis$top,file=file,iprn=iprn)
  
  # data set 6
    write_modflow_array(dis$botm,file=file,iprn=iprn)
    
  # data set 7
    for(i in 1:dis$nper) {
      write_variables(dis$perlen[i],dis$nstp[i],dis$tsmult[i],dis$sstr[i], file=file)  
    }
}
