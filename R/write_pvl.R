#' Write a MODFLOW parameter value file
#' 
#' @param pvl an \code{\link{RMODFLOW}} pvl object
#' @param file filename to write to; typically '*.pvl'
#' @return \code{NULL}
#' @export
write_pvl <- function(pvl, file)
{
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Parameter Value File created by RMODFLOW, version',v,'at',date(),'\n'), file=file)
    cat(paste('#', comment(pvl)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    write_modflow_variables(pvl$np, file=file)
  
  # data set 2
    for(i in 1:pvl$np) {
      write_modflow_variables(pvl$parnam[i], pvl$parval[i], file=file)
    }  
}
