#' Write a MODFLOW parameter value file
#' 
#' @param pvl an \code{\link{RMODFLOW}} pvl object
#' @param file filename to write to; typically '*.pvl'
#' @return \code{NULL}
#' @export
rmf_write_pvl <- function(pvl,
                      file = {cat('Please select pvl file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Parameter Value File created by RMODFLOW, version',v,'at',date(),'\n'), file=file)
    cat(paste('#', comment(pvl)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    rmfi_write_variables(pvl$np, file=file)
  
  # data set 2
    for(i in 1:pvl$np) {
      rmfi_write_variables(pvl$parnam[i], pvl$parval[i], file=file)
    }  
}

#' @describeIn rmf_write_pvl Deprecated function name
write_pvl <- function(...) {
  .Deprecated(new = "rmf_write_pvl", old = "write_pvl")
  rmf_write_pvl(...)
}
