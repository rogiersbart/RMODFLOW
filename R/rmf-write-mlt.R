#' Write a MODFLOW multiplier file
#' 
#' @param mlt an \code{\link{RMODFLOW}} mlt object
#' @param file filename to write to; typically '*.mlt'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmf_write_mlt <- function(mlt,
                      file = {cat('Please select mlt file to overwrite or provide new filename ...\n'); file.choose()},
                      iprn=-1) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Multiplier File created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(mlt)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    rmfi_write_variables(mlt$nml, file=file)
  
  # data set 2 + 3 
  for(i in 1:mlt$nml)
  {
    rmfi_write_variables(mlt$mltnam[i], file=file) 
    if(length(mlt$rmlt[[i]])==1) {
      cat(paste('CONSTANT ', mlt$rmlt[[i]], '\n', sep=''), file=file, append=TRUE)
    } else {
      rmfi_write_array(mlt$rmlt[[i]], file = file, iprn = iprn)
    }
  }  
}

#' @describeIn rmf_write_mlt Deprecated function name
write_mlt <- function(...) {
  .Deprecated(new = "rmf_write_mlt", old = "write_mlt")
  rmf_write_mlt(...)
}
