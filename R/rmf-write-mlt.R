#' Write a MODFLOW multiplier file
#'
#' \code{rmf_write_mlt} writes an MODFLOW multiplier file based on a \code{RMODFLOW} mlt object
#'
#' @param mlt an \code{RMODFLOW} mlt object
#' @param file filename to write to; typically '*.mlt'
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_mlt}}, \code{\link{rmf_create_mlt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?mult.htm}

rmf_write_mlt <-  function(mlt, file={cat('Please choose mlt file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Multiplier File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(mlt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(mlt$nml, file=file)
  
  for (i in 1:mlt$nml){
    
    # data set 2
    rmfi_write_variables(mlt$mltnam[i], ifelse((!is.null(mlt$functn) && mlt$functn[i]), 'FUNCTION', ''), file=file)
    
    # data set 3
    if(is.null(mlt$functn) || (!is.null(mlt$functn) && !mlt$functn[i])) rmfi_write_array(mlt$rmlt[[i]], file=file, ...)

    # data set 4
    if(!is.null(mlt$functn) && mlt$functn[i]) rmfi_write_variables(mlt$operators[[i]], mlt$iprn[i], file=file) 

  }
}