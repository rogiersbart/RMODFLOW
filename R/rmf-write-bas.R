#' Write a MODFLOW basic file
#' 
#' \code{rmf_write_bas} writes a MODFLOW basic file based on an \code{\link{RMODFLOW}} bas object.
#' 
#' @param bas an \code{\link{RMODFLOW}} bas object
#' @param file filename to write to; typically '*.bas'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
rmf_write_bas <- function(bas,
                          file = {cat('Please select bas file to overwrite or provide new filename ...\n'); file.choose()},
                          iprn=-1, 
                          ...) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Basic Package created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(bas)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    options <- NULL
    if(bas$xsection) {
      options <- paste(options, 'XSECTION ',sep='')
      warning('XSECTION: assuming ibound and strt arrays are of dimensions NLAY x NCOL')
    }
    if(bas$chtoch) options <- paste(options, 'CHTOCH ',sep='')
    if(bas$free) options <- paste(options, 'FREE ',sep='')
    if(bas$printtime) options <- paste(options, 'PRINTTIME ',sep='')
    if(bas$showprogress) options <- paste(options, 'SHOWPROGRESS ',sep='')
    if(bas$stoperror) options <- paste(options,'STOPERROR ',bas$stoper,sep='')
    rmfi_write_variables(options, file=file)
      
  # data set 2
    rmfi_write_array(bas$ibound, file = file, iprn = iprn, xsection = bas$xsection, ...)
  
  # data set 3
    rmfi_write_variables(as.character(bas$hnoflo), file=file, format = ifelse(bas$free, 'free', 'fixed'))
  
  # data set 4
    rmfi_write_array(bas$strt, file = file, iprn = iprn, xsection = bas$xsection, ...)
    
}

#' @describeIn rmf_write_bas Deprecated function name
#' @export
write_bas <- function(...) {
  .Deprecated(new = "rmf_write_bas", old = "write_bas")
  rmf_write_bas(...)
}
