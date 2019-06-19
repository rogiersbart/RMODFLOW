#' Write modflow variables
#' Internal function used in the write_* functions for writing single line datasets
#' @param format either \code{'fixed'} or \code{'free'}. Fixed format assumes 10 character spaces for each value
#' @keywords internal
rmfi_write_variables <- function(..., file, append=TRUE, format = 'free') {
  arg <- unlist(list(...))
  arg <- arg[nchar(arg) > 0] # removes empty elements
  if(format == 'free') {
    cat(paste0(paste(arg, sep=' ',collapse=' '), '\n'), file=file, append=append)
  } else if(format == 'fixed') { # optional items are always free format so no need to adjust code for characters
    cat(paste0(paste0(formatC(arg, width=10),collapse=''), '\n'), file=file, append=append)
  } 
}