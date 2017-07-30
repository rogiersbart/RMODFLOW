#' Write modflow variables
#' Internal function used in the write_* functions for writing single line datasets
#' @keywords internal
rmfi_write_variables <- function(..., file, append=TRUE) {
  cat(paste0(paste(..., sep=' ',collapse=' '), '\n'), file=file, append=append)
}
