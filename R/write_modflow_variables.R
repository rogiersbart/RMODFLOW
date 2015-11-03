#' Write modflow variables
#' 
write_modflow_variables <- function(..., file, append=TRUE) {
  cat(paste0(paste(..., sep=' ',collapse=' '), '\n'), file=file, append=append)
}
