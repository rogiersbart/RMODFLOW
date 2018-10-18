
#' Writes a MODFLOW list
#' @param list a \code{rmf_list} object
#' @param names of columns to write besides i, j and k columns
#' @param file file to write to
#' @param format, character; either 'fixed' or 'free'. Fixed format assumes 10-character spaces per variable
#' @param ... ignored
#' @keywords internal

rmfi_write_list = function(list, varnames = setdiff(colnames(list), c('k','i','j')),  file, format = 'free', ...) {
  
  list = list[,c('k','i','j', varnames)]
  
  for(i in 1:nrow(list)) {
    rmfi_write_variables(list[i,], file = file, format = format, append = TRUE)
  }
}
