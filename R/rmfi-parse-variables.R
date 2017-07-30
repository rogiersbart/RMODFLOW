#' Read modflow variables
#' If all are numbers, returns numeric, otherwise returns character vector
#' @keywords internal
rmfi_parse_variables <- function(remaining_lines) {
  variables <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),' |\t')[[1]])
  if(!any(is.na(suppressWarnings(as.numeric(variables))))) variables <- as.numeric(variables)
  return(list(variables=variables,remaining_lines= remaining_lines[-1]))
}
