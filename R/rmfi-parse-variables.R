#' Read modflow variables
#' If all are numbers, returns numeric, otherwise returns character vector
#' @param n integer; number of variables to be returned. Only used when format is \code{'fixed'}
#' @param format character, either \code{'free'} or \code{'fixed'}. When 'fixed', reads 10-character fields and converts to numeric. Empty fields are set to zero.
#' @param ... ignored
#' @keywords internal
rmfi_parse_variables <- function(remaining_lines, n, format = 'free', ...) {
  if(format == 'free') {
    variables <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),' |\t|,')[[1]])
    if(!any(is.na(suppressWarnings(as.numeric(variables))))) variables <- as.numeric(variables)
  } else if(format == 'fixed') { # every value has 10 characters; empty values are zero
    variables <- (unlist(lapply(seq(1,nchar(remaining_lines[1]), by=10), 
                                function(i) paste0(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),'')[[1]][i:(i+9)], collapse=''))))
    variables <- lapply(strsplit(variables, " |t"), rmfi_remove_empty_strings)
    variables[which(lengths(variables)==0)] = 0 # empty values are set to 0
    variables <-  unlist(variables)
    if(!any(is.na(suppressWarnings(as.numeric(variables))))) {
      variables <- as.numeric(variables)
      if(length(variables) < n) variables <- c(variables, rep(0, n - length(variables))) # append 0's if values are missing
    }
  }
  return(list(variables=variables,remaining_lines= remaining_lines[-1]))
}