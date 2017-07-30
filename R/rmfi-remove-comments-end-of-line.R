#' Remove comments at the end of a string
#' @param line A string.
#' @return The string, without the commented part.
#' @keywords internal
rmfi_remove_comments_end_of_line <- function(line) {
  if(grepl('#',line)) return(substr(line,1,regexpr('#',line)-1))
  else return(line)
}