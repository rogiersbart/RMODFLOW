#' Read comments
#' Internal function used in the read_* functions to read comments
rmfi_parse_comments <- function(remaining_lines) {
  i <- 0
  comments <- NULL
  while(i==0) {
    if(substr(remaining_lines[1], 1, 1) == '#') {
      comments <- append(comments, substr(remaining_lines[1], 2, nchar(remaining_lines[1])))
      remaining_lines <- remaining_lines[-1]
    } else {
      i <- 1
    }
  }
  return(list(comments = comments, remaining_lines = remaining_lines))
}
