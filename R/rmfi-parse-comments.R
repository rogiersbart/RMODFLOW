#' Read comments
#' Internal function used in the read_* functions to read comments
#' @details prevents copying of RMODFLOW header comment
#' @keywords internal
rmfi_parse_comments <- function(remaining_lines) {
  v <- paste("RMODFLOW, version",  packageDescription("RMODFLOW")$Version)
  i <- 0
  comments <- NULL
  while(i==0) {
    if(substr(remaining_lines[1], 1, 1) == '#') {
      com <- substr(remaining_lines[1], 2, nchar(remaining_lines[1]))
      if(!identical(trimws(strsplit(com, 'by ')[[1]][2]), trimws(v)) && nchar(trimws(com)) > 0)  comments <- append(comments, com)
      remaining_lines <- remaining_lines[-1]
    } else {
      i <- 1
    }
  }
  return(list(comments = comments, remaining_lines = remaining_lines))
}
