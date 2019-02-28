#' Read a MODFLOW name file
#' 
#' \code{rmf_read_nam} reads in a MODFLOW name file and returns it as an \code{\link{RMODFLOW}} nam object.
#' 
#' @param file filename; typically '*.nam'
#' @return object of class nam
#' @importFrom readr read_lines
#' @export
rmf_read_nam <- function(file = {cat('Please select nam file ...\n'); file.choose()}) {

  nam <- list()
  lines <- read_lines(file)
  indices <- rep(T,length(lines))
  for(i in 1:length(lines)) {
    if(strsplit(rmfi_remove_empty_strings(strsplit(lines[i],' ')[[1]])[1], "")[[1]][1] == "#") {
      comment(nam) = append(comment(nam), lines[i])
      indices[i] <-  FALSE
    } else {
      lines[i] <- rmfi_remove_comments_end_of_line(lines[i])
    }
  }
  nam_lines <- lines[indices]
  nam_lines <- lapply(strsplit(nam_lines, ' '), rmfi_remove_empty_strings)
  nam_lines <- lapply(nam_lines, function(i) rmfi_ifelse0(length(unlist(i))< 4, c(unlist(i),NA), unlist(i)))
  
  nam <-  data.frame(do.call(rbind, nam_lines), stringsAsFactors = F)
  colnames(nam) <- c('ftype','nunit','fname', 'options')
  nam$nunit<- as.numeric(nam$nunit)
  
  attr(nam, 'dir') = dirname(file)
  class(nam) <- c('nam','data.frame')
  return(nam)
}

#' @describeIn rmf_read_nam Deprecated function name
#' @export
read_nam <- function(...) {
  .Deprecated(new = "rmf_read_nam", old = "read_nam")
  rmf_read_nam(...)
}
