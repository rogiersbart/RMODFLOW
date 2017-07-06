#' Read a projection file
#' 
#' \code{read_prj} reads in projection file and returns it as a prj object.
#' 
#' @param file filename; typically '*.prj'
#' @return object of class prj
#' @importFrom readr read_lines
#' @export
rmf_read_prj <- function(file = {cat('Please select prj file ...\n'); file.choose()}) {
  prj.lines <- readr::read_lines(file)
  prj <- list()
  prj$projection <- prj.lines[1]
  prj$origin <- as.numeric(RMODFLOW:::rmfi_remove_empty_strings(strsplit(prj.lines[2],' ')[[1]]))
  prj$rotation <- as.numeric(RMODFLOW:::rmfi_remove_empty_strings(strsplit(prj.lines[3],' ')[[1]])[1])
  if(length(prj.lines) > 3) prj$starttime <- as.POSIXct(prj.lines[4])
  class(prj) <- 'prj'
  return(prj)
}

#' @describeIn rmf_read_prj Deprecated function name
read_prj <- function(...) {
  .Deprecated(new = "rmf_read_prj", old = "read_prj")
  rmf_read_prj(...)
}
