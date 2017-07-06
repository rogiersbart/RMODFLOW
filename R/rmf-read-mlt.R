#' Read a MODFLOW multiplier file
#' 
#' \code{read_mlt} reads in a MODFLOW multiplier file and returns it as an \code{\link{RMODFLOW}} mlt object.
#' 
#' @param file filename; typically '*.mlt'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @return object of class mlt
#' @importFrom readr read_lines
#' @export
rmf_read_mlt <- function(file = {cat('Please select mlt file ...\n'); file.choose()},
                         dis = {cat('Please select dis file ...\n'); rmf_read_dis(file.choose())}) {
  mlt <- list()
  mlt_lines <- read_lines(file)
  
  # data set 0
    data_set_0 <- rmfi_parse_comments(mlt_lines)
    comment(mlt) <- data_set_0$comments
    mlt_lines <- data_set_0$remaining_lines
    rm(data_set_0)

  # data set 1
    mlt$nml <- as.numeric(mlt_lines[1])
    mlt_lines <- mlt_lines[-1]
  
  # data set 2 + 3
    mlt$rmlt <- list()
    for(i in 1:mlt$nml) {
      mlt$mltnam[i] <- as.character(strsplit(mlt_lines[1],' ')[1])
      mlt_lines <- mlt_lines[-1]
      data_set <- rmfi_parse_array(mlt_lines,dis$nrow,dis$ncol,1)
      mlt_lines <- data_set$remaining_lines
      mlt$rmlt[[i]] <- data_set$array
      rm(data_set)
    }
  
  comment(mlt) <- comments
  class(mlt) <- c('mlt','rmf_package')
  return(mlt)
}

#' @describeIn rmf_read_mlt Deprecated function name
read_mlt <- function(...) {
  .Deprecated(new = "rmf_read_mlt", old = "read_mlt")
  rmf_read_mlt(...)
}
