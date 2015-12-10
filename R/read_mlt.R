#' Read a MODFLOW multiplier file
#' 
#' \code{read_mlt} reads in a MODFLOW multiplier file and returns it as an \code{\link{RMODFLOW}} mlt object.
#' 
#' @param file filename; typically '*.mlt'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @return object of class mlt
#' @importFrom readr read_lines
#' @export
read_mlt <- function(file = {cat('Please select mlt file...\n'); file.choose()},
                     dis = {cat('Please select dis file...\n'); read_dis(file.choose())}) {
  mlt <- NULL
  mlt.lines <- read_lines(file)
  
  # data set 0
    comments <- get_comments_from_lines(mlt.lines)
    mlt.lines <- remove_comments_from_lines(mlt.lines)
  
  # data set 1
    mlt$nml <- as.numeric(mlt.lines[1])
    mlt.lines <- mlt.lines[-1]
  
  # data set 2 + 3
    mlt$rmlt <- list()
    for(i in 1:mlt$nml) {
      mlt$mltnam[i] <- as.character(strsplit(mlt.lines[1],' ')[1])
      mlt.lines <- mlt.lines[-1]
      data_set <- read_array(mlt.lines,dis$nrow,dis$ncol,1)
      mlt.lines <- data_set$remaining_lines
      mlt$rmlt[[i]] <- data_set$array
      rm(data_set)
    }
  
  comment(mlt) <- comments
  class(mlt) <- c('mlt','modflow_package')
  return(mlt)
}
