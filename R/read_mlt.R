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
  
  # Data set 0
    comments <- get_comments_from_lines(mlt.lines)
    mlt.lines <- remove_comments_from_lines(mlt.lines)
  
  # Data set 1
    mlt$NML <- as.numeric(mlt.lines[1])
    mlt.lines <- mlt.lines[-1]
  
  # Data set 2 + 3
    mlt$RMLT <- list()
    for(i in 1:mlt$NML) {
      mlt$MLTNAM[i] <- as.character(strsplit(mlt.lines[1],' ')[1])
      mlt.lines <- mlt.lines[-1]
      dataSet <- read_modflow_array(mlt.lines,dis$NROW,dis$NCOL,1)
      mlt.lines <- dataSet$remaining_lines
      mlt$RMLT[[i]] <- dataSet$modflow_array
      rm(dataSet)
    }
  
  comment(mlt) <- comments
  class(mlt) <- c('mlt','modflow_package')
  return(mlt)
}
