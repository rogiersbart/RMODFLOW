#' Read a MODFLOW basic file
#' 
#' \code{read_ba6} reads in a MODFLOW basic file and returns it as an \code{\link{RMODFLOW}} ba6 object.
#' 
#' @param file filename; typically '*.ba6'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @return object of class ba6
#' @importFrom readr read_lines
#' @export
read_ba6 <- function(file, dis=read_dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')))
{
  ba6 <- NULL
  ba6.lines <- read_lines(file)
  
  # Data set 0
    comments <- get_comments_from_lines(ba6.lines)
    ba6.lines <- remove_comments_from_lines(ba6.lines)
  
  # Data set 1
    dataSet1 <- remove_empty_strings(strsplit(ba6.lines[1],' '))
    ba6$XSECTION <- 'XSECTION' %in% dataSet1
    ba6$CHTOCH <- 'CHTOCH' %in% dataSet1
    ba6$FREE <- 'FREE' %in% dataSet1
    ba6$PRINTTIME <- 'PRINTTIME' %in% dataSet1
    ba6$SHOWPROGRESS <- 'SHOWPROGRESS' %in% dataSet1
    ba6$STOPERROR <- 'STOPERROR' %in% dataSet1
    if(ba6$STOPERROR) ba6$STOPER <- as.numeric(dataSet1[match('STOPERROR',dataSet1)+1]) else ba6$STOPER <- as.numeric(NA)
    ba6.lines <- ba6.lines[-1]
    
  # Data set 2
    dataSet2 <- read_modflow_array(ba6.lines,ifelse(ba6$XSECTION,dis$NLAY,dis$NROW),dis$NCOL,ifelse(ba6$XSECTION,1,dis$NLAY))
    ba6.lines <- dataSet2$remaining_lines
    ba6$IBOUND <- dataSet2$modflow_array
    rm(dataSet2)
  
  # Data set 3
    ba6$HNOFLO <- as.numeric(strsplit(ba6.lines[1],' ')[[1]])
    ba6.lines <- ba6.lines[-1]
    
  # Data set 4
    dataSet4 <- read_modflow_array(ba6.lines,ifelse(ba6$XSECTION,dis$NLAY,dis$NROW),dis$NCOL,ifelse(ba6$XSECTION,1,dis$NLAY))
    ba6.lines <- dataSet4$remaining_lines
    ba6$STRT <- dataSet4$modflow_array
    rm(dataSet4)
  
  comment(ba6) <- comments
  class(ba6) <- c('ba6','modflow_package')
  return(ba6)
}



