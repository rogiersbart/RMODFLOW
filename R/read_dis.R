#' Read a MODFLOW discretization file
#' 
#' \code{read_dis} reads in a MODFLOW discretization file and returns it as an \code{\link{RMODFLOW}} dis object.
#' 
#' @param file filename; typically '*.dis'
#' @return object of class dis
#' @importFrom readr read_lines
#' @export
read_dis <- function(file)
{
  dis.lines <- read_lines(file)
  dis <- NULL
  
  # Data set 0
    comments <- get_comments_from_lines(dis.lines)
    dis.lines <- remove_comments_from_lines(dis.lines)
  
  # Data set 1
    dataSet1 <- remove_empty_strings(strsplit(dis.lines[1],' ')[[1]])
    dis.lines <- dis.lines[-1]  
    dis$NLAY <- as.numeric(dataSet1[1])
    dis$NROW <- as.numeric(dataSet1[2])
    dis$NCOL <- as.numeric(dataSet1[3])
    dis$NPER <- as.numeric(dataSet1[4])
    dis$ITMUNI <- as.numeric(dataSet1[5])
    dis$LENUNI <- as.numeric(dataSet1[6])
    rm(dataSet1)
    
  # Data set 2
    dis$LAYCBD <- as.numeric(remove_empty_strings(strsplit(dis.lines[1],' ')[[1]]))
    dis.lines <- dis.lines[-1]
    
  # Data set 3
    dis.lines <- dis.lines[-1]
    dis$DELR <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
    dis.lines <- dis.lines[-1]
    
  # Data set 4
    dis.lines <- dis.lines[-1]
    dis$DELC <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
    dis.lines <- dis.lines[-1]
    
  # Data set 5
    dataSet5 <- int_get_modflow_array(dis.lines,dis$NROW,dis$NCOL,1)
    dis.lines <- dataSet5$remaining.lines
    dis$TOP <- dataSet5$mfarray
    rm(dataSet5)
  
  # Data set 6
    dataSet6 <- int_get_modflow_array(dis.lines,dis$NROW,dis$NCOL,dis$NLAY+sum(dis$LAYCBD))
    dis.lines <- dataSet6$remaining.lines
    dis$BOTM <- dataSet6$mfarray
    rm(dataSet6)
  
  # Data set 7
    dataSet7 <- strsplit(dis.lines[1],' ')[[1]]
    dis.lines <- dis.lines[-1]
    dis$PERLEN <- as.numeric(dataSet7[1])
    dis$NSTP <- as.numeric(dataSet7[2])
    dis$TSMULT <- as.numeric(dataSet7[3])
    dis$SSTR <- as.character(dataSet7[4])
    rm(dataSet7)
  
  comment(dis) <- comments
  class(dis) <- c('dis','modflow_package')
  return(dis)
}