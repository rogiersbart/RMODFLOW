#' Read a MODFLOW discretization file
#' 
#' \code{read_dis} reads in a MODFLOW discretization file and returns it as an \code{\link{RMODFLOW}} dis object.
#' 
#' @param file filename; typically '*.dis'
#' @return object of class dis
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{write_dis}}, \code{\link{create_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
read_dis <- function(file = {cat('Please select dis file...\n'); file.choose()}) {
  dis_lines <- read_lines(file)
  dis <- NULL
  
  # data set 0
    comments <- get_comments_from_lines(dis_lines)
    dis_lines <- remove_comments_from_lines(dis_lines)
  
  # data set 1
    data_set1 <- remove_empty_strings(strsplit(dis_lines[1],' ')[[1]])
    dis_lines <- dis_lines[-1]  
    dis$nlay <- as.numeric(data_set1[1])
    dis$nrow <- as.numeric(data_set1[2])
    dis$ncol <- as.numeric(data_set1[3])
    dis$nper <- as.numeric(data_set1[4])
    dis$itmuni <- as.numeric(data_set1[5])
    dis$lenuni <- as.numeric(data_set1[6])
    rm(data_set1)
    
  # data set 2
    dis$laycbd <- as.numeric(remove_empty_strings(strsplit(dis_lines[1],' ')[[1]]))
    dis_lines <- dis_lines[-1]
    
  # data set 3
    dis_lines <- dis_lines[-1]
    dis$delr <- as.numeric(strsplit(dis_lines[1],' ')[[1]])
    dis_lines <- dis_lines[-1]
    
  # data set 4
    dis_lines <- dis_lines[-1]
    dis$delc <- as.numeric(strsplit(dis_lines[1],' ')[[1]])
    dis_lines <- dis_lines[-1]
    
  # data set 5
    data_set5 <- read_array(dis_lines,dis$nrow,dis$ncol,1)
    dis_lines <- data_set5$remaining_lines
    dis$top <- data_set5$array
    rm(data_set5)
  
  # data set 6
    data_set6 <- read_array(dis_lines,dis$nrow,dis$ncol,dis$nlay+sum(dis$laycbd))
    dis_lines <- data_set6$remaining_lines
    dis$botm <- data_set6$array
    rm(data_set6)
  
  # data set 7
    dis$perlen <- dis$nstp <- dis$tsmult <- dis$sstr <- rep(NA,dis$nper)
    for(i in 1:dis$nper) {
      data_set7 <- strsplit(dis_lines[1],' ')[[1]]
      dis_lines <- dis_lines[-1]
      dis$perlen[i] <- as.numeric(data_set7[1])
      dis$nstp[i] <- as.numeric(data_set7[2])
      dis$tsmult[i] <- as.numeric(data_set7[3])
      dis$sstr[i] <- as.character(data_set7[4])
      rm(data_set7)
    }
  
  comment(dis) <- comments
  class(dis) <- c('dis','modflow_package')
  return(dis)
}
