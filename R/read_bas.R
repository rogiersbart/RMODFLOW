#' Read a MODFLOW basic file
#' 
#' \code{read_bas} reads in a MODFLOW basic file and returns it as an \code{\link{RMODFLOW}} bas object.
#' 
#' @param file filename; typically '*.bas'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @return object of class bas
#' @importFrom readr read_lines
#' @export
read_bas <- function(file = {cat('Please select bas file...\n'); file.choose()},
                     dis = {cat('Please select dis file...\n'); read_dis(file.choose())}) {
  bas <- NULL
  bas.lines <- read_lines(file)
  
  # data set 0
    comments <- get_comments_from_lines(bas.lines)
    bas.lines <- remove_comments_from_lines(bas.lines)
  
  # data set 1
    dataSet1 <- remove_empty_strings(strsplit(bas.lines[1],' '))
    bas$XSECTION <- 'XSECTION' %in% dataSet1
    bas$CHTOCH <- 'CHTOCH' %in% dataSet1
    bas$FREE <- 'FREE' %in% dataSet1
    bas$PRINTTIME <- 'PRINTTIME' %in% dataSet1
    bas$SHOWPROGRESS <- 'SHOWPROGRESS' %in% dataSet1
    bas$STOPERROR <- 'STOPERROR' %in% dataSet1
    if(bas$STOPERROR) bas$STOPER <- as.numeric(dataSet1[match('STOPERROR',dataSet1)+1]) else bas$STOPER <- as.numeric(NA)
    bas.lines <- bas.lines[-1]
    
  # data set 2
    dataSet2 <- read_array(bas.lines,ifelse(bas$XSECTION,dis$NLAY,dis$NROW),dis$NCOL,ifelse(bas$XSECTION,1,dis$NLAY))
    bas.lines <- dataSet2$remaining_lines
    bas$IBOUND <- dataSet2$modflow_array
    rm(dataSet2)
  
  # data set 3
    bas$HNOFLO <- as.numeric(strsplit(bas.lines[1],' ')[[1]])
    bas.lines <- bas.lines[-1]
    
  # data set 4
    dataSet4 <- read_array(bas.lines,ifelse(bas$XSECTION,dis$NLAY,dis$NROW),dis$NCOL,ifelse(bas$XSECTION,1,dis$NLAY))
    bas.lines <- dataSet4$remaining_lines
    bas$STRT <- dataSet4$modflow_array
    rm(dataSet4)
  
  comment(bas) <- comments
  class(bas) <- c('bas','modflow_package')
  return(bas)
}



