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
    bas$xsection <- 'XSECTION' %in% dataSet1
    bas$chtoch <- 'CHTOCH' %in% dataSet1
    bas$free <- 'FREE' %in% dataSet1
    bas$printtime <- 'PRINTTIME' %in% dataSet1
    bas$showprogress <- 'SHOWPROGRESS' %in% dataSet1
    bas$stoperror <- 'STOPERROR' %in% dataSet1
    if(bas$stoperror) bas$stoper <- as.numeric(dataSet1[match('stoperror',dataSet1)+1]) else bas$stoper <- as.numeric(NA)
    bas.lines <- bas.lines[-1]
    
  # data set 2
    dataSet2 <- read_array(bas.lines,ifelse(bas$xsection,dis$nlay,dis$nrow),dis$ncol,ifelse(bas$xsection,1,dis$nlay))
    bas.lines <- dataSet2$remaining_lines
    bas$ibound <- dataSet2$modflow_array
    rm(dataSet2)
  
  # data set 3
    bas$hnoflo <- as.numeric(strsplit(bas.lines[1],' ')[[1]])
    bas.lines <- bas.lines[-1]
    
  # data set 4
    dataSet4 <- read_array(bas.lines,ifelse(bas$xsection,dis$nlay,dis$nrow),dis$ncol,ifelse(bas$xsection,1,dis$nlay))
    bas.lines <- dataSet4$remaining_lines
    bas$strt <- dataSet4$modflow_array
    rm(dataSet4)
  
  comment(bas) <- comments
  class(bas) <- c('bas','modflow_package')
  return(bas)
}