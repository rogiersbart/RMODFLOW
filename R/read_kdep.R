#' Read a MODFLOW hydraulic conductivity depth-dependence capability file
#' 
#' \code{read_kdep} reads in a MODFLOW Hydraulic-Conductivity Depth-Dependence Capability file and returns it as an \code{\link{RMODFLOW}} kdep object.
#' 
#' @param file Filename; typically *.kdep
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param huf hydrogeologic unit file object; defaults to that with the same filename but with extension '.huf'
#' @return object of class kdep
#' @importFrom readr read_lines
#' @export
read_kdep <- function(file = {cat('Please select kdep file...\n'); file.choose()},
                      dis = {cat('Please select corresponding dis file...\n'); read_dis(file.choose())},
                      huf = {cat('Please select corresponding huf file...\n'); read_huf(file.choose(), dis = dis)}) {
  
  kdep.lines <- read_lines(file)
  kdep <- NULL
  
  # data set 0
    comments <- get_comments_from_lines(kdep.lines)
    kdep.lines <- remove_comments_from_lines(kdep.lines)
  
  # data set 1
    dataSet1 <- split_line_numbers(kdep.lines[1])
    kdep.lines <- kdep.lines[-1]
    kdep$NPKDEP <- dataSet1[1]
    kdep$IFKDEP <- dataSet1[2]
    rm(dataSet1)
  
  # data set 2
    if(kdep$IFKDEP > 0) {
      dataSet2 <- read_array(kdep.lines,dis$NROW,dis$NCOL,1)
      kdep.lines <- dataSet2$remaining_lines
      kdep$RS <- dataSet2$modflow_array
      rm(dataSet2)
    }
  
  # data set 3-4
    kdep$PARNAM <- vector(mode='character',length=kdep$NPKDEP)
    kdep$PARTYP <- vector(mode='character',length=kdep$NPKDEP)
    kdep$Parval <- vector(mode='numeric',length=kdep$NPKDEP)
    kdep$NCLU <- vector(mode='numeric',length=kdep$NPKDEP)
    kdep$Mltarr <- matrix(nrow=huf$NHUF, ncol=kdep$NPKDEP)
    kdep$Zonarr <- matrix(nrow=huf$NHUF, ncol=kdep$NPKDEP)
    kdep$IZ <- matrix(nrow=huf$NHUF, ncol=kdep$NPKDEP)
    for(i in 1:kdep$NPKDEP) {
      line.split <- split_line_words(kdep.lines[1]); kdep.lines <- kdep.lines[-1]
      kdep$PARNAM[i] <- line.split[1]
      kdep$PARTYP[i] <- line.split[2]
      kdep$Parval[i] <- as.numeric(line.split[3])
      kdep$NCLU[i] <- as.numeric(line.split[4])
      for(j in 1:kdep$NCLU[i]) {
        line.split <- split_line_words(kdep.lines[1]); kdep.lines <- kdep.lines[-1]
        k <- which(huf$HGUNAM == line.split[1])
        kdep$Mltarr[k,i] <- line.split[2]
        kdep$Zonarr[k,i] <- line.split[3]
        kdep$IZ[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  comment(kdep) <- comments
  class(kdep) <- c('kdep','modflow_package')
  return(kdep)
}
