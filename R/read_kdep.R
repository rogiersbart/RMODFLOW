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
    data_set1 <- split_line_numbers(kdep.lines[1])
    kdep.lines <- kdep.lines[-1]
    kdep$npkdep <- data_set1[1]
    kdep$ifkdep <- data_set1[2]
    rm(data_set1)
  
  # data set 2
    if(kdep$ifkdep > 0) {
      data_set2 <- read_array(kdep.lines,dis$nrow,dis$ncol,1)
      kdep.lines <- data_set2$remaining_lines
      kdep$rs <- data_set2$array
      rm(data_set2)
    }
  
  # data set 3-4
    kdep$parnam <- vector(mode='character',length=kdep$npkdep)
    kdep$partyp <- vector(mode='character',length=kdep$npkdep)
    kdep$parval <- vector(mode='numeric',length=kdep$npkdep)
    kdep$nclu <- vector(mode='numeric',length=kdep$npkdep)
    kdep$mltarr <- matrix(nrow=huf$nhuf, ncol=kdep$npkdep)
    kdep$zonarr <- matrix(nrow=huf$nhuf, ncol=kdep$npkdep)
    kdep$iz <- matrix(nrow=huf$nhuf, ncol=kdep$npkdep)
    for(i in 1:kdep$npkdep) {
      line.split <- split_line_words(kdep.lines[1]); kdep.lines <- kdep.lines[-1]
      kdep$parnam[i] <- line.split[1]
      kdep$partyp[i] <- line.split[2]
      kdep$parval[i] <- as.numeric(line.split[3])
      kdep$nclu[i] <- as.numeric(line.split[4])
      for(j in 1:kdep$nclu[i]) {
        line.split <- split_line_words(kdep.lines[1]); kdep.lines <- kdep.lines[-1]
        k <- which(huf$hgunam == line.split[1])
        kdep$mltarr[k,i] <- line.split[2]
        kdep$zonarr[k,i] <- line.split[3]
        kdep$iz[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  comment(kdep) <- comments
  class(kdep) <- c('kdep','modflow_package')
  return(kdep)
}
