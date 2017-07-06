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
rmf_read_kdep <- function(file = {cat('Please select kdep file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          huf = {cat('Please select corresponding huf file ...\n'); rmf_read_huf(file.choose(), dis = dis)}) {
  
  kdep_lines <- read_lines(file)
  kdep <- list()
  
  # data set 0
    data_set_0 <- rmfi_parse_comments(kdep_lines)
    comments <- data_set_0$comments
    kdep_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    data_set_1 <- rmfi_parse_variables(kdep_lines)
    kdep$npkdep <- as.numeric(data_set_1$variables[1])
    kdep$ifkdep <- as.numeric(data_set_1$variables[2])
    kdep_lines <- data_set_1$remaining_lines
    rm(data_set_1)
  
  # data set 2
    if(kdep$ifkdep > 0) {
      data_set2 <- rmfi_parse_array(kdep_lines,dis$nrow,dis$ncol,1)
      kdep_lines <- data_set2$remaining_lines
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
      dat <- rmfi_parse_variables(kdep_lines)
      line.split <- dat$variables
      kdep_lines <- dat$remaining_lines
      kdep$parnam[i] <- line.split[1]
      kdep$partyp[i] <- line.split[2]
      kdep$parval[i] <- as.numeric(line.split[3])
      kdep$nclu[i] <- as.numeric(line.split[4])
      for(j in 1:kdep$nclu[i]) {
        dat <- rmfi_parse_variables(kdep_lines)
        line.split <- dat$variables
        kdep_lines <- dat$remaining_lines
        k <- which(huf$hgunam == line.split[1])
        kdep$mltarr[k,i] <- line.split[2]
        kdep$zonarr[k,i] <- line.split[3]
        kdep$iz[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  comment(kdep) <- comments
  class(kdep) <- c('kdep','rmf_package')
  return(kdep)
}

#' @describeIn rmf_read_kdep Deprecated function name
#' @export
read_kdep <- function(...) {
  .Deprecated(new = "rmf_read_kdep", old = "read_kdep")
  rmf_read_kdep(...)
}
