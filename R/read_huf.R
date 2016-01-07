#' Read a MODFLOW hydrogeologic unit flow file
#' 
#' \code{read_huf} reads in a MODFLOW hydrogeologic unit flow file and returns it as an \code{\link{RMODFLOW}} huf object.
#' 
#' @param file filename; typically '*.huf'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @return object of class huf
#' @importFrom readr read_lines
#' @export
read_huf <- function(file = {cat('Please select huf file...\n'); file.choose()},
                     dis = read_dis()) {
  
  huf_lines <- read_lines(file)
  huf <- NULL

  # data set 0
    data_set_0 <- read_comments(huf_lines)
    comment(huf) <- data_set_0$comments
    huf_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    data_set_1 <- read_variables(huf_lines)
    huf$ihufcb <- data_set_1$variables[1]
    huf$hdry <- data_set_1$variables[2]
    huf$nhuf <- data_set_1$variables[3]
    huf$nphuf <- data_set_1$variables[4]
    huf$iohufheads <- ifelse(is.na(data_set_1$variables[5]),0,data_set_1$variables[5])
    huf$iohufflows <- ifelse(is.na(data_set_1$variables[6]),0,data_set_1$variables[6])
    huf_lines <- data_set_1$remaining_lines
    rm(data_set_1)
  
  # data set 2
    data_set_2 <- read_variables(huf_lines)
    huf$lthuf <- data_set_2$variables
    huf_lines <- data_set_2$remaining_lines
    rm(data_set_2)
  
  # data set 3   
    data_set_3 <- read_variables(huf_lines)
    huf$laywt <- data_set_3$variables
    huf_lines <- data_set_3$remaining_lines
    rm(data_set_3)
  
  # data set 4
    if(sum(huf$laywt > 0)) {
      data_set_4 <- read_variables(huf_lines)
      huf$wetfct <- data_set_4$variables[1]
      huf$iwetit <- data_set_4$variables[2]
      huf$ihdwet <- data_set_4$variables[3]
      huf_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
  
  # data set 5
    data_set_5 <- read_modflow_array(huf_lines,dis$nrow,dis$ncol,sum(which(huf$laywt!=0)))
    huf$wetdry <- data_set_5$array
    huf_lines <- data_set_5$remaining_lines
    rm(data_set_5)
  
  # data set 6-8
    huf$hgunam <- vector(mode='character',length=huf$nhuf)
    huf$top <- create_rmodflow_array(dim=c(dis$nrow, dis$ncol, huf$nhuf))
    huf$thck <- create_rmodflow_array(dim=c(dis$nrow, dis$ncol, huf$nhuf))
    for(i in 1:huf$nhuf) {
      data_set <- read_variables(huf_lines)
      huf$hgunam[i] <- data_set$variables[1]
      huf_lines <- data_set$remaining_lines
      data_set <- read_modflow_array(huf_lines,dis$nrow,dis$ncol,2)
      huf_lines <- data_set$remaining_lines
      huf$top[,,i] <- data_set$array[,,1]
      huf$thck[,,i] <- data_set$array[,,2]  
    }
    rm(data_set)
  
  # data set 9
    huf$hguhani <- vector(mode='numeric',length=huf$nhuf)   
    huf$hguvani <- vector(mode='numeric',length=huf$nhuf)
    data_set_9 <- read_variables(huf_lines)
    if(data_set_9$variables[1] == 'ALL') {
      huf$hguhani <- rep(as.numeric(data_set_9$variables[2]),huf$nhuf)
      huf$hguvani <- rep(as.numeric(data_set_9$variables[3]),huf$nhuf)
      huf_lines <- data_set_9$remaining_lines
    } else {
      k <- which(huf$hgunam == data_set_9$variables[1])
      huf$hguhani[k] <- as.numeric(data_set_9$variables[2])
      huf$hguvani[k] <- as.numeric(data_set_9$variables[3])
      huf_lines <- data_set_9$remaining_lines
      for(i in 2:huf$nhuf) {
        data_set_9 <- read_variables(huf_lines)
        k <- which(huf$hgunam == data_set_9$variables[1])
        huf$hguhani[k] <- as.numeric(data_set_9$variables[2])
        huf$hguvani[k] <- as.numeric(data_set_9$variables[3])
        huf_lines <- data_set_9$remaining_lines
      }      
    }
    rm(data_set_9)
  
  # data set 10-11
    huf$parnam <- vector(mode='character',length=huf$nphuf)
    huf$partyp <- vector(mode='character',length=huf$nphuf)
    huf$parval <- vector(mode='numeric',length=huf$nphuf)
    huf$nclu <- vector(mode='numeric',length=huf$nphuf)
    huf$mltarr <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    huf$zonarr <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    huf$iz <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    for(i in 1:huf$nphuf) {
      data_set_10 <- read_variables(huf_lines)
      huf_lines <- data_set_10$remaining_lines
      huf$parnam[i] <- data_set_10$variables[1]
      huf$partyp[i] <- data_set_10$variables[2]
      huf$parval[i] <- as.numeric(data_set_10$variables[3])
      huf$nclu[i] <- as.numeric(data_set_10$variables[4])
      for(j in 1:huf$nclu[i]) {
        data_set_11 <- read_variables(huf_lines)
        huf_lines <- data_set_11$remaining_lines
        k <- which(huf$hgunam == data_set_11$variables[1])
        huf$mltarr[k,i] <- data_set_11$variables[2]
        huf$zonarr[k,i] <- data_set_11$variables[3]
        huf$iz[k,i] <- paste(data_set_11$variables[-c(1:3)],collapse=' ')
      } 
    }
    rm(data_set_10, data_set_11)
  
  # data set 12
    # These are print options, not implemented yet...
  
  class(huf) <- c('huf','modflow_package')
  return(huf)
}
