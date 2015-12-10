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
                     dis = {cat('Please select corresponding dis file...\n'); read_dis(file.choose())}) {
  
  huf.lines <- read_lines(file)
  huf <- NULL

  # data set 0
    comments <- get_comments_from_lines(huf.lines)
    huf.lines <- remove_comments_from_lines(huf.lines)
  
  # data set 1
    data_set1 <- split_line_numbers(huf.lines[1])
    huf.lines <- huf.lines[-1]
    huf$ihufcb <- data_set1[1]
    huf$hdry <- data_set1[2]
    huf$nhuf <- data_set1[3]
    huf$nphuf <- data_set1[4]
    huf$iohufheads <- data_set1[5]
    huf$iohufflows <- data_set1[6]
    rm(data_set1)
  
  # data set 2
    huf$lthuf <- split_line_numbers(huf.lines[1]); huf.lines <- huf.lines[-1]
  
  # data set 3                            
    huf$laywt <- split_line_numbers(huf.lines[1]); huf.lines <- huf.lines[-1]
  
  # data set 4
    if(sum(huf$laywt > 0)) {
      data_set4 <- split_line_numbers(huf.lines[1])
      huf.lines <- huf.lines[-1]
      huf$wetfct <- data_set4[1]
      huf$iwetit <- data_set4[2]
      huf$ihdwet <- data_set4[3]
      rm(data_set4)
    }
  
  # data set 5
    data_set5 <- read_array(huf.lines,dis$nrow,dis$ncol,sum(which(huf$laywt!=0)))
    huf.lines <- data_set5$remaining_lines
    huf$wetdry <- data_set5$array
    rm(data_set5)
  
  # data set 6-8
    huf$hgunam <- vector(mode='character',length=huf$nhuf)
    huf$top <- array(dim=c(dis$nrow, dis$ncol, huf$nhuf)); class(huf$top) <- 'mf3darray'
    huf$thck <- array(dim=c(dis$nrow, dis$ncol, huf$nhuf)); class(huf$thck) <- 'mf3darray'
    for(i in 1:huf$nhuf) {
      huf$hgunam[i] <- split_line_words(huf.lines[1])[1]
      huf.lines <- huf.lines[-1]
      data_set <- read_array(huf.lines,dis$nrow,dis$ncol,2)
      huf.lines <- data_set$remaining_lines
      huf$top[,,i] <- data_set$array[,,1]
      huf$thck[,,i] <- data_set$array[,,2]
      rm(data_set)
    }
  
  # data set 9
    huf$hguhani <- vector(mode='numeric',length=huf$nhuf)   
    huf$hguvani <- vector(mode='numeric',length=huf$nhuf)
    if(as.character(strsplit(huf.lines[1],' ')[[1]][1] == 'ALL')) {
      splitted.line <- split_line_words(huf.lines[1])
      huf$hguhani[1] <- as.numeric(splitted.line[2])
      huf$hguvani[1] <- as.numeric(splitted.line[3])
      huf.lines <- huf.lines[-1]
      for(i in 1:huf$nhuf) huf$hguhani[i] <- huf$hguhani[1]
      for(i in 1:huf$nhuf) huf$hguvani[i] <- huf$hguvani[1]
    } else {
      for(i in 1:huf$nhuf) {
        splitted.line <- split_line_words(huf.lines[1])
        k <- which(huf$hgunam == splitted.line[1])
        huf$hguhani[k] <- as.numeric(splitted.line[2])
        huf$hguvani[k] <- as.numeric(splitted.line[3])
        huf.lines <- huf.lines[-1]
      }      
    }
  
  # data set 10-11
    huf$parnam <- vector(mode='character',length=huf$nphuf)
    huf$partyp <- vector(mode='character',length=huf$nphuf)
    huf$parval <- vector(mode='numeric',length=huf$nphuf)
    huf$nclu <- vector(mode='numeric',length=huf$nphuf)
    huf$mltarr <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    huf$zonarr <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    huf$iz <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    for(i in 1:huf$nphuf) {
      line.split <- split_line_words(huf.lines[1]); huf.lines <- huf.lines[-1]
      huf$parnam[i] <- line.split[1]
      huf$partyp[i] <- line.split[2]
      huf$parval[i] <- as.numeric(line.split[3])
      huf$nclu[i] <- as.numeric(line.split[4])
      for(j in 1:huf$nclu[i]) {
        line.split <- split_line_words(huf.lines[1]); huf.lines <- huf.lines[-1]
        k <- which(huf$hgunam == line.split[1])
        huf$mltarr[k,i] <- line.split[2]
        huf$zonarr[k,i] <- line.split[3]
        huf$iz[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  # data set 12
    # These are print options, not implemented yet...
  
  comment(huf) <- comments
  class(huf) <- c('huf','modflow_package')
  return(huf)
}
