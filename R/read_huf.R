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

  # Data set 0
    comments <- get_comments_from_lines(huf.lines)
    huf.lines <- remove_comments_from_lines(huf.lines)
  
  # Data set 1
    dataSet1 <- split_line_numbers(huf.lines[1])
    huf.lines <- huf.lines[-1]
    huf$IHUFCB <- dataSet1[1]
    huf$HDRY <- dataSet1[2]
    huf$NHUF <- dataSet1[3]
    huf$NPHUF <- dataSet1[4]
    huf$IOHUFHEADS <- dataSet1[5]
    huf$IOHUFFLOWS <- dataSet1[6]
    rm(dataSet1)
  
  # Data set 2
    huf$LTHUF <- split_line_numbers(huf.lines[1]); huf.lines <- huf.lines[-1]
  
  # Data set 3                            
    huf$LAYWT <- split_line_numbers(huf.lines[1]); huf.lines <- huf.lines[-1]
  
  # Data set 4
    if(sum(huf$LAYWT > 0)) {
      dataSet4 <- split_line_numbers(huf.lines[1])
      huf.lines <- huf.lines[-1]
      huf$WETFCT <- dataSet4[1]
      huf$IWETIT <- dataSet4[2]
      huf$IHDWET <- dataSet4[3]
      rm(dataSet4)
    }
  
  # Data set 5
    dataSet5 <- read_modflow_array(huf.lines,dis$NROW,dis$NCOL,sum(which(huf$LAYWT!=0)))
    huf.lines <- dataSet5$remaining_lines
    huf$WETDRY <- dataSet5$modflow_array
    rm(dataSet5)
  
  # Data set 6-8
    huf$HGUNAM <- vector(mode='character',length=huf$NHUF)
    huf$TOP <- array(dim=c(dis$NROW, dis$NCOL, huf$NHUF)); class(huf$TOP) <- 'mf3darray'
    huf$THCK <- array(dim=c(dis$NROW, dis$NCOL, huf$NHUF)); class(huf$THCK) <- 'mf3darray'
    for(i in 1:huf$NHUF) {
      huf$HGUNAM[i] <- split_line_words(huf.lines[1])[1]
      huf.lines <- huf.lines[-1]
      dataSet <- read_modflow_array(huf.lines,dis$NROW,dis$NCOL,2)
      huf.lines <- dataSet$remaining_lines
      huf$TOP[,,i] <- dataSet$modflow_array[,,1]
      huf$THCK[,,i] <- dataSet$modflow_array[,,2]
      rm(dataSet)
    }
  
  # Data set 9
    huf$HGUHANI <- vector(mode='numeric',length=huf$NHUF)   
    huf$HGUVANI <- vector(mode='numeric',length=huf$NHUF)
    if(as.character(strsplit(huf.lines[1],' ')[[1]][1] == 'ALL')) {
      splitted.line <- split_line_words(huf.lines[1])
      huf$HGUHANI[1] <- as.numeric(splitted.line[2])
      huf$HGUVANI[1] <- as.numeric(splitted.line[3])
      huf.lines <- huf.lines[-1]
      for(i in 1:huf$NHUF) huf$HGUHANI[i] <- huf$HGUHANI[1]
      for(i in 1:huf$NHUF) huf$HGUVANI[i] <- huf$HGUVANI[1]
    } else {
      for(i in 1:huf$NHUF) {
        splitted.line <- split_line_words(huf.lines[1])
        k <- which(huf$HGUNAM == splitted.line[1])
        huf$HGUHANI[k] <- as.numeric(splitted.line[2])
        huf$HGUVANI[k] <- as.numeric(splitted.line[3])
        huf.lines <- huf.lines[-1]
      }      
    }
  
  # Data set 10-11
    huf$PARNAM <- vector(mode='character',length=huf$NPHUF)
    huf$PARTYP <- vector(mode='character',length=huf$NPHUF)
    huf$Parval <- vector(mode='numeric',length=huf$NPHUF)
    huf$NCLU <- vector(mode='numeric',length=huf$NPHUF)
    huf$Mltarr <- matrix(nrow=huf$NHUF, ncol=huf$NPHUF)
    huf$Zonarr <- matrix(nrow=huf$NHUF, ncol=huf$NPHUF)
    huf$IZ <- matrix(nrow=huf$NHUF, ncol=huf$NPHUF)
    for(i in 1:huf$NPHUF) {
      line.split <- split_line_words(huf.lines[1]); huf.lines <- huf.lines[-1]
      huf$PARNAM[i] <- line.split[1]
      huf$PARTYP[i] <- line.split[2]
      huf$Parval[i] <- as.numeric(line.split[3])
      huf$NCLU[i] <- as.numeric(line.split[4])
      for(j in 1:huf$NCLU[i]) {
        line.split <- split_line_words(huf.lines[1]); huf.lines <- huf.lines[-1]
        k <- which(huf$HGUNAM == line.split[1])
        huf$Mltarr[k,i] <- line.split[2]
        huf$Zonarr[k,i] <- line.split[3]
        huf$IZ[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  # Data set 12
    # These are print options, not implemented yet...
  
  comment(huf) <- comments
  class(huf) <- c('huf','modflow_package')
  return(huf)
}
