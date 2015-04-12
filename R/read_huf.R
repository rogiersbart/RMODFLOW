#' Read a MODFLOW hydrogeologic unit flow file
#' 
#' \code{read_ba6} reads in a MODFLOW hydrogeologic unit flow file and returns it as an \code{\link{RMODFLOW}} huf object.
#' 
#' @param file Filename; typically *.huf
#' @param dis Corresponding discretization file; typically *.dis
#' @return Object of class huf
#' @export
read_huf <- function(file, dis=read_dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')))
{
  huf.lines <- scan(file, what=character(), sep='\n')
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
    if(sum(huf$LAYWT > 0))
    {
      dataSet4 <- split_line_numbers(huf.lines[1])
      huf.lines <- huf.lines[-1]
      huf$WETFCT <- dataSet4[1]
      huf$IWETIT <- dataSet4[2]
      huf$IHDWET <- dataSet4[3]
      rm(dataSet4)
    }
  
  # Data set 5
    dataSet5 <- int_get_modflow_array(huf.lines,dis$NROW,dis$NCOL,sum(which(huf$LAYWT!=0)))
    huf.lines <- dataSet5$remaining.lines
    huf$WETDRY <- dataSet5$mfarray
    rm(dataSet5)
  
  # Data set 6-8
    huf$HGUNAM <- vector(mode='character',length=huf$NHUF)
    huf$TOP <- array(dim=c(dis$NROW, dis$NCOL, huf$NHUF)); class(huf$TOP) <- 'mf3darray'
    huf$THCK <- array(dim=c(dis$NROW, dis$NCOL, huf$NHUF)); class(huf$THCK) <- 'mf3darray'
    for(i in 1:huf$NHUF)
    {
      huf$HGUNAM[i] <- split_line_words(huf.lines[1])[1]
      huf.lines <- huf.lines[-1]
      dataSet <- int_get_modflow_array(huf.lines,dis$NROW,dis$NCOL,2)
      huf.lines <- dataSet$remaining.lines
      huf$TOP[,,i] <- dataSet$mfarray[,,1]
      huf$THCK[,,i] <- dataSet$mfarray[,,2]
      rm(dataSet)
    }
  
  # Data set 9
    huf$HGUHANI <- vector(mode='numeric',length=huf$NHUF)   
    huf$HGUVANI <- vector(mode='numeric',length=huf$NHUF)
    if(as.character(strsplit(huf.lines[1],' ')[[1]][1] == 'ALL'))
    {
      splitted.line <- split_line_words(huf.lines[1])
      huf$HGUHANI[1] <- as.numeric(splitted.line[2])
      huf$HGUVANI[1] <- as.numeric(splitted.line[3])
      huf.lines <- huf.lines[-1]
      for(i in 1:huf$NHUF) huf$HGUHANI[i] <- huf$HGUHANI[1]
      for(i in 1:huf$NHUF) huf$HGUVANI[i] <- huf$HGUVANI[1]
    }else{
      for(i in 1:huf$NHUF)
      {
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
    for(i in 1:huf$NPHUF)
    {
      line.split <- split_line_words(huf.lines[1]); huf.lines <- huf.lines[-1]
      huf$PARNAM[i] <- line.split[1]
      huf$PARTYP[i] <- line.split[2]
      huf$Parval[i] <- as.numeric(line.split[3])
      huf$NCLU[i] <- as.numeric(line.split[4])
      for(j in 1:huf$NCLU[i])
      {
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