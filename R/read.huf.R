#' Read a MODFLOW hydrogeologic unit flow file
#' 
#' \code{read.ba6} reads in a MODFLOW hydrogeologic unit flow file and returns it as an \code{\link{RMODFLOW}} huf object.
#' 
#' @param file Filename; typically *.huf
#' @param dis Corresponding discretization file; typically *.dis
#' @return Object of class huf
#' @export
read.huf <- function(file, dis=read.dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')))
{
  huf.lines <- scan(file, what=character(), sep='\n')
  huf <- NULL
  # Data set 0
  huf.lines <- remove.comments.from.lines(huf.lines)
  # Data set 1
  line.split <- split.line.num(huf.lines[1]); huf.lines <- huf.lines[-1]
  huf$IHUFCB <- line.split[1]
  huf$HDRY <- line.split[2]
  huf$NHUF <- line.split[3]
  huf$NPHUF <- line.split[4]
  huf$IOHUFHEADS <- line.split[5]
  huf$IOHUFFLOWS <- line.split[6]
  # Data set 2
  huf$LTHUF <- split.line.num(huf.lines[1]); huf.lines <- huf.lines[-1]
  # Data set 3                            
  huf$LAYWT <- split.line.num(huf.lines[1]); huf.lines <- huf.lines[-1]
  # Data set 4
  if(sum(huf$LAYWT > 0))
  {
    line.split <- split.line.num(huf.lines[1]); huf.lines <- huf.lines[-1]
    huf$WETFCT <- line.split[1]
    huf$IWETIT <- line.split[2]
    huf$IHDWET <- line.split[3]
  }
  # Data set 5
  huf$WETDRY <- array(dim=c(dis$NROW, dis$NCOL, sum(which(huf$LAYWT!=0))))
  class(huf$WETDRY) <- 'mf3darray'
  for(m in 1:length(huf$LAYWT))
  {
    if(huf$LAYWT[m]==1)
    {  
      huf.lines <- huf.lines[-1]
      for(n in 1:dis$NROW) 
      {
        hud$WETDRY[n,,m] <- split.line.num(huf.lines[1])
        huf.lines <- huf.lines[-1]
      }         
    }
  }
  # Data set 6-8
  huf$HGUNAM <- vector(mode='character',length=huf$NHUF)
  huf$TOP <- array(dim=c(dis$NROW, dis$NCOL, huf$NHUF)); class(huf$TOP) <- 'mf3darray'
  huf$THCK <- array(dim=c(dis$NROW, dis$NCOL, huf$NHUF)); class(huf$THCK) <- 'mf3darray'
  for(i in 1:huf$NHUF)
  {
    huf$HGUNAM[i] <- split.line.char(huf.lines[1])[1]
    huf.lines <- huf.lines[-1]
    huf.lines <- huf.lines[-1]
    for(n in 1:dis$NROW) 
    {
      huf$TOP[n,,i] <- split.line.num(huf.lines[1])
      huf.lines <- huf.lines[-1]
    }
    huf.lines <- huf.lines[-1]
    for(n in 1:dis$NROW) 
    {
      huf$THCK[n,,i] <- split.line.num(huf.lines[1])
      huf.lines <- huf.lines[-1]
    }                            
  }
  # Data set 9
  huf$HGUHANI <- vector(mode='numeric',length=huf$NHUF)   
  huf$HGUVANI <- vector(mode='numeric',length=huf$NHUF)
  if(as.character(strsplit(huf.lines[1],' ')[[1]][1] == 'ALL'))
  {
    splitted.line <- split.line.char(huf.lines[1])
    huf$HGUHANI[1] <- as.numeric(splitted.line[2])
    huf$HGUVANI[1] <- as.numeric(splitted.line[3])
    huf.lines <- huf.lines[-1]
    for(i in 1:huf$NHUF) huf$HGUHANI[i] <- huf$HGUHANI[1]
    for(i in 1:huf$NHUF) huf$HGUVANI[i] <- huf$HGUVANI[1]
  }else{
    for(i in 1:huf$NHUF)
    {
      splitted.line <- split.line.char(huf.lines[1])
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
    line.split <- split.line.char(huf.lines[1]); huf.lines <- huf.lines[-1]
    huf$PARNAM[i] <- line.split[1]
    huf$PARTYP[i] <- line.split[2]
    huf$Parval[i] <- as.numeric(line.split[3])
    huf$NCLU[i] <- as.numeric(line.split[4])
    for(j in 1:huf$NCLU[i])
    {
      line.split <- split.line.char(huf.lines[1]); huf.lines <- huf.lines[-1]
      k <- which(huf$HGUNAM == line.split[1])
      huf$Mltarr[k,i] <- line.split[2]
      huf$Zonarr[k,i] <- line.split[3]
      huf$IZ[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
    } 
  }
  # Data set 12
  # Print options, not implemented
  class(huf) <- 'huf'
  return(huf)
}