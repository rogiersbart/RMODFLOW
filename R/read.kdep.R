#' Read a MODFLOW hydraulic conductivity depth-dependence capability file
#' 
#' \code{read.kdep} reads in a MODFLOW Hydraulic-Conductivity Depth-Dependence Capability file and returns it as an \code{\link{RMODFLOW}} kdep object.
#' 
#' @param file Filename; typically *.kdep
#' @param dis Corresponding discretization file; typically *.dis
#' @return Object of class kdep
#' @export
read.kdep <- function(file, dis=read.dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')), huf=read.huf(paste(substring(file,1,nchar(file)-4),'.huf',sep=''),dis))
{
  kdep.lines <- scan(file, what=character(), sep='\n')
  kdep <- NULL
  
  # Data set 0
    kdep.lines <- remove.comments.from.lines(kdep.lines)
  
  # Data set 1
    dataSet1 <- split.line.num(kdep.lines[1])
    kdep.lines <- kdep.lines[-1]
    kdep$NPKDEP <- dataSet1[1]
    kdep$IFKDEP <- dataSet1[2]
    rm(dataSet1)
  
  # Data set 2
    if(kdep$IFKDEP > 0)
    {
      dataSet2 <- get.mfarray(kdep.lines,dis$NROW,dis$NCOL,1)
      kdep.lines <- dataSet2$remaining.lines
      kdep$RS <- dataSet2$mfarray
      rm(dataSet2)
    }
  
  # Data set 3-4
    kdep$PARNAM <- vector(mode='character',length=kdep$NPKDEP)
    kdep$PARTYP <- vector(mode='character',length=kdep$NPKDEP)
    kdep$Parval <- vector(mode='numeric',length=kdep$NPKDEP)
    kdep$NCLU <- vector(mode='numeric',length=kdep$NPKDEP)
    kdep$Mltarr <- matrix(nrow=huf$NHUF, ncol=kdep$NPKDEP)
    kdep$Zonarr <- matrix(nrow=huf$NHUF, ncol=kdep$NPKDEP)
    kdep$IZ <- matrix(nrow=huf$NHUF, ncol=kdep$NPKDEP)
    for(i in 1:kdep$NPKDEP)
    {
      line.split <- split.line.char(kdep.lines[1]); kdep.lines <- kdep.lines[-1]
      kdep$PARNAM[i] <- line.split[1]
      kdep$PARTYP[i] <- line.split[2]
      kdep$Parval[i] <- as.numeric(line.split[3])
      kdep$NCLU[i] <- as.numeric(line.split[4])
      for(j in 1:kdep$NCLU[i])
      {
        line.split <- split.line.char(kdep.lines[1]); kdep.lines <- kdep.lines[-1]
        k <- which(huf$HGUNAM == line.split[1])
        kdep$Mltarr[k,i] <- line.split[2]
        kdep$Zonarr[k,i] <- line.split[3]
        kdep$IZ[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
      } 
    }
  
  class(kdep) <- c('kdep','mfpackage')
  return(kdep)
}