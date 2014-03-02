################################################################################
### read.ba6 ###################################################################
################################################################################
read.ba6 <- function(file, dis=read.dis(paste(substring(file,1,nchar(file)-4),'.dis',sep='')))
{
  ba6.lines <- scan(file, what=character(), sep='\n')
  
  # Data set 0
  ba6.lines <- remove.comments.from.lines(ba6.lines)
  
  # Data set 1
  ba6.dataset1 <- remove.empty.strings(strsplit(ba6.lines[1],' '))
  ba6 <- NULL
  ba6$XSECTION <- 'XSECTION' %in% ba6.dataset1
  ba6$CHTOCH <- 'CHTOCH' %in% ba6.dataset1
  ba6$FREE <- 'FREE' %in% ba6.dataset1
  ba6$PRINTTIME <- 'PRINTTIME' %in% ba6.dataset1
  ba6$SHOWPROGRESS <- 'SHOWPROGRESS' %in% ba6.dataset1
  ba6$STOPERROR <- 'STOPERROR' %in% ba6.dataset1
  if(ba6$STOPERROR) ba6$STOPER <- as.numeric(ba6.dataset1[match('STOPERROR',ba6.dataset1)+1]) else ba6$STOPER <- as.numeric(NA)
  ba6.lines <- ba6.lines[-1]
  
  # Data set 2
  if(ba6$XSECTION)
  {
    ba6$IBOUND <- matrix(nrow=dis$NLAY, ncol=dis$NCOL)
    class(ba6$IBOUND) <- 'mf2darray'
    ba6.lines <- ba6.lines[-1]
    for(i in 1:dis$NLAY) 
    {
      ba6$IBOUND[i,] <- as.numeric(strsplit(ba6.lines[1],' ')[[1]])
      ba6.lines <- ba6.lines[-1]
    }
  } else {
    ba6$IBOUND <- array(dim=c(dis$NROW, dis$NCOL, dis$NLAY))
    class(ba6$IBOUND) <- 'mf3darray'    
    for(i in 1:(dis$NLAY)) 
    {   
      ba6.lines <- ba6.lines[-1]
      for(j in 1:dis$NROW) 
      {
        ba6$IBOUND[j,,i] <- as.numeric(strsplit(ba6.lines[1],' ')[[1]])
        ba6.lines <- ba6.lines[-1]
      }         
    }
  }
  
  # Data set 3
  ba6$HNOFLO <- as.numeric(strsplit(ba6.lines[1],' ')[[1]])
  ba6.lines <- ba6.lines[-1]
  
  # Data set 4
  if(ba6$XSECTION)
  {
    ba6$STRT <- matrix(nrow=dis$NLAY, ncol=dis$NCOL)
    class(ba6$STRT) <- 'mf2darray'
    ba6.lines <- ba6.lines[-1]
    for(i in 1:dis$NLAY) 
    {
      ba6$STRT[i,] <- as.numeric(strsplit(ba6.lines[1],' ')[[1]])
      ba6.lines <- ba6.lines[-1]
    }
  } else {
    ba6$STRT <- array(dim=c(dis$NROW, dis$NCOL, dis$NLAY))
    class(ba6$STRT) <- 'mf3darray'    
    for(i in 1:(dis$NLAY)) 
    {   
      ba6.lines <- ba6.lines[-1]
      for(j in 1:dis$NROW) 
      {
        ba6$STRT[j,,i] <- as.numeric(strsplit(ba6.lines[1],' ')[[1]])
        ba6.lines <- ba6.lines[-1]
      }         
    }
  }
  class(ba6) <- 'ba6'
  return(ba6)
}



