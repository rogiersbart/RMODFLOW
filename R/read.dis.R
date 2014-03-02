#' Read a MODFLOW discretization file
#' @param file Filename; typically *.dis
#' @return Object of class dis
#' @export
read.dis <- function(file)
{
  dis.lines <- scan(file, what=character(), sep='\n')
  
  # Data set 0
  dis.lines <- remove.comments.from.lines(dis.lines)
  
  # Data set 1
  dis.dataset1 <- strsplit(dis.lines[1],' ')
  dis <- NULL
  dis$NLAY <- as.numeric(dis.dataset1[[1]][1])
  dis$NROW <- as.numeric(dis.dataset1[[1]][2])
  dis$NCOL <- as.numeric(dis.dataset1[[1]][3])
  dis$NPER <- as.numeric(dis.dataset1[[1]][4])
  dis$ITMUNI <- as.numeric(dis.dataset1[[1]][5])
  dis$LENUNI <- as.numeric(dis.dataset1[[1]][6])
  dis.lines <- dis.lines[-1]
  
  # Data set 2
  dis$LAYCBD <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
  dis.lines <- dis.lines[-1]
  
  # Data set 3
  dis.lines <- dis.lines[-1]
  dis$DELR <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
  dis.lines <- dis.lines[-1]
  
  # Data set 4
  dis.lines <- dis.lines[-1]
  dis$DELC <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
  dis.lines <- dis.lines[-1]
  
  # Data set 5
  dis.lines <- dis.lines[-1]
  dis$TOP <- matrix(nrow=dis$NROW, ncol=dis$NCOL); class(dis$TOP) <- 'mf2darray'
  for(i in 1:dis$NROW) 
  {
    dis$TOP[i,] <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
    dis.lines <- dis.lines[-1]
  }
  
  # Data set 6
  dis$BOTM <- array(dim=c(dis$NROW, dis$NCOL, dis$NLAY+sum(dis$LAYCBD))); class(dis$BOTM) <- 'mf3darray'
  for(i in 1:(dis$NLAY+sum(dis$LAYCBD))) 
  {   
    dis.lines <- dis.lines[-1]
    for(j in 1:dis$NROW) 
    {
      dis$BOTM[j,,i] <- as.numeric(strsplit(dis.lines[1],' ')[[1]])
      dis.lines <- dis.lines[-1]
    }         
  }
  
  # Data set 7
  dis.dataset7 <- strsplit(dis.lines[1],' ')[[1]]
  dis$PERLEN <- as.numeric(dis.dataset7[1])
  dis$NSTP <- as.numeric(dis.dataset7[2])
  dis$TSMULT <- as.numeric(dis.dataset7[3])
  dis$SSTR <- as.character(dis.dataset7[4])
  
  class(dis) <- 'dis'
  return(dis)
}