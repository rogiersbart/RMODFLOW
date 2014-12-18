#' Read a MODFLOW budget file
#' 
#' \code{read.bud} reads in a MODFLOW budget file
#' 
#' @param file Filename; typically "Budget.out"
#' @return Object of class bud
#' @export
read.bud <- function(file)
{
  con <- file(file,open='rb')
  
  bud <- list()
  KSTP <- readBin(con,what='integer',n=1)
  KPER <- readBin(con,what='integer',n=1)
  DESC <- readChar(con,nchars=16)
  while(length(DESC!=0))
  {

    if(DESC=='   CONSTANT HEAD') { name <- 'CONSTANT_HEAD'
    } else if(DESC=='         STORAGE') { name <- 'STORAGE'
    } else if(DESC=='FLOW RIGHT FACE ') { name <- 'FLOW_RIGHT_FACE'
    } else if(DESC=='FLOW FRONT FACE ') { name <- 'FLOW_FRONT_FACE'
    } else if(DESC=='FLOW LOWER FACE ') { name <- 'FLOW_LOWER_FACE'
    } else if(DESC=='           WELLS') { name <- 'WELLS'
    } else if(DESC=='   RIVER LEAKAGE') { name <- 'RIVER_LEAKAGE'
    } else if(DESC=='        RECHARGE') { name <- 'RECHARGE'
    } else if(DESC=='          DRAINS') { name <- 'DRAINS'
    } else if(DESC==' HEAD DEP BOUNDS') { name <- 'HEAD_DEP_BOUNDS'
    } else {name <- DESC}
    
    bud[[name]][[KPER]] <- list()
    bud[[name]][[KPER]][[KSTP]] <- list()
    bud[[name]][[KPER]][[KSTP]]$KSTP <- KSTP
    bud[[name]][[KPER]][[KSTP]]$KPER <- KPER
    bud[[name]][[KPER]][[KSTP]]$DESC <- DESC
    bud[[name]][[KPER]][[KSTP]]$NCOL <- readBin(con,what='integer',n=1)
    bud[[name]][[KPER]][[KSTP]]$NROW <- readBin(con,what='integer',n=1)
    bud[[name]][[KPER]][[KSTP]]$NLAY <- readBin(con,what='integer',n=1)
    
    if(bud[[name]][[KPER]][[KSTP]]$NLAY > 0)
    {
      bud[[name]][[KPER]][[KSTP]]$data <- array(readBin(con,what='numeric',n=bud[[name]][[KPER]][[KSTP]]$NCOL*bud[[name]][[KPER]][[KSTP]]$NROW*bud[[name]][[KPER]][[KSTP]]$NLAY,size=4),dim=c(bud[[name]][[KPER]][[KSTP]]$NCOL,bud[[name]][[KPER]][[KSTP]]$NROW,bud[[name]][[KPER]][[KSTP]]$NLAY))
    } else {
      bud[[name]][[KPER]][[KSTP]]$ITYPE <- readBin(con,what='integer',n=1)
      bud[[name]][[KPER]][[KSTP]]$DELT <- readBin(con,what='numeric',n=1,size=4)
      bud[[name]][[KPER]][[KSTP]]$PERTIM <- readBin(con,what='numeric',n=1,size=4)
      bud[[name]][[KPER]][[KSTP]]$TOTIM <- readBin(con,what='numeric',n=1,size=4)
      if(bud[[name]][[KPER]][[KSTP]]$ITYPE==5)
      {
        bud[[name]][[KPER]][[KSTP]]$NVAL <- readBin(con,what='integer',n=1)
      } else {
        bud[[name]][[KPER]][[KSTP]]$NVAL <- 1
      }
      if(bud[[name]][[KPER]][[KSTP]]$NVAL > 1)
      {
        bud[[name]][[KPER]][[KSTP]]$CTMP <- rep(NA, (bud[[name]][[KPER]][[KSTP]]$NVAL-1))
        for(nr in 1:(bud[[name]][[KPER]][[KSTP]]$NVAL-1))
        {
          bud[[name]][[KPER]][[KSTP]]$CTMP[nr] <- readChar(con,nchars=16)
        }
      }
      
      if(bud[[name]][[KPER]][[KSTP]]$ITYPE %in% c(2,5))
      {
        bud[[name]][[KPER]][[KSTP]]$NLIST <- readBin(con,what='integer',n=1)
        if(bud[[name]][[KPER]][[KSTP]]$NLIST > 0)
        {
          bud[[name]][[KPER]][[KSTP]]$data <- as.data.frame(matrix(,nrow=bud[[name]][[KPER]][[KSTP]]$NLIST,ncol=bud[[name]][[KPER]][[KSTP]]$NVAL+1))
          names(bud[[name]][[KPER]][[KSTP]]$data)[1] <- 'ICELL'
          for(nr in 1:bud[[name]][[KPER]][[KSTP]]$NLIST)
          {
            bud[[name]][[KPER]][[KSTP]]$data[nr,] <- c(readBin(con,what='integer',n=1),readBin(con,what='numeric',n=bud[[name]][[KPER]][[KSTP]]$NVAL,size=4))
          }
        }
      }
      if(bud[[name]][[KPER]][[KSTP]]$ITYPE %in% c(0,1))
      {
        bud[[name]][[KPER]][[KSTP]]$data <- array(readBin(con,what='numeric',n=bud[[name]][[KPER]][[KSTP]]$NCOL*bud[[name]][[KPER]][[KSTP]]$NROW*abs(bud[[name]][[KPER]][[KSTP]]$NLAY),size=4),dim=c(bud[[name]][[KPER]][[KSTP]]$NCOL,bud[[name]][[KPER]][[KSTP]]$NROW,abs(bud[[name]][[KPER]][[KSTP]]$NLAY)))
      }
      if(bud[[name]][[KPER]][[KSTP]]$ITYPE ==3)
      {
        bud[[name]][[KPER]][[KSTP]]$layer <- matrix(readBin(con,what='integer',n=bud[[name]][[KPER]][[KSTP]]$NCOL*bud[[name]][[KPER]][[KSTP]]$NROW),ncol=bud[[name]][[KPER]][[KSTP]]$NCOL,nrow=bud[[name]][[KPER]][[KSTP]]$NROW,byrow=TRUE)
        bud[[name]][[KPER]][[KSTP]]$data <- matrix(readBin(con,what='numeric',n=bud[[name]][[KPER]][[KSTP]]$NCOL*bud[[name]][[KPER]][[KSTP]]$NROW,size=4),ncol=bud[[name]][[KPER]][[KSTP]]$NCOL,nrow=bud[[name]][[KPER]][[KSTP]]$NROW,byrow=TRUE)
      }
      if(bud[[name]][[KPER]][[KSTP]]$ITYPE ==4)
      {
        bud[[name]][[KPER]][[KSTP]]$data <- matrix(readBin(con,what='numeric',n=bud[[name]][[KPER]][[KSTP]]$NCOL*bud[[name]][[KPER]][[KSTP]]$NROW,size=4),ncol=bud[[name]][[KPER]][[KSTP]]$NCOL,nrow=bud[[name]][[KPER]][[KSTP]]$NROW,byrow=TRUE)
      }
    }
    KSTP <- readBin(con,what='integer',n=1)
    KPER <- readBin(con,what='integer',n=1)
    DESC <- readChar(con,nchars=16)
  }

  close(con)

  # class specification?
  return(bud)
}