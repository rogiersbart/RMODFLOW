#' Read a MODFLOW budget file
#' 
#' \code{read_bud} reads in a MODFLOW budget file
#' 
#' @param file filename; typically 'Budget.out'
#' @param binary logical; is source file binary?
#' @return object of class bud
#' @importFrom readr read_lines
#' @export
read_bud <- function(file,binary=TRUE)
{
  if(binary)
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
    class(bud) <- c('bud','modflow_package')
    return(bud)
  } else {
    bud <- list()
    bud.lines <- read_lines(file)
    while(length(bud.lines)!=0)
    {
      name <- substr(bud.lines[1],25,40)
      cat('Processing',name,'...\n')    
      bud[[name]] <- list()
      bud[[name]]$sp <- as.numeric(substr(bud.lines[1],1,12))
      bud[[name]]$ts <- as.numeric(substr(bud.lines[1],13,24))
      bud[[name]]$ncols <- as.numeric(substr(bud.lines[1],41,52))
      bud[[name]]$nrows <- as.numeric(substr(bud.lines[1],53,64))
      bud[[name]]$nlays <- as.numeric(substr(bud.lines[1],65,76))
      bud.lines <- bud.lines[-1]
      
      bud[[name]]$code <- as.numeric(substr(bud.lines[1],1,12))
      bud[[name]]$delt <- as.numeric(substr(bud.lines[1],13,27))
      bud[[name]]$pertim <- as.numeric(substr(bud.lines[1],28,42))
      bud[[name]]$totim <- as.numeric(substr(bud.lines[1],43,57))
      bud.lines <- bud.lines[-1]
      
      if(bud[[name]]$code==1)
      {
        nrecords <- bud[[name]]$ncols * bud[[name]]$nrows * bud[[name]]$nlays
        nlines <- ceiling(nrecords/5)
        dataVector <- NULL
        dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nlines],collapse=' ')))
        bud[[name]]$data <- array(dataVector,dim=c(bud[[name]]$ncols,bud[[name]]$nrows,bud[[name]]$nlays))
        bud[[name]]$data <- aperm(bud[[name]]$data,c(2,1,3))
        names(bud[[name]]$data) <- c('ID','FLUX')
        bud.lines <- bud.lines[-c(1:nlines)]
        bud.lines <- bud.lines[-1]
      }
      if(bud[[name]]$code==2)
      {
        nrecords <- as.numeric(remove_empty_strings(strsplit(bud.lines[1],' ')[[1]]))
        bud.lines <- bud.lines[-1]
        dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nrecords],collapse=' ')))
        bud[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=2,byrow=T))
        names(bud[[name]]$data) <- c('ID','FLUX')
        bud.lines <- bud.lines[-c(1:nrecords)]
        bud.lines <- bud.lines[-1]
      }
      if(bud[[name]]$code==3 | bud[[name]]$code==4)
      {
        nrecords <- bud[[name]]$ncols * bud[[name]]$nrows
        nlines <- ceiling(nrecords/5)
        dataVector <- NULL
        dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nlines],collapse=' ')))
        bud[[name]]$data <- matrix(dataVector,ncol=bud[[name]]$ncols,nrow=bud[[name]]$nrows,byrow=T)
        bud.lines <- bud.lines[-c(1:nlines)]
        bud.lines <- bud.lines[-1]
      }
      if(bud[[name]]$code==5)
      {
        nvalues <- as.numeric(remove_empty_strings(strsplit(bud.lines[1],' ')[[1]]))
        bud.lines <- bud.lines[-1]
        if(nvalues > 1)
        {
          additionalColumns <- rep(NA,nvalues-1)
          for(i in 1:(nvalues-1))
          {
            additionalColumns[i] <- remove_empty_strings(strsplit(bud.lines[1],' ')[[1]])
            bud.lines <- bud.lines[-1]
          }
          #bud.lines <- bud.lines[-1] #IFACE
          #bud.lines <- bud.lines[-1] #CONDFACT
          #bud.lines <- bud.lines[-1] #CELLGRP
        }
        nrecords <- as.numeric(remove_empty_strings(strsplit(bud.lines[1],' ')[[1]]))
        bud.lines <- bud.lines[-1]
        dataVector <- as.numeric(split_line_numbers(paste(bud.lines[1:nrecords],collapse=' ')))
        bud[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=nvalues+1,byrow=T))
        if(nvalues > 1) names(bud[[name]]$data) <- c('ID','FLUX',additionalColumns)
        if(nvalues == 1)names(bud[[name]]$data) <- c('ID','FLUX')
        bud.lines <- bud.lines[-c(1:nrecords)]
        bud.lines <- bud.lines[-1]
      }
    }
    class(bud) <- c('bud','modflow_package')
    return(bud)
  }

}
