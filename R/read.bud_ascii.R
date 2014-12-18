#' Read a MODFLOW budget ascii file
#' 
#' \code{read.bud} reads in a MODFLOW budget ascii file created from the binary file (typically *.bud) using \code{Read_budget.exe} and returns it as an \code{\link{RMODFLOW}} bud object.
#' 
#' @param file Filename; typically "Budget.out"
#' @return Object of class bud
#' @export
read.bud_ascii <- function(file)
{
  
  #file <- 'MODFLOW test files/Reference local model/Budget.out'
  #source('R/utilities.r')
  
  bud <- list()
  bud.lines <- scan(file, what=character(), sep='\n')
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
      dataVector <- as.numeric(split.line.num(paste(bud.lines[1:nlines],collapse=' ')))
      bud[[name]]$data <- array(dataVector,dim=c(bud[[name]]$ncols,bud[[name]]$nrows,bud[[name]]$nlays))
      bud[[name]]$data <- aperm(bud[[name]]$data,c(2,1,3))
      names(bud[[name]]$data) <- c('ID','FLUX')
      bud.lines <- bud.lines[-c(1:nlines)]
      bud.lines <- bud.lines[-1]
    }
    if(bud[[name]]$code==2)
    {
      nrecords <- as.numeric(remove.empty.strings(strsplit(bud.lines[1],' ')[[1]]))
      bud.lines <- bud.lines[-1]
      dataVector <- as.numeric(split.line.num(paste(bud.lines[1:nrecords],collapse=' ')))
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
      dataVector <- as.numeric(split.line.num(paste(bud.lines[1:nlines],collapse=' ')))
      bud[[name]]$data <- matrix(dataVector,ncol=bud[[name]]$ncols,nrow=bud[[name]]$nrows,byrow=T)
      bud.lines <- bud.lines[-c(1:nlines)]
      bud.lines <- bud.lines[-1]
    }
    if(bud[[name]]$code==5)
    {
      nvalues <- as.numeric(remove.empty.strings(strsplit(bud.lines[1],' ')[[1]]))
      bud.lines <- bud.lines[-1]
      if(nvalues > 1)
      {
        additionalColumns <- rep(NA,nvalues-1)
        for(i in 1:(nvalues-1))
        {
          additionalColumns[i] <- remove.empty.strings(strsplit(bud.lines[1],' ')[[1]])
          bud.lines <- bud.lines[-1]
        }
        #bud.lines <- bud.lines[-1] #IFACE
        #bud.lines <- bud.lines[-1] #CONDFACT
        #bud.lines <- bud.lines[-1] #CELLGRP
      }
      nrecords <- as.numeric(remove.empty.strings(strsplit(bud.lines[1],' ')[[1]]))
      bud.lines <- bud.lines[-1]
      dataVector <- as.numeric(split.line.num(paste(bud.lines[1:nrecords],collapse=' ')))
      bud[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=nvalues+1,byrow=T))
      if(nvalues > 1) names(bud[[name]]$data) <- c('ID','FLUX',additionalColumns)
      if(nvalues == 1)names(bud[[name]]$data) <- c('ID','FLUX')
      bud.lines <- bud.lines[-c(1:nrecords)]
      bud.lines <- bud.lines[-1]
    }
  }
  return(bud)
}