mfOptim <- function(nam,optimPval,initialPval,fixed,dir=getwd(),mfVersion='mf2005',doPlot=T,report='', ...)
{
  cat('### mfOptim output ###\n', file=report)
  if(class(initialPval)=='character') pval <- read.pval(file=paste(dir,'/pval/',initialPval,sep=''))
  if(class(initialPval)=='numeric')
  {
    pval <- read.pval(file=paste(dir,'/pval/',optimPval,sep=''))
    pval$Parval <- initialPval
  }
  #write.pval(pval, file=paste(dir,'/pval/',optimPval,sep=''))
  optimVariableParams <- function(parValues)
  {
    pval$Parval[which(!fixed)] <- parValues
    write.pval(pval, file=paste(dir,'/pval/',optimPval,sep=''))
    rmse <- mfRun(nam,dir=dir,mfVersion=mfVersion,doPlot=doPlot)
    cat(paste('RMSE=',rmse,'Parval=',paste(pval$Parval,collapse=' '),'\n'), file=report, append=T)
    return(rmse)
  }
  optim(pval$Parval[which(!fixed)],optimVariableParams, ...)
}
mfOptimFinal <- function(nam,pval,fixed,dir=getwd(),mfVersion='mf2005',doPlot=T,report='', ...)
{
  cat('### mfOptim output ###\n', file=report)
  pval <- read.pval(file=paste(dir,'/pval/',pval,sep=''), read.all=TRUE)
  #write.pval(pval, file=paste(dir,'/pval/',optimPval,sep=''))
  optimVariableParams <- function(parValues)
  {
    pval$Parval[which(!fixed)] <- parValues
    pval$Parval[9:13] <- pval$Parval[15:19]*pval$Parval[14]
    write.pval(pval, file=paste(dir,'/pval/',optimPval,sep=''))
    rmse <- mfRun(nam,dir=dir,mfVersion=mfVersion,doPlot=doPlot)
    cat(paste('RMSE=',rmse,'Parval=',paste(pval$Parval,collapse=' '),'\n'), file=report, append=T)
    return(rmse)
  }
  optim(pval$Parval[which(!fixed)],optimVariableParams, ...)
}
