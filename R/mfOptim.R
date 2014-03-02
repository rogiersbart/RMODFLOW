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


# mfOptim <- function(parameters, doPlot=T, report='mfOptim report.txt')
# {
#   if(model.run.number==1 | ((model.run.number-1)-((model.run.number-1) %/% 9)*9) == 0)
#   {
#     hed <- read.hed('mf2005/output.hed',dis,ba6)
#     #hed[which(is.na(hed))] <- -999
#     ba6$STRT <- hed
#     ba6$STRT[which(ba6$STRT == -888)] <- ba6Backup$STRT[which(ba6$STRT == -888)]
#     write.ba6(ba6,'mf2005/input_57.ba6')
#   }
#   parameterFile <- read.csv('parameters.csv')
#   parameterFile$value <- parameters
#   write.csv(parameterFile, 'parameters.csv',row.names=F)
#   pval <- read.pval('mf2005/initial.pval')
#   pval$Parval[3] <- parameters[1]
#   pval$Parval[4] <- parameters[2]
#   pval$Parval[10:13] <- pval$Parval[10:13] * parameters[4]
#   pval$Parval[9] <- parameters[3] * pval$Parval[11]
#   write.pval(pval,'mf2005/input.pval')
#   run.model()
#   newDirect <- read.table('mf2005/output.hpr',header=T)
#   names(newDirect) <- c('sim','obs','id')
#   newDirect <- newDirect[which(newDirect$sim >= 0),] # remove dry obs
#   newDerived <- derived.obs('mf2005/output.hpr')  
#   names(newDerived) <- c('sim','obs')
#   newCombined <- rbind(newDerived,newDirect[,1:2])
#   newRMSE <- rmse(newCombined$obs, newCombined$sim)
#   if(doPlot)
#   {
#     plot(newDirect$obs,newDirect$sim, xlim=c(13.5,26), ylim=c(13.5,26), xlab='Observed values',ylab='Simulated equivalents')
#     lines(c(22.5,22.5)+0.7,c(14.5,20.5)-1,lty=2, col='lightgray')
#     lines(c(19.5,25.5)+0.7,c(17.5,17.5)-1,lty=2, col='lightgray')
#     points(newDerived$obs+22.5+0.7, newDerived$sim+17.5-1, pch=2)
#     lines(c(19.5,25.5,25.5,19.5,19.5)+0.7,c(14.5,14.5,20.5,20.5,14.5)-1)
#     abline(0,1,lty=2)
#     abline(1,1,lty=2,col='lightgray'); abline(-1,1,lty=2,col='lightgray')
#     lines(c(19.5,25.5)+0.7,c(14.5,20.5)-1,lty=2)
#     
#   }
#   cat(paste('RMSE=',newRMSE,'Parval=',paste(parameters,collapse=' '),'\n'), file=report, append=T)
#   cat(paste('RMSE=',newRMSE,'Parval=',paste(parameters,collapse=' '),'\n'))
#   model.run.number <- model.run.number + 1 # or <<- ???
#   return(newRMSE)
# }
