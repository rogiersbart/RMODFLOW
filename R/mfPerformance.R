mfPerformance <- function(path,savePNG=F,file=NULL, residuals=F, doPlot=T)
{
  obsAndSims <- read.table(path,header=T)
  obsAndSims <- obsAndSims[which(obsAndSims$SIMULATED.EQUIVALENT!=-888),]
  observations <- obsAndSims$OBSERVED.VALUE
  predictions <- obsAndSims$SIMULATED.EQUIVALENT
  DRY <- 0; if(-888 %in% predictions) DRY <- length(which(predictions == -888))
  if(DRY > 0) predictions <- predictions[-which(predictions == -888)]
  names <- obsAndSims$OBSERVATION.NAME
  performance <- performance(observations,predictions)
  performance$rmse <- sqrt(performance$mse)
  notRoundedPerformance <- performance
  performance <- round(performance,2)
  if(doPlot)
  {
    ylab <- 'Simulated equivalents (m)'
    if(residuals)
    {
      predictions <- observations - predictions
      ylab <- 'Residuals (Obs - Sim; m)'
    }
    plot(observations,predictions, col='white', xlab='Observed values (m)',ylab=ylab)
    if(!residuals) abline(0,1)
    if(!residuals) abline(0.5,1,lty=2)
    if(!residuals) abline(-0.5,1,lty=2)
    if(residuals) abline(h=0)
    if(residuals) abline(h=c(-0.5,0.5),lty=2)
    text(observations,predictions,names,cex=0.5)
    if(!residuals) legend('bottomright',c(expression(y==x),expression(y==x%+-%0.5*m)),lty=c(1,2),bty='n')
    if(!residuals) legend('topleft',c(paste('RMSE =',performance$rmse),paste('MAE =',performance$mae),
                                      paste('ME =',performance$me),paste('R^2 =',performance$r2),
                                      paste('NSeff =',performance$nseff)),bty='n',x.intersp=0.25)  
    if(DRY > 0) legend('bottomleft',paste('DRY predictions:',DRY),col='red',bty='n')
  }
  if(savePNG)
  {
    png(filename=file ,width=1000, height=1000, res=200)
    ylab <- 'Simulated equivalents (m)'
    if(residuals)
    {
      predictions <- observations - predictions
      ylab <- 'Residuals (Obs - Sim; m)'
    }
    plot(observations,predictions, col='white', xlab='Observed values (m)',ylab=ylab)
    if(!residuals) abline(0,1)
    if(!residuals) abline(0.5,1,lty=2)
    if(!residuals) abline(-0.5,1,lty=2)
    if(residuals) abline(h=0)
    if(residuals) abline(h=c(-0.5,0.5),lty=2)
    text(observations,predictions,names,cex=0.5)
    if(!residuals) legend('bottomright',c(expression(y==x),expression(y==x%+-%0.5*m)),lty=c(1,2),bty='n')
    if(!residuals) legend('topleft',c(paste('RMSE =',performance$rmse),paste('MAE =',performance$mae),
                       paste('ME =',performance$me),paste('R^2 =',performance$r2),
                       paste('NSeff =',performance$nseff)),bty='n',x.intersp=0.25)
    if(DRY > 0) legend('bottomleft',paste('DRY predictions:',DRY),col='red',bty='n')
    dev.off()    
  }
  return(notRoundedPerformance$rmse)
}