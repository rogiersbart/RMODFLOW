#' Generic function to get model performance measures
#' 
#' @return \code{NULL}
#'
#' @rdname performance
#' @method performance hpr
#' @export
#' @import RTOOLZ
performance.hpr <- function(hpr,savePNG=F,file=NULL, residuals=F, doPlot=T)
{
  obsAndSims <- data.frame(SIMULATED.EQUIVALENT=hpr$SIMULATED.EQUIVALENT, OBSERVED.VALUE=hpr$OBSERVED.VALUE,OBSERVATION.NAME=hpr$OBSERVATION.NAME)[which(hpr$SIMULATED.EQUIVALENT!=-888),]
  observations <- obsAndSims$OBSERVED.VALUE
  predictions <- obsAndSims$SIMULATED.EQUIVALENT
  DRY <- 0; if(-888 %in% predictions) DRY <- length(which(predictions == -888))
  if(DRY > 0) predictions <- predictions[-which(predictions == -888)]
  names <- obsAndSims$OBSERVATION.NAME
  perform <- performance_measures(observations,predictions)
  perform$rmse <- sqrt(perform$mse)
#  notRoundedPerformance <- perform
  perform <- round(perform,2)
#   if(doPlot)
#   {
#     ylab <- 'Simulated equivalents (m)'
#     if(residuals)
#     {
#       predictions <- observations - predictions
#       ylab <- 'Residuals (Obs - Sim; m)'
#     }
#     plot(observations,predictions, col='white', xlab='Observed values (m)',ylab=ylab)
#     if(!residuals) abline(0,1)
#     if(!residuals) abline(0.5,1,lty=2)
#     if(!residuals) abline(-0.5,1,lty=2)
#     if(residuals) abline(h=0)
#     if(residuals) abline(h=c(-0.5,0.5),lty=2)
#     text(observations,predictions,names,cex=0.5)
#     if(!residuals) legend('bottomright',c(expression(y==x),expression(y==x%+-%0.5*m)),lty=c(1,2),bty='n')
#     if(!residuals) legend('topleft',c(paste('RMSE =',perform$rmse),paste('MAE =',perform$mae),
#                                       paste('ME =',perform$me),paste('R^2 =',perform$r2),
#                                       paste('NSeff =',perform$nseff)),bty='n',x.intersp=0.25)  
#     if(DRY > 0) legend('bottomleft',paste('DRY predictions:',DRY),col='red',bty='n')
#   }
#   if(savePNG)
#   {
#     png(filename=file ,width=1000, height=1000, res=200)
#     ylab <- 'Simulated equivalents (m)'
#     if(residuals)
#     {
#       predictions <- observations - predictions
#       ylab <- 'Residuals (Obs - Sim; m)'
#     }
#     plot(observations,predictions, col='white', xlab='Observed values (m)',ylab=ylab)
#     if(!residuals) abline(0,1)
#     if(!residuals) abline(0.5,1,lty=2)
#     if(!residuals) abline(-0.5,1,lty=2)
#     if(residuals) abline(h=0)
#     if(residuals) abline(h=c(-0.5,0.5),lty=2)
#     text(observations,predictions,names,cex=0.5)
#     if(!residuals) legend('bottomright',c(expression(y==x),expression(y==x%+-%0.5*m)),lty=c(1,2),bty='n')
#     if(!residuals) legend('topleft',c(paste('RMSE =',perform$rmse),paste('MAE =',perform$mae),
#                                       paste('ME =',perform$me),paste('R^2 =',perform$r2),
#                                       paste('NSeff =',perform$nseff)),bty='n',x.intersp=0.25)
#     if(DRY > 0) legend('bottomleft',paste('DRY predictions:',DRY),col='red',bty='n')
#     dev.off()    
#   }
  return(perform)
}