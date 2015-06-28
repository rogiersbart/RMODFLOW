#' Get model performance measures from a hpr object
#' 
#' @param hpr head predictions file object
#' @return performance measures
#'
#' @rdname performance
#' @method performance hpr
#' @export
#' @import RTOOLZ
performance.hpr <- function(hpr)
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
  return(perform)
}