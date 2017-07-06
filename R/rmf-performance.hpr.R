#' Get model performance measures from a hpr object
#' 
#' @param hpr head predictions file object
#' @return performance measures
#'
#' @rdname rmf_performance
#' @method rmf_performance hpr
#' @export
rmf_performance.hpr <- function(hpr) {
  obsAndSims <- data.frame(simulated_equivalent=hpr$simulated_equivalent, observed_value=hpr$observed_value,observation_name=hpr$observation_name)[which(hpr$simulated_equivalent!=-888),]
  observations <- obsAndSims$observed_value
  predictions <- obsAndSims$simulated_equivalent
  dry <- 0; if(-888 %in% predictions) dry <- length(which(predictions == -888))
  if(dry > 0) predictions <- predictions[-which(predictions == -888)]
  names <- obsAndSims$observation_name
  perform <- rmfi_performance_measures(observations,predictions)
  perform$rmse <- sqrt(perform$mse)
#  notRoundedPerformance <- perform
  perform <- round(perform,2)
  return(perform)
}
