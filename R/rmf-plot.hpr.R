#' Plot a MODFLOW head predictions file
#' 
#' @param hpr head predictions file object
#' @param type plot type: 'scatter' or 'residual'
#' @method rmf_plot hpr
#' @export
rmf_plot.hpr <- function(hpr,type='scatter') {
  dat <- data.frame(simulated_equivalent=hpr$simulated_equivalent, observed_value=hpr$observed_value,observation_name=hpr$observation_name)[which(hpr$simulated_equivalent!=-888),]
  if(type=='scatter') {
    return(  ggplot(dat,aes(x=observed_value,y=simulated_equivalent))+
               geom_point(aes(colour=abs(observed_value-simulated_equivalent)))+
               geom_abline(aes(intercept=0,slope=1),linetype='dashed')+
               scale_colour_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               xlab('Observed value')+ylab('Simulated equivalent')
    )
  } else if(type=='residual') {
    return(  ggplot(dat,aes(x=observation_name,y=simulated_equivalent-observed_value))+
               geom_bar(aes(fill=abs(observed_value-simulated_equivalent)),stat='identity')+
               scale_fill_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               xlab('Observation name')+ylab('Simulated equivalent - observed value')
    )
  }
}

#' @describeIn rmf_plot.hpr Deprecated function name
plot.hpr <- function(...) {
  .Deprecated(new = "rmf_plot.hpr", old = "plot.hpr")
  rmf_plot.hpr(...)
}
