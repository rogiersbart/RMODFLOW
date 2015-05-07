#' Plot a MODFLOW head predictions file
#' 
#' @method plot hpr
#' @export
plot.hpr <- function(hpr,type='scatter')
{
  dat <- data.frame(SIMULATED.EQUIVALENT=hpr$SIMULATED.EQUIVALENT, OBSERVED.VALUE=hpr$OBSERVED.VALUE,OBSERVATION.NAME=hpr$OBSERVATION.NAME)[which(hpr$SIMULATED.EQUIVALENT!=-888),]
  if(type=='scatter')
  {
    return(  ggplot(dat,aes(x=OBSERVED.VALUE,y=SIMULATED.EQUIVALENT))+
               geom_point(aes(colour=abs(OBSERVED.VALUE-SIMULATED.EQUIVALENT)))+
               geom_abline(aes(intercept=0,slope=1),linetype='dashed')+
               scale_colour_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               xlab('Observed value')+ylab('Simulated equivalent')
    )
  } else if(type=='residual')
  {
    return(  ggplot(dat,aes(x=OBSERVATION.NAME,y=SIMULATED.EQUIVALENT-OBSERVED.VALUE))+
               geom_bar(aes(fill=abs(OBSERVED.VALUE-SIMULATED.EQUIVALENT)),stat='identity')+
               scale_fill_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               xlab('Observation name')+ylab('Simulated equivalent - observed value')
    )
  }

}