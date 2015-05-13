#' Plot a MODFLOW sensitivity analysis object
#' 
#' @method plot sen
#' @export
plot.sen <- function(sen,type='css')
{
  if(type=='css')
  {
    dat <- data.frame(PARNAM=sen$PARNAM,css=sen$css)
    dat$PARNAM <- factor(as.character(dat$PARNAM),levels=dat$PARNAM[order(dat$css,decreasing=TRUE)])
    return(  ggplot(dat,aes(x=PARNAM,y=css))+
               geom_bar(stat='identity')
    )
  } else if(type=='dss')
  {
    stop('dss plotting not implemented yet')
  }
  
}