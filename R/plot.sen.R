#' Plot a MODFLOW sensitivity analysis object
#' 
#' @param sen sensitivity analysis object
#' @param plot type: 'css' or 'dss'
#' @method plot sen
#' @export
plot.sen <- function(sen,type='css')
{
  if(type=='css')
  {
    dat <- data.frame(parnam=sen$parnam,css=sen$css)
    dat$parnam <- factor(as.character(dat$parnam),levels=dat$parnam[order(dat$css,decreasing=TRUE)])
    return(  ggplot(dat,aes(x=parnam,y=css))+
               geom_bar(stat='identity')
    )
  } else if(type=='dss')
  {
    stop('dss plotting not implemented yet')
  }
  
}
