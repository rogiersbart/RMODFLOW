#' Plot a MODFLOW sensitivity analysis object
#' 
#' @param sen sensitivity analysis object
#' @param plot type: 'css' or 'dss'
#' @method rmf_plot sen
#' @export
rmf_plot.sen <- function(sen,type='css')
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

#' @describeIn rmf_plot.sen Deprecated function name
#' @export
plot.sen <- function(...) {
  .Deprecated(new = "rmf_plot.sen", old = "plot.sen")
  rmf_plot.sen(...)
}
