#' Run a MODFLOW model
#' 
#' \code{read_ba6} reads in a MODFLOW basic file and returns it as an \code{\link{RMODFLOW}} ba6 object.
#' 
#' @param file Filename; typically "*.ba6"
#' @return Object of class ba6
#' @export
run_modflow <- function(nam,dir=getwd(),mfVersion='mf2005',doPlot=T)
{
  shell(paste('cd',dir,'&',mfVersion,nam))
}