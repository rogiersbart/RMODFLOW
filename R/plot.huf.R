#' Plot a 2D section through a MODFLOW 3D array
#' 
#' \code{plot.huf} plots a 2D section through a MODFLOW 3D array.
#' 
#' @param huf an object of class huf
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param dis discretization file object
#' @param bas basic file object; optional
#' @param mask a 3D array with 0 or F indicating inactive cells optional; defaults to having all cells active or, if bas is provided, bas$ibound
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'factor' or 'grid'
#' @param levels labels that should be used on the factor legend; huf$hgunam is used by default
#' @param grid logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param title plot title
#' @param hed hed object for only plotting the saturated part of the grid; possibly subsetted with time step number; by default, last time step is used
#' @param l time step number for subsetting the hed object
#' @param ... parameters provided to plot.rmodflow_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method plot huf
#' @export
plot.huf <- function(huf,
                          i = NULL,
                          j = NULL,
                          k = NULL,
                          dis,
                          bas = NULL,
                          #mask = ifelse0(is.null(bas),array*0+1,bas$ibound),
                          #zlim = range(array[ifelse0(is.null(i),c(1:dim(array)[1]),i),ifelse0(is.null(j),c(1:dim(array)[2]),j),ifelse0(is.null(k),c(1:dim(array)[3]),k)][as.logical(mask[ifelse0(is.null(i),c(1:dim(array)[1]),i),ifelse0(is.null(j),c(1:dim(array)[2]),j),ifelse0(is.null(k),c(1:dim(array)[3]),k)])], finite=TRUE),
                          colour_palette = rev_rainbow,
                          nlevels = 7,
                          levels = huf$hgunam,
                          type='fill',
                          grid = FALSE,
                          add=FALSE,
                          title = NULL,
                          hed = NULL,
                          l = NULL,
                          ...) {
    hufdis <- convert_huf_to_dis(huf = huf, dis = dis)
    huf_array <- create_array(rep(1:huf$nhuf,each=dis$nrow*dis$ncol),dim=c(dis$nrow,dis$ncol,huf$nhuf))
    p <- plot(huf_array, dis = hufdis, i=i,j=j,k=k,colour_palette=colour_palette,nlevels=nlevels,type='factor',add=add,title=title,levels=levels)
    if(grid == TRUE) {
      return(p + plot(dis$botm, dis = dis, i=i,j=j,k=k,bas=bas,type='grid',add=TRUE))
    } else if(grid == 'huf') {
      return(p + plot(hufdis$botm, dis = hufdis, i=i,j=j,k=k,type='grid',add=TRUE))
    } else {
      return(p)
    }
}
