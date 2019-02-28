#' Plot a 2D section through a MODFLOW 4D array
#' 
#' \code{rmf_plot.rmf_4d_array} plots a 2D section through a MODFLOW 4D array.
#' 
#' @param array an object of class rmf_3d_array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot
#' @param ... parameters provided to plot.rmf_3d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_4d_array
#' @export
rmf_plot.rmf_4d_array <- function(array,
                                       i = NULL,
                                       j = NULL,
                                       k = NULL,
                                       l = NULL,
                                       ...) {
  if(!is.null(l)) {
    rmf_plot(rmf_create_array(array(array[,,,l],dim=dim(array)[1:3])), i=i, j=j, k=k, ...)
  } else if(!is.null(i) & !is.null(j) & !is.null(k)) {
    ggplot(na.omit(data.frame(value=c(array[i,j,k,]), time = attributes(array)$totim)),aes(x=time,y=value))+geom_path()
  } else {
    warning('Plotting final stress period results.', call. = FALSE)
    rmf_plot(rmf_create_array(array(array[,,,dim(array)[4]],dim=dim(array)[1:3])), i=i, j=j, k=k, ...)
  }
}

#' @describeIn rmf_plot.rmf_4d_array Deprecated function name
#' @export
plot.rmf_4d_array <- function(...) {
  .Deprecated(new = "rmf_plot.rmf_4d_array", old = "plot.rmf_4d_array")
  rmf_plot.rmf_4d_array(...)
}
