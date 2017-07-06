#' Convert modflow coordinates to real world coordinates
#' 
#' @param x modflow x coordinate
#' @param y modflow y coordinate
#' @param z modflow z coordinate
#' @param i modflow row number
#' @param j modflow column number
#' @param k modflow layer number
#' @param prj prj object
#' @param dis dis object
#' @details Provide either xyz or ijk
#' @return data frame with real world x and y coordinates
#' @export
rmf_convert_grid_to_xyz <- function(x = NULL,
                                    y = NULL,
                                    z = NULL,
                                    i = NULL,
                                    j = NULL,
                                    k = NULL,
                                    prj,
                                    dis = NULL) {
  if(!is.null(x)) {
    angle <- atan(y/x)*180/pi+prj$rotation
    angle[which(is.na(angle))] <- prj$rotation + 90
    s <- sqrt(x^2+y^2)
    x <- prj$origin[1]+ cos(angle*pi/180)*s
    y <- prj$origin[2]+ sin(angle*pi/180)*s
    if(!is.null(z)) z <- prj$origin[3]+z
    ifelse(!is.null(z),return(data.frame(x=x,y=y,z=z)),return(data.frame(x=x,y=y)))
  } else if(!is.null(i)) {
    # add k, roff coff loff, and vectorize
    y_grid <- c(cumsum(rev(dis$delc))-rev(dis$delc)/2)[(dis$nrow-i+1)]
    x_grid <- c(cumsum(dis$delr)-dis$delr/2)[j]
    s <- sqrt(x_grid^2+y_grid^2)
    angle <- asin(y_grid/s)*180/pi + prj$rotation
    x_grid <- cos(angle*pi/180)*s
    y_grid <- sin(angle*pi/180)*s
    x <- prj$origin[1] + x_grid
    y <- prj$origin[2] + y_grid
    dat <- data.frame(x=x,y=y)
    return(dat)
  }
}                                                                           

#' @describeIn rmf_convert_grid_to_xyz Deprecated function name
convert_grid_to_xyz <- function(...) {
  .Deprecated(new = "rmf_convert_grid_to_xyz", old = "convert_grid_to_xyz")
  rmf_convert_grid_to_xyz(...)
}
