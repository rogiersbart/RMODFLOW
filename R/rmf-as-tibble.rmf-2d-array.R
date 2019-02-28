#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_tibble.rmf_2d_array <- function(array,
                                       dis,
                                       mask = array * 0 + 1,
                                       prj = NULL,
                                       crs = NULL) {
  xy <- expand.grid(cumsum(dis$delr)-dis$delr/2,sum(dis$delc)-(cumsum(dis$delc)-dis$delc/2))
  names(xy) <- c('x','y')
  mask[which(mask==0)] <- NA
  ids <- factor(1:(dis$nrow*dis$ncol))
  xWidth <- rep(dis$delr,dis$nrow)
  yWidth <- rep(dis$delc,each=dis$ncol)
  positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4))
  positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
  positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
  positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
  positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
  positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
  positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
  positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
  positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
  values <- data.frame(id = ids,value = c(t(array*mask^2)))
  if(!is.null(prj)) {
    new_positions <- convert_grid_to_xyz(x=positions$x,y=positions$y,prj=prj)
    positions$x <- new_positions$x
    positions$y <- new_positions$y
  }
  if(!is.null(crs)) {
    positions <- rmfi_convert_coordinates(positions,from=CRS(prj$projection),to=crs)
  }
  return(tibble::as_tibble(na.omit(merge(values, positions, by=c("id")))))
}