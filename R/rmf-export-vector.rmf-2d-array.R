#' Generic function to export vectors
#' 
#' @rdname rmf_export_vector
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS
#' @importFrom rgdal writeOGR
#' @export
rmf_export_vector.rmf_2d_array <- function(array,
                                           dis,
                                           bas = NULL,
                                           mask = rmfi_ifelse0(is.null(bas),array*0+1,bas$ibound[,,1]),
                                           prj=NULL,
                                           crs=NULL,
                                           file='rmf_vector',
                                           type = 'ESRI Shapefile',
                                           include_ijk = FALSE) {

  
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
  if(!is.null(prj)) {
    new_positions <- convert_grid_to_xyz(x=positions$x,y=positions$y,prj=prj)
    positions$x <- new_positions$x
    positions$y <- new_positions$y
  }
  if(!is.null(crs)) {
    positions <- rmfi_convert_coordinates(positions,from=CRS(prj$projection),to=crs)
  }
  
  positions_matrix_x <- matrix(positions$x,nrow=length(ids),ncol=4,byrow=TRUE)
  positions_matrix_y <- matrix(positions$y,nrow=length(ids),ncol=4,byrow=TRUE)
  positions_matrix <- cbind(ids,positions_matrix_x, positions_matrix_y)
  create_polygon_from_row <- function(dat) Polygons(list(Polygon(data.frame(x=dat[2:5],y=dat[6:9]))), ID = dat[1])
  polygons_list <- apply(positions_matrix, 1, create_polygon_from_row)
  
  
#   # takes too long
#   polygons_list <- list(length=length(ids))
#   for(i in 1:length(ids)) {
#     polygons_list[[i]] <- Polygons(list(Polygon(positions[which(positions$id==ids[i]),c('x','y')])), ID = ids[i])
#   }
  
  # apply mask!!
  # add i, j to data.frame
  
  SP <- SpatialPolygons(polygons_list, proj4string=CRS(prj$projection))
  DF <- data.frame(value = c(t(array*mask^2)), row.names = ids)
  if(include_ijk) {
    ijk <- convert_modflow_id_to_ijk(1:(dis$nrow*dis$ncol), dis)
    DF$i <- ijk$i
    DF$j <- ijk$j
    DF$k <- ijk$k
  }
  ids_to_keep <- row.names(na.omit(DF))
  SPDF <- SpatialPolygonsDataFrame(SP[which(1:(dis$nrow*dis$ncol) %in% ids_to_keep)], na.omit(DF))
  writeOGR(SPDF, dsn = '.', layer = file, driver = type, overwrite_layer = TRUE)
}
