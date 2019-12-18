
## SPATIAL --> RMODFLOW
##

#' Title
#'
#' @param obj 
#' @param dis 
#' @param select 
#' @param prj 
#' @param kper 
#' @param op 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_list.sf <- function(obj,
                           dis,
                           select = colnames(sf::st_set_geometry(obj, NULL)), 
                           prj = NULL, 
                           k = NULL,
                           kper = attr(obj, 'kper'),
                           op = sf::st_intersects) {
  
  # TODO check if obj projection == dis projection
  
  target <- rmf_create_array(1:(dis$nrow*dis$ncol), dim = c(dis$nrow, dis$ncol)) %>%
    rmf_as_sf(dis = dis, name = 'id')
  
  # subset
  ints <- target[obj, op = op] 
  ijk <- rmf_convert_id_to_ijk(id = ints$id, dis = dis)
  
  # k depends on z for XYZ points
  if(is.null(k)) {
    if(class(sf::st_geometry(obj)[[1]])[1] == "XYZ") {
      coords <- as.data.frame(sf::st_coordinates(obj))
      coords_grid <- rmf_convert_xyz_to_grid(x = coords$X, y = coords$Y, z = coords$Z, dis = dis, prj = prj, output = 'ijk')
      ijk$k <- coords_grid$k
    }
  } else {
    ijk$k <- k
  }
  
  rlst <- cbind(ijk, sf::st_set_geometry(obj, NULL)[select]) %>%
    rmf_create_list(kper = kper)
  
  return(rlst)
}


#' Title
#'
#' @param obj 
#' @param dis 
#' @param select 
#' @param k 
#' @param prj 
#' @param sparse 
#' @param op 
#' @param kper 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_array.sf <- function(obj, 
                            dis,
                            select,
                            k = NULL,
                            prj = NULL,
                            sparse = TRUE,
                            op = sf::st_intersects,
                            kper = attr(obj, 'kper'),
                            ...) {
  
  ar <- rmf_as_list(obj, dis = dis, select = select, prj = prj, k = k, op = op) %>%
    rmf_as_array(dis = dis, select = 4, sparse = sparse, kper = kper, ...)
  return(ar)
  
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param select
#' @param k  
#' @param prj 
#' @param kper 
#' @param op 

#'
#' @return
#' @export
#'
#' @examples
rmf_as_list.stars <- function(obj,
                              dis,
                              select = names(obj),
                              k = NULL,
                              prj = NULL,
                              kper = attr(obj, 'kper'),
                              op = sf::st_intersects) {
  
  lst <- sf::st_as_sf(obj[select]) %>%
    rmf_as_list(dis = dis, k = k, prj = prj, kper = kper, op = op)
  
  return(lst)
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param select 
#' @param kper 
#' @param prj 
#' @param crs 
#' @param resample 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_array.stars <- function(obj,
                               dis, 
                               prj = NULL,
                               crs = NULL,
                               select = 1,
                               resample = TRUE,
                               method = 'bilinear',
                               kper = attr(obj, 'kper')) {
  
  # TODO check if obj projection == dis projection
  
  if(resample) {
    target <- rmf_as_stars(dis$top, dis = dis, prj = prj)
    ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
    ar <- t(ar[[1]]) %>%
      rmf_create_array(dim = c(dis$nrow, dis$ncol), kper = kper)
  } else {
    # TODO error out if obj dimensions do not coincide with dis domain
    ar <- t(obj[[select]]) %>%
      rmf_create_array(dim = c(dis$nrow, dis$ncol), kper = kper)
  }
  
  # TODO deal with 3d & 4d data
  # 2D
  
  # 3D
  
  # 4D
  
  return(ar)
  
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_array.Raster <- function(obj, 
                                dis,
                                ...) {
  rmf_as_array(stars::st_as_stars(obj), dis = dis, ...)
}

## RMODFLOW --> SPATIAL
##


#' Generic function to convert rmf_array objects to simple features
#' 
#' @rdname rmf_as_sf
#' @export
rmf_as_sf <- function(...) {
  UseMethod('rmf_as_sf')
}

#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#' @param name 
#' @param as_points 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_sf.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, crs = NULL, name = 'value', as_points = FALSE) {
  
  # faster to convert to stars and then to sf than to manually create sf object
  
  # TODO rotation does not work properly in stars when delta != 1;
  #
  # If affine works properly in stars, simply create stars and use sf::st_as_sf(stars):
  s <- rmf_as_stars(array, dis = dis, mask = mask, prj = prj, crs = crs, name = name)
  f <- sf::st_as_sf(s, as_points = as_points)
  # reset crs because stars drops EPSG
  if(!is.null(crs)) {
    f <- sf::st_set_crs(f, sf::st_crs(crs)) 
  } else if(!is.null(prj)) {
    f <- sf::st_set_crs(f, sf::st_crs(prj$projection)) 
  }
  
  #
  # If affine does not work properly, manually adjust geotransform parameters
  #
  ## < MOVED TO rmf_as_stars
  # prj_stars <- prj
  # if(!is.null(prj_stars)) prj_stars$rotation <- 0 # to prevent warning in rmf_as_stars; affine parameters are overwritten below
  # 
  # s <- rmf_as_stars(array, dis = dis, mask = mask, prj = prj_stars, crs = crs, name = name)
  # df <- data.frame(x = c(s[[1]]))
  # colnames(df) <- name
  # 
  # rot <- ifelse(is.null(prj), 0, - prj$rotation * pi/180)
  # gtf <- stars:::get_geotransform(s)
  # gtf[3] <- gtf[2]*-sin(rot)
  # gtf[2] <- gtf[2]*cos(rot)
  # gtf[5] <- gtf[6]*sin(rot)
  # gtf[6] <- gtf[6]*cos(rot)
  # 
  # f <- sf::st_as_sfc(stars::st_dimensions(s), as_points = as_points, geotransform = gtf) 
  # f <- sf::st_sf(df, geometry = f)
  ## >
  
  return(f)
  
}

rmf_as_sf.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, crs = NULL, name = 'value', as_points = FALSE) {
  # Too slow
  # s_2d <- rmf_as_sf(array[,,1], dis = dis, prj = prj, crs = crs, name = name, as_points = as_points)
  # s_3d <- do.call(rbind, replicate(dim(array)[3], s_2d, simplify = FALSE))
  # s_3d[[name]] <- c(array)
  # s_3d$layer <- rep(1:dim(array)[3], each = prod(dim(array)[1:2]))
  # s_3d <- s_3d[which(mask^2 == 1),]
  # return(s_3d)
}

rmf_as_sf.rmf_4d_array <- function() {
  
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param prj 
#' @param crs 
#' @param as_points 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_sf.rmf_list <- function(obj, dis, prj = NULL, crs = NULL, as_points = FALSE) {
  
  # TODO check z coordinate in points; add z coordinate (or top/bottom) in polygons?
  df <- rmf_as_tibble(obj, dis = dis, prj = prj, crs = crs, as_points = as_points) %>%  
    as.data.frame()
  if(as_points) {
    geom <- lapply(1:nrow(df), function(i) sf::st_point(as.numeric(df[i, c('x', 'y', 'z')])))
  } else {
    set_poly <- function(ids, df) {
      df <- subset(df, id == ids)
      m <- matrix(c(df$x, df$y), ncol = 2)
      m <- rbind(m, m[1,])
      sf::st_polygon(list(m))
    }
    ids <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis)
    geom <- lapply(seq_along(ids), function(i) set_poly(ids[i], df))
  }
  sfc <- sf::st_sfc(geom)
  s <- sf::st_sf(as.data.frame(obj[,-which(colnames(obj) %in% c('i', 'j', 'k'))]), geom = sfc,
                 crs = rmfi_ifelse0(is.null(crs), sf::st_crs(prj)))
  return(s)
}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_stars <- function(...) {
  UseMethod('rmf_as_stars')
}


#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_stars.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, crs = NULL, name = 'value') {
  
  array[which(mask^2 != 1)] <- NA
  m <- t(as.matrix(array))
  dim(m) <- c(x = dim(m)[1], y = dim(m)[2]) # named dim
  
  # origin
  org <- rmfi_ifelse0(is.null(prj), c(0, 0, 0), prj$origin)
  
  # TODO rotation does not work properly in stars;
  
  # create stars object
  # d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = org[2] + rev(c(cumsum(dis$delc))), affine = rep(aff, 2))
  d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = org[2] + c(0, cumsum(dis$delc)))
  s <- stars::st_as_stars(m, dimensions = d)
  names(s) <- name
  
  # rot <- ifelse(is.null(prj), 0, - prj$rotation * pi/180)
  rot <- ifelse(is.null(prj), 0, prj$rotation * pi/180)
  gtf <- stars:::get_geotransform(s)
  gtf[3] <- gtf[2]*-sin(rot)
  gtf[2] <- gtf[2]*cos(rot)
  gtf[5] <- gtf[6]*sin(rot)
  gtf[6] <- gtf[6]*cos(rot)
  
  attr(attr(s, 'dimensions'), 'raster')$affine <- c(gtf[3], gtf[5])
  attr(s, 'dimensions')[[1]]$delta <- gtf[2]
  attr(s, 'dimensions')[[2]]$delta <- gtf[6]
  
  # add projection
  projection <- NULL
  if(!is.null(prj)) projection <- prj$projection
  if(!is.null(projection)) {
    s <- sf::st_set_crs(s, sf::st_crs(projection))
  }
  if(!is.null(crs)) {
    if(is.null(projection)) stop('Also specify prj with projection if crs is specified', call. = FALSE)
    s <- sf::st_transform(s, sf::st_crs(crs))
  }
  
  return(s)
}

rmf_as_stars.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, crs = NULL, name = 'value') {
  
  array[which(mask^2 != 1)] <- NA
  
  # TODO see if there's no native stars function whichs adds a dimension
  ar <- lapply(1:dim(array)[3], function(i) rmf_as_stars(array[,,i], dis = dis, prj = prj, crs = crs, name = name))
  ar_3d <- do.call(c, ar) %>% 
    merge() %>%
    setNames(name) %>%
    stars::st_set_dimensions(3, values = 1:dim(array)[3], names = c('layer'))
  return(ar_3d)

}

rmf_as_stars.rmf_4d_array <- function() {
  
}

rmf_as_stars.rmf_list <- function() {
  
  rmf_as_array() %>% rmf_as_stars()
  
}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_raster <- function(...) {
  UseMethod('rmf_as_raster')
}

rmf_as_raster.rmf_2d_array <- function() {
  
}

rmf_as_raster.rmf_3d_array <- function() {
  
}

rmf_as_raster.rmf_4d_array <- function() {
  
}

rmf_as_raster.rmf_list <- function() {
  
}

## UTILS

#' Title
#'
#' @param obj 
#' @param dis 
#' @param prj 
#' @param op 
#' @param sparse 
#'
#' @return
#' @export
#'
#' @examples
rmf_get_ibound <- function(obj,
                           dis,
                           prj = NULL,
                           op = sf::st_intersects,
                           sparse = FALSE) {
  
  # TODO check if obj & dis have same projection
  
  target <- rmf_create_array(1:(dis$nrow*dis$ncol), dim = c(dis$nrow, dis$ncol)) %>%
    rmf_as_sf(dis = dis, prj = prj)
  
  active <- target[obj, op = op]
  
  ibound <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol))
  ibound[active$value] <- 1
  if(!sparse) ibound <- rmf_create_array(ibound, dim = c(dis$nrow, dis$ncol, dis$nlay))
  return(ibound)
}

