
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
#' @param k 
#' @param ... 
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
                           op = sf::st_intersects,
                           unique = 'POINT' %in% class(sf::st_geometry(obj)[[1]]),
                           ...) {
  
  # TODO check if obj projection == dis projection
  
  target <- rmf_as_sf(dis$top, dis = dis, prj = prj, id = 'r', name = '.v')
  
  # spatial join
  ints <- sf::st_join(obj, target, join = op, left = FALSE, ...) # TODO should left be user-specified?
  
  if(unique) {
    eq <- vapply(sf::st_equals(ints), function(i) i[1], 1)
    ints <- ints[unique(eq), ]
  }
  
  ijk <- rmf_convert_id_to_ijk(id = ints$.id, dis = dis, type = 'r')
  
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
  
  rlst <- cbind(ijk, sf::st_set_geometry(ints, NULL)[select]) %>%
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
  
  ar <- rmf_as_list(obj, dis = dis, select = select, prj = prj, k = k, op = op, ...) %>%
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
                              op = sf::st_intersects,
                              ...) {
  
  lst <- sf::st_as_sf(obj[select]) %>%
    rmf_as_list(dis = dis, k = k, prj = prj, kper = kper, op = op, ...)
  
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
                               select = 1,
                               resample = TRUE,
                               method = 'bilinear',
                               kper = attr(obj, 'kper')) {
  
  # TODO check if obj projection == dis projection
  # TODO implement support for higher dimension stars objects
  if(length(stars::st_dimensions(obj)) > 2) stop('Support for stars object with more than 2 dimensions not yet implemented', call. = FALSE)
  if(any(vapply(stars::st_dimensions(obj), function(i) inherits(i$values, 'sfc'), TRUE))) stop('stars objects with sfc dimensions not supported', call. = FALSE)
  
  target <- rmf_as_stars(dis$top, dis = dis, prj = prj, id = FALSE)
  if(resample) {
    ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
    ar <- t(ar[[1]]) %>% c() %>% rev() %>%
      rmf_create_array(dim = c(dis$nrow, dis$ncol), kper = kper)
  } else {
    # error out if obj dimensions do not coincide with dis domain
    if(!identical(sf::st_bbox(obj), sf::st_bbox(target))) stop('Spatial extents of obj and dis do not coincide. Consider setting resample = TRUE', call. = FALSE)
    if(!identical(setNames(dim(obj), NULL), setNames(dim(target), NULL))) stop('Spatial resolution of obj and dis are not the same. Consider setting resample = TRUE', call. = FALSE)
    
    ar <- t(obj[[select]]) %>% c() %>% rev() %>%
      rmf_create_array(dim = c(dis$nrow, dis$ncol), kper = kper)
  }
  
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
rmf_as_sf.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', as_points = FALSE, id = 'r') {
  
  # faster to convert to stars and then to sf than to manually create sf object
  
  # TODO rotation does not work properly in stars when delta != 1;
  # If affine works properly in stars, simply create stars and use sf::st_as_sf(stars):
  
  s <- rmf_as_stars(array, dis = dis, mask = mask, prj = prj, name = name, id = id)
  f <- sf::st_as_sf(s, as_points = as_points)
  # reset crs because stars drops EPSG code
  if(!is.null(prj)) f <- sf::st_set_crs(f, sf::st_crs(prj$projection)) 
  
  return(f)
  
}

#' @rdname rmf_as_sf
#' @method rmf_as_sf rmf_3d_array
#' @export
rmf_as_sf.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', as_points = FALSE, id = 'r') {
  
  # TODO better to create a tibble from array; convert to data frame (for consistency) and do st_join with target grid using .id as binding column
  template <- lk <- rmf_as_sf(array[,,1], dis = dis, mask = mask, prj = prj, name = name, as_points = as_points, id = id)
  template$.layer <- lk$.layer <- 1
  for(k in 2:dim(array)[3]) {
    template[[name]] <- c(t(array[,,k]))
    if(id %in% c('r', 'modflow')) template[['.id']] <- template[['.id']] + prod(dim(array)[1:2])
    template$.layer <- k
    lk <- rbind(lk, template)
  }
  
  return(lk)
}

rmf_as_sf.rmf_4d_array <- function() {
  # TODO create a tibble from array; convert to data frame (for consistency) and do st_join with target grid using .id as binding column
  stop('Not yet implemented', call. = FALSE)
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param prj 
#' @param crs 
#' @param as_points 
#'
#' @details returned z coordinate when as_points = TRUE reflects cell node 
#'
#' @return
#' @export
#' 
#' @examples
rmf_as_sf.rmf_list <- function(obj, dis, prj = NULL, as_points = FALSE, id = 'r') {
  
  # TODO set type of id through rmf_as_tibble
  df <- rmf_as_tibble(obj, dis = dis, prj = prj, as_points = as_points) %>%  
    as.data.frame()
  if(as_points) {
    geom <- lapply(1:nrow(df), function(i) sf::st_point(as.numeric(df[i, c('x', 'y', 'z')])))
    .top <- 1
    .botm <- 1
  } else {
    set_poly <- function(ids, df) {
      df <- subset(df, id == ids)
      m <- matrix(c(df$x, df$y), ncol = 2)
      m <- rbind(m, m[1,])
      sf::st_polygon(list(m))
    }
    ids <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis)
    geom <- lapply(seq_along(ids), function(i) set_poly(ids[i], df))
    
    # top & botm of cells
    if(any(dis$laycbd != 0)) warning("Quasi-3D confining beds detected. Returned top and botm only represent numerical layers.", call. = FALSE)
    cbd <- rmfi_confining_beds(dis)
    nnlay <- which(!cbd)[-length(cbd)]
    tops <- rmf_create_array(c(c(dis$top), c(dis$botm[,,nnlay])), dim = c(dis$nrow, dis$ncol, length(which(!cbd))))
    .top <- tops[ids] 
    .botm <- dis$botm[ids]
  }
  sfc <- sf::st_sfc(geom)
  nms <- colnames(obj)[-which(colnames(obj) %in% c('i', 'j', 'k'))]
  
  s <- cbind(setNames(as.data.frame(obj[, nms]), nms), .id = 1, .top, .botm) %>%
       sf::st_sf(geom = sfc,
                 crs = rmfi_ifelse0(is.null(prj), NA, prj$projection))
  if(as_points) s$.top <- s$.botm <- NULL
  
  if(id %in% c('modflow', 'r')) {
    s$.id <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis, type = id)
  } else {
    s$.id <- NULL
  }
  
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
rmf_as_stars.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', id = 'r') {
  
  array[which(mask^2 != 1)] <- NA
  array <- array[rev(1:dim(array)[1]),] # in MODFLOW, rows are numbered from N-S
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
  
  # .id
  if(id == 'modflow') {
    id <- aperm(array(1:prod(dim(array)[1:2]), dim = rev(dim(array)[1:2])), c(2,1))
    s$.id <- c(aperm(id[rev(1:dim(id)[1]), ], c(2,1)))
  } else if(id == 'r') {
    id <- array(1:prod(dim(array)[1:2]), dim = dim(array)[1:2])
    s$.id <- c(aperm(id[rev(1:dim(id)[1]), ], c(2,1)))
  }

  # projection
  projection <- NULL
  if(!is.null(prj)) projection <- prj$projection
  if(!is.null(projection)) {
    s <- sf::st_set_crs(s, sf::st_crs(projection))
  }

  return(s)
}

rmf_as_stars.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', id = 'r') {
  
  array[which(mask^2 != 1)] <- NA
  
  # TODO see if there's no native stars function whichs adds a dimension
  ar <- lapply(1:dim(array)[3], function(i) rmf_as_stars(array[,,i], dis = dis, prj = prj, name = name, id = id))
  ar_3d <- do.call(c, ar) %>% 
    merge() %>%
    setNames(name) %>%
    stars::st_set_dimensions(3, values = 1:dim(array)[3], names = c('layer'))
  return(ar_3d)

}

rmf_as_stars.rmf_4d_array <- function() {
  stop('Not yet implemented', call. = FALSE)
}

rmf_as_stars.rmf_list <- function(obj, dis, prj = NULL, ...) {
  
  rmf_as_array(obj, dis = dis, ...) %>% rmf_as_stars(dis = dis, prj = prj, ...)
  
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

rmf_as_raster.rmf_2d_array <- function(array, dis, prj = NULL, ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

rmf_as_raster.rmf_3d_array <- function(array, dis, prj = NULL, ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

rmf_as_raster.rmf_4d_array <- function(array, dis, prj = NULL, ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

rmf_as_raster.rmf_list <- function(obj, dis, prj = NULL, ...) {
  rmf_as_stars(obj, dis = dis, prj = prj, ...) %>% as('Raster')
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
rmf_create_ibound <- function(obj,
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


rmf_create_grid <- function(obj, 
                            nrow = 10,
                            ncol = 10,
                            nlay = 3,
                            cellsize = NULL,
                            rotation = 0, 
                            op = sf::st_intersects) {
  
  if(rotation != 0) {
    
  } else {
    gr <- sf::st_make_grid(obj, cellsize = if(!is.null(cellsize)) {cellsize}, n = c(nrow, ncol))
  }
  
}


