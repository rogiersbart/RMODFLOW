
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
                           prj = rmf_get_prj(dis), 
                           k = NULL,
                           kper = attr(obj, 'kper'),
                           op = sf::st_intersects,
                           ...) {
  
  # TODO check if obj projection == dis projection
  target <- rmf_as_sf(dis$top, dis = dis, prj = prj, id = 'r', name = '.v')
  
  # spatial join
  obj$.unq_id <- seq_len(nrow(obj))
  ints <- sf::st_join(obj, target, join = op, left = FALSE, ...) # TODO should left be user-specified?
  
  # remove duplicate geometries caused by feature located on cell boundary
  ints <- ints[!duplicated(ints$.unq_id), ]
  ints$.unq_id <- NULL
  obj$.unq_id <- NULL
  
  ijk <- rmf_convert_id_to_ijk(id = ints$id, dis = dis, type = 'r')
  
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
                            prj = rmf_get_prj(dis),
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
                              prj = rmf_get_prj(dis),
                              kper = attr(obj, 'kper'),
                              op = sf::st_intersects,
                              ...) {
  
  lst <- sf::st_as_sf(obj[select], as_points = FALSE, merge = FALSE) %>%
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
#' @param ... ignored
#'
#' @return
#' @export
#'
#' @examples
rmf_as_array.stars <- function(obj,
                               dis, 
                               prj = rmf_get_prj(dis),
                               select = 1,
                               resample = TRUE,
                               method = 'bilinear',
                               kper = attr(obj, 'kper'),
                               ...) {
  
  dims <- stars::st_dimensions(obj)
  ndim <- length(dims)
  
  # TODO check if obj projection == dis projection

  if(ndim > 4) stop('Support for obj with more than 4 dimensions not implemented', call. = FALSE)
  if(any(vapply(dims, function(i) inherits(i$values, 'sfc'), TRUE))) stop('stars objects with sfc dimensions are not supported', call. = FALSE)

  target <- rmf_as_stars(dis$top, dis = dis, prj = prj, id = FALSE)
  
  if(resample) {
    # st_warp doesn't work when objects don't have crs
    if(is.null(prj) || is.na(sf::st_crs(prj$crs)) || is.na(sf::st_crs(obj))) {
      stop('obj crs and/or prj are missing. Consider setting resample = FALSE', call. = FALSE)
    }
  } else {
    # error out if obj dimensions do not coincide with dis domain
    if(!identical(sf::st_bbox(obj), sf::st_bbox(target))) stop('Spatial extents of obj and dis do not coincide. Consider setting resample = TRUE', call. = FALSE)
    if(!identical(setNames(dim(obj)[1:2], NULL), setNames(dim(target), NULL))) stop('Spatial resolution of obj and dis are not the same. Consider setting resample = TRUE', call. = FALSE)
  }
  
  # 2D
  if(ndim == 2) {
    if(resample) {
      ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
      ar <- t(ar[[1]]) %>% c() %>%
        rmf_create_array(dim = c(dis$nrow, dis$ncol), kper = kper)
    } else {
      # extract array
      ar <- t(obj[[select]]) %>% c() %>%
        rmf_create_array(dim = c(dis$nrow, dis$ncol), kper = kper)
    }
  } else if(ndim == 3) {
    # 3D
    nnlay <- dis$nlay + sum(dis$laycbd != 0)
    if(dim(obj)[3] != nnlay) stop('Third dimension of stars object should have length equal to dis$nlay (+ number of optional confining beds)', call. = FALSE)
    
    if(resample) {
      ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
      ar <- aperm(ar[[1]], c(2,1,3)) %>% c() %>% 
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay), kper = kper)
    } else {
      # extract array
      ar <- aperm(obj[[select]], c(2,1,3)) %>% c() %>% 
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay), kper = kper)
    }
    
  } else if(ndim == 4) {
    # 4D
    nnlay <- dis$nlay + sum(dis$laycbd != 0)
    if(dim(obj)[3] != nnlay) stop('Third dimension of stars object should have length equal to dis$nlay (+ number of optional confining beds)', call. = FALSE)
    if(dim(obj)[4] != sum(dis$nstp)) stop('Fourth dimension of stars object should have length equal to sum(dis$nstp)' , call. = FALSE)
    
    if(resample) {
      ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
      ar <- aperm(ar[[1]], c(2,1,3,4)) %>% c() %>% 
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp)), kper = kper)
    } else {
      # extract array
      ar <- aperm(obj[[select]], c(2,1,3,4)) %>% c() %>% 
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp)), kper = kper)
    }
  }
  
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
#' @param ... ignored
#'
#' @details returned z coordinate when as_points = TRUE reflects cell node 
#'
#' @return
#' @export
#' @rdname rmf_as_sf
#' @method rmf_as_sf rmf_2d_array
#' @examples
rmf_as_sf.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = rmf_get_prj(dis), name = 'value', as_points = FALSE, id = 'r', ...) {
  
  # faster to convert to stars and then to sf than to manually create sf object
  
  # TODO rotation does not work properly in stars when delta != 1;
  # If affine works properly in stars, simply create stars and use sf::st_as_sf(stars):
  
  s <- rmf_as_stars(array, dis = dis, mask = mask, prj = prj, name = name, id = id)
  f <- sf::st_as_sf(s, as_points = as_points)
  # reset crs because stars drops EPSG code
  if(!is.null(prj)) f <- sf::st_set_crs(f, sf::st_crs(prj$crs)) 
  
  return(f)
  
}

#' @rdname rmf_as_sf
#' @method rmf_as_sf rmf_3d_array
#' @export
rmf_as_sf.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = rmf_get_prj(dis), name = 'value', as_points = FALSE, id = 'r', ...) {
  
  target <- rmf_as_sf(dis$top, dis = dis, prj = prj, as_points = as_points, id = 'r') %>%
    subset(select = 'id')
  
  tbl <- rmf_as_tibble(array, dis = dis, mask = mask, prj = prj, as_points = TRUE, id = 'r') %>%
    as.data.frame()
  tbl <- subset(tbl, select = -which(colnames(tbl) %in% rmfi_ifelse0(as_points, c('x', 'y'), c('x', 'y', 'z'))))
  tbl$ido <- rep(tbl$id[seq_len(prod(dis$nrow, dis$ncol))], dis$nlay)
  colnames(tbl) <- replace(colnames(tbl), which(colnames(tbl) == 'id'), 'id_3d')
  
  f <- merge(target, tbl, by.x = 'id', by.y = 'ido', sort = FALSE)
  f$id <- NULL
  colnames(f) <- replace(colnames(f), which(colnames(f) == 'id_3d'), 'id')
  colnames(f) <- replace(colnames(f), which(colnames(f) == 'value'), name)
  
  # change id if necessary
  if(id == 'modflow') {
    f$id <- rmf_convert_id_to_id(f$id, dis = dis, from = 'r', to = 'modflow')
  } else if(id != 'r') {
    f$id <- NULL
  }
  
  return(f)
}

#' @rdname rmf_as_sf
#' @method rmf_as_sf rmf_4d_array
#' @export
rmf_as_sf.rmf_4d_array <- function(array, dis, mask = array(1, dim = dim(array)), prj = rmf_get_prj(dis), name = 'value', as_points = FALSE, id = 'r', ...) {

  target <- rmf_as_sf(dis$top, dis = dis, prj = prj, as_points = as_points, id = 'r') %>%
    subset(select = 'id')
  
  tbl <- rmf_as_tibble(array, dis = dis, mask = mask, prj = prj, as_points = TRUE, id = 'r') %>%
    as.data.frame()
  tbl <- subset(tbl, select = -which(colnames(tbl) %in% rmfi_ifelse0(as_points, c('x', 'y'), c('x', 'y', 'z'))))
  tbl$ido <- rep(tbl$id[seq_len(prod(dis$nrow, dis$ncol))], dis$nlay * dim(array)[4])
  colnames(tbl) <- replace(colnames(tbl), which(colnames(tbl) == 'id'), 'id_4d')
  
  f <- merge(target, tbl, by.x = 'id', by.y = 'ido', sort = FALSE)
  f$id <- NULL
  colnames(f) <- replace(colnames(f), which(colnames(f) == 'id_4d'), 'id')
  colnames(f) <- replace(colnames(f), which(colnames(f) == 'value'), name)
  
  # change id if necessary
  if(id == 'modflow') {
    f$id <- rmf_convert_id_to_id(f$id, dis = dis, from = 'r', to = 'modflow')
  } else if(id != 'r') {
    f$id <- NULL
  }
  
  return(f)
}

#' @rdname rmf_as_sf
#' @method rmf_as_sf rmf_list
#' @export
rmf_as_sf.rmf_list <- function(obj, dis, prj = rmf_get_prj(dis), as_points = FALSE, id = 'r', ...) {
  
  df <- rmf_as_tibble(obj, dis = dis, prj = prj, as_points = as_points, id = 'r', ...) %>%  
    as.data.frame()
  ids <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis, type = 'r')
  
  if(as_points) {
    geom <- lapply(1:nrow(df), function(i) sf::st_point(as.numeric(df[i, c('x', 'y', 'z')])))
  } else {
    set_poly <- function(ids, df) {
      df <- subset(df, id == ids)
      m <- matrix(c(df$x, df$y), ncol = 2)
      m <- rbind(m, m[1,])
      sf::st_polygon(list(m))
    }
    geom <- lapply(seq_along(ids), function(i) set_poly(ids[i], df))
  }
  
  # top & botm
  cell_tops <- subset(df, select = rmfi_ifelse0(as_points, c('z', 'top', 'botm'), c('top', 'botm')))
  if(!as_points) cell_tops <- cell_tops[seq(1, nrow(cell_tops), by = 4), ]
  
  # create sf
  sfc <- sf::st_sfc(geom)
  nms <- colnames(obj)[-which(colnames(obj) %in% c('i', 'j', 'k'))]
  
  s <- cbind(data.frame(id = ids), setNames(as.data.frame(obj[, nms]), nms), cell_tops) %>%
       sf::st_sf(geom = sfc,
                 crs = rmfi_ifelse0(is.null(prj), NA, prj$crs))
  
  # change id if necessary
  if(id == 'modflow') {
    s$id <- rmf_convert_id_to_id(s$id, dis = dis, from = 'r', to = 'modflow')
  } else if(id != 'r') {
    s$id <- NULL
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
#' @param ... ignored
#'
#' @return
#' @export
#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_2d_array
#' @examples
rmf_as_stars.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = rmf_get_prj(dis), name = 'value', id = 'r', ...) {
  
  array[which(mask^2 != 1)] <- NA
  m <- t(as.matrix(array))
  dim(m) <- c(x = dim(m)[1], y = dim(m)[2]) # named dim
  
  # origin
  org <- rmfi_ifelse0(is.null(prj), c(0, 0, 0), prj$origin)
  
  length_mlt <- rmfi_prj_length_multiplier(dis, prj, to = "xyz")
  
  # TODO rotation does not work properly in stars;
  
  # create stars object
  # use negative deltay: more consistent with how most raster data is read from file
  # d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = rev(org[2] + c(0, cumsum(dis$delc))), affine = rep(aff, 2))
  d <-  stars::st_dimensions(x = org[1] + (c(0, cumsum(dis$delr)) * length_mlt), y = rev(org[2] + (c(0, cumsum(dis$delc)) * length_mlt)))
  s <- stars::st_as_stars(m, dimensions = d)
  names(s) <- name
  
  rot <- ifelse(is.null(prj), 0, - prj$rotation * pi/180)
  gtf <- stars:::get_geotransform(s)
  gtf[3] <- gtf[2]*-sin(rot)
  gtf[2] <- gtf[2]*cos(rot)
  gtf[5] <- gtf[6]*sin(rot)
  gtf[6] <- gtf[6]*cos(rot)
  
  attr(attr(s, 'dimensions'), 'raster')$affine <- c(gtf[3], gtf[5])
  attr(s, 'dimensions')[[1]]$delta <- gtf[2]
  attr(s, 'dimensions')[[2]]$delta <- gtf[6]
  
  # id
  if(id == 'modflow') {
    ids <- aperm(array(1:prod(dim(array)[1:2]), dim = rev(dim(array)[1:2])), c(2,1))
    s$id <- c(aperm(ids, c(2,1)))
  } else if(id == 'r') {
    ids <- array(1:prod(dim(array)[1:2]), dim = dim(array)[1:2])
    s$id <- c(aperm(ids, c(2,1)))
  }

  # projection
  projection <- NULL
  if(!is.null(prj)) projection <- prj$crs
  if(!is.null(projection)) {
    s <- sf::st_set_crs(s, sf::st_crs(projection))
  }

  return(s)
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_3d_array
#' @export
rmf_as_stars.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = rmf_get_prj(dis), name = 'value', id = 'r', ...) {
  
  array[which(mask^2 != 1)] <- NA
  
  s <- rmf_as_stars(array[,,1], dis = dis, prj = prj, name = 'layer_1', id = id)
  ids <- c(s$id) + c(rep(0, prod(dis$nrow, dis$ncol)), rep(prod(dis$nrow, dis$ncol) * seq_len(dis$nlay - 1), each = prod(dis$nrow, dis$ncol)))
  d <- stars::st_dimensions(s)
  
  array_list <- lapply(seq_len(dim(array)[3]), 
                        function(i) t(as.matrix(array[,,i]))) %>% 
    setNames(paste('layer', seq_len(dim(array)[3]), sep = '_'))
  s <- stars::st_as_stars(array_list, dimensions = d) %>%
    merge() %>%
    setNames(name) %>%
    stars::st_set_dimensions(names = c(names(d), 'layer'))
  s$id <- ids
  dim(s[[name]]) <- dim(s$id) # TODO this might change in the stars API
  
  return(s)
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_4d_array
#' @export
rmf_as_stars.rmf_4d_array <- function(array, dis, mask = array(1, dim = dim(array)[1:3]), prj = rmf_get_prj(dis), name = 'value', id = 'r', ...) {
  
  mask <- array(mask, dim = c(dim(mask), dim(array)[4]))
  array[which(mask^2 != 1)] <- NA
  
  s <- rmf_as_stars(array[,,,1], dis = dis, prj = prj, name = 'layer_1', id = id)
  ids <- c(s$id) 
  d <- stars::st_dimensions(s)
  
  # time <- rep(rmfi_ifelse0(is.null(attr(array, 'totim')), 1:dim(array)[4], attr(array, 'totim')[!is.na(attr(array, 'totim'))]),
  #             each = prod(dis$nrow, dis$ncol, dis$nlay))
  
  time <- rmfi_ifelse0(is.null(attr(array, 'totim')), 1:dim(array)[4], attr(array, 'totim')[!is.na(attr(array, 'totim'))])

  array_list <- lapply(seq_len(dim(array)[4]), 
                       function(i) aperm(as.array(array[,,,i]), c(2,1,3))) %>% 
                setNames(time)
  
  s <- stars::st_as_stars(array_list, dimensions = d) %>%
    merge() %>%
    setNames(name) %>%
    stars::st_set_dimensions(names = c(names(d), 'time')) %>%
    stars::st_set_dimensions(which = 4, values = time)
  s$id <- ids
  dim(s[[name]]) <- dim(s$id) # TODO this might change in the stars API
  
  return(s)
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_list
#' @export
rmf_as_stars.rmf_list <- function(obj, dis, prj = rmf_get_prj(dis), ...) {
  
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

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_2d_array
#' @export
rmf_as_raster.rmf_2d_array <- function(array, dis, prj = rmf_get_prj(dis), ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_3d_array
#' @export
rmf_as_raster.rmf_3d_array <- function(array, dis, prj = rmf_get_prj(dis), ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_4d_array
#' @export
rmf_as_raster.rmf_4d_array <- function(array, dis, l = NULL, prj = rmf_get_prj(dis), ...) {
  if(is.null(l)) stop('Please provide a l argument to subset the 4d array', call. = FALSE)
  rmf_as_stars(array[,,,l], dis = dis, prj = prj, ...) %>% as('Raster')
}

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_list
#' @export
rmf_as_raster.rmf_list <- function(obj, dis, prj = rmf_get_prj(dis), ...) {
  rmf_as_stars(obj, dis = dis, prj = prj, ...) %>% as('Raster')
}

## Projection 
##

#' Title
#'
#' @param origin in length units of crs
#' @param rotation 
#' @param crs 
#' @param ulcoordinate 
#' @param nodecoord 
#' @param dis 
#'
#' @return
#' @export
#'
#' @examples
rmf_create_prj <- function(origin = c(0, 0, 0), 
                           rotation = 0, 
                           crs = NA,
                           ulcoordinate = FALSE,
                           nodecoord = FALSE,
                           dis = NULL) {
  
  crs <- sf::st_crs(crs)
  origin <- unlist(origin)
  
  if(ulcoordinate) {
    if(is.null(dis)) stop('Please provide a dis object when ulcoordinate = TRUE', call. = FALSE)
    length_mlt <- rmfi_prj_length_multiplier(dis, prj = list(crs = crs), to = 'xyz')
    
    if(nodecoord) { # set origin as corner coordinate
      origin[1] <- origin[1] - ((dis$delr[1]/2) * length_mlt)
      origin[2] <- origin[2] + ((dis$delc[1]/2) * length_mlt)
    } 
    
    # unrotated lowerleft coordinate
    unrot_x <- origin[1]
    unrot_y <- origin[2] - (sum(dis$delc) * length_mlt)
    angle <- rotation * pi/180
    
    # rotated lowerleft coordinate
    xRot = origin[1] + cos(angle)*(unrot_x - origin[1]) - sin(angle)*(unrot_y - origin[2])
    yRot = origin[2] + sin(angle)*(unrot_x - origin[1]) + cos(angle)*(unrot_y - origin[2])
    
    origin[1] <- xRot
    origin[2] <- yRot
    
  } else if(nodecoord) {
    # set origin as corner coordinate
    if(is.null(dis)) stop('Please provide a dis object when nodecoord = TRUE', call. = FALSE)
    length_mlt <- rmfi_prj_length_multiplier(dis, prj = list(crs = crs), to = 'xyz')
    
    origin[1] <- origin[1] - ((dis$delr[dis$nrow]/2) * length_mlt)
    origin[2] <- origin[2] - ((dis$delc[1]/2) * length_mlt)
  }
  
  # z coordinate
  origin[3] <- ifelse(length(origin) > 2, origin[3], 0)
  
  prj <- list()
  prj$origin <- setNames(origin, c('x', 'y', 'z'))
  prj$rotation <- rotation
  prj$crs <- crs
  
  class(prj) <- 'prj'
  return(prj)
}

#' @export
print.prj <- function(prj) {
  cat('RMODFLOW Projection object:', '\n')
  cat('Origin coordinates (x y z) of the lowerleft corner:', '\n')
  cat(' ', prj$origin, '\n')
  cat('Grid rotation (degrees counterclockwise):', '\n')
  cat(' ', prj$rotation, '\n')
  cat('Coordinate Reference System:', '\n')
  if(is.na(prj$crs)) {
    cat(' NA')
  } else {
    cat(' EPSG:', prj$crs$epsg, '\n')
    cat(' proj4string:', prj$crs$proj4string)
  }

}

#' Functions to get, set, transform and check presence of prj objects
#' 
#' @param dis \code{RMODFLOW} dis object
#' @param modflow \code{RMODFLOW} modflow object
#' @param prj \code{RMODFLOW} prj object
#' @param file path to discretization file; typically "*.dis"
#' @param crs crs to transform to. Input for \code{sf::st_crs}.
#' 
#' @name prj_auxiliary
NULL

#' 
#' @return \code{rmf_get_prj} returns a \code{RMODFLOW} prj object if present; otherwise \code{NULL}
#' @export
#' @rdname prj_auxiliary
#' @examples
rmf_get_prj <- function(...) {
  UseMethod('rmf_get_prj')
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_get_prj dis
rmf_get_prj.dis <- function(dis) {
  if(rmf_has_prj(dis)) {
    return(dis$prj)
  } else {
    return(NULL)
  }
}

#'
#' @export
#' @rdname prj_auxiliary
#' @method rmf_get_prj dis
rmf_get_prj.modflow <- function(modflow) {
  if(rmf_has_prj(modflow)) {
    return(modflow$dis$prj)
  } else {
    return(NULL)
  }
}

#'
#' @return \code{rmf_has_prj} returns a logical depending on whether or not a \code{RMODFLOW} prj object is present
#' @export
#' @rdname prj_auxiliary
#' @examples
rmf_has_prj <- function(...) {
  UseMethod('rmf_has_prj')
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_has_prj dis
rmf_has_prj.dis <- function(dis) {
  !is.null(dis$prj) && inherits(dis$prj, 'prj')
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_has_prj modflow
rmf_has_prj.modflow <- function(modflow) {
  !is.null(modflow$dis$prj) && inherits(modflow$dis$prj, 'prj')
}

#'
#' @return \code{rmf_set_prj} returns either a \code{RMODFLOW} dis or modflow object with the prj set or nothing when writing directly to a file 
#' @export
#' @rdname prj_auxiliary
#' @examples
rmf_set_prj <- function(...) {
  UseMethod('rmf_set_prj')
}

#' @details \code{rmf_set_prj.character} writes the projection information of \code{prj} directly into the header comments of the discretization file
#' @export
#' @rdname prj_auxiliary
#' @method rmf_set_prj character
rmf_set_prj.character <- function(file, dis, prj = rmf_get_prj(dis)) {
  
  # TODO only write if prj is present: keep?
  if(!is.null(prj)) {
    lines <- readr::read_lines(file)
    comment_lines <- rmfi_parse_comments(lines)
    st <- grep('Start RMODFLOW projection information', comment_lines$comments)
    if(length(st) > 0) {
      end <- grep('End RMODFLOW projection information', comment_lines$comments)
      warning('Overwriting existing RMODFLOW projection information in file', call. = FALSE)
      comment_lines$comments <- comment_lines$comments[-c(st:end)]
    }
    v <- packageDescription("RMODFLOW")$Version
    readr::write_lines(paste('# MODFLOW Discretization File created by RMODFLOW, version', v), path=file, append = FALSE)
    readr::write_lines(paste0("#", comment_lines$comments), path = file, append = TRUE)
    rmfi_write_prj(dis, prj = prj, file = file)
    readr::write_lines(comment_lines$remaining_lines, path = file, append = TRUE)
  } else {
    warning('prj is NULL. No projection information is written.', call. = FALSE)
  }
  
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_set_prj dis
rmf_set_prj.dis <- function(dis, prj) {
  if(rmf_has_prj(dis)) warning('Overwriting existing prj object in dis object', call. = FALSE)
  dis$prj <- prj
  return(dis)
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_set_prj modflow
rmf_set_prj.modflow <- function(modflow, prj) {
  if(rmf_has_prj(modflow)) warning('Overwriting existing prj object in modflow object', call. = FALSE)
  modflow$dis$prj <- prj
  return(modflow)
}

#'
#' @return \code{rmf_transform_prj} returns an \code{RMODFLOW} prj object with transformed crs
#' @details \code{rmf_transform_prj} transforms the origin coordinates to the new crs.
#' @export
#' @rdname prj_auxiliary
#' @examples
rmf_transform_prj <- function(...) {
  UseMethod('rmf_transform_prj')
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_transform_prj prj
rmf_transform_prj.prj <- function(prj, crs) {
  if(missing(crs)) stop('Please supply a crs argument to transform to', call. = FALSE)
  crs <- sf::st_crs(crs)
  
  origin <- data.frame(x = prj$origin[1], y = prj$origin[2], z = ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
  transf <- rmfi_convert_coordinates(origin, from = prj$crs, to = crs)
  
  prj <- rmf_create_prj(origin = transf[1,], rotation = prj$rotation, crs = crs)
  return(prj)
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_transform_prj dis
rmf_transform_prj.dis <- function(dis, crs) {
  if(!rmf_has_prj(dis)) stop('dis object has no prj object to transform', call. = FALSE)
  prj <- rmf_get_prj(dis)
  prj <- rmf_transform(prj, crs)
  return(prj)
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_transform_prj modflow
rmf_transform_prj.modflow <- function(modflow, crs) {
  if(!rmf_has_prj(modflow)) stop('modflow object has no prj object to transform', call. = FALSE)
  prj <- rmf_get_prj(dis)
  prj <- rmf_transform(prj, crs)
  return(prj)
}

#' Title
#'
#' @param prj 
#' @param file 
#'
#' @return
#' @keywords internal
rmfi_write_prj <- function(dis, prj, file) {
  
  # TODO only write if prj is present: keep?
  if(!is.null(prj)) {
    extent <- rmf_extent(dis, prj)
    
    cat('#', 'Start RMODFLOW projection information', '\n', file = file, append = TRUE)
    cat('#', 'Upper left corner:', paste0('(', paste(extent$corners['ul',], collapse = ', '), ')'), '\n', file=file, append=TRUE)
    cat('#', 'Lower left corner:', paste0('(', paste(extent$corners['ll',], collapse = ', '), ')'), '\n', file=file, append=TRUE)
    cat('#', 'Upper right corner:', paste0('(', paste(extent$corners['ur',], collapse = ', '), ')'), '\n', file=file, append=TRUE)
    cat('#', 'Lower right corner:', paste0('(', paste(extent$corners['lr',], collapse = ', '), ')'), '\n', file=file, append=TRUE)
    cat('#', 'Grid angle (in degrees counterclockwise):', ifelse(is.null(prj), 0, prj$rotation), '\n', file = file, append = TRUE)
    cat('#', 'Z coordinate lower left corner:', ifelse(is.null(prj) || is.na(prj$origin[3]), 0, prj$origin[3]), '\n', file=file, append = TRUE)
    
    if(is.null(prj) || is.na(prj$crs)) {
      cat('#', 'proj4string:', 'NA', '\n', file = file, append = TRUE)
    } else { # if crs is present it should have either epsg, proj4string or wkt (for sf >= 0.9)
      if(!is.na(prj$crs$epsg)) {
        cat('#', 'epsg:', prj$crs$epsg, '\n', file = file, append = TRUE)
      } else if(!is.na(prj$crs$proj4string)) {
        cat('#', 'proj4string:', prj$crs$proj4string, '\n', file = file, append = TRUE)
      } else {
        if(packageVersion('sf') >= '0.9') {
          cat('#', 'wkt:', '\n', file = file, append = TRUE)
          cat(paste('#', prj$crs$wkt), sep = '\n', file = file, append = TRUE)
        }
      }
    }
    cat('#', 'End RMODFLOW projection information', '\n', file = file, append = TRUE)
  }
 
} 

#' Title
#'
#' @param comments 
#'
#' @return
#' @keywords internal
rmfi_parse_prj <- function(comments) {
  
  # first check if RMODFLOW projection information is present
  st <- grep('Start RMODFLOW projection information', comments)
  if(length(st) > 0) {
    end <- grep('End RMODFLOW projection information', comments)
    rmf_comments <- comments[st:end]
    rmf_end <- grep('End RMODFLOW projection information', rmf_comments)
    
    # origin
    ll_line <- grep('Lower left corner', rmf_comments, ignore.case = TRUE)[1]
    coords <- rmfi_parse_variables(rmf_comments[ll_line])$variables
    x <- as.numeric(gsub('\\(', '', coords[4]))
    y <- as.numeric(gsub('\\)', '', coords[5]))
    
    z_line <- grep('Z coordinate', rmf_comments)
    z_coord <- rmfi_parse_variables(rmf_comments[z_line])$variables
    z <- as.numeric(z_coord[6])
    origin <- c(x, y, z)
    
    # rotation
    rot_line <- grep('Grid angle', rmf_comments, ignore.case = TRUE)
    rot <- rmfi_parse_variables(rmf_comments[rot_line])$variables
    rotation <- as.numeric(rot[6])
    
    # crs
    prj4 <- grep('proj4string', rmf_comments, ignore.case = TRUE)
    epsg <- grep('epsg', rmf_comments, ignore.case = TRUE)
    wkt <- grep('wkt', rmf_comments, ignore.case = TRUE)
    
    if(length(wkt) > 0) {
      crs <- trimws(paste0(rmf_comments[(wkt+1):(rmf_end-1)], collapse = '\n'))
    } else if(length(epsg) > 0) {
      crs <- as.numeric(sub('epsg: ', '', rmf_comments[epsg], ignore.case = TRUE))
    } else if(length(prj4) > 0) {
      crs <- sub('proj4string: ', '', rmf_comments[prj4], ignore.case = TRUE)
    } else {
      crs <- NA
    }
    if(is.character(crs) && toupper(trimws(crs)) == "NA") crs <- NA
    
    prj <- rmf_create_prj(origin = origin, rotation = rotation, crs = crs)
    comments <- comments[-c(st:end)]
    
  } else {
    # else check if ModelMuse type projection information is present
    
    # origin
    ll_line <- grep('Lower left corner', comments, ignore.case = TRUE)
    if(length(ll_line) > 0) {
      coords <- rmfi_parse_variables(comments[ll_line])$variables
      x <- as.numeric(gsub('\\(', '', coords[4]))
      y <- as.numeric(gsub('\\)', '', coords[5]))
      origin <- c(x, y)
      if(any(is.na(origin))) origin <- NA
    } else {
      origin <- NA
    }
    
    # if not proper format, set prj to NULL
    if(length(origin) == 1 && is.na(origin)) {
      prj <- NULL
    } else {
      # rotation
      rot_line <- grep('Grid angle', comments, ignore.case = TRUE)
      if(length(rot_line) > 0) {
        rot <- rmfi_parse_variables(comments[rot_line])$variables
        rotation <- as.numeric(rot[6])
      } else {
        rotation <- 0
      }
      prj <- rmf_create_prj(origin = origin, rotation = rotation)
    }
  } 

  return(list(prj = prj, remaining_comments = comments))
}


#' Title
#'
#' @param file 
#' @param dis 
#'
#' @return
#' @export
#'
#' @examples
rmf_read_usgs_model_reference <- function(file = {cat('Please select usgs.model.reference file ...\n'); file.choose()},
                                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}) {
  
  lines <- readr::read_lines(file)
  
  # remove commented lines
  comments <- which(substr(lines, 1, 1) == "#")
  if(length(comments) > 0) lines <- lines[-comments]
  
  # origin
  xul <- grep('xul', lines, ignore.case = TRUE)
  yul <- grep('yul', lines, ignore.case = TRUE)
  x <- as.numeric(rmfi_parse_variables(lines[xul])$variables[2])
  y <- as.numeric(rmfi_parse_variables(lines[yul])$variables[2])
  origin <- c(x, y)
  
  # rotation
  rot <- grep('rotation', lines, ignore.case = TRUE)
  rotation <- as.numeric(rmfi_parse_variables(lines[rot])$variables[2])
  
  # crs
  epsg <- grep('epsg', lines, ignore.case = TRUE)
  prj4 <- grep('proj4', lines, ignore.case = TRUE)
  
  if(length(epsg) > 0) {
    crs <- as.numeric(rmfi_parse_variables(lines[epsg])$variables[2])
  } else if(length(prj4) > 0) {
    crs <- sub('proj4', '', lines[prj4], ignore.case = TRUE)
  } else {
    crs <- NA
  }
  
  prj <- rmf_create_prj(origin = origin, rotation = rotation, crs = crs, ulcoordinate = TRUE, dis = dis)
  return(prj)
}



# TODO add to utils

#' Title
#'
#' @param dis 
#' @param prj 
#'
#' @return
#' @export
#'
#' @examples
rmf_extent <- function(dis, prj = rmf_get_prj(dis)) {
  
  corners <- data.frame(x = rep(c(0, sum(dis$delr)), each = 2), y = c(0, sum(dis$delc), sum(dis$delc), 0))
  row.names(corners) <- c('ll', 'ul', 'ur', 'lr')
  
  if(!is.null(prj)) {
    coords <- rmf_convert_grid_to_xyz(x = corners$x,
                                      y = corners$y,
                                      prj = prj,
                                      dis = dis)
    corners <- structure(coords, row.names = row.names(corners))
  } 
  
  bbox <- sf::st_bbox(c(xmin = min(corners$x),
                        xmax = max(corners$x),
                        ymin = min(corners$y),
                        ymax = max(corners$y)),
                      crs = ifelse(is.null(prj), NA, prj$crs))

  return(list(corners = corners, bbox = bbox))
}



#' Obtain a multiplier to convert MODFLOW length units to projection length units
#'
#' @param dis \code{RMODFLOW} dis object
#' @param prj \code{RMODFLOW} prj object
#' @param to either 'grid' or 'xyz' specifying if the multiplier should convert coordinates to the modflow grid or to real world coordinates (inverse)
#' @details The MODFLOW length unit \code{(dis$lenuni)} can be different from the projection unit \code{(prj$crs$units)}. When converting coordinates
#' using \code{rmf_convert_grid_to_xyz} or \code{rmf_convert_xyz_to_grid} the difference in length unit needs to be corrected for.
#' @return single numeric value which can be used to multiply MODFLOW coordinates with so to convert them to prj length units.
#' @keywords internal
rmfi_prj_length_multiplier <- function(dis, prj, to) {
  
  if(dis$lenuni == 0 || is.null(prj) || is.na(prj$crs) || is.null(prj$crs$units)) {
    mlt <- 1
  } else {
    
    # units in m
    un <- c('km','m','dm','cm','mm','kmi','in','ft','yd','mi','fath','ch',
            'link','us-in','us-ft','us-yd','us-ch','us-mi','ind-yd','ind-ft') 
    conv <- c(1000,1,0.1,0.01,0.001,1852,0.0254,0.3048,0.9144,1609.344,1.828804,20.11684,
              0.2011684,0.02540005,0.3048006,0.9144018,20.116840234,1609.347,0.9143988,0.3047996)
    prj_un <- prj$crs$units
    
    # convert prj units & mf units to meter
    prj_to_meter <- conv[which(un == prj_un)]
    mf_to_meter <- switch(as.character(dis$lenuni),
                          '1' = conv[which(un == 'ft')],
                          '2' = conv[which(un == 'm')],
                          '3' = conv[which(un == 'cm')])
    
    mlt <- mf_to_meter / prj_to_meter
  }
  mlt <- ifelse(to == 'grid', 1/mlt, mlt)
  return(mlt)
}

