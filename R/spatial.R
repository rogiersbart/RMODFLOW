
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
                               prj = NULL,
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
    if(is.null(prj) || is.na(sf::st_crs(prj$projection)) || is.na(sf::st_crs(obj))) {
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
rmf_as_sf.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', as_points = FALSE, id = 'r', ...) {
  
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
rmf_as_sf.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', as_points = FALSE, id = 'r', ...) {
  
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
rmf_as_sf.rmf_4d_array <- function(array, dis, mask = array(1, dim = dim(array)), prj = NULL, name = 'value', as_points = FALSE, id = 'r', ...) {

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
rmf_as_sf.rmf_list <- function(obj, dis, prj = NULL, as_points = FALSE, id = 'r', ...) {
  
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
                 crs = rmfi_ifelse0(is.null(prj), NA, prj$projection))
  
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
rmf_as_stars.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', id = 'r', ...) {
  
  array[which(mask^2 != 1)] <- NA
  m <- t(as.matrix(array))
  dim(m) <- c(x = dim(m)[1], y = dim(m)[2]) # named dim
  
  # origin
  org <- rmfi_ifelse0(is.null(prj), c(0, 0, 0), prj$origin)
  
  # TODO rotation does not work properly in stars;
  
  # create stars object
  # use negative deltay: more consistent with how most raster data is read from file
  # d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = rev(org[2] + c(0, cumsum(dis$delc))), affine = rep(aff, 2))
  d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = rev(org[2] + c(0, cumsum(dis$delc))))
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
  if(!is.null(prj)) projection <- prj$projection
  if(!is.null(projection)) {
    s <- sf::st_set_crs(s, sf::st_crs(projection))
  }

  return(s)
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_3d_array
#' @export
rmf_as_stars.rmf_3d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, name = 'value', id = 'r', ...) {
  
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
rmf_as_stars.rmf_4d_array <- function(array, dis, mask = array(1, dim = dim(array)[1:3]), prj = NULL, name = 'value', id = 'r', ...) {
  
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

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_2d_array
#' @export
rmf_as_raster.rmf_2d_array <- function(array, dis, prj = NULL, ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_3d_array
#' @export
rmf_as_raster.rmf_3d_array <- function(array, dis, prj = NULL, ...) {
  rmf_as_stars(array, dis = dis, prj = prj, ...) %>% as('Raster')
}

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_4d_array
#' @export
rmf_as_raster.rmf_4d_array <- function(array, dis, l = NULL, prj = NULL, ...) {
  if(is.null(l)) stop('Please provide a l argument to subset the 4d array', call. = FALSE)
  rmf_as_stars(array[,,,l], dis = dis, prj = prj, ...) %>% as('Raster')
}

#' @rdname rmf_as_raster
#' @method rmf_as_raster rmf_list
#' @export
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

rmf_create_prj <- function(origin = c(0, 0), 
                           rotation = 0, 
                           crs = NA) {
  
  # TODO
  # add corner_coord = FALSE when origin represents cell node
  # add upperleft = TRUE when origin represents upperleft cell
  
  prj <- list()
  # z coordinate
  origin[3] <- ifelse(length(origin) > 2, origin[3], 0)
  crs <- sf::st_crs(crs)
  prj$origin <- origin
  prj$rotation <- rotation
  prj$crs <- crs
  
  class(prj) <- 'prj'
  return(prj)
}

print.prj <- function(prj) {
  cat('RMODFLOW Projection object:', '\n')
  cat('Origin coordinates (x y z) of the bottomleft corner:', '\n')
  cat(' ', prj$origin, '\n')
  cat('Grid rotation (degrees counterclockwise):', '\n')
  cat(' ', prj$rotation, '\n')
  print(prj$crs)
}
