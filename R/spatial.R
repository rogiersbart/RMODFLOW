
# SPATIAL to RMODFLOW ----

#' Convert a simple features object to rmf_list
#'
#' @param obj \code{sf} object
#' @param dis \code{RMODFLOW} dis object
#' @param select integer or character specifying columns from \code{obj} to select. Defaults to all columns
#' @param prj \code{RMODFLOW} prj object
#' @param k optional integer vector of length \code{nrow(obj)} specifying the layer index for each feature. If not present, all features are assumed to be in layer 1.
#' @param kper optional integers specifying the stress-periods during which this rmf_list is active
#' @param op geometric operator to use in the spatial join. Defaults to \code{sf::st_intersects}. See details.
#' @param ... additional arguments passed to \code{sf::st_join}
#' 
#' @details A spatial join between the MODFLOW grid (as polygons) and \code{obj} is performed using \code{sf::st_join(left = FALSE, op = op)}.
#' The geometric operator \code{op} can be any kind described in the \code{sf} help pages. See \code{?sf::st_intersects}.
#' 
#' @return a \code{RMODFLOW} rmf_list object
#' @export
#'
#' @examples
#' dis <- rmf_create_dis()
#' 
#' # point
#' pts <- sf::st_sfc(list(sf::st_point(c(150, 312)), sf::st_point(c(500, 500)), sf::st_point(c(850, 566))))
#' obj <- sf::st_sf(q = c(-500, -400, -300), geom = pts)
#' 
#' (rlst <- rmf_as_list(obj, dis))
#' 
#' # 4 cells selected for second point on cell edges
#' rmf_plot(rlst, dis, k = 1, grid = TRUE) +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE)
#' 
#' prj <- rmf_create_prj(rotation = 12)
#' rmf_as_list(obj, dis, prj = prj, k = c(2, 2, 3))
#' 
#' # multipoint
#' mp <- sf::st_multipoint(rbind(c(150,312), c(500, 500), c(850, 566)))
#' obj <- sf::st_sf(q = -500, geom = sf::st_sfc(mp))
#' 
#' rmf_as_list(obj, dis)
#' 
#' # linestring
#' s1 <- rbind(c(150,312), c(500, 500), c(850, 566))
#' ls1 <- sf::st_linestring(s1)
#' s2 <- rbind(c(100,100), c(500, 555))
#' ls2 <- sf::st_linestring(s2)
#' 
#' obj <- sf::st_sf(conductance = 500, quality = c('good', 'poor'), geom = sf::st_sfc(ls1, ls2))
#' 
#' rmf_as_list(obj, dis, select = 'conductance')
#' 
#' # multilinestring
#' mls <- sf::st_multilinestring(list(s1, s2))
#' 
#' obj <- sf::st_sf(conductance = 500, quality = 'mixed', geom =   sf::st_sfc(mls))
#' 
#' rmf_as_list(obj, dis) %>% 
#'   rmf_plot(dis, k = 1, grid = TRUE) +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE)
#' 
#' # op = sf::st_crosses
#' rmf_as_list(obj, dis, op = sf::st_crosses) %>% 
#'   rmf_plot(dis, k = 1, grid = TRUE) +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE)
#' 
#' # polygon
#' p1 <- rbind(c(120, 120), c(120, 760), c(800, 800), c(120, 120))
#' pol1 <- sf::st_polygon(list(p1))
#' 
#' obj <- sf::st_sf(head = 15, geom = sf::st_sfc(pol1))
#' 
#' # op = sf::st_intersects
#' rmf_as_list(obj, dis) %>%
#'   rmf_plot(dis, k = 1, grid = TRUE) +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')
#' 
#' # op = sf::st_covers
#' rmf_as_list(obj, dis, op = sf::st_covers) %>%
#'   rmf_plot(dis, k = 1, grid = TRUE) +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')
#' 
#' p2 <- rbind(c(410, 125), c(812, 133), c(902, 488), c(410, 125))
#' pol2 <- sf::st_polygon(list(p1, p2))
#' 
#' (obj <- sf::st_sf(head = 15, geom = sf::st_sfc(pol2)))
#' 
#' rmf_as_list(obj, dis) %>%
#'   rmf_plot(dis, k = 1, grid = TRUE, variable = 'head', type = 'factor') +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')
#' 
#' pol2 <- sf::st_polygon(list(p2))
#' (obj <- sf::st_sf(head = c(15, 12), geom = sf::st_sfc(pol1, pol2)))
#' 
#' rmf_as_list(obj, dis) %>%
#'   rmf_plot(dis, k = 1, grid = TRUE, variable = 'head', type = 'factor') +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')
#' 
#' # multipolygon
#' p3 <- rbind(c(150, 960), c(440, 960), c(440, 875), c(150, 875), c(150, 960))
#' mpol <- sf::st_multipolygon(list(list(p1, p2), list(p3)))
#' 
#' (obj <- sf::st_sf(head = 15, geom = sf::st_sfc(mpol)))
#' 
#' rmf_as_list(obj, dis) %>%
#'   rmf_plot(dis, k = 1, grid = TRUE, variable = 'head', type = 'factor') +
#'   ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')
#' 
#' # geometry collection
#' gc <- sf::st_geometrycollection(list(mp, mpol, ls1))
#' 
#' (obj <- sf::st_sf(head = 15, geom = sf::st_sfc(gc)))
#' 
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
  # obj$.unq_id <- seq_len(nrow(obj))
  ints <- sf::st_join(obj, target, join = op, left = FALSE, ...)
  
  # TODO remove duplicate geometries caused by feature located on cell boundary
  # code below does not work properly as it removes all extra copies of the same *feature* 
  # as e.g. in a MULTI* or LINESTRING/POLYGON feature
  #
  # ints <- ints[!duplicated(ints$.unq_id), ]
  # ints$.unq_id <- NULL
  # obj$.unq_id <- NULL
  
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
  rownames(rlst) <- 1:nrow(rlst)
  
  return(rlst)
}

#' Convert a simple features object to a rmf_array (rasterize)
#'
#' @param obj \code{sf} object
#' @param dis \code{RMODFLOW} dis object
#' @param select integer or character specifying which column from \code{obj} to rasterize
#' @param na_value value of the cells in the array which are not specified in obj; defaults to 0
#' @param prj \code{RMODFLOw} prj object
#' @param sparse logical; should a 2d (TRUE; default) or 3d (FALSE) array be returned
#' @param kper optional integers specifying the stress-periods during which this array is active
#' @param ... additional arguments passed to \code{stars::st_rasterize}
#'
#' @details \code{stars::st_rasterize} is used to rasterize \code{obj} to the MODFLOW grid which calls GDALRasterize.
#'  Alternatively, the user can call \code{rmf_as_list} on \code{obj} and \code{rmf_as_array} on the resulting \code{rmf_list}. Results may differ.
#'
#' @return a \code{rmf_2d_array} if \code{sparse = TRUE}; a \code{rmf_3d_array} if \code{sparse = FALSE}
#' @export
#'
#' @examples
#' sfc <- sf::st_sfc(list(sf::st_point(c(100,200)), sf::st_point(c(750, 800)), sf::st_point(c(700, 850))))
#' obj <- sf::st_sf(q = c(-500, -400, -300), geom = sfc)
#' dis <- rmf_create_dis()
#' rmf_as_array(obj, dis = dis, select = 'q')
#' rmf_as_array(obj, dis = dis, select = 'q', options = c('MERGE_ALG=ADD'))
#' 
#' # alternative
#' rmf_as_list(obj, dis = dis, select = 'q') %>%
#'   rmf_as_array(dis = dis)
rmf_as_array.sf <- function(obj, 
                            dis,
                            select,
                            na_value = 0,
                            prj = rmf_get_prj(dis),
                            sparse = TRUE,
                            kper = attr(obj, 'kper'),
                            ...) {
  
  select <- select

  target <- rmf_as_stars(rmf_create_array(na_value, dim = c(dis$nrow, dis$ncol)), dis = dis, prj = prj, id = FALSE)
  s <- stars::st_rasterize(obj[select], template = target, ...)
  ar <- rmf_as_array(s, dis = dis, prj = prj, select = 1, resample = FALSE, kper = kper)
  if(!sparse) ar <- rmf_create_array(ar, dim = c(dis$nrow, dis$ncol, dis$nlay))
    
  # ar <- rmf_as_list(obj, dis = dis, select = select, prj = prj, k = k, op = op, ...) %>%
  #   rmf_as_array(dis = dis, select = 4, sparse = sparse, kper = kper, ...)
  
  return(ar)
  
}

#' Convert a stars object to rmf_list
#'
#' @param obj \code{stars} object
#' @param dis \code{RMODFLOW} list object
#' @param select integer or character vector denoting the variables to select from \code{obj}. Defaults to all variables.
#' @param k optional integer specifying the layer index for each feature. If not present, all features are assumed to be in layer 1.
#' @param prj \code{RMODFLOW} prj object
#' @param kper optional integers specifying the stress-periods during which this rmf_list is active
#' @param op geometric operator to use in the spatial join. Defaults to \code{sf::st_intersects}. See details.
#' @param ... additional arguments passed to \code{rmf_as_list.sf}
#' 
#' @details \code{obj} is converted to \code{sf} using \code{sf::st_as_sf}. \code{rmf_as_list} is then called on the resulting \code{sf} object.
#' This function is intended for \code{stars} objects with geometry dimensions (e.g. \code{sf} objects).
#' @return a \code{rmf_list} object
#' @export
#'
#' @examples
#' sfc <- sf::st_sfc(list(sf::st_point(c(100,200)), sf::st_point(c(750, 800)), sf::st_point(c(700, 850))))
#' obj <- sf::st_sf(q = c(-500, -400, -300), m = 2, geom = sfc)
#' s <- stars::st_as_stars(obj)
#' dis <- rmf_create_dis()
#' rmf_as_list(s, dis)
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

#' Convert a stars object to rmf_array
#'
#' @param obj \code{stars} object
#' @param dis \code{RMODFLOW} dis object
#' @param select integer or character specifying which variable from \code{obj} to select
#' @param kper optional integers specifying the stress-periods during which this array is active
#' @param prj \code{RMODFLOW} prj object
#' @param resample logical specifying if \code{obj} should be resampled to the MODFLOW grid. Defaults to TRUE.
#' @param method character specifying the resampling method when \code{resample = TRUE}. Defaults to 'bilinear'. See details.
#' @param ... ignored
#'
#' @details If \code{resample = TRUE}, \code{stars::st_warp} is called with the specified method. For possible \code{method} values, see \code{?stars::st_warp}.
#'  If \code{resample = FALSE}, the array is pulled directly from \code{obj}. The latter will fail when the dimensions and crs of \code{obj} and the MODFLOW grid are 
#'  not exactly the same. In that case, the user should consider setting \code{resample = TRUE}.
#'  
#'  \code{rmf_as_array} currently can not handle \code{obj} or \code{prj} without defined crs.
#'  
#'  This function is intended for use with 2D \code{stars} objects with dimensions X & Y, 3D \code{stars} objects with dimensions 
#'  X, Y and \code{dis$nlay} or 4D \code{stars} objects with dimensions X, Y, \code{dis$nlay} and \code{sum(dis$nstp)}.
#'
#' @return a \code{rmf_2d_array}, \code{rmf_3d_array} or \code{rmf_4d_array} depending on the number of dimensions in \code{obj}
#' @export
#'
#' @examples
#' dis <- rmf_create_dis()
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol), dim = c(dis$nrow, dis$ncol))
#' s <- rmf_as_stars(r, dis = dis)
#' 
#' rmf_as_array(s, dis = dis, resample = FALSE)
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
  
  # TODO stars::st_warp does not work when objects don't have crs defined. Perhaps use sf::st_interpolate_aw ?
  
  # TODO check if obj projection == dis projection

  if(ndim > 4) stop('Support for obj with more than 4 dimensions not implemented', call. = FALSE)
  if(any(vapply(dims, function(i) inherits(i$values, 'sfc'), TRUE))) stop('stars objects with sfc dimensions are not supported', call. = FALSE)

  target <- rmf_as_stars(rmf_create_array(1, dim = c(dis$nrow, dis$ncol)), dis = dis, prj = prj, id = FALSE)
  
  if(resample) {
    # st_warp doesn't work when objects don't have crs
    # no problem if crs are different since st_warp will transform to destination crs
    if(is.null(prj) || is.na(sf::st_crs(prj$crs)) || is.na(sf::st_crs(obj))) {
      stop('obj crs and/or prj are missing. Consider setting resample = FALSE', call. = FALSE)
    }
  } else {
    # error out if obj dimensions do not coincide with dis domain or if crs are different
    if(sf::st_crs(obj) != sf::st_crs(target)) stop('crs of obj and prj differ', call. = FALSE)
    if(!all(sf::st_bbox(obj) == sf::st_bbox(target))) stop('Spatial extents of obj and dis do not coincide. Consider setting resample = TRUE', call. = FALSE)
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
    # reverse rows
    ar <- ar[rev(seq_len(dim(ar)[1])),]
    
  } else if(ndim == 3) {
    # 3D
    nnlay <- dis$nlay + sum(dis$laycbd != 0)
    if(dim(obj)[3] != nnlay) stop('Third dimension of stars object should have length equal to dis$nlay (+ number of optional confining beds)', call. = FALSE)
    
    if(resample) {
      target <- rmf_as_stars(rmf_create_array(1, dim = c(dis$nrow, dis$ncol, nnlay)), dis = dis, prj = prj, id = FALSE)
      ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
      ar <- aperm(ar[[1]], c(2,1,3)) %>% c() %>%
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay), kper = kper)
    } else {
      # extract array
      ar <- aperm(obj[[select]], c(2,1,3)) %>% c() %>%
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay), kper = kper)
    }
    # reverse rows
    ar <- ar[rev(seq_len(dim(ar)[1])),,]
    
  } else if(ndim == 4) {
    # 4D
    nnlay <- dis$nlay + sum(dis$laycbd != 0)
    if(dim(obj)[3] != nnlay) stop('Third dimension of stars object should have length equal to dis$nlay (+ number of optional confining beds)', call. = FALSE)
    if(dim(obj)[4] != sum(dis$nstp)) stop('Fourth dimension of stars object should have length equal to sum(dis$nstp)' , call. = FALSE)
    
    if(resample) {
      target <- rmf_as_stars(rmf_create_array(1, dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp))), dis = dis, prj = prj, id = FALSE)
      ar <- stars::st_warp(obj[select], target, method = method, use_gdal = method != 'near')
      ar <- aperm(ar[[1]], c(2,1,3,4)) %>% c() %>%
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp)), kper = kper)
    } else {
      # extract array
      ar <- aperm(obj[[select]], c(2,1,3,4)) %>% c() %>%
        rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp)), kper = kper)
    }
    # reverse rows
    ar <- ar[rev(seq_len(dim(ar)[1])),,,]
  }
  
  return(ar)
}

#' Convert a raster object to rmf_array
#'
#' @param obj object of class \code{RasterLayer}, \code{RasterStack} or \code{RasterBrick}
#' @param dis \code{RMODFLOW} object
#' @param ... additional arguments passed to \code{\link{rmf_as_array.stars}}
#'
#' @details \code{obj} is first converted to \code{stars} using \code{stars::st_as_stars}. \code{rmf_as_array.stars} is called on the resulting \code{stars} object.
#'
#' @return a \code{rmf_2d_array} or \code{rmf_3d_array} depending on the dimensions of \code{obj}
#' @export
#'
#' @examples
#' dis <- rmf_create_dis()
#' r <- raster::raster(matrix(1:prod(dis$nrow, dis$ncol), 10, 10),
#'                     xmx = sum(dis$delr), ymx = sum(dis$delc))
#' rmf_as_array(r, dis, resample = FALSE)
rmf_as_array.Raster <- function(obj, 
                                dis,
                                ...) {
  rmf_as_array(stars::st_as_stars(obj), dis = dis, ...)
}



# RMODFLOW to SPATIAL ----


#' Functions to convert rmf_array and rmf_list objects to simple features
#' 
#' @param array \code{rmf_2d_array}, \code{rmf_3d_array} or \code{rmf_4d_array} object
#' @param obj \code{rmf_list} object
#' @param dis \code{RMODFLOW} dis object
#' @param mask a 2d array when \code{array} is 2d or a 3d array when \code{array} is 3d or 4d that can be coerced to logical. Used to specify which cells to convert to sf. Defaults to all cells.
#' @param prj \code{RMODFLOW} prj object
#' @param name character specifying the name of the resulting variable in the sf object. Defaults to \code{'value'}
#' @param as_points logical; should returned sf object represent cell-centered nodal points (TRUE) or cell polygons (FALSE, default)
#' @param id either \code{'r'} (default) or \code{'modflow'}. Specifies which type of cell id is returned. R uses column-major array ordering whereas MODFLOW uses row-major ordering.
#' @param ... additional arguments passed to \code{rmf_as_tibble} when converting a \code{rmf_list} object. Otherwise, ignored.
#' 
#' @details The returned z coordinate when \code{as_points = TRUE} reflects the cell node.
#' The crs is taken from the \code{prj} argument.
#'  
#' @return A \code{sf} object with point geometries representing the cell-centered nodes when \code{as_points = TRUE}. When \code{as_points = FALSE},
#' the geometries are polygons representing the entire cell. 
#' When converting a \code{rmf_array}, the \code{sf} object has following variables: one with the array values per cell/node, 
#' one containing the cell id (when \code{id} is \code{'r'} or \code{'modflow'}), top and bottom of the cells when the array is 3d or 4d plus the z value of the node when \code{as_points = TRUE}.
#' When a 4d array is converted, an additional time column is added as well.
#' 
#' When converting a \code{rmf_list}, all variables in the \code{rmf_list} object are retained with the addition of the cell id column (when \code{id} is \code{'r'} or \code{'modflow'}) and
#' top and bottom columns if \code{as_points = FALSE} or a z column when \code{as_points = TRUE}.
#' 
#' @rdname rmf_as_sf
#' @export
#' @examples
#' dis <- rmf_create_dis()
#' 
#' # 2d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol), dim = c(dis$nrow, dis$ncol))
#' rmf_as_sf(r, dis = dis)
#' rmf_as_sf(r, dis = dis, as_points = TRUE)
#' 
#' # 3d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol, dis$nlay), dim = c(dis$nrow, dis$ncol, dis$nlay))
#' rmf_as_sf(r, dis = dis, id = 'modflow')
#' rmf_as_sf(r, dis = dis, as_points = TRUE)
#' 
#' # 4d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol, dis$nlay, 2), dim = c(dis$nrow, dis$ncol, dis$nlay, 2))
#' rmf_as_sf(r, dis = dis, id = FALSE)
#' 
#' # rmf_list
#' l <- rmf_create_list(data.frame(i = 1, j = 1:2, k = c(3, 2), q = c(-500, -400)))
#' rmf_as_sf(l, dis = dis)
#' 
rmf_as_sf <- function(...) {
  UseMethod('rmf_as_sf')
}

#' @export
#' @rdname rmf_as_sf
#' @method rmf_as_sf rmf_2d_array
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
rmf_as_sf.rmf_4d_array <- function(array, dis, mask = array(1, dim = dim(array)[1:3]), prj = rmf_get_prj(dis), name = 'value', as_points = FALSE, id = 'r', ...) {

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

#' Functions to convert rmf_array and rmf_list objects to stars objects
#'
#' @param array \code{rmf_2d_array}, \code{rmf_3d_array} or \code{rmf_4d_array} object
#' @param obj \code{rmf_list} object
#' @param dis \code{RMODFLOW} dis object
#' @param mask a 2d array when \code{array} is 2d or a 3d array when \code{array} is 3d or 4d that can be coerced to logical. Used to specify which cells to convert. Defaults to all cells.
#' @param prj \code{RMODFLOW} prj object
#' @param name character specifying the name of the resulting variable in the stars object. Defaults to \code{'value'}
#' @param id either \code{'r'} (default) or \code{'modflow'}. Specifies which type of cell id is returned. R uses column-major array ordering whereas MODFLOW uses row-major ordering.
#' @param select integer or character specifying which column of the \code{rmf_list} object to convert to \code{stars} variable.
#' @param ... additional arguments passed to \code{rmf_as_array} when converting a \code{rmf_list} object. Otherwise, ignored.
#'
#' @details The crs is taken from the \code{prj} argument.
#'
#' @return a \code{stars} object with x and y dimensions when \code{array} is 2d, x, y and layer (integer representing MODFLOW layer; similar to bands) when \code{array} is 3d,
#' x, y, layer and time dimensions when \code{array} is 4d. When converting a \code{rmf_list} object, it is first converted to a \code{rmf_array} using \code{rmf_as_array}.
#' Two variables are present in the returned \code{stars} object, one with the array values and on with the cell id (when \code{id} is \code{'r'} or \code{'modflow'}).
#' 
#' @export
#' @examples
#' dis <- rmf_create_dis()
#'
#' # 2d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol), dim = c(dis$nrow, dis$ncol))
#' rmf_as_stars(r, dis = dis)
#' 
#' # 3d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol, dis$nlay), dim = c(dis$nrow, dis$ncol, dis$nlay))
#' rmf_as_stars(r, dis = dis, id = 'modflow')
#' 
#' # 4d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol, dis$nlay, 2), dim = c(dis$nrow, dis$ncol, dis$nlay, 2))
#' rmf_as_stars(r, dis = dis, id = FALSE)
#' 
#' # rmf_list
#' l <- rmf_create_list(data.frame(i = 1, j = 1:2, k = c(3, 2), q = c(-500, -400), d = 35))
#' rmf_as_stars(l, dis = dis, select = 'q')
#' 
rmf_as_stars <- function(...) {
  UseMethod('rmf_as_stars')
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_2d_array
#' @export
rmf_as_stars.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = rmf_get_prj(dis), name = 'value', id = 'r', ...) {
  
  array[which(mask == 0)] <- NA
  m <- t(as.matrix(array[rev(seq_len(dim(array)[1])),])) # stars origin will be bottomleft instead of R topleft, so reverse row order
  dim(m) <- c(x = dim(m)[1], y = dim(m)[2]) # named dim
  
  # origin
  org <- rmfi_ifelse0(is.null(prj), c(0, 0, 0), prj$origin)
  
  length_mlt <- rmfi_prj_length_multiplier(dis, prj, to = "xyz")
  
  # TODO rotation does not work properly in stars;
  
  # create stars object
  # TODO specify affine parameters directly instead of get_geotransform. Only possible when stars API is updated to allow this
  # d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = rev(org[2] + c(0, cumsum(dis$delc))), affine = rep(aff, 2))
 
  # TODO use negative deltay: more consistent with how most raster data is read from file. Problem is that origin is then upper left and problems are created with rotations
  # d <-  stars::st_dimensions(x = org[1] + (c(0, cumsum(dis$delr)) * length_mlt), y = rev(org[2] + (c(0, cumsum(dis$delc)) * length_mlt)))
  
  d <-  stars::st_dimensions(x = org[1] + (c(0, cumsum(dis$delr)) * length_mlt), y = org[2] + (c(0, cumsum(dis$delc)) * length_mlt))
  s <- stars::st_as_stars(m, dimensions = d)
  names(s) <- name
  
  # rot <- ifelse(is.null(prj), 0, - prj$rotation * pi/180) # for negative deltay
  rot <- ifelse(is.null(prj), 0, prj$rotation * pi/180)
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
    s$id <- c(aperm(ids[rev(1:dim(ids)[1]), ], c(2,1)))
  } else if(id == 'r') {
    ids <- array(1:prod(dim(array)[1:2]), dim = dim(array)[1:2])
    s$id <- c(aperm(ids[rev(1:dim(ids)[1]), ], c(2,1)))
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
  
  array[which(mask == 0)] <- NA
  
  s <- rmf_as_stars(array[,,1], dis = dis, prj = prj, name = 'layer_1', id = id)
  if(id %in% c('r', 'modflow')) ids <- c(s$id) + c(rep(0, prod(dis$nrow, dis$ncol)), rep(prod(dis$nrow, dis$ncol) * seq_len(dis$nlay - 1), each = prod(dis$nrow, dis$ncol)))
  d <- stars::st_dimensions(s)
  
  array_list <- lapply(seq_len(dim(array)[3]), 
                        function(i) t(as.matrix(array[rev(seq_len(dim(array)[1])),,i]))) %>% 
    setNames(paste('layer', seq_len(dim(array)[3]), sep = '_'))
  s <- stars::st_as_stars(array_list, dimensions = d) %>%
    merge() %>%
    setNames(name) %>%
    stars::st_set_dimensions(names = c(names(d), 'layer'))
  if(id %in% c('r', 'modflow')) {
    s$id <- ids
    dim(s[[name]]) <- dim(s$id) # TODO this might change in the stars API
  }

  return(s)
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_4d_array
#' @export
rmf_as_stars.rmf_4d_array <- function(array, dis, mask = array(1, dim = dim(array)[1:3]), prj = rmf_get_prj(dis), name = 'value', id = 'r', ...) {
  
  mask <- array(mask, dim = c(dim(mask), dim(array)[4]))
  array[which(mask == 0)] <- NA
  
  s <- rmf_as_stars(array[,,,1], dis = dis, prj = prj, name = 'layer_1', id = id)
  if(id %in% c('r', 'modflow')) ids <- c(s$id) 
  d <- stars::st_dimensions(s)
  
  # time <- rep(rmfi_ifelse0(is.null(attr(array, 'totim')), 1:dim(array)[4], attr(array, 'totim')[!is.na(attr(array, 'totim'))]),
  #             each = prod(dis$nrow, dis$ncol, dis$nlay))
  
  time <- rmfi_ifelse0(is.null(attr(array, 'totim')), 1:dim(array)[4], attr(array, 'totim')[!is.na(attr(array, 'totim'))])

  array_list <- lapply(seq_len(dim(array)[4]), 
                       function(i) aperm(as.array(array[rev(seq_len(dim(array)[1])),,,i]), c(2,1,3))) %>% 
                setNames(time)
  
  s <- stars::st_as_stars(array_list, dimensions = d) %>%
    merge() %>%
    setNames(name) %>%
    stars::st_set_dimensions(names = c(names(d), 'time')) %>%
    stars::st_set_dimensions(which = 4, values = time)
  if(id %in% c('r', 'modflow')) {
    s$id <- ids
    dim(s[[name]]) <- dim(s$id) # TODO this might change in the stars API
  }
  
  return(s)
}

#' @rdname rmf_as_stars
#' @method rmf_as_stars rmf_list
#' @export
rmf_as_stars.rmf_list <- function(obj, dis, select, prj = rmf_get_prj(dis), name = 'value', id = 'r', ...) {
  
  rmf_as_array(obj, dis = dis, select = select, ...) %>% rmf_as_stars(dis = dis, prj = prj, name = name, id = id, ...)
  
}

#' Functions to convert rmf_array and rmf_list objects to raster objects
#' 
#' @param array \code{rmf_2d_array}, \code{rmf_3d_array} or \code{rmf_4d_array} object
#' @param obj \code{rmf_list} object
#' @param dis \code{RMODFLOW} dis object
#' @param prj \code{RMODFLOW} prj object
#' @param l integer specifying which dimension of a 4d array to convert to raster.
#' @param select integer or character specifying which column of the \code{rmf_list} object to convert to \code{raster} object variable.
#' @param ... additional arguments passed to \code{rmf_as_stars}
#'
#' @details the objects are first converted to \code{stars} using \code{rmf_as_stars}.
#'  Conversions to \code{raster} objects will fail when arrays are rectilinear or rotated.
#'
#' @return an object of class \code{RasterLayer} if array is 2d, or \code{RasterBrick} when array is 3d or 4d with the bands representing the MODFLOW layers.
#'  The variable values are obtained from the array or from the \code{select} column for \code{rmf_list} objects.
#' 
#' @export
#' @seealso \code{\link{rmf_as_stars}}
#' @examples
#' dis <- rmf_create_dis()
#' 
#' # 2d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol), dim = c(dis$nrow, dis$ncol))
#' rmf_as_raster(r, dis = dis)
#' 
#' # 3d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol, dis$nlay), dim = c(dis$nrow, dis$ncol, dis$nlay))
#' rmf_as_raster(r, dis = dis, id = 'modflow')
#' 
#' # 4d array
#' r <- rmf_create_array(1:prod(dis$nrow, dis$ncol, dis$nlay, 2), dim = c(dis$nrow, dis$ncol, dis$nlay, 2))
#' rmf_as_raster(r, dis = dis, l = 2)
#' 
#' # rmf_list
#' l <- rmf_create_list(data.frame(i = 1, j = 1:2, k = c(2, 2), q = c(-500, -400), d = 35))
#' rmf_as_raster(l, dis = dis, select = 'q')
#' 
#' # rotated grids can not be converted to raster
#' \dontrun{
#' prj <- rmf_create_prj(rotation = 12)
#' rmf_as_raster(r, dis = dis, l = 2, prj = prj)
#' }
#' 
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
rmf_as_raster.rmf_list <- function(obj, dis, select, prj = rmf_get_prj(dis), ...) {
  rmf_as_stars(obj, dis = dis, prj = prj, select = select, ...) %>% as('Raster')
}

# PROJECTION ----

#' Create a RMODFLOW projection object
#'
#' @param origin numeric vector with the x & y (and optionally z) coordinates of the origin which by default is the lowerleft corner of the model. Defaults to \code{c(0, 0, 0)}.
#' @param rotation numeric; counterclockwise rotation angle (in degrees). Rotation is around the lowerleft corner of the model. Defaults to 0.
#' @param crs coordinate reference system of the model. Any values accepted by \code{sf::st_crs} may be defined. Defaults to \code{NA}.
#' @param ulcoordinate logical; if \code{TRUE}, \code{origin} refers to the upperleft cell instead of the lowerleft.
#' @param nodecoordinate logical; if \code{TRUE}, \code{origin} refers to the cell center instead of the cell corner.
#' @param dis \code{RMODFLOW} dis object. Only required when \code{ulcoordinate} or \code{nodecoordinate} is TRUE. 
#'
#' @details \code{origin} should be specified in the length units defined by \code{crs}.
#' If no z coordinate is specified in \code{origin}, it is set to zero.
#' \code{crs} is set by a call to \code{sf::st_crs}.
#' Rotation is around the lowerleft corner of the model.
#' \code{RMODFLOW} does not work optimally for geographic coordinate systems. 
#'
#' @return an object of class \code{prj} which is a list with the (1) origin vector containing x, y and z coordinates of the lowerleft corner,
#'  (2) the rotation angle in degrees counterclockwise and (3) the coordinate reference system as an \code{sf crs} object
#' @export
#'
#' @examples
#' rmf_create_prj(origin = c(152082, 168000.2), rotation = -12, crs = 31370)
#' 
#' dis <- rmf_create_dis()
#' rmf_create_prj(origin = c(120, 300, 13), ulcoordinate = TRUE, dis = dis)
#' 
rmf_create_prj <- function(origin = c(0, 0, 0), 
                           rotation = 0, 
                           crs = NA,
                           ulcoordinate = FALSE,
                           nodecoordinate = FALSE,
                           dis = NULL) {
  
  crs <- sf::st_crs(crs)
  if(!is.na(crs) && sf::st_is_longlat(crs)) warning("RMODFLOW does not work optimally for geographic coordinates. Please consider using a projected crs.", call. = FALSE)
  origin <- unlist(origin)
  
  if(ulcoordinate) {
    if(is.null(dis)) stop('Please provide a dis object when ulcoordinate = TRUE', call. = FALSE)
    length_mlt <- rmfi_prj_length_multiplier(dis, prj = list(crs = crs), to = 'xyz')
    
    if(nodecoordinate) { # set origin as corner coordinate
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
    
  } else if(nodecoordinate) {
    # set origin as corner coordinate
    if(is.null(dis)) stop('Please provide a dis object when nodecoordinate = TRUE', call. = FALSE)
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
    cat(' ', 'EPSG:', prj$crs$epsg, '\n')
    cat(' ', 'proj4string:', prj$crs$proj4string)
    if(is.na(prj$crs$epsg) && (is.character(prj$crs$proj4string) && length(prj$crs$proj4string) == 1) && !is.null(prj$crs$wkt)) {
      cat('\n', ' wkt defined')
    }
  }
cat('\n')
}

#' Functions to get, set, transform and check presence of prj objects
#' 
#' @param dis \code{RMODFLOW} dis object
#' @param modflow \code{RMODFLOW} modflow object
#' @param prj \code{RMODFLOW} prj object
#' @param file path to discretization file; typically "*.dis"
#' @param crs coordinate reference system to transform to. Input for \code{sf::st_crs}.
#' 
#' @name prj_auxiliary
NULL

#' 
#' @return \code{rmf_get_prj} returns a \code{RMODFLOW} prj object if present; otherwise \code{NULL}
#' @export
#' @rdname prj_auxiliary
#' @examples
#' dis <- rmf_read_dis(rmf_example_file('water-supply-problem.dis'))
#' rmf_get_prj(dis)
#' 
#' m <- rmf_read(rmf_example_file('example-model.nam'), verbose = FALSE)
#' rmf_get_prj(m)
#' 
#' # return NULL
#' rmf_get_prj(rmf_create_dis())
#' 
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
#' @method rmf_get_prj modflow
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
#' rmf_has_prj(dis)
#' rmf_has_prj(m)
#' rmf_has_prj(rmf_create_dis())
#' 
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
#' @return \code{rmf_set_prj} returns either a \code{RMODFLOW} dis or modflow object with the prj set or nothing when writing directly to a file.
#' @details If \code{prj} information is already present, a warning is raised when overwriting.
#' @export
#' @rdname prj_auxiliary
#' @examples
#' dis <- rmf_create_dis()
#' prj <- rmf_create_prj(origin = c(100, -150))
#' rmf_set_prj(dis, prj)
#' 
#' # write directly to header comments of file
#' f <- tempfile()
#' rmf_write_dis(dis, file = f)
#' rmf_set_prj(f, dis, prj)
#' rmf_read_dis(f)
#' 
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
#' @details \code{rmf_transform_prj} transforms the origin coordinates to the new crs. If no \code{prj} was set, an error is raised.
#' @export
#' @rdname prj_auxiliary
#' @examples
#' prj <- rmf_create_prj(origin = c(152082, 168000.2), rotation = -12, crs = 31370)
#' dis <- rmf_create_dis(prj = prj)
#' 
#' rmf_transform_prj(prj, crs = 4326)
#' rmf_transform_prj(dis, crs = 3044)
#' 
#' \dontrun{
#' # error when no prj is present
#' rmf_transform_prj(rmf_create_dis(), 3044)
#' }
#' 
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
  prj <- rmf_transform_prj(prj, crs)
  return(prj)
}

#' @export
#' @rdname prj_auxiliary
#' @method rmf_transform_prj modflow
rmf_transform_prj.modflow <- function(modflow, crs) {
  if(!rmf_has_prj(modflow)) stop('modflow object has no prj object to transform', call. = FALSE)
  prj <- rmf_get_prj(modflow)
  prj <- rmf_transform_prj(prj, crs)
  return(prj)
}

#' Read RMODFLOW projection information from a USGS model reference file
#'
#' @param file path to the USGS model reference file; typically "usgs.model.reference"
#' @param dis \code{RMODFLOW} dis object
#'
#' @return a \code{prj} object
#' @export
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

# INTERNALS -----

#' Write prj object information to file header
#'
#' @param dis \code{RMODFLOW} dis object
#' @param prj \code{RMODFLOW} prj object
#' @param file character with path of file to write to. Typically the discretization file.
#' @details Writes RMODFLOW projection information into the header of a file, typically the discretization file. All lines start with "#".
#' This information consists of a starter line, the coordinates of the 4 model corners, the grid rotation angle in degrees counterclockwise,
#' the z coordinate of the lower left corner and the crs description. This might be a EPSG code, a proj4string, or a wkt string. 
#' A ending line specifies the end of the RMODFLOW projection information.
#' @return \code{NULL}
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
    
    if(is.null(prj$crs) || is.na(prj$crs)) {
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

#' Read RMODFLOW projection information from header comments
#'
#' @param comments strings possibly containing RMODFLOW projection information.
#' @details \code{comments} is typically the output of \code{rmfi_parse_comments} as called when reading the discretization file. 
#' RMODFLOW projection is typically present in the header comments of the discretization file.
#' @return a list with a \code{prj} object and the remaining comments
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
    unit_df <- rmfd_supported_length_units
    prj_un <- prj$crs$units
    
    # convert prj units & mf units to meter
    if(!(prj_un %in% unit_df$unit)) {
      stop('Length unit not supported. Please check RMODFLOW:::rmfd_supported_length_units for supported length units.', call. = FALSE)
    } else {
      prj_to_meter <- unit_df$conv[which(unit_df$unit == prj_un)]
      mf_to_meter <- switch(as.character(dis$lenuni),
                            '1' = unit_df$conv[which(unit_df$unit == 'ft')],
                            '2' = unit_df$conv[which(unit_df$unit == 'm')],
                            '3' = unit_df$conv[which(unit_df$unit == 'cm')])
      
      mlt <- mf_to_meter / prj_to_meter
    }

  }
  mlt <- ifelse(to == 'grid', 1/mlt, mlt)
  return(mlt)
}

# UTILS ----

#' Obtain the corners and bounding box of the MODFLOW grid
#'
#' @param dis \code{RMODFLOW} dis object
#' @param prj optional \code{RMODFLOW} prj object
#'
#' @return a list with (1) the coordinates of the grid corners as a data.frame and (2) the bounding box of the grid as 
#' a \code{sf bbox} object
#' @export
#'
#' @examples
#' dis <- rmf_create_dis()
#' prj <- rmf_create_prj(origin = c(152082, 168000.2), rotation = -12, crs = 31370)
#' rmf_extent(dis, prj)
#' 
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
                      crs = rmfi_ifelse0(is.null(prj), NA, prj$crs))

  return(list(corners = corners, bbox = bbox))
}
