
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
  
  # check if obj projection == dis projection
  
  dis_sf <- rmf_create_array(1:(dis$nrow*dis$ncol), dim = c(dis$nrow, dis$ncol)) %>%
    rmf_as_sf(dis = dis, name = 'id')
  
  # subset
  ints <- dis_sf[obj, op = op] 
  
  # deal with 3D data, e.g.. depth of wells
  # use a 3D array for that 
  # need to obtain a k index in that case
  ijk <- rmf_convert_id_to_ijk(id = ints$id, dis = dis)
  
  # k depends on z 
  # TODO also for multipoint ?
  if(is.null(k)) {
    # if(class(sf::st_geometry(obj)[[1]])[1] == "XYZ") {
    #   
    #   ijk$k <- rmf_convert_xyz_to_grid()
    # }
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
#' @param prj 
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
                            prj = NULL,
                            op = sf::st_intersects,
                            kper = attr(obj, 'kper'),
                            ...) {
  
  ar <- rmf_as_list(obj, dis = dis, select = select, prj = prj, op = op) %>%
    rmf_as_array(dis = dis, select = 4, kper = kper, ...)
  return(ar)
  
}

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
rmf_as_list.stars <- function(obj,
                              dis,
                              select,
                              prj = NULL,
                              kper = attr(obj, 'kper'),
                              op = sf::st_intersects) {
  
  lst <- sf::st_as_sf(stars) %>%
    rmf_as_list(dis = dis, select = select, prj = prj, kper = kper, op = op)
  
  return(lst)
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param select 
#' @param kper 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_array.stars <- function(obj,
                               dis, 
                               select = 1,
                               kper = attr(obj, 'kper')) {
  
  # 2D
  ar <- obj[[select]] %>% 
    t() %>%
    rmf_create_array(dim = c(dis$nrow, dis$ncol))
  
  # 3D
  
  # 4D
  
  return(ar)
  
}

#' Title
#'
#' @param obj 
#' @param dis 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_array.raster <- function(obj, 
                                dis) {
  
}

## RMODFLOW --> SPATIAL
##

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

#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#' @param name 
#'
#' @method rmf_as_stars rmf_2d_array
rmf_as_stars.rmf_2d_array <- function(array, dis, mask = array*0 + 1, prj = NULL, crs = NULL, name = 'value') {
  
  array[which(mask^2 != 1)] <- NA
  m <- t(as.matrix(array))
  dim(m) <- c(x = dim(m)[1], y = dim(m)[2]) # named dim
  
  # origin
  org <- rmfi_ifelse0(is.null(prj), c(0, 0, 0), prj$origin)
  
  # TODO rotation does not work properly in stars;
  
  # create stars object
  # d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = org[2] + rev(c(cumsum(dis$delc))), affine = rep(aff, 2))
  d <-  stars::st_dimensions(x = org[1] + c(0, cumsum(dis$delr)), y = org[2] + rev(c(cumsum(dis$delc))))
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

rmf_as_sf.rmf_list <- function(obj, dis, prj = NULL, crs = NULL, geom = 'polygon') {
  
  
  
}
