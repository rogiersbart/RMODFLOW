
#'
#' Convert an object to a RMODFLOW array
#'
#'
#' @rdname rmf_as_array
#' @export

rmf_as_array <- function(...) {
  UseMethod('rmf_as_array')
}

#'
#' Convert a rmf_list to a RMODFLOW array
#' 
#' @param obj a \code{rmf_list}
#' @param dis a \code{RMODFLOW dis} object
#' @param select either a single integer or character specifying which column to select from the \code{rmf_list}. Defaults to the 4th column, i.e. the first variable.
#' @param na_value value of the cells in the array which are not specified in obj; defaults to 0
#' @param sparse logical; indicating if the returned array should be 2D or 3D. See details. Defaults to TRUE.
#' @param kper sets the kper attribute of the returned array; defaults to the kper attribute of obj
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum.
#' @param ... additional arguments passed to fun.
#' 
#' @details the dimension of the returned array is guessed from the supplied dis object. If there is only one unique k value in obj, 
#'  \code{sparse}, determines the dimensions of the returned array. If \code{sparse = TRUE}, a 2D array is returned. If \code{sparse = FALSE}, a 3D array is returned.
#'  When there is more than one unique k value in obj, a 3D array is always returned.
#'
#' @return either a \code{rmf_2d_array} or a \code{rmf_3d_array}
#' @rdname rmf_as_array
#' @export
#' @seealso \code{\link{rmf_as_list}}
#' 

rmf_as_array.rmf_list <- function(obj, 
                                  dis, 
                                  select = 4,
                                  na_value = 0,
                                  sparse = TRUE,
                                  kper = attr(obj, 'kper'),
                                  fun = sum,
                                  ...) {
  
  df <- list(values = obj[[select]])
  
  if(length(unique(obj$k)) == 1 && sparse) {
    df$id <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = 1, dis = dis, type = 'r')
    ar <- rmf_create_array(na_value, dim = c(dis$nrow, dis$ncol), kper = kper)
  } else {
    df$id <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis, type = 'r')
    ar <- rmf_create_array(na_value, dim = c(dis$nrow, dis$ncol, dis$nlay), kper = kper)
  }
  
  # deal with additive data
  if(is.numeric(df$values)) df <- aggregate(list(values = obj[[select]]), by = list(id = df$id), FUN = fun, ...)
  
  ar[as.numeric(df$id)] <- as.numeric(df$values)
  return(ar)
  
}

#'
#' Convert objects to a rmf_list
#'
#' @rdname rmf_as_list
#' @export

rmf_as_list <- function(...) {
  UseMethod('rmf_as_list')
}

#'
#' Converts a rmf_2d_array or rmf_3d_array to a rmf_list
#'
#' @param obj a \code{rmf_2d_array} or \code{rmf_3d_array}
#' @param dis a \code{RMODFLOW dis} object; only used when ijk is suppplied
#' @param name character; name of the resulting column which holds the extracted values from the array; defaults to "value"
#' @param mask a logical array with the same dimensions as obj. Used to select cells from obj; defaults to all cells
#' @param ijk optional; a data.frame with i, j and k columns used to select the cells. Overwrites the use of mask.
#' @param kper sets the kper attribute of the returned list; defaults to the kper attribute of obj
#' 
#' @details \code{\link{rmf_convert_id_to_ijk}} can be used to obtain a ijk data.frame from either MODFLOW cell id's or R cell id's.
#' 
#' @return a \code{rmf_list}
#' @rdname rmf_as_list
#' @export
#' @seealso \code{\link{rmf_as_array}}


rmf_as_list.rmf_3d_array <- function(obj, 
                                     dis, 
                                     name = "value",
                                     mask = 1 + obj*0, 
                                     ijk = NULL,
                                     kper = attr(obj, 'kper')) {
  
  if(!is.null(ijk)) {
    id <- rmf_convert_ijk_to_id(i = ijk$i, j = ijk$j, k = ijk$k, dis = dis, type = 'r') 
    values <- obj[id, drop = TRUE]
    
    lst <- cbind(ijk$i, ijk$j, ijk$k, values)
    colnames(lst) <- c("i", "j", "k", name)
    
  } else {
    id <- which(as.logical(mask))
    values <- obj[id, drop = TRUE]
    
    lst <- cbind(arrayInd(id, .dim = dim(obj)), values)
    colnames(lst) <- c("i", "j", "k", name)
    
  }
  
  return(rmf_create_list(lst, kper = kper))
}

#'
#' @rdname rmf_as_list
#' @details will set k to 1 
#' @export
rmf_as_list.rmf_2d_array <- function(obj, ...) {
  obj <- rmf_create_array(obj, dim = c(dim(obj), 1))
  rmf_as_list(obj, ...)
}

#' 
#' @param l integer index used to subset the 4th dimension of a \code{rmf_4d_array}.
#'
#' @rdname rmf_as_list
#' @export
rmf_as_list.rmf_4d_array <- function(obj, l, ...) {
  if(missing(l)) stop('Please specify a l argument', call. = FALSE)
  rmf_as_list(obj[,,,l], ...)
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
#'
#' @return
#' @export
#'
#' @examples
rmf_as_sf.rmf_2d_array <- function(array,
                                   dis,
                                   mask = array * 0 + 1,
                                   prj = NULL,
                                   crs = NULL) {
  df <- rmf_as_tibble(array, dis, mask, prj, crs)
  df <- df %>%
    dplyr::group_by(id, value) %>% tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, function(df) sf::st_polygon(list(as.matrix(rbind(df, df[1,]))))))
  if (is.null(prj)) {
    df$data <- sf::st_sfc(df$data)
  } else {
    df$data <- sf::st_sfc(df$data, crs = prj$projection)
  }
  df <- sf::st_sf(df)
  return(df)
}

#' Generic function to convert rmf_array objects to tibbles
#' 
#' @rdname rmf_as_tibble
#' @export
rmf_as_tibble <- function(...) {
  UseMethod('rmf_as_tibble')
}

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
    new_positions <- rmf_convert_grid_to_xyz(x=positions$x,y=positions$y,prj=prj)
    positions$x <- new_positions$x
    positions$y <- new_positions$y
  }
  if(!is.null(crs)) {
    positions <- rmfi_convert_coordinates(positions,from=sp::CRS(prj$projection),to=crs)
  }
  return(tibble::as_tibble(na.omit(merge(values, positions, by=c("id")))))
}

#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#' @param i 
#' @param j 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_tibble.rmf_3d_array <- function(array,
                                       i = NULL,
                                       j = NULL,
                                       k = NULL,
                                       dis,
                                       mask = array * 0 + 1,
                                       prj = NULL,
                                       crs = NULL) {
  if(!is.null(k)) {
    mask <- mask # evaluate before making changes to array
    array <- array[,,k]
    class(array) <- 'rmf_2d_array'
    mask <- mask[,,k]
    rmf_as_tibble(array = array, dis = dis, mask = mask, prj = prj, crs = crs)
  } else {
    xy <- NULL
    xy$x <- cumsum(dis$delr)-dis$delr/2
    xy$y <- rev(cumsum(dis$delc)-dis$delc/2)
    mask[which(mask==0)] <- NA
    
    # reason for optionally explicitely representing confining bed thickness:
    # this function might eventually replace code in rmf_plot functions and confining beds should explicitely be plotted
    if(any(dis$laycbd != 0) && dim(array)[3] != dim(dis$botm)[3]) {
      warning('Quasi-3D confining beds detected. Adding their thicknesses to the overlying numerical layers. Otherwise make sure the array explicitly contains Quasi-3D confining beds.')
      dis$thck <- rmf_calculate_thickness(dis, collapse_cbd = TRUE)
      botm <- rmfi_ifelse0(dis$nlay + sum(dis$laycbd != 0) > 1, dis$botm[,,cumsum((dis$laycbd != 0) +1)], dis$botm)
      nnlay <- dis$nlay
      dis$center <- botm
      for(a in 1:nnlay) dis$center[,,a] <- botm[,,a]+dis$thck[,,a]/2
    } else {
      if(any(dis$laycbd != 0)) warning('Quasi-3D confining beds detected; explicitly representing them.')
      dis$thck <- rmf_calculate_thickness(dis)
      nnlay <- dis$nlay + sum(dis$laycbd != 0)
      dis$center <- dis$botm
      for(a in 1:nnlay) dis$center[,,a] <- dis$botm[,,a]+dis$thck[,,a]/2
    }
    
    if(is.null(i) & !is.null(j)) {
      ids <- factor(1:(dis$nrow*dis$nlay))
      xWidth <- rep(rev(dis$delc),dis$nlay)
      yWidth <- dis$thck[,j,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$y,each=4),y=rep(dis$center[,j,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((array[,j,]*mask[,j,]^2)))
    } else if(!is.null(i) & is.null(j)) {
      ids <- factor(1:(dis$ncol*dis$nlay))
      xWidth <- rep(dis$delr,dis$nlay)
      yWidth <- dis$thck[i,,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(dis$center[i,,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((array[i,,]*mask[i,,]^2)))
    }
    return(tibble::as_tibble(na.omit(merge(values, positions, by=c("id")))))
  }
}

#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#' @param i 
#' @param j 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_tibble.rmf_4d_array <- function(array,
                                       i = NULL,
                                       j = NULL,
                                       k = NULL,
                                       l = NULL,
                                       dis,
                                       mask = array * 0 + 1,
                                       prj = NULL,
                                       crs = NULL) {
  if(!is.null(l)) {
    rmf_as_tibble(rmf_create_array(array[,,,l]), i = i, j = j, k = k, dis = dis, mask = mask[,,,l], prj = prj, crs = crs)
  } else if(!is.null(i) & !is.null(j) & !is.null(k)) {
    tibble::tibble(value = array[i, j, k, ], time = attributes(array)$totim)
  } else {
    if(dis$nper > 1 || dis$nstp[1] > 1) warning('Using final stress period results.', call. = FALSE)
    rmf_as_tibble(rmf_create_array(array[,,,dim(array)[4]]), i = i, j = j, k = k, dis = dis, mask = mask[,,,dim(array)[4]], prj = prj, crs = crs)
  }
}

#' Title
#'
#' @param obj 
#' @param dis 
#' @param ijk 
#' @param prj 
#' @param crs 
#' @param as_points 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_tibble.rmf_list <- function(obj,
                                   dis,
                                   ijk = NULL,
                                   prj = NULL,
                                   crs = NULL,
                                   as_points = TRUE) {
  
  if(!is.null(ijk)) {
    ijk_id <- rmf_convert_ijk_to_id(i = ijk$i, j = ijk$j, k = ijk$k, dis = dis, type = 'modflow')
    obj_id <-  rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis, type = 'modflow')
    obj <- subset(obj, obj_id %in% ijk_id)
  }
  
  if(as_points) {
    coords <- rmf_convert_grid_to_xyz(i = obj$i, j = obj$j, k = obj$k, dis = dis, prj = prj)
    if(!is.null(crs)) {
      if(is.null(prj)) stop('Please specify prj if crs is specified', call. = FALSE)
      coords_prj <- rmfi_convert_coordinates(coords, from = prj$projection, to = crs)
      coords$x <- coords_prj$x
      coords_y <- coords_prj$y
    }
    df <- data.frame(id = rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis))
    data <- as.data.frame(subset(obj, select = -which(colnames(obj) %in% c('i', 'j', 'k'))))
    df <- cbind(df, data, coords)
    
  } else {
    xy <- rmf_convert_grid_to_xyz(i = obj$i, j = obj$j, k = obj$k, dis = dis)
    id <- rmf_convert_ijk_to_id(i = obj$i, j = obj$j, k = obj$k, dis = dis)
    xWidth <- dis$delr[obj$j]
    yWidth <- dis$delc[obj$i]
    positions <- data.frame(id = rep(id, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4), z=rep(xy$z, each = 4))
    positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
    positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
    positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
    positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
    positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
    positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
    positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
    positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
    data <- as.data.frame(subset(obj, select = -which(colnames(obj) %in% c('i', 'j', 'k'))))
    values <- cbind(id, data)
    if(!is.null(prj)) {
      new_positions <- rmf_convert_grid_to_xyz(x=positions$x,y=positions$y,z=positions$z,prj=prj)
      positions$x <- new_positions$x
      positions$y <- new_positions$y
      positions$z <- new_positions$z
    }
    if(!is.null(crs)) {
      positions <- rmfi_convert_coordinates(positions,from=sp::CRS(prj$projection),to=crs)
    }
    df <- tibble::as_tibble(na.omit(merge(values, positions, by=c("id"))))
  }
  return(tibble::as_tibble(df))
}

#' Calculate a \code{rmf_2d_array} or \code{rmf_3d_array} from multiplier arrays, zone arrays and/or parameter values
#'
#' Given a multiplier array and/or zone array with corresponding zone numbers, calculate a \code{rmf_2d_array} or \code{rmf_3d_array}. Parameter values can be used to multiply the arrays as well.
#' 
#' @param dis dis object; used to set the dimensions of the array
#' @param layer optional numeric vector with the layer indices to which \code{mltarr, zonarr and iz} apply. Should have the same length as \code{mltarr, zonarr and iz}. Only used for flow parameter arrays. See details.
#' @param mltarr either a list of multiplier arrays or a single multiplier array. The keyword \code{"NONE"} indicates no multiplier array is present. 
#' @param zonarr either a list of the zone arrays or a single zone array. The keyword \code{"ALL"} indicates no zone array is present.
#' @param iz only read when zonarr is not \code{"ALL"}. A list where each element is a numeric vector with the zone numbers for the corresponding zone array.
#' @param parval vector of parameter values corresponding to \code{mltarr, zonarr and iz} which are multiplied with the final array. Typically used when the array represents a parameter.
#' @details if mltarr (zonarr) is a list, certain elements are allowed to be set to \code{"NONE"} (\code{"ALL"}) indicating the multiplier (zone) array is not active for this cluster.
#'          if mltarr is \code{"NULL"} and zonarr is \code{"ALL"}, all the cells in the returned array will be equal to the parameter value.
#'          Multiple multiplier arrays, zone arrays and/or parameter values can be used to calculate the final values. Cells that correspond to multiple multiplier arrays, zone arrays and/or parameter values will be summed. Resulting arrays for the same layer will also be summed.
#'          If more than 1 layer index is specified, a \code{rmf_3d_array} is returned
#' @return \code{rmf_2d_array} or \code{rmf_3d_array} with the values calculated from the multiplier and/or zone arrays.
#' @export

rmf_calculate_array <-  function(dis,
                                 layer = NULL,
                                 mltarr,
                                 zonarr, 
                                 iz = NULL,
                                 parval = 1.0) {
  
  if(is.array(mltarr)) mltarr <- list(mltarr)
  if(is.array(zonarr)) zonarr <- list(zonarr)
  nclu <- max(length(mltarr), length(zonarr), length(parval))
  if(nclu > 1) {
    if(length(parval) == 1) parval <- rep(parval, nclu)
    if(length(mltarr) == 1) mltarr <- rep(mltarr, nclu)
    if(length(zonarr) == 1) zonarr <- rep(zonarr, nclu)
  }
  
  dim <- c(dis$nrow, dis$ncol)
  
  # function to create the array
  set_parm <- function(dim, mult, zone, iz, p_value) {
    
    if(!is.array(mult)) mult <- array(1.0, dim = dim)
    
    zon_l <- array(TRUE, dim = dim)
    if(is.array(zone)) zon_l[!(zone %in% iz)] <- FALSE
    
    mult[zon_l] <- mult[zon_l]*p_value
    mult[!zon_l] <- NA
    return(mult)
  }
  
  # create the array for every cluster then sum the clusters
  arr <- lapply(1:nclu, function(i) set_parm(dim = dim, mult = mltarr[[i]], zone = zonarr[[i]], iz = iz[[i]], p_value = parval[i]))
  if(is.null(layer) || length(layer) == 1) {
    arr <- rmf_create_array(apply(abind::abind(arr, along = 3), c(1,2), sum, na.rm = TRUE))
  } else {
    # handle layers for flow arrays
    if(any(duplicated(layer)) > 0) {
      arr <- lapply(1:dis$nlay, function(i) apply(abind::abind(arr[layer == i], along = 3), c(1,2), sum, na.rm = TRUE))
    } 
    arr <- rmf_create_array(abind::abind(arr, along = 3))
  }
  
  attr(arr, 'dimnames') <- NULL
  return(arr)
}

#' Calculate layer thicknesses
#'
#' @param dis \code{RMODFLOW} dis object
#' @param collapse_cbd logical; should the thickness of an underlying confining bed be added to the overlying layer ? Defaults to FALSE.
#' @param only_layers logical; should only the thicknesses of model layers be returned ? Defaults to FALSE
#' @details The bottom layer can not have a confining bed below.
#'          When collapse_cbd is TRUE, thicknesses of confining beds are added to their respective overlying layers. The confining beds are then removed. 
#'          When only_layers is TRUE, the thicknesses of all layers and confining beds are calculated and only the layers are returned. This is useful to calculate e.g. transmissivities.
#'          By default, thicknesses of confining layers are therefore also included in the returned array.
#' @return rmf_3d_array with the layer thicknesses. If collapse_cbd = TRUE or only_layers = TRUE or if there are no confining beds present, there are \code{dis$nlay} layers. 
#'         Otherwise there are \code{dis$nlay + number of confining beds} layers
#' @export
#'
rmf_calculate_thickness <- function(dis, collapse_cbd = FALSE, only_layers = FALSE) {
  
  # nnlay is the 3th dimension of dis$botm
  nnlay <- dis$nlay + sum(dis$laycbd != 0)
  thck <- rmf_create_array(dim = c(dis$nrow, dis$ncol, ifelse(collapse_cbd, dis$nlay, nnlay)))
  thck[,,1] <- dis$top - dis$botm[,,1]
  
  # which nnlay are confining beds
  cbd <- rmfi_confining_beds(dis = dis)
  
  if(dis$nlay > 1) {
    if(collapse_cbd && any(dis$laycbd != 0)) {
      botm <- rmf_create_array(dim = c(dis$nrow, dis$ncol, dis$nlay))
      
      # botm of confining bed
      botm[,,dis$laycbd != 0] <- dis$botm[,,as.logical(cbd)]
      
      # botm of layer
      no_botm <- sort(c(which(cbd == TRUE) - 1, which(cbd == TRUE)))
      botm[,,dis$laycbd == 0] <- dis$botm[,,which(!(c(1:nnlay) %in% no_botm))]
      thck[,,1] <- dis$top - botm[,,1]
      
    } else {
      botm <- dis$botm
    }
    
    for(k in 2:ifelse(collapse_cbd, dis$nlay, nnlay)) {
      thck[,,k] <- botm[,,k-1] - botm[,,k]
    }
    
    if(only_layers && !collapse_cbd) thck <- thck[,,!cbd]
    
  }
  
  
  return(thck)
  
}

#' Get cell x, y and z coordinates from a dis object
#' 
#' @param dis dis object
#' @param include_faces logical; should face coordinates be included?
#' @return list with with cell coordinate 3d arrays
#' @rdname rmf_cell_coordinates
#' @method rmf_cell_coordinates dis
#' @export
rmf_cell_coordinates.dis <- function(dis,
                                     include_faces = FALSE) {
  if(any(dis$laycbd != 0)) warning("Quasi-3D confining beds detected. Returned z coordinates only represent numerical layers.")
  cell_coordinates <- NULL
  cell_coordinates$z <- dis$botm*NA
  cell_coordinates$z[,,1] <- (dis$top+dis$botm[,,1])/2
  
  nnlay <- dis$nlay + sum(dis$laycbd != 0)
  if(nnlay > 1) {
    for(k in 2:nnlay) {
      cell_coordinates$z[,,k] <- (dis$botm[,,(k-1)]+dis$botm[,,k])/2
    }
    # remove the confining beds
    cbd <- rmfi_confining_beds(dis)
    cell_coordinates$z <- cell_coordinates$z[,,!cbd]
    dis$botm <- dis$botm[,,!cbd]
  }
  
  cell_coordinates$x <- cell_coordinates$z*0
  cell_coordinates$y <- cell_coordinates$z*0
  cell_coordinates$y[,,] <- rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)
  cell_coordinates$x[,,] <- rep(c(cumsum(dis$delr)-dis$delr/2),each=dis$nrow)
  if(include_faces) {
    dis$delr <- array(rep(dis$delr,each=dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay))
    dis$delc <- array(rep(dis$delc,dis$ncol),dim=c(dis$nrow,dis$ncol,dis$nlay))
    cell_coordinates$lower <- dis$botm
    cell_coordinates$upper <- 2 * cell_coordinates$z - dis$botm
    cell_coordinates$left <- cell_coordinates$x - dis$delr/2
    cell_coordinates$right <- cell_coordinates$x + dis$delr/2 
    cell_coordinates$front <- cell_coordinates$y - dis$delc/2
    cell_coordinates$back <- cell_coordinates$y + dis$delc/2
  }
  return(cell_coordinates)
}

#' Get cell coordinates from a huf object
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param include_faces logical; should face coordinates be included?
#' @return 3d array with cell coordinates
#'
#' @rdname rmf_cell_coordinates
#' @method rmf_cell_coordinates huf
#' @export
rmf_cell_coordinates.huf <- function(huf,
                                     dis = NULL,
                                     include_faces = FALSE) {
  cell_coordinates <- NULL
  cell_coordinates$z <- huf$top - huf$thck/2
  class(cell_coordinates$z) <- 'rmf_3d_array'
  if(!is.null(dis)) {
    cell_coordinates$x <- cell_coordinates$z*0
    cell_coordinates$y <- cell_coordinates$z*0
    cell_coordinates$y[,,] <- rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)
    cell_coordinates$x[,,] <- rep(c(cumsum(dis$delr)-dis$delr/2),each=dis$nrow)
  }
  if(include_faces) {
    dis$delr <- array(rep(dis$delr,each=dis$nrow),dim=c(dis$nrow,dis$ncol,huf$nhuf))
    dis$delc <- array(rep(dis$delc,dis$ncol),dim=c(dis$nrow,dis$ncol,huf$nhuf))
    cell_coordinates$lower <- cell_coordinates$z - huf$thck/2
    cell_coordinates$upper <- cell_coordinates$z + huf$thck/2
    cell_coordinates$left <- cell_coordinates$x - dis$delr/2
    cell_coordinates$right <- cell_coordinates$x + dis$delr/2 
    cell_coordinates$front <- cell_coordinates$y - dis$delc/2
    cell_coordinates$back <- cell_coordinates$y + dis$delc/2
  }
  return(cell_coordinates)
}

#' Generic function to get cell coordinates
#' 
#' @rdname rmf_cell_coordinates
#' @export
rmf_cell_coordinates <- function(...) {
  UseMethod('rmf_cell_coordinates')
}

#' Get cell dimensions from a dis object
#' 
#' @param dis dis object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @param include_volume logical; should the cell volumes be included?
#' @param include_faces logical; should face areas be included?
#' @return list with cell dimension 3d arrays
#' @rdname rmf_cell_dimensions
#' @method rmf_cell_dimensions dis
#' @export
rmf_cell_dimensions.dis <- function(dis,
                                    hed = NULL,
                                    include_volume = FALSE,
                                    include_faces = FALSE) {
  cell_dimensions <- list()
  # remove the confining beds
  if(any(dis$laycbd != 0)) warning("Quasi-3D confining beds detected. Returned z/upper/lower dimensions only represent numerical layers.")
  nnlay <- dis$nlay + sum(dis$laycbd != 0)
  cbd <- rmfi_confining_beds(dis)
  if (is.null(hed) | ifelse(is.null(hed),FALSE,dim(hed)[4] == 1)) {
    cell_top <- dis$botm
    cell_top[,,1] <- dis$top
    if(nnlay > 1) {
      cell_top[,,2:nnlay] <- dis$botm[,,c(1:(nnlay-1))]
      cell_top <- cell_top[,,!cbd]
      dis$botm <- dis$botm[,,!cbd]
    }
    if(!is.null(hed)) {
      cell_top[which(cell_top > hed[,,,1])] <- hed[which(cell_top > hed[,,,1])]
    }
    cell_dimensions$z <- rmf_create_array(cell_top - dis$botm)
    cell_dimensions$x <- rmf_create_array(rep(dis$delc,dis$ncol*dis$nlay),dim=c(dis$nrow,dis$ncol,dis$nlay))
    cell_dimensions$y <- rmf_create_array(rep(dis$delr, dis$nlay, each = dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay))
    if(include_volume) cell_dimensions$volume <- rmf_create_array(with(cell_dimensions, x * y * z))
    if(include_faces) {
      cell_dimensions$front <- rmf_create_array(with(cell_dimensions, x * z))
      cell_dimensions$back <- cell_dimensions$front
      cell_dimensions$left <- rmf_create_array(with(cell_dimensions, y * z))
      cell_dimensions$right <- cell_dimensions$left
      cell_dimensions$lower <- rmf_create_array(with(cell_dimensions, x * y))
      cell_dimensions$upper <- cell_dimensions$lower
    }  
  } else {
    cell_top <- rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp)))
    cell_top[,,1,] <- dis$top
    if(nnlay > 1) {
      cell_top[,,2:nnlay,] <- dis$botm[,,c(1:(nnlay-1))]
      cell_top <- cell_top[,,!cbd]
      dis$botm <- dis$botm[,,!cbd]
    }
    cell_top[which(cell_top > hed[,,,])] <- hed[which(cell_top > hed[,,,])] 
    cell_dimensions$z <- rmf_create_array(cell_top - dis$botm)
    cell_dimensions$x <- rmf_create_array(rep(dis$delc,dis$ncol*dis$nlay),dim=c(dis$nrow,dis$ncol,dis$nlay))
    cell_dimensions$y <- rmf_create_array(rep(dis$delr, dis$nlay, each = dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay))
    if(include_volume) cell_dimensions$volume <- rmf_create_array(with(cell_dimensions, x * y * z))
    if(include_faces) {
      cell_dimensions$front <- rmf_create_array(with(cell_dimensions, x * z))
      cell_dimensions$back <- cell_dimensions$front
      cell_dimensions$left <- rmf_create_array(with(cell_dimensions, y * z))
      cell_dimensions$right <- cell_dimensions$left
      cell_dimensions$lower <- rmf_create_array(with(cell_dimensions, x * y))
      cell_dimensions$upper <- cell_dimensions$lower
    }
  }
  return(cell_dimensions)
}

#' Get cell dimensions from a huf object
#' 
#' @param huf huf object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @return list with cell dimension 3d arrays
#'
#' @rdname rmf_cell_dimensions
#' @method rmf_cell_dimensions huf
#' @export
rmf_cell_dimensions.huf <- function(huf,
                                    dis = NULL,
                                    hed = NULL,
                                    include_volume = FALSE,
                                    include_faces = FALSE) {
  cell_dimensions <- list()
  huf_top <- huf$top
  for(k in 2:huf$nhuf) huf_top[,,k] <- huf_top[,,(k-1)] - huf$thck[,,k-1]
  huf_thickness <- huf$thck
  huf_bottom <- huf_top
  huf_bottom[,,1:(huf$nhuf-1)] <- huf_top[,,2:huf$nhuf]
  huf_bottom[,,huf$nhuf] <- huf_top[,,huf$nhuf] - huf_thickness[,,huf$nhuf]
  if(!is.null(hed)) {
    huf_top[which(huf_top > hed)] <- hed[which(huf_top > hed)]
  }
  cell_dimensions$z <- huf_top - huf_bottom
  if(!is.null(dis)) {
    cell_dimensions$x <- cell_dimensions$y <- cell_dimensions$z
    dis_cell_dimensions <- cell_dimensions(dis)
    cell_dimensions$x[,,] <- dis_cell_dimensions$x[,,1]
    cell_dimensions$y[,,] <- dis_cell_dimensions$y[,,1]
  }
  if(include_volume) cell_dimensions$volume <- structure(with(cell_dimensions, x * y * z), class = 'rmf_3d_array')
  if(include_faces) {
    cell_dimensions$front <- structure(with(cell_dimensions, x * z), class = 'rmf_3d_array')
    cell_dimensions$back <- cell_dimensions$front
    cell_dimensions$left <- structure(with(cell_dimensions, y * z), class = 'rmf_3d_array')
    cell_dimensions$right <- cell_dimensions$left
    cell_dimensions$lower <- structure(with(cell_dimensions, x * y), class = 'rmf_3d_array')
    cell_dimensions$upper <- cell_dimensions$lower
  }  
  return(cell_dimensions)
}

#' Generic function to get cell dimensions
#' 
#' @rdname rmf_cell_dimensions
#' @export
rmf_cell_dimensions <- function(...) {
  UseMethod('rmf_cell_dimensions')
}

#' Get information from a dis object at a certain grid cell
#' 
#' @param dis a discretization file object
#' @param i row number
#' @param j column number
#' @return \code{NULL}
#'
#' @rdname rmf_cell_info
#' @method rmf_cell_info dis
#' @export
rmf_cell_info.dis <- function(dis,
                              i,
                              j) {
  cat('Column width = ',dis$delr[j], '\n')
  cat('Row width = ', dis$delc[i], '\n')
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  df <- data.frame('Top' = dis$top[i,j], 'Botom' = dis$botm[i,j,1], 'Thickness' = dis$top[i, j]-dis$botm[i,j,1])
  rows <- 'Layer 1'
  
  nnlay <- dis$nlay + sum(dis$laycbd != 0)
  if(nnlay > 1) {
    cbd <- rep(0, nnlay)
    cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
    for(k in 2:nnlay)
    {
      df[k, ] <- c(dis$botm[i, j, k-1], dis$botm[i, j, k], dis$botm[i, j, k-1] - dis$botm[i, j, k])
      if(cbd[k]) {
        rows <- c(rows, paste('Quasi-3D Confining Bed below layer ',k-1))
      } else {
        rows <- c(rows, paste('Layer', k))
      }
    }
  }
  row.names(df) <- rows
  print(df)
  
}

#' Get information from a huf object at a certain grid cell
#' 
#' @param huf a hydrogeologic unit file object
#' @param i row number
#' @param j column number
#' @return \code{NULL}
#'
#' @rdname rmf_cell_info
#' @method rmf_cell_info huf
#' @export
rmf_cell_info.huf <- function(huf,
                              i,
                              j) {
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  df <- data.frame('Name' = huf$hgunam[1], 'Top' = huf$top[i,j,k], 'Botom' = huf$top[i, j, k]-huf$thck[i, j, k], 'Thickness' =  huf$thck[i, j, k], stringsAsFactors = FALSE)
  rows <- 'Unit 1'
  
  if(huf$nhuf > 1) {
    for(k in 2:huf$nhuf)
    {
      df[k,] <- c(huf$hgunam[k], huf$top[i,j,k], huf$top[i,j,k]-huf$thck[i,j,k], huf$thck[i,j,k])
      rows <- c(rows, paste('Unit', k))
    }
  }
  row.names(df) <- rows
  print(df)

}

#' Generic function to get information at a certain grid cell
#' 
#' @rdname rmf_cell_info
#' @export
rmf_cell_info <- function(...) {
  UseMethod('rmf_cell_info')
}

#' Convert a bcf to a lpf object
#'
#' @param bcf \code{RMODFLOW} bcf object
#' @param dis \code{RMODFLOW} dis object
#' @param storagecoefficient logical; should STORAGECOEFFICIENT keyword be included?; defaults to FALSE
#' @param constantcv logical; should CONSTANTCV keyword be included?; defaults to FALSE
#' @param thickstrt logical; should THICKSTRT keyword be included?; defaults to FALSE
#' @param nocvcorrection logical; should NOCVCORRECTION keyword be included?; defaults to FALSE
#' @param novfc logical; should NOVFC keyword be included?; defaults to FALSE
#' @param noparcheck logical; should NOPARCHECK keyword be included?; defaults to FALSE
#' @param ss_value numeric; value of specific storage for layers with laycon == 1. Defaults to 1e-5. See details
#' @param wetdry_value numeric; value of wetdry for layer with laycon == 2. Defaults to -0.01. See details.
#' @param vk_bot 2d array with the vertical hydraulic conductivity values in the bottom layer. Defaults to NULL. See details.
#' @details Since vcont is not set for the bottom layer, the vertical conductivity in the bottom layer is assumed to be the same as hk unless a vk_bot argument is supplied.
#'          This affects the calculations of vk in the overlying layers as well.
#'          
#'          When confining beds are present, both vkcb and vk are unknowns. Vkcb is therefore set equal to vk and a warning is thrown.
#'          
#'          Layers with laycon = 1 in BCF should represent the upper layer of the model and have unconfined conditions. In this case, BCF needs only a specific yield value.
#'          When converting to LPF however, a specific storage value is also needed and can not be determined from the bcf object. The \code{ss_value} argument specifies this 
#'          specific storage value.
#'          
#'          Layers with laycon = 2 are converted to convertible layer types in LPF. In BCF, these layers do not have wetdry variables specified. In LPF, convertible layers can have 
#'          wetdry variables specified. The \code{wetdry_value} argument sets the wetdry values for layers with laycon = 2.
#' @return object of class lpf
#' @export
#' 
#' @seealso \code{\link{rmf_create_lpf}}, \code{\link{rmf_create_bcf}}, \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}
rmf_convert_bcf_to_lpf <- function(bcf,
                                   dis,
                                   storagecoefficient = FALSE,
                                   constantcv = FALSE,
                                   thickstrt = FALSE,
                                   nocvcorrection = FALSE,
                                   novfc = FALSE,
                                   noparcheck = FALSE,
                                   ss_value = 1e-5,
                                   wetdry_value = -0.01,
                                   vk_bot = NULL) {
  
  # data set 1
  names(bcf) <- replace(names(bcf), which(names(bcf) == 'ibcfcb'), 'ilpfcb')
  bcf$nplpf <- 0
  
  # options
  bcf$storagecoefficient <- storagecoefficient
  bcf$constantcv <- constantcv
  bcf$thickstrt <- thickstrt
  bcf$nocvcorrection <- nocvcorrection
  bcf$novfc <- novfc
  bcf$noparcheck <- noparcheck
  
  # laytyp
  bcf$laytyp <- as.numeric(bcf$laycon != 0)
  laycon <- bcf$laycon
  bcf$laycon <- NULL
  
  # horizontal anisotropy
  names(bcf) <- replace(names(bcf), which(names(bcf) == 'trpy'), 'chani')
  
  # vertical anisotropy
  bcf$layvka <- rep(0, dis$nlay)
  
  # wetting
  bcf$laywet <- rep(bcf$iwdflg, dis$nlay)
  bcf$iwdflg <- NULL
  if(all(bcf$laywet == 0)) bcf$wetfct <- bcf$iwetit <- bcf$ihdwet <- NULL
  
  # get thickness
  thck <- rmf_calculate_thickness(dis = dis, only_layers = TRUE)
  
  # hk
  bcf$hk <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(c(0,2) %in% laycon)) {
    layer_id <- which(laycon %in% c(0,2))
    bcf$hk[,,layer_id] <- bcf$tran[,,layer_id]/thck[,,layer_id]
    bcf$tran <- NULL
  } 
  if(any(c(1,3) %in% laycon)) {
    layer_id <- which(laycon %in% c(1,3))
    bcf$hk[,,layer_id] <- bcf$hy[,,layer_id]
    bcf$hy <- NULL
  }
  
  # vk
  bcf$vka <- rmf_create_array(dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(is.null(vk_bot)) vk_bot <- bcf$hk[,,dis$nlay]
  bcf$vka[,,dis$nlay] <- vk_bot
  
  if(dis$nlay > 1) {
    if(any(dis$laycbd != 0)) {
      
      # vkcb
      warning('vkcb will be set equal to vk', call. = FALSE)
      thck <- rmf_calculate_thickness(dis = dis, collapse_cbd = TRUE)
      for(k in (dis$nlay - 1):1) {
        bcf$vka[,,k] <- (0.5*thck[,,k])/((1/bcf$vcont[,,k]) - (0.5*thck[,,k+1]/bcf$vka[,,k+1]))
      }
      thck <- rmf_calculate_thickness(dis = dis, only_layers = TRUE)
      bcf$vkcb <- bcf$vka
      
    } else {
      for(k in (dis$nlay - 1):1) {
        bcf$vka[,,k] <- (0.5*thck[,,k])/((1/bcf$vcont[,,k]) - (0.5*thck[,,k+1]/bcf$vka[,,k+1]))
      }
    }
  } 
  bcf$vcont <- NULL
  
  if('TR' %in% dis$sstr) {
    # ss
    bcf$ss <- rmf_create_array(dim = c(dis$nrow, dis$ncol, dis$nlay))
    if(any(laycon == 0)) bcf$ss[,,which(laycon == 0)] <- bcf$sf1[,,which(laycon == 0)]/thck[,,which(laycon == 0)]
    if(any(laycon == 1)) bcf$ss[,,which(laycon == 1)] <- ss_value
    if(any(laycon %in% c(2,3))) bcf$ss[,,which(laycon %in% c(2,3))] <- bcf$sf1[,,which(laycon %in% c(2,3))]/thck[,,which(laycon %in% c(2,3))]
    
    
    # sy
    if(any(bcf$laytyp != 0)) {
      bcf$sy <- rmf_create_array(dim = c(dis$nrow, dis$ncol, dis$nlay))
      if(any(laycon == 1)) bcf$sy[,,which(laycon == 1)] <- bcf$sf1[,,which(laycon == 1)]
      if(any(laycon %in% c(2,3))) bcf$sy[,,which(laycon %in% c(2,3))] <- bcf$sf2[,,which(laycon %in% c(2,3))]
    }
    
    bcf$sf1 <- bcf$sf2 <- NULL
  }
  
  # wetdry
  if(any(bcf$laywet > 0) && any(laycon == 2) && !is.null(wetdry)) {
    bcf$wetdry[,,which(laycon == 2)] <- wetdry_value
  }
  
  class(bcf) <- replace(class(bcf), which(class(bcf) == 'bcf'), 'lpf')
  comment(bcf) <- c(comment(bcf), 'LPF object converted from BCF object')
  return(bcf)
}

#' Convert cbc object fluxes to darcy velocities
#' 
#' @param cbc \code{RMODFLOW} cbc object
#' @param dis \code{RMODFLOW} dis object
#' @param hed \code{RMODFLOW} hed object; optional; if specified, the saturated cell thickness is used
#' @param porosity optional 3d array with porosity values. If used, the groundwater velocities are returned.
#' @return list of 4d arrays: right, front, lower, left, back, upper, qx, qy, qz and q; all represent darcy velocities (or groundwater velocities if porosity is specified): the first six at the different cell faces, the last four represent the components and magnitude at the cell center
#' @export
rmf_convert_cbc_to_darcy <- function(cbc,
                                     dis,
                                     hed = NULL,
                                     porosity = NULL) {
  
  # check if all flow components are present
  flow <- c('flow_front_face', 'flow_right_face', 'flow_lower_face')
  flow <- flow[which(flow %in% names(cbc))[1]]
  if(!"flow_front_face" %in% names(cbc)) cbc$flow_front_face <- cbc[[flow]] * 0
  if(!"flow_right_face" %in% names(cbc)) cbc$flow_right_face <- cbc[[flow]] * 0
  if(!"flow_lower_face" %in% names(cbc)) cbc$flow_lower_face <- cbc[[flow]] * 0
  
  thck <- rmf_create_array(rmf_cell_dimensions(dis = dis, hed = hed)$z,dim=dim(cbc$flow_right_face))
  delc <- rmf_create_array(rep(dis$delc,dis$ncol),dim=dim(cbc$flow_right_face))
  delr <- rmf_create_array(rep(dis$delr,each=dis$nrow),dim=dim(cbc$flow_right_face))
  darcy <- list()

  darcy$right <- cbc$flow_right_face
  darcy$front <- -cbc$flow_front_face
  darcy$lower <- -cbc$flow_lower_face/delc/delr
  darcy$left <- darcy$back <- darcy$upper <- darcy$right * 0
  if(dis$ncol > 1) darcy$left[,c(2:dis$ncol),,] <- darcy$right[,c(1:(dis$ncol-1)),,] else darcy$left <- darcy$right * 0
  if(dis$nrow > 1) darcy$back[c(2:dis$nrow),,,] <- darcy$front[c(1:(dis$nrow-1)),,,] else darcy$back <- darcy$front * 0
  if(dis$nlay > 1) darcy$upper[,,c(2:dis$nlay),] <- darcy$lower[,,c(1:(dis$nlay-1)),] else darcy$upper <- darcy$lower * 0
  darcy$right <- darcy$right/delc/thck
  darcy$left <- darcy$left/delc/thck
  darcy$front <- darcy$front/delr/thck
  darcy$back <- darcy$back/delr/thck
  
  # TODO: other fluxes
  for (kper in 1:dis$nper) {
    for (kstp in 1:dis$nstp[kper]) {
      step <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper -1 ] + kstp)
      if ('recharge' %in% names(cbc)) {
         # TODO check if sum is correct
         darcy$upper[,,,step] <- darcy$upper[,,,step] - cbc$recharge[,,,step]/delc[,,,1]/delr[,,,1]
      }
      if('drains' %in% names(cbc)) {
        drains <- rmf_as_array(subset(cbc$drains, nstp == step), dis = dis, select = 'flow', sparse = FALSE)
        darcy$upper[,,,step] <- darcy$upper[,,,step] - drains[,,,step]/delc[,,,1]/delr[,,,1]
      }
      if('river_leakage' %in% names(cbc)) {
        river <- rmf_as_array(subset(cbc$river_leakage, nstp == step), dis = dis, select = 'flow', sparse = FALSE)
        darcy$upper[,,,step] <- darcy$upper[,,,step] - river[,,,step]/delc[,,,1]/delr[,,,1]
      }
    }
  }
  
  if(is.null(porosity)) {
    porosity <- 1
  } else {
    porosity <- rmf_create_array(porosity, dim = dim(cbc[[flow]]))
  }
  
  darcy$qx <- (darcy$right + darcy$left) / (2 * porosity)
  darcy$qy <- (darcy$front + darcy$back) / (2 * porosity)
  darcy$qz <- (darcy$lower + darcy$upper) / (2 * porosity)
  darcy$q <- sqrt((darcy$qx)^2 + (darcy$qy)^2 + (darcy$qz)^2)
  # drop unnecessary attributes
  #darcy <- lapply(darcy, function(i) rmf_create_array(array(i, dim = dim(cbc[[flow]]))))
  return(darcy)
}

#' Convert a dis object to correspond to the saturated volume
#' 
#' @param dis dis object
#' @param hed hed object
#' @return dis object
#' @export
rmf_convert_dis_to_saturated_dis <- function(dis,
                                             hed, 
                                             l = NULL) {
  if(length(dim(hed))==4) {
    if(!is.null(l)) {
      hed <- hed[,,,l]
    } else {
      warning('Using final stress period heads to determine saturated part of grid.', call. = FALSE)
      hed <- hed[,,,dim(hed)[4]]
    }
  }
  # adjusting confining beds  - REVIEW required
  nnlay <- dis$nlay + sum(dis$laycbd != 0)
  cbd <- rmfi_confining_beds(dis)
  if(nnlay > 1) {
    if(any(dis$laycbd != 0)) warning('Quasi-3D confining beds detected. Not taking them into account for the calculation of saturated dis.')
    thck <- botm <- rmf_calculate_thickness(dis)
    dis$botm <- dis$botm[,,!cbd]
    dis$botm[,,1:(dis$nlay-1)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])] <- hed[,,2:(dis$nlay)][which(hed[,,2:(dis$nlay)] < dis$botm[,,1:(dis$nlay-1)])]
    botm[,,!cbd] <- dis$botm
    if (1 %in% cbd) botm[,,which(cbd == 1)] <- botm[,,which(cbd == 1)-1] - thck[,,which(cbd == 1)]
    dis$botm <- botm
  } 
  dis$top <- rmf_create_array(c(hed[,,1]), dim = c(dis$nrow, dis$ncol))
  return(dis)
}

#' Convert modflow coordinates to real world coordinates
#' 
#' @param x modflow x coordinate
#' @param y modflow y coordinate
#' @param z modflow z coordinate
#' @param i modflow row number
#' @param j modflow column number
#' @param k modflow layer number
#' @param roff modflow row offset
#' @param coff modflow column offset
#' @param loff modflow layer offset
#' @param prj prj object
#' @param dis dis object
#' @details Provide either xyz or ijk
#'   If xyz is provided, it is reprojected using the optional prj object.
#'   
#'   Optional roff, coff and loff values can be supplied along the i, j and k indices.
#'   
#'   k indices should not represent Quasi-3D confining beds. If a real world coordinate should be obtained from a Quasi-3D confining bed,
#'   set the k index to the overlying model layer and supply a loff value (relative to the thickness of the overlying model layer)
#'   
#' @return data frame with real world x and y (and optionally z) coordinates
#' @export
rmf_convert_grid_to_xyz <- function(x = NULL,
                                    y = NULL,
                                    z = NULL,
                                    i = NULL,
                                    j = NULL,
                                    k = NULL,
                                    roff = NULL,
                                    coff = NULL,
                                    loff = NULL,
                                    prj = NULL,
                                    dis = NULL) {
  if(!is.null(x)) {
    if(!is.null(prj)) {
      if(length(prj$origin) <= 2) prj$origin <-  c(prj$origin, 0)
      # counterclockwise rotation is negative. Now corresponds to rmf_convert_xyz_to_grid
      angle <- atan(y/x)*180/pi+prj$rotation
      angle[which(is.na(angle))] <- 90-prj$rotation
      s <- sqrt(x^2+y^2)
      x <- prj$origin[1]+ cos(angle*pi/180)*s
      y <- prj$origin[2]+ sin(angle*pi/180)*s
      if(!is.null(z)) z <- prj$origin[3]+z
    }
    ifelse(!is.null(z),return(data.frame(x=x,y=y,z=z)),return(data.frame(x=x,y=y)))
    
  } else if(!is.null(i)) {
    if(is.null(dis)) stop('Please provide a dis object', call. = FALSE)
    y_grid <- c(cumsum(rev(dis$delc))-rev(dis$delc)/2)[(dis$nrow-i+1)]
    x_grid <- c(cumsum(dis$delr)-dis$delr/2)[j]
    
    if(!is.null(k)) {
      
      # set thicknesses of cells
      if(any(k > dis$nlay)) stop('k is greater than dis$nlay', call. = FALSE)
      thck <- rmf_calculate_thickness(dis)
      nnlay <- dis$nlay + sum(dis$laycbd != 0)

      #  vectorize this:
      
      # adjust botm for presence of confining beds
      df <- data.frame(i=i,j=j,k=k)
      if(!is.null(loff)) df$loff <- loff
      cbd <- rmfi_confining_beds(dis)
      if(any(dis$laycbd != 0)) warning('Quasi-3D confining beds detected. Not taking them into account for the calculation of the z coordinate.')
      if(nnlay > 1) {
        thck_nocbd <- thck[,,!cbd]
      } else {
        thck_nocbd <- thck
      }
      
      for(x in 1:nrow(df)) {
        k_adj <- ifelse(df$k[x] == 1, df$k[x], df$k[x] + sum((dis$laycbd != 0)[1:(df$k[x]-1)]))
        cell_thchk <- sum(thck[df$i[x],df$j[x],k_adj:nnlay])
        # calculate z of cell center; normalize with bottom of model (which can never be a confined bed)
        df$z[x] <- cell_thchk-thck_nocbd[df$i[x],df$j[x],df$k[x]]/2 + dis$botm[df$i[x],df$j[x],nnlay]
        if(!is.null(loff)) df$z[x] <- df$z[x] - df$loff[x]*thck[df$i[x],df$j[x],df$k[x]]
        
      }
      z_grid <- df$z
      rm(df)
    }
    
    # offset
    if(!is.null(roff)) y_grid <- y_grid - roff*dis$delc[i]
    if(!is.null(coff)) x_grid <- x_grid + coff*dis$delr[j]
    
    x <- x_grid
    y <- y_grid
    if(!is.null(k)) z <- z_grid
    
    if(!is.null(prj)) {
      if(length(prj$origin) <= 2) prj$origin <- c(prj$origin, 0)
      s <- sqrt(x_grid^2+y_grid^2)
      angle <- asin(y_grid/s)*180/pi + prj$rotation
      x_grid <- cos(angle*pi/180)*s
      y_grid <- sin(angle*pi/180)*s
      x <- prj$origin[1] + x_grid
      y <- prj$origin[2] + y_grid
      if(!is.null(k)) {
        z <- prj$origin[3] + z_grid
      }
    }
    
    dat <- data.frame(x=x,y=y)
    if(!is.null(k)) dat <- data.frame(x=x,y=y,z=z)
    return(dat)
  }
}                                                                           

#' Convert a hob object to a time series data frame
#' 
#' @param hob hob object
#' @param dis dis object
#' @param prj prj object
#' @return time series data frame containing name, time and head columns
#' @export
rmf_convert_hob_to_time_series <- function(hob,
                                           dis,
                                           prj) {
  toffset <- lubridate::days(ifelse(hob$irefsp==1,0,cumsum(dis$perlen)[hob$irefsp-1]) + hob$toffset * hob$tomulth)
  time_series <- data.frame(name = hob$obsloc, time = prj$starttime + toffset, head = hob$hobs)
  return(time_series)
}

#' Convert a huf to a dis object (for plotting)
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @return dis object containing the layers of the huf object
#' @export
rmf_convert_huf_to_dis <- function(huf,
                                   dis) {

  new_dis <- dis
  new_dis$nlay <- huf$nhuf
  new_dis$top <- huf$top[,,1]
  new_dis$botm <- huf$top - huf$thck
  
  new_dis$top[which(new_dis$top > dis$top)] <- dis$top[which(new_dis$top > dis$top)]
  if(huf$nhuf > 1) {
    for(k in 1:huf$nhuf) {
      new_dis$botm[,,k][which(new_dis$botm[,,k] > dis$top)] <- dis$top[which(new_dis$botm[,,k] > dis$top)]
      new_dis$botm[,,k][which(new_dis$botm[,,k] < dis$botm[,,dis$nlay])] <- dis$botm[,,dis$nlay][which(new_dis$botm[,,k] < dis$botm[,,dis$nlay])]
    }
  }

  new_dis$botm <- rmf_create_array(new_dis$botm)
  new_dis$top <- rmf_create_array(new_dis$top)
  return(new_dis)
}

#' Convert a parameter defined on the HUF grid to the numerical grid
#' 
#' @param huf huf object
#' @param dis dis object
#' @param parameters either a list of huf parameters. Defaults to the parameters of the supplied \code{huf} object.
#' @param values vector of parameter values of length \code{nhuf}, in the order of \code{hgunam}; overwrites \code{parameters}. All values should typically represent the same parameter type.
#' @param grid target grid; either \code{'dis'} (default) or \code{'huf'}. When \code{'huf'}, no averaging is performed
#' @param mask masking 3d array for averaging, typically the \code{ibound} array, to speed up grid conversion; defaults to including all cells
#' @param type type of averaging that should be performed when \code{grid == 'dis'}; either arithmetic (default), harmonic or geometric
#' @param partyp which parameter type to convert; used to subset \code{parameters}. Possible values are \code{'HK' (default), 'HANI', 'VK', 'VANI', 'SS', 'SY', 'SYTP'}. Only used with \code{parameters}. Defaults to 'arithmetic' expect when partyp = 'VK' ('harmonic').
#' @param pvl optional \code{RMODFLOW} pvl object. Used to overwrite the parval attributes if \code{parameters} is supplied
#' @return rmf_3d_array with the parameter values. Dimensions are \code{dis$nrow, dis$ncol, dis$nlay} when \code{grid == 'dis'} or \code{dis$nrow, dis$ncol, huf$nhuf} when \code{grid == 'huf'} 
#' @details Either \code{parameters} or \code{values} should be supplied. The former is used for more complex parametrizations including multiplier and/or zone arrays.
#'      The latter is used when a single parameter value of for each unit is sufficient. When \code{values} is used, all values typically represent the same parameter type, e.g. \code{'HK'}. 
#'      Similarly, when \code{parameters} is used, only one value for \code{partyp} should be supplied.
#' @export
rmf_convert_huf_to_grid <- function(huf,
                                    dis,
                                    parameters = huf$parameters,
                                    values = NULL,
                                    grid = 'dis',
                                    mask = rmf_create_array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                                    type = ifelse(partyp == 'VK', 'harmonic', 'arithmetic'),
                                    partyp = 'HK',
                                    pvl = NULL) {

  # if parameters is supplied
  if(is.null(values)) {
    parm_type <- vapply(parameters, function(i) attr(i, 'partyp'), 'HK')
    
    # error check
    if(partyp == "VK" && all(huf$hguvani > 0)) stop('No VK specified in huf object through parameters or HGUVANI values', call. = FALSE)
    if(partyp == "VANI" && all(huf$hguvani == 0)) stop('No VANI specified in huf object through parameters or HGUVANI values', call. = FALSE)
    if(partyp == "HANI" && all(huf$hguhani == 0) && !("HANI" %in% parm_type)) stop('No HANI specified in huf object through parameters or HGUHANI values', call. = FALSE)
    if(partyp %in% c("HK", "SS", "SY", "SYTP") && !(partyp %in% parm_type)) stop(paste('No', partyp, 'parameters specified in huf object'), call. = FALSE)
    
    parameters <- parameters[parm_type == partyp]
    hgunam <- unique(unlist(lapply(parameters, function(i) attr(i, 'hgunam'))))
    hgunam <- which(huf$hgunam %in% hgunam)
    
    # replace parval if value is in pvl object
    if(!is.null(pvl) && length(parameters) > 0) {
      
      replace_parval <- function(parameter) {
        if(attr(parameter, 'parnam') %in% pvl$parnam) {
          old_parval <- attr(parameter, 'parval')
          new_parval <- pvl$parval[which(pvl$parnam == attr(parameter, 'parnam'))]
          parameter <- (parameter/old_parval)*new_parval
          attr(parameter, 'parval') <- new_parval
          return(parameter)
        } else {
          return(parameter)
        }
      }
      
      parameters <- lapply(parameters, replace_parval)
    }
    
    # get a single array for each hgu
    get_array <- function(hgu) {
      parm_index <- vapply(parameters, function(i) ifelse(hgu %in% attr(i, 'hgunam'), which(attr(i, 'hgunam') == hgu), 0), 1)
      hgu_array <- lapply(seq_along(parm_index), function(i) if(parm_index[i] > 0) rmfi_ifelse0(length(dim(parameters[[i]])) > 2, parameters[[i]][,,parm_index[i]], parameters[[i]] ))
      hgu_array <- abind::abind(hgu_array, along = 3)
      if(!is.null(hgu_array)) hgu_array <- apply(hgu_array, c(1,2), FUN = sum, na.rm = TRUE)
      return(hgu_array)
    }
    
    # if partyp is SYTP return a single array which is hgu independent
    if(partyp == 'SYTP') {
      hgu_array <- unlist(lapply('SYTP', get_array))
      return(rmf_create_array(hgu_array, dim = c(dis$nrow, dis$ncol, 1)))
      
    } else {
      
      hgu_array <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol, huf$nhuf))
      if(length(hgunam) > 0) hgu_array[,,hgunam] <- unlist(lapply(huf$hgunam, get_array))
      
      # if partyp is HANI, VANI and not everything is supplied by parameters: get information from HGUHANI/HGUVANI
      hgu_todo <- c(1:huf$nhuf)[-hgunam]
      if(length(hgu_todo) > 0) {
        if(partyp == 'HANI') {
          # hgu_todo <- which(huf$hguhani > 0 && (c(1:huf$nhuf) %in% hgu_todo))
          hgu_todo <- which(huf$hguhani > 0)
          hgu_array[,,hgu_todo] <- rep(abs(huf$hguhani[hgu_todo]), each = dis$nrow, dis$ncol)
        }
        if(partyp == 'VANI') {
          hgu_todo <- which(huf$hguvani > 0 && (c(1:huf$nhuf) %in% hgu_todo))
          hgu_array[,,hgu_todo] <- rep(huf$hguvani[hgu_todo], each = dis$nrow, dis$ncol)
        }
      }
    }
  } 
  
  if(grid == 'huf') {
    return(rmf_create_array(hgu_array, dim = c(dis$nrow, dis$ncol, huf$nhuf)))
    
  } else if(grid == 'dis') {
    
    # averaging
    if(any(dis$laycbd != 0)) warning('Using Quasi-3D confining beds as explicit layers')
    num_grid_array <- dis$botm*NA
    huf$botm <- huf$thck*NA
    huf$botm[,,1] <- huf$top[,,1]-huf$thck[,,1]
    if(huf$nhuf > 1) {
      for(k in 2:dim(huf$thck)[3]) {
        huf$botm[,,k] <- huf$botm[,,k-1]-huf$thck[,,k]
      } 
    }
    dis$top <- array(c(dis$top,dis$botm[,,1:(dis$nlay-1)]),dim=c(dis$nrow,dis$ncol,dis$nlay))
    
    i <- rep(1:dis$nrow,dis$ncol*dis$nlay)
    j <- rep(rep(1:dis$ncol,each=dis$nrow),dis$nlay)
    k <- rep(1:dis$nlay,each=dis$nrow*dis$ncol)
    num_grid_array[which(mask==0)] <- 0
    
    get_weighted_mean <- function(cell) {
      iCell <- i[cell]
      jCell <- j[cell]
      kCell <- k[cell]
      cell_top <- dis$top[iCell,jCell,kCell]
      cell_botm <- dis$botm[iCell,jCell,kCell]
      thck <- pmin(huf$top[iCell,jCell,],cell_top) - pmax(huf$botm[iCell,jCell,],cell_botm)
      thck[which(thck < 0)] <- 0
      
      if(is.null(values)) values <- hgu_array[iCell,jCell,]
      
      if(type=='arithmetic') return(weighted.mean(values,thck))
      if(type=='harmonic') return(rmfi_weighted_harmean(values,thck))
      if(type=='geometric') return(rmfi_weighted_geomean(values,thck))
    }
    # TODO speed up
    weighted_means <- vapply(which(mask!=0),get_weighted_mean, 1.0)
    num_grid_array[which(mask!=0)] <- weighted_means
    num_grid_array <- rmf_create_array(num_grid_array, dim=c(dis$nrow,dis$ncol,dis$nlay))
    return(num_grid_array)
  }
}

#' Convert a huf to a lpf object
#'
#' @param huf \code{RMODFLOW} huf object
#' @param dis \code{RMODFLOW} dis object
#' @param mask masking 3d array for averaging \code{\link{rmf_convert_huf_to_grid}}, typically the \code{ibound} array, to speed up grid conversion; defaults to including all cells
#' @param vka character indicating what variable the VKA array in the resulting lpf object represents. Possible values are \code{'VK'} or \code{'VANI'}. If all HGUVANI values are the same, the default vka is set correspondingly. If HGUVANI varies between hgu's, the default vka is \code{'VANI'}.
#' @param averaging named character vector of weighted averaging to use in \code{\link{rmf_convert_huf_to_grid}}. Possible values are 'arithmetic', 'harmonic' and 'geometric'. Names should correspond to the \code{partyp} defined in the huf object. Defaults to 'arithmetic' for every parameter type except for 'VK' ('harmonic').
#' @param pvl optional \code{RMODFLOW} pvl object; used to overwrite huf parameter values in \code{\link{rmf_convert_huf_to_grid}}. Defaults to NULL.
#' @param ... arguments passed to \code{\link{rmf_create_lpf}}
#' @return a \code{RMODFLOW} lpf object
#' @details Huf parameters are converted to non-parameter data averaged over the \code{dis} grid using \code{\link{rmf_convert_huf_to_grid}}.
#' The resulting lpf object will therefore not have any flow parameters.
#' If HGUVANI varies per hgu, a correct conversion to a lpf vka array is not possible (i.e. because part of the layer might represent VANI from hgu A and another part VK from hgu B). Therefore, the user has to decide whether vka represents vk or vani in its entirety. 
#' @export
#' @seealso \code{\link{rmf_convert_huf_to_grid}}, \code{\link{rmf_create_lpf}}
#'
rmf_convert_huf_to_lpf <- function(huf, 
                                   dis, 
                                   mask = NULL,
                                   vka = ifelse(all(huf$hguvani == 0), 'VK', 'VANI'),
                                   averaging = c(HK = 'arithmetic', HANI = 'arithmetic', VK = 'harmonic', VANI = 'arithmetic', SS = 'arithmetic', SY = 'arithmetic'),
                                   pvl = NULL,
                                   ...) {
  
    if(is.null(mask)) mask <- rmf_create_array(1, dim = c(dis$nrow, dis$ncol, dis$nlay))

    hk <- rmf_convert_huf_to_grid(huf = huf, dis = dis, mask = mask, partyp = 'HK', type = averaging['HK'], pvl = pvl)
    hani <- rmf_convert_huf_to_grid(huf = huf, dis = dis, mask = mask, partyp = 'HANI', type = averaging['HANI'], pvl = pvl)

    vk <- vani <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay))
    if(any(huf$hguvani == 0)) vk <- rmf_convert_huf_to_grid(huf = huf, dis = dis, mask = mask, partyp = 'VK', type = averaging['VK'], pvl = pvl)
    if(any(huf$hguvani > 0)) vani <- rmf_convert_huf_to_grid(huf = huf, dis = dis, mask = mask, partyp = 'VANI', type = averaging['VANI'], pvl = pvl)
    if(vka == 'VK') {
      vani <- hk/vani
    } else if(vka == 'VANI') {
      vk <- hk/vk
    }
    vk[which(is.na(vk))] <- 0
    vani[which(is.na(vani))] <- 0
    vka_array <- vk + vani
    
    
    ss <- sy <- NULL
    if('TR' %in% dis$sstr) {
      ss <- rmf_convert_huf_to_grid(huf = huf, dis = dis, mask = mask, partyp = 'SS', type = averaging['SS'], pvl = pvl)
      if(any(huf$lthuf != 0)) sy <- rmf_convert_huf_to_grid(huf = huf, dis = dis, mask = mask, partyp = 'SY', type = averaging['SY'], pvl = pvl)
    } 
    
    lpf <- rmf_create_lpf(dis = dis, 
                          ilpfcb = huf$ihufcb, 
                          hdry = huf$hdry, 
                          laytyp = huf$lthuf,
                          chani = rep(0, dis$nlay), 
                          layvka = rmfi_ifelse0(vka == 'VK', rep(0, dis$nlay), rep(1, dis$nlay)),
                          laywet = huf$laywt,
                          wetfct = huf$wetfct,
                          iwetit = huf$iwetit,
                          ihdwet = huf$ihdwet,
                          parameters = NULL,
                          hk = hk,
                          hani = hani,
                          vka = vka_array,
                          ss = ss,
                          sy = sy,
                          wetdry = huf$wetdry,
                          ...)
    
    comment(lpf) <- c(comment(lpf), 'LPF object converted from HUF object')
    return(lpf)
}

#' Convert a huf to a mask object
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param bas bas object, corresponding to the huf object; defaults to NULL
#' @return mask rmf_3d_array
#' @export
rmf_convert_huf_to_mask <- function(huf, dis, bas = NULL) {
  mask <- rmfi_convert_huf_to_nlay(huf = huf, dis = dis, bas = bas)
  mask[which(mask==0)] <- NA
  mask <- mask/mask
  mask[which(huf$thck==0)] <- NA
  return(mask)
}

#' Convert an \code{ibound} array to lower, upper, left, right, front and back logical arrays indicating presence of a neighbouring active cell
#' 
#' @param ibound 3d \code{ibound} array as specified in a MODFLOW BAS object
#' @return list of lower, upper, left, right, front and back logical 3d arrays
#' @export
rmf_convert_ibound_to_neighbours <- function(ibound) {
  ibound <- ibound != 0
  nrow <- dim(ibound)[1]
  ncol <- dim(ibound)[2]
  nlay <- dim(ibound)[3]
  neighbours <- list()
  neighbours$lower <- rmf_create_array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$upper <- rmf_create_array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$left <- rmf_create_array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$right <- rmf_create_array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$front <- rmf_create_array(FALSE, dim = c(nrow, ncol, nlay))
  neighbours$back <- rmf_create_array(FALSE, dim = c(nrow, ncol, nlay))
  
  if(nlay > 1) {
    neighbours$lower[,,1:(nlay-1)] <- ibound[,,2:nlay]
    neighbours$upper[,,2:nlay] <- ibound[,,1:(nlay-1)]
  }
  if(ncol > 1) {
    neighbours$left[,2:ncol,] <- ibound[,1:(ncol-1),]
    neighbours$right[,1:(ncol-1),] <- ibound[,2:ncol,]
  }
  if(nrow > 1) {
    neighbours$front[1:(nrow-1),,] <- ibound[2:nrow,,]
    neighbours$back[2:nrow,,] <- ibound[1:(nrow-1),,]
  }
  
  return(neighbours)
}

#' Convert id to id
#' 
#' @param id cell id, providing the place of the number in an input file 2d or 3d array
#' @param from 'r' or 'modflow'. The type of id to convert from. Defaults to 'modflow'
#' @param to 'r' or 'modflow'. The type of id to convert to. Defaults to 'r'
#' @param dis a discretisation file object
#' @details a modflow id provides the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @export
rmf_convert_id_to_id = function(id, dis, from = 'modflow', to = 'r') {
  
  ijk <-  rmf_convert_id_to_ijk(id, dis = dis, type = from)
  idn <-  rmf_convert_ijk_to_id(i = ijk$i, j = ijk$j, k = ijk$k, dis = dis, type = to)
  
  return(idn)
  
}

#' Convert id to ijk
#' 
#' @param id cell id, providing the place of the number in an input file 3d array
#' @param dis a discretisation file object
#' @param type 'r' or 'modflow'; defaults to 'r'
#' @details a modflow id provides the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @export
rmf_convert_id_to_ijk <- function(id,
                                  dis,
                                  type = 'r') {
  k <- (id - 1) %/% (dis$nrow*dis$ncol)
  id <- id-k*(dis$nrow*dis$ncol)
  if(type == 'r') {
    j <- (id - 1) %/% dis$nrow
    i <- id-j*dis$nrow
    return(data.frame(i=i,j=j+1,k=k+1))  
  } else if (type == 'modflow') {
    i <- (id - 1) %/% dis$ncol
    j <- id-i*dis$ncol
    return(data.frame(i=i+1,j=j,k=k+1))
  } else {
    stop('Please provide a valid id type.', call. = FALSE)
  }  
}

#' Convert ijk to id
#' 
#' @param i vector of row numbers
#' @param j vector of column numbers
#' @param k vector of layer numbers
#' @param dis a discretization file object
#' @return cell ids, providing the place of the cell in an input file 3d array
#' @details a modflow id provides the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @export
rmf_convert_ijk_to_id <- function(i,
                                  j,
                                  k,
                                  dis,
                                  type = 'r') {
  if(type == 'r') {
    return((k-1)*dis$nrow*dis$ncol+(j-1)*dis$nrow+i)
  } else if (type == 'modflow') {
    return((k-1)*dis$nrow*dis$ncol+(i-1)*dis$ncol+j)
  } else {
    stop('Please provide a valid id type.', call. = FALSE)
  }
  
}

#' Convert a lpf to a upw object
#'
#' @param lpf \code{RMODFLOW} lpf object
#' @param iphdry logical; indicating if head will be set to hdry when it's less than 1E-4 above the cell bottom; defaults to TRUE
#' @return Object of class upw
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @export
#' @seealso \code{\link{rmf_create_upw}}, \code{\link{rmf_convert_upw_to_lpf}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_convert_lpf_to_upw <- function(lpf, iphdry = TRUE) {
  
  names(lpf) <- replace(names(lpf), which(names(lpf) == 'ilpfcb'), 'iupwcb')
  names(lpf) <- replace(names(lpf), which(names(lpf) == 'nplpf'), 'npupw')

  if(!is.null(lpf$storagecoefficient)) lpf$storagecoefficient <- NULL
  if(!is.null(lpf$constantcv)) lpf$constantcv <- NULL
  if(!is.null(lpf$thickstrt)) lpf$thickstrt <- NULL
  if(!is.null(lpf$nocvcorrection)) lpf$nocvcorrection <- NULL
  if(!is.null(lpf$novfc)) lpf$novfc <- NULL
  if(!is.null(lpf$noparcheck)) lpf$noparcheck <- NULL
  
  lpf$iphdry <- iphdry
  
  if(any(lpf$laywet > 0)) {
    lpf$wetfct <- lpf$iwetit <- lpf$ihdwet <- lpf$wetdry <- NULL
    lpf$laywet <- rep(0, length(lpf$laywet))
  }
  
  class(lpf) <- replace(class(lpf), which(class(lpf) == 'lpf'), 'upw')
  return(lpf)
}

#' Convert a upw to a lpf object
#'
#' @param upw \code{RMODFLOW} upw object
#' @param storagecoefficient logical; should STORAGECOEFFICIENT keyword be included?; defaults to FALSE
#' @param constantcv logical; should CONSTANTCV keyword be included?; defaults to FALSE
#' @param thickstrt logical; should THICKSTRT keyword be included?; defaults to FALSE
#' @param nocvcorrection logical; should NOCVCORRECTION keyword be included?; defaults to FALSE
#' @param novfc logical; should NOVFC keyword be included?; defaults to FALSE
#' @param noparcheck logical; should NOPARCHECK keyword be included?; defaults to FALSE
#' @param laywet vector of flags for each layer, indicating if wetting is active; defaults to 0 for each layer
#' @param wetfct is a factor that is included in the calculation of the head that is initially established at a cell when it is converted from dry to wet; defaults to 0.1
#' @param iwetit is the iteration interval for attempting to wet cells; defaults to 1
#' @param ihdwet is a flag that determines which equation is used to define the initial head at cells that become wet; defaults to 0
#' @param wetdry 3d array with a wetting threshold and flag indicating which neighboring cells can cause a cell to become wet; defaults to NULL. If not read for a specific layer, set all values in that layer to NA.
#'
#' @return object of class lpf
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @export
#' @seealso \code{\link{rmf_create_lpf}}, \code{\link{rmf_convert_lpf_to_upw}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
rmf_convert_upw_to_lpf <- function(upw, 
                                   storagecoefficient = FALSE,
                                   constantcv = FALSE,
                                   thickstrt = FALSE,
                                   nocvcorrection = FALSE,
                                   novfc = FALSE,
                                   noparcheck = FALSE,
                                   laywet = upw$laywet,
                                   wetfct = 0.1,
                                   iwetit = 1,
                                   ihdwet = 0,
                                   wetdry = NULL) {
  
  names(upw) <- replace(names(upw), which(names(upw) == 'iupwcb'), 'ilpfcb')
  names(upw) <- replace(names(upw), which(names(upw) == 'npupw'), 'nplpf')
  
  upw$iphdry <- NULL
  
  upw$storagecoefficient <- storagecoefficient
  upw$constantcv <- constantcv
  upw$thickstrt <- thickstrt
  upw$nocvcorrection <- nocvcorrection
  upw$novfc <- novfc
  upw$noparcheck <- noparcheck
  
  upw$laywet <- laywet
  
  if(any(upw$laywet != 0)) {
    upw$wetfct <- wetfct
    upw$iwetit <- iwetit
    upw$ihdwet <- ihdwet
    
    if(any(upw$laytyp != 0)) {
      if(is.null(wetdry)) stop('Please specify a wetdry argument', call. = FALSE)
      upw$wetdry <- rmf_create_array(wetdry, dim = dim(upw$hk))
    }
  }
  
  class(upw) <- replace(class(upw), which(class(upw) == 'upw'), 'lpf')
  return(upw)
  
}

#' Convert real world coordinates to modflow coordinates
#' 
#' @param x real world x coordinate
#' @param y real world y coordinate
#' @param z real world z coordinate; optional
#' @param prj prj object
#' @param dis dis object; optional
#' @param output character; containing 'xyz','ijk' and/or 'off' for the return of x, y, z, i, j, k, roff, coff and loff modflow coordinates
#' @details
#' If dis is not provided, only x, y and z coordinates are returned. If z is not provided, no third dimension coordinates are returned. For the x, y and z modflow coordinates, the origin is placed at the lower left corner of the grid.
#' If the xyz coordinate falls on a boundary of two cells, the minimum ijk indices are returned. 
#'
#' If the z coordinate falls within a Quasi-3D confining bed, the layer index of the overlying model layer is returned. The loff value then represents the fractional distance from the center of the overlying model layer.
#' @return data frame with modflow coordinates
#' @export
rmf_convert_xyz_to_grid <- function(x,y,prj=NULL,z=NULL,dis=NULL,output='xyz') {
  output_xyz <- 'xyz' %in% output
  output_ijk <- 'ijk' %in% output
  output_off <- 'off' %in% output
  if(!is.null(prj)) {
    if(length(prj$origin) <= 2) prj$origin <-  c(prj$origin, 0)
    x <- x-prj$origin[1]
    y <- y-prj$origin[2]
    angle <- atan(y/x)*180/pi - prj$rotation
    angle[which(is.na(angle))] <- 90-prj$rotation
    s <- sqrt(x^2+y^2)
    x <- cos(angle*pi/180)*s
    y <- sin(angle*pi/180)*s
    if(!is.null(z)) z <- z - prj$origin[3]
  }
  dat <- data.frame(x=x,y=y)
  if(!is.null(z)) dat$z <- z
  if(output_ijk || output_off) {
    if(is.null(dis)) stop('Please provide dis argument', call. = FALSE)    
    if(ncol(dat)==3) {
      dis$thck <- dis$tops <- dis$botm
      dis$thck <- rmf_calculate_thickness(dis)
      dis$tops[,,1] <- dis$top
      nnlay <- dim(dis$botm)[3] 
      if(nnlay > 1) dis$tops[,,2:nnlay] <- dis$botm[,,(2:nnlay)-1]
    }
    if(any(dis$laycbd != 0)) {
      warning('Quasi-3D confining beds detected. Returned coordinates/indices only represent numerical layers.')
      cbd <- rmfi_confining_beds(dis)
    }
    for(i in 1:nrow(dat)) {
      dat$i[i] <- which(cumsum(dis$delc) >= sum(dis$delc)-dat$y[i])[1]
      dat$j[i] <- which(cumsum(dis$delr) >= dat$x[i])[1]
      dat$roff[i] <- (sum(dis$delc)-dat$y[i] -(cumsum(dis$delc) - dis$delc/2)[dat$i[i]])/dis$delc[dat$i[i]]
      dat$coff[i] <- (dat$x[i] -(cumsum(dis$delr) - dis$delr/2)[dat$j[i]])/dis$delr[dat$j[i]]
      
      if(dat$x[i] < 0 || dat$x[i] > sum(dis$delr)) {
        dat$j[i] <- NA
        dat$roff[i] <- NA
        dat$coff[i] <- NA
        warning('x coordinate out of bounds. Setting j index and roff/coff to NA', call. = FALSE)
      } 
      if(dat$y[i] < 0 || dat$y[i] > sum(dis$delc)) {
        dat$i[i] <- NA
        dat$roff[i] <- NA
        dat$coff[i] <- NA
        warning('y coordinate out of bounds. Setting i index and roff/coff to NA', call. = FALSE)
      }
      
      if(!is.null(z)) {
        if(is.na(dat$i[i]) || is.na(dat$j[i])) {
          warning('i and/or j index is NA. Setting corresponding k index and loff to NA as well.', call. = FALSE)
          dat$k[i] <- NA
          dat$loff[i] <- NA
        } else if(dat$z[i] < dis$botm[dat$i[i], dat$j[i],nnlay] || dat$z[i] > dis$top[dat$i[i], dat$j[i]]) {
          warning('z coordinate out of bounds. Setting k index and loff to NA.', call. = FALSE)
          dat$k[i] <- NA
          dat$loff[i] <- NA
        } else {
          dat$k[i] <- which(dis$botm[dat$i[i],dat$j[i],] <= dat$z[i])[1]
          k_org <- dat$k[i]
          
          # adjust for cbd
          if(any(dis$laycbd != 0)) {
            dat$k[i] <- dat$k[i] - sum(cbd[1:dat$k[i]])
            if(dis$laycbd[dat$k[i]] != 0) k_org <- dat$k[i]
          }
          dat$loff[i] <- -(dat$z[i]-(dis$tops[dat$i[i],dat$j[i],k_org]+dis$botm[dat$i[i],dat$j[i],k_org])/2)/dis$thck[dat$i[i],dat$j[i],k_org]
        }
      }
    }
    
    columns <-  vector(mode = "character")
    if(output_xyz) {
      if(!is.null(z)){
        columns <- append(columns, c('x','y','z'))
      } else {
        columns <- append(columns, c('x','y'))
      }
    }
    if(output_ijk) {
      if(!is.null(z)) {
        columns <- append(columns, c('i','j','k'))
      } else {
        columns <- append(columns, c('i','j'))
      }
    }
    if(output_off) {
      if(!is.null(z)) {
        columns <- append(columns, c('roff','coff','loff'))
      } else {
        columns <- append(columns, c('roff','coff'))
      }
    }
    
    return(dat[,columns])
  } else {
    return(dat)
  }
}

#' Copy files from specified paths to current working directory
#'
#' @param filenames character vector of filenames
#' @param ... additional arguments provided to file.copy
#'
#' @export
rmf_copy_to_wd <- function(filenames, ...) {
  file.copy(filenames, basename(filenames), ...)
}

#' Add rmf array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @param kper integer vector specifying the stress periods in which the array is active. Used for defining boundary conditions. Defaults to \code{NULL}
#' @param dimlabels character vector specifying the labels of the dimensions; defaults to \code{i, j, k, l} for the first, second, third and fourth dimension, respectively.
#' @param ... 
#' @details subsetting a \code{rmf_array} will return a \code{rmf_array} as long as the object has a dim argument (i.e. has 2 or more free dimensions). Atomic vectors are therefore never \code{rmf_arrays}. 
#'          When \code{l} is not specified when subsetting a \code{rmf_4d_array}, a \code{rmf_4d_array} will always be returned.
#'          Furthermore, unlike subsetting \code{arrays}, dimensions with length 1 will not be dropped unless the \code{drop} argument is set to \code{TRUE}
#' @return either a \code{rmf_2d_array}, a \code{rmf_3d_array} or \code{rmf_4d_array} object
#' @export

rmf_create_array <- function(obj = NA, dim = NULL, kper = attr(obj, 'kper'), dimlabels = attr(obj, 'dimlabels')) {
  if(!is.null(dim)) {
    att <- attributes(obj)
    obj <- array(obj, dim = dim)
    attributes(obj) <- c(attributes(obj), att[-which(names(att) %in% c('dim','dimlabels'))])
  }
  if(is.null(dimlabels)) dimlabels <- c('i', 'j', 'k', 'l')[1:length(dim(obj))]
  if(length(dim(obj))==2) {
    class(obj) <- 'rmf_2d_array'
  } else if(length(dim(obj))==3) {
    class(obj) <- 'rmf_3d_array'
  } else if(length(dim(obj))==4) {
    class(obj) <- 'rmf_4d_array'
  } else {
    stop('Please provide 2d matrix, or 2d, 3d or 4d array.', call. = FALSE)
  }
  attr(obj, 'kper') <- kper
  attr(obj, 'dimlabels') <- dimlabels
  
  return(obj)
}

#' @export
"[.rmf_4d_array" <-  function(x, i, j, k, l, ...) {
  if(missing(i) && missing(j) && missing(k) && missing(l)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1, missing(k) || length(k) > 1, missing(l) || length(l) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  
  obj <-  NextMethod(..., drop = drop)
  
  # l missing -> always 4d unless all other indices are given
  if(!drop && sum(miss) > 1) {
    if(!miss[4]) {
      dim(obj) <- dim(obj)[miss]
    } 
  } 
  
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmf_4d_array', 'rmf_2d_array')
  }
  else if (length(dim(obj)) == 3) {
    class(obj) <- replace(class(x), class(x) == 'rmf_4d_array', 'rmf_3d_array')
  } 
  else if (length(dim(obj)) == 4) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmf_4d_array')
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[rmfi_ifelse0(miss[4], rmfi_ifelse0(!drop && sum(miss) > 1, rep(TRUE, 4), miss), miss)]
  if(!missing(l)) {
    if(!is.null(attr(obj,'kstp'))) attr(obj,'kstp') <- attr(obj,'kstp')[l]
    if(!is.null(attr(obj,'kper'))) attr(obj,'kper') <- attr(obj,'kper')[l]
    if(!is.null(attr(obj,'pertim'))) attr(obj,'pertim') <- attr(obj,'pertim')[l]
    if(!is.null(attr(obj,'totim'))) attr(obj,'totim') <- attr(obj,'totim')[l]
    if(!is.null(attr(obj,'nstp'))) attr(obj,'nstp') <- attr(obj,'nstp')[l]
  }
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
"[.rmf_3d_array" <-  function(x, i, j, k, ...) {
  if(missing(i) && missing(j) && missing(k)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1, missing(k) || length(k) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  
  obj <-  NextMethod(..., drop = drop)
  
  if(!drop && sum(miss) > 1) {
    dim(obj) <- dim(obj)[miss]
  } 
  
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmf_3d_array', 'rmf_2d_array')
  }
  else if (length(dim(obj)) == 3) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmf_3d_array')
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[miss]
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
"[.rmf_2d_array" <-  function(x, i, j, ...) {
  if(missing(i) && missing(j)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  
  obj <-  NextMethod(..., drop = drop)
  
  if(!drop && sum(miss) > 1) {
    dim(obj) <- dim(obj)[miss]
  } 
  
  if (length(dim(obj)) == 2) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmf_2d_array')
  }
  
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[miss]
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
as.matrix.rmf_2d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.matrix.rmf_3d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.matrix.rmf_4d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.array.rmf_2d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, kper = NULL)

#' @export
as.array.rmf_3d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, kper = NULL)

#' @export
as.array.rmf_4d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, kper = NULL)

#' @export
aperm.rmf_2d_array <- function(a, perm, ...) {
  att <- attributes(a)
  a <- aperm.default(a, perm = perm, ...)
  att$dimlabels <- att$dimlabels[perm]
  att$dim <- attr(a, 'dim')
  attributes(a) <- att
  return(a)
}

#' @export
aperm.rmf_3d_array <- function(a, perm, ...) {
  att <- attributes(a)
  a <- aperm.default(a, perm = perm, ...)
  att$dimlabels <- att$dimlabels[perm]
  att$dim <- attr(a, 'dim')
  attributes(a) <- att
  return(a)
}

#' @export
aperm.rmf_4d_array <- function(a, perm, ...) {
  att <- attributes(a)
  a <- aperm.default(a, perm = perm, ...)
  att$dimlabels <- att$dimlabels[perm]
  att$dim <- attr(a, 'dim')
  attributes(a) <- att
  return(a)
}

#' @export
apply.rmf_2d_array <- function(X, ...) {
  apply(as.array(X), ...)
}

#' @export
apply.rmf_3d_array <- function(X, ...) {
  apply(as.array(X), ...)
}

#' @export
apply.rmf_4d_array <- function(X, ...) {
  apply(as.array(X), ...)
}

#' @export
t.rmf_2d_array <- function(obj) {
  obj <- t.default(obj)
  attr(obj, "dimlabels") <- rev(attr(obj, "dimlabels"))
  return(obj)
}

#' @export
as.matrix.rmf_2d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.matrix.rmf_3d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.matrix.rmf_4d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.array.rmf_2d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, kper = NULL)

#' @export
as.array.rmf_3d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, kper = NULL)

#' @export
as.array.rmf_4d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, kper = NULL)

#'
#' Create a MODFLOW parameter
#'
#' @export

rmf_create_parameter <- function(...) {
  UseMethod('rmf_create_parameter')
}


#' Create an array parameter
#'
#' Create a parameter from multiplier and/or zone arrays used in MODFLOW boundary condition packages and flow packages
#'
#' @param dis \code{RMODFLOW} dis object. Used to dimension the final array.
#' @param kper integer vector with the stress period numbers during which the parameter is active. Specifying kper indicates that this parameter represent boundary condition information.
#' @param layer integer vector denoting the layer indices represented by the parameter. Specifying layer indicates that this parameter represent flow package information; the partyp argument should be set as well. Multiple instances of the same layer index are allowed.
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply values in the array. Defaults to 1.0
#' @param mltnam character vector; specifying the names of multiplier arrays that are used to build the parameter. The keyword \code{"NONE"} indicates no multiplier array is present. 
#' @param zonnam character vector; specifying the names of zone arrays that are used to build the parameter. The keyword \code{"ALL"} indicates no multiplier array is present. 
#' @param iz list where each element is a numeric vector with the zone numbers for the corresponding zone array. Only read when the corresponding \code{zonnam} is not \code{"ALL"}.
#' @param partyp character specifying the type of flow parameter. Allowed values are \code{HK, HANI, VK, VANI, SS, SY}, \code{VKCB} for lpf and \code{SYTP} for huf. Not used when \code{layer} is \code{NULL}.
#' @param mlt \code{RMODFLOW} mlt object which holds the multiplier arrays specified in \code{mltnam}
#' @param zon \code{RMODFLOW} zon object which holds the zone arrays specified in \code{zonnam}
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' @param hgunam character vector specifying the name(s) of the hydrogeological unit(s) if the parameter represents a huf parameter; defaults to NULL. See details.
#' @details A boundary-condition parameter is created by setting the kper argument. A flow parameter is created by setting the layer and partyp arguments.
#'          If the boundary-condition parameter is to be time-varying, a separate parameter should be created for each instance with a unique \code{instnam} but with the same \code{name} 
#'          Typically, an array parameter is build from a single multiplier and/or zone array combination. However, multiple combinations can be used.
#'          If \code{mltnam} is \code{"NONE"} and \code{zonnam} is \code{"ALL"}, \code{parval} applies to all cells in the array and \code{mlt} and \code{zon} are not read.
#'          If \code{hgunam} is specified, \code{layer} can not be specified.
#' @return an \code{rmf_2d_array} object of class \code{rmf_parameter}
#' @export
#' @seealso \code{\link{rmf_create_array}}
#'
rmf_create_parameter.default <- function(dis,
                                         kper = NULL,
                                         layer = NULL,
                                         parnam,
                                         parval = 1.0, 
                                         mltnam = 'NONE', 
                                         zonnam = 'ALL',
                                         iz = NULL, 
                                         partyp = NULL,
                                         mlt = NULL, 
                                         zon = NULL, 
                                         instnam = NULL,
                                         hgunam = NULL) {
  
  if(is.null(kper) && (is.null(layer) && is.null(hgunam))) stop("Please specify either the kper argument (for boundary condition arrays) or the layer argument (for flow parameter arrays).", call. = FALSE)
  if(!is.null(layer) && is.null(partyp)) stop("Please specify the partyp argument", call. = FALSE)
  if(!is.null(layer) && !is.null(hgunam)) stop("Please specify either the layer or the hgunam argument", call. = FALSE)
  mltarr <- as.list(mltnam)
  zonarr <- as.list(zonnam)
  
  if(any(toupper(mltnam) != "NONE")) {
    if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
    mlt_id <- which(toupper(mltnam) != "NONE")
    mltarr[mlt_id] <- mlt$rmlt[mltnam[mlt_id]]
  }
  if(any(toupper(zonnam) != "ALL")) {
    if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
    if(is.null(iz)) stop('Please provide a iz argument', call. = FALSE)
    if(!inherits(iz, 'list')) stop('The iz argument should be a list', call. = FALSE)
    zon_id <- which(toupper(zonnam) != "ALL")
    zonarr[zon_id] <- zon$izon[zonnam[zon_id]]
  }
  
  
  arr <- rmf_calculate_array(dis = dis,
                             layer = rmfi_ifelse0(!is.null(hgunam), 1:length(hgunam), layer),
                             mltarr = mltarr,
                             zonarr = zonarr,
                             iz = iz,
                             parval = parval)
  
  attr(arr, 'kper') <- kper
  attr(arr, 'parnam') <- parnam
  attr(arr, 'parval') <- parval
  attr(arr, 'layer') <- layer
  attr(arr, 'partyp') <- partyp
  attr(arr, 'mlt') <- mltnam
  attr(arr, 'zon') <- zonnam
  attr(arr, 'iz') <- iz
  attr(arr, 'instnam') <- instnam
  attr(arr, 'hgunam') <- hgunam
  class(arr) <- c('rmf_parameter', class(arr))
  return(arr)
  
}


#'
#' Concise function for creating a 2D-array parameter.
#' 
#' Create a MODFLOW parameter from a 2D-array.
#' 
#' @param array a \code{rmf_2d_array} object
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply values in the array. Defaults to 1.0
#' @param kper integer vector with the stress period numbers during which the parameter is active. Specifying kper indicates that this parameter represent boundary condition information.
#' @param layer integer vector denoting the layer indices represented by the parameter. Specifying layer indicates that this parameter represent flow package information and the partyp argument needs to be set as well.
#' @param partyp character specifying the type of flow parameter. Allowed values are \code{HK, HANI, VK, VANI, SS, SY and VKCB}. Not used when \code{layer} is \code{NULL}.
#' @param mltnam character vector specifying the name of the resulting multiplier array. 
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' 
#' @details This function will only set the multiplier array. The user must make sure that this multiplier array is written to a separate MLT file when running a MODFLOW simulation.
#' This function is intended to be used when no multiplier and/or zone arrays are specified for a parameter.
#' A boundary-condition parameter is created by setting the kper argument. A flow parameter is created by setting the layer and partyp arguments.
#' @return a \code{rmf_2d_array} object of class \code{rmf_parameter}
#' @export
#' 
rmf_create_parameter.rmf_2d_array <-  function(array,
                                               parnam, 
                                               parval = 1.0,
                                               kper = attr(array, 'kper'),
                                               layer = NULL,
                                               partyp = NULL,
                                               mltnam = parnam,
                                               instnam = NULL) {
  
  if(is.null(kper) && is.null(layer)) stop("Please specify either the kper argument (for boundary condition arrays) or the layer argument (for flow parameter arrays).", call. = FALSE)
  if(!is.null(layer) && is.null(partyp)) stop("Please specify the parameter partyp", call. = FALSE)
  
  if(length(unique(c(array))) == 1) mltnam <-  'NONE'
  
  attr(array, 'kper') <- kper
  attr(array, 'parnam') <- parnam
  attr(array, 'parval') <- parval
  attr(array, 'layer') <- layer
  attr(array, 'partyp') <- partyp
  attr(array, 'mlt') <- mltnam
  attr(array, 'zon') <- 'ALL'
  attr(array, 'iz') <- NULL
  attr(array, 'instnam') <- instnam
  class(array) <- c('rmf_parameter', class(array))
  return(array)
}


#'
#' Concise function for creating a 3D-array parameter.
#' 
#' Create a list of MODFLOW flow parameters from a 3D-array.
#' 
#' @param array a \code{rmf_3d_array} object
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply values in the array. Defaults to 1.0
#' @param layer integer vector denoting the layer indices represented by the parameter. Defaults to \code{seq(1, dim(array)[3])}
#' @param partyp character specifying the type of flow parameter. Allowed values are \code{HK, HANI, VK, VANI, SS, SY and VKCB}. 
#' @param mltnam character vector specifying the name of the resulting multiplier array. 
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' 
#' @details This function will only set the multiplier arrays for all layers of the 3D array. The user must make sure that this multiplier array is written to a separate MLT file when running a MODFLOW simulation.
#' This function is intended to be used when no multiplier and/or zone arrays are specified for a parameter.
#' Only flow parameters can be created with this function. A singly partyp is appointed to all layers.
#' @return a \code{rmf_3d_array} objects of class \code{rmf_parameter}
#' @export
#' 

rmf_create_parameter.rmf_3d_array <-  function(array,
                                               parnam, 
                                               parval = 1.0,
                                               layer = 1:dim(array)[3],
                                               partyp,
                                               mltnam = parnam,
                                               instnam = NULL) {
  # subset if not all layers are needed
  if(dim(array)[3] != length(layer)) array <- array[,,layer]
  unq <- vapply(1:dim(array)[3], function(i) length(unique(c(array[,,i]))) == 1, TRUE)
  if(length(mltnam) == 1) mltnam <- paste(mltnam, layer, sep = '_')
  mltnam[unq] <- 'NONE'
  
  attr(array, 'parnam') <- parnam
  attr(array, 'parval') <- parval
  attr(array, 'layer') <- layer
  attr(array, 'partyp') <- partyp
  attr(array, 'mlt') <- mltnam
  attr(array, 'zon') <- rep('ALL', length(layer))
  attr(array, 'iz') <- NULL
  attr(array, 'instnam') <- instnam
  class(array) <- c('rmf_parameter', class(array))
  
  return(array)
  
}



#' Create a List Data input parameter
#'
#' Create a parameter for List Data input used in MODFLOW boundary condition packages.
#'
#' @param rmf_list a rmf_list object
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply the flux controlling variable in the data.frame. Defaults to 1.0
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' @details the variable in the data.frame which is multiplied differs between boundary condition packages. 
#'          if the parameter is to be time-varying, a separate parameter should be created for each instance with a unique \code{instnam} but with the same \code{name} 
#' @return an object of class \code{rmf_parameter} and \code{rmf_list} 
#' @export
#' @seealso \code{\link{rmf_create_list}}
#' 

rmf_create_parameter.rmf_list <- function(rmf_list,
                                          parnam, 
                                          parval = 1.0,
                                          instnam = NULL, 
                                          kper = attr(rmf_list, 'kper')) {
  
  attr(rmf_list, 'kper') <- kper
  attr(rmf_list, 'parnam') <- parnam
  attr(rmf_list, 'parval') <- parval
  attr(rmf_list, 'instnam') <- instnam
  class(rmf_list) <- c('rmf_parameter', class(rmf_list))
  return(rmf_list)
}

#' Calculate the Darcy flux
#'
#' @export
rmf_darcy_flux <- function(...) {
  UseMethod('rmf_darcy_flux')
}

#' Calculate the Darcy flux from a 2d array
#'
#' @param hed 2d array with the hydraulic heads
#' @param dis \code{RMODFLOW} dis object
#' @param hk 2d array with the hydraulic conductivities along rows
#' @param hani either a 2d array or a single value specifying the horizontal anisotropy. See details. Defaults to isotropic conditions.
#' @param porosity optional 2d array with the porosity values as fractions. See details. Defaults to NULL
#' @param mask optional logical 2d array used to set the active cells; defaults to NULL
#' @details Horizontal anisotropy is the ratio of hydraulic conductivity along columns (Y direction) to the hydraulic conductivity along rows (X direction).
#'          Darcy flux (or Darcy velocity) is not the actual velocity at which the fluid travels. In order to obtain this fluid velocity, a porosity argument should be specified.
#'          The flux components are positive in the direction of increasing Cartesian axes.
#' @return a list with the magnitude, x and y components of the Darcy flux (or fluid velocity if porosity is specified) as 2d arrays
#' @export
#' @rdname rmf_darcy_flux
#'
rmf_darcy_flux.rmf_2d_array <- function(hed, dis, hk, hani = rep(1, dis$nlay), porosity = NULL, mask = NULL) {
  
  hk <- rmf_create_array(hk, dim = c(dis$nrow, dis$ncol))
  if(!is.array(hani) && length(hani) == 1) hani <- rep(hani, dis$nlay)
  
  if(!is.array(hani)) hani <- rmf_create_array(rep(hani, each = dis$nrow*dis$ncol), dim = c(dis$nrow, dis$ncol))
  hky <- hk*hani
  
  if(is.null(porosity)) {
    porosity <- 1
  } else {
    porosity <- rmf_create_array(porosity, dim = c(dis$nrow, dis$ncol))
  }
  
  if(is.null(mask)) mask <- hed*0 + 1
  grad <- rmf_gradient(hed, dis = dis, mask = mask)
  
  fx <- -hk*grad$x/porosity
  fy <- -hky*grad$y/porosity
  mag <- sqrt(fx^2 + fy^2)
  
  return(list(magnitude = mag, x = fx, y = fy))
  
}

#' Calculate the Darcy flux from a 3d array
#'
#' @param hed 3d array with the hydraulic heads
#' @param dis \code{RMODFLOW} dis object
#' @param flow optional \code{RMODFLOW} flow package, i.e. one of lpf, bcf, huf or upw. Overwrites the use of hk, hani, vka & vani. See details.
#' @param hk 3d array with the hydraulic conductivities along rows
#' @param hani either a 3d array or a single value for each layer specifying the horizontal anisotropy. See details. Defaults to isotropic conditions.
#' @param vka 3d array with either vertical hydraulic conductivities or vertical anisotropies depending on the value of vani. See details.
#' @param vani a single logical value for each layer specifying whether vka represents vertical hydraulic conductivity (FALSE) or vertical anisotropy for the layer (TRUE). Defaults to FALSE for all layers.
#' @param porosity optional 3d array with the porosity values as fractions. See details. Defaults to NULL
#' @param mask optional logical 3d array used to set the active cells; defaults to NULL
#' @details When flow is specified, all flow parameters are obtained from the flow package.
#'          Horizontal anisotropy is the ratio of hydraulic conductivity along columns (Y direction) to the hydraulic conductivity along rows (X direction).
#'          Vertical anisotropy is the ratio of horizontal hydraulic conductivity along rows (X direction) to the vertical hydraulic conductivity.
#'          Darcy flux (or Darcy velocity) is not the actual velocity at which the fluid travels. In order to obtain this fluid velocity, a porosity argument should be specified.
#'          The flux components are positive in the direction of increasing Cartesian axes.
#' @return a list with the magnitude, x, y and z components of the Darcy flux (or fluid velocity if porosity is specified) as 3d arrays
#' @export
#' @rdname rmf_darcy_flux
#' 
rmf_darcy_flux.rmf_3d_array <- function(hed, dis, flow = NULL, hk, hani = rep(1, dis$nlay), vka, vani = rep(FALSE, dis$nlay), porosity = NULL, mask = NULL) {
  
  if(!is.null(flow)) {
    
    if('huf' %in% class(flow)) {
      flow <- rmf_convert_huf_to_lpf(flow, dis = dis, vka = 'VK', mask = mask)
    } else if('bcf' %in% class(flow)) {
      flow <- rmf_convert_bcf_to_lpf(flow, dis = dis)
    } 
    
    hk <- flow$hk
    hani <- rmfi_ifelse0(is.null(flow$hani), flow$chani, flow$hani)
    vka <- flow$vka
    vani <- flow$layvka > 0
    
  }
  
  hk <- rmf_create_array(hk, dim = c(dis$nrow, dis$ncol, dis$nlay))
  vka <- rmf_create_array(vka, dim = c(dis$nrow, dis$ncol, dis$nlay))
  if(length(vani) == 1) vani <- rep(vani, dis$nlay)
  
  vka[,,which(vani)] <- hk[,,which(vani)]/vka[,,which(vani)]
  
  if(!is.array(hani)) {
    if(length(hani) == 1) hani <- rep(hani, dis$nlay)
    hani <- rmf_create_array(rep(hani, each = dis$nrow*dis$ncol), dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  hky <- hk*hani
  
  if(is.null(porosity)) {
    porosity <- 1
  } else {
    porosity <- rmf_create_array(porosity, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  
  if(is.null(mask)) mask <- hed*0 + 1
  grad <- rmf_gradient(hed, dis = dis, mask = mask)
  
  fx <- -hk*grad$x/porosity
  fy <- -hky*grad$y/porosity
  fz <- -vka*grad$z/porosity
  mag <- sqrt(fx^2 + fy^2 + fz^2)
  
  return(list(magnitude = mag, x = fx, y = fy, z = fz))
  
}

#' Calculate the Darcy flux from a 4d array
#'
#' @param hed 4d array with the hydraulic heads
#' @param dis \code{RMODFLOW} dis object
#' @param l integer index used to subset the 4th dimension of hed
#' @param ... additional arguments passed to \code{\link{rmf_darcy_flux.rmf_3d_array}}
#' @details hed is subsetted to a 3d array and passed to \code{\link{rmf_darcy_flux.rmf_3d_array}}.
#' @export
#' @rdname rmf_darcy_flux
rmf_darcy_flux.rmf_4d_array <- function(hed, dis, l, ...) {
  if(missing(l)) stop('Please specify a l argument', call. = FALSE)
  rmf_darcy_flux(hed[,,,l], dis = dis, ...)
}

#' Calculate a gradient field
#'
#' @rdname rmf_gradient
#' @export
#'
rmf_gradient <- function(...) {
  UseMethod('rmf_gradient')
}

#' Calculate a 2d gradient field
#' 
#' \code{rmf_gradient.rmf_2d_array} calculates the x and y components of the gradient from a 2d scalar field
#'
#' @param obj 2d array with the scalars
#' @param dis \code{RMODFLOW} dis object
#' @param na_value optional; sets these values in obj to 0; defaults to NULL
#' @param mask logical 2d array indicating which cells to include in the gradient calculation; defaults to all cells active
#' @return a list with the x and y components of the gradient field as 2d arrays
#' @details The gradient is evaluated in the direction of increasing x & y values.
#' @export
#'
#' @rdname rmf_gradient
#' 
rmf_gradient.rmf_2d_array <- function(obj, dis, na_value = NULL, mask = obj*0 + 1) {
    
  coords <- rmf_cell_coordinates(dis)
  x <- coords$x[1,,1]
  y <- coords$y[,1,1]
  
  n <- dis$nrow
  m <- dis$ncol
  
  if(!is.null(na_value)) obj[which(obj == na_value)] <- 0
  obj <- obj*mask
    
  gX <- gY <- 0 * obj
  if(n > 1) {
    gY[1, ] <- (obj[2, ] - obj[1, ])/(y[2] - y[1])
    gY[n, ] <- (obj[n, ] - obj[n - 1, ])/(y[n] - y[n - 1])
    if (n > 2) gY[2:(n - 1), ] <- (obj[3:n, ] - obj[1:(n - 2), ])/(y[3:n] - y[1:(n - 2)])
  }
  if(m > 1) {
    gX[, 1] <- (obj[, 2] - obj[, 1])/(x[2] - x[1])
    gX[, m] <- (obj[, m] - obj[, m - 1])/(x[m] - x[m - 1])
    if (m > 2) gX[, 2:(m - 1)] <- (obj[, 3:m] - obj[, 1:(m - 2)])/(x[3:m] - x[1:(m - 2)])
  }
  
  return(list(x = gX, y = gY))
  
}

#' Calculate a 3d gradient field
#' 
#' \code{rmf_gradient.rmf_3d_array} calculates the x, y and z components of the gradient from a 3d scalar field
#'
#' @param obj 3d array with the scalars
#' @param dis \code{RMODFLOW} dis object
#' @param na_value optional; sets these values in obj to 0; defaults to NULL
#' @param mask logical 3d array indicating which cells to include in the gradient calculation; defaults to all cells active
#' @return a list with the x, y and z components of the gradient field as 3d arrays
#' @details The gradient is evaluated in the direction of increasing x, y & z values.
#' @export
#'
#' @rdname rmf_gradient
#' 
rmf_gradient.rmf_3d_array <- function(obj, dis, na_value = NULL, mask = obj*0 + 1) {
  
  coords <- rmf_cell_coordinates(dis)
  x <- coords$x[1,,1]
  y <- coords$y[,1,1]
  z <- coords$z
  
  n <- dis$nrow
  m <- dis$ncol
  k <- dis$nlay
  
  if(!is.null(na_value)) obj[which(obj == na_value)] <- 0
  obj <- obj*mask
  
  gX <- gY <- gZ <- 0 * obj
  if(n > 1) {
    gY[1,,] <- (obj[2,,] - obj[1,,])/(y[2] - y[1])
    gY[n,,] <- (obj[n,,] - obj[n - 1,,])/(y[n] - y[n - 1])
    if (n > 2) gY[2:(n - 1),,] <- (obj[3:n,,] - obj[1:(n - 2),,])/(y[3:n] - y[1:(n - 2)])
  }
  if(m > 1) {
    gX[,1,] <- (obj[,2,] - obj[,1,])/(x[2] - x[1])
    gX[,m,] <- (obj[,m,] - obj[,m - 1,])/(x[m] - x[m - 1])
    if (m > 2) gX[,2:(m - 1),] <- (obj[,3:m,] - obj[,1:(m - 2),])/(x[3:m] - x[1:(m - 2)])
  }
  if(k > 1) {
    gZ[,,1] <- (obj[,,2] - obj[,,1])/(z[,,2] - z[,,1])
    gZ[,,k] <- (obj[,,k] - obj[,,k - 1])/(z[,,k] - z[,,k - 1])
    if (k > 2) gZ[,,2:(k - 1)] <- (obj[,,3:k] - obj[,,1:(k - 2)])/(z[,,3:k] - z[,,1:(k - 2)])
  }
  
  return(list(x = gX, y = gY, z = gZ))
  
}

#' Calculate a 3d gradient field from a 4d array
#' 
#' \code{rmf_gradient.rmf_4d_array} calculates the x, y and z components of the gradient from a 4d scalar field where the 4th dimension is time
#'
#' @param obj 4d array with the scalars
#' @param dis \code{RMODFLOW} dis object
#' @param l integer index used to subset the 4th dimension of the 4d array
#' @param ... additional arguments passed to \code{\link{rmf_gradient.rmf_3d_array}}
#' @return a list with the x, y and z components of the gradient field as 3d arrays
#' @details the 4d array is subsetted on the 4th dimension to a 3d array from which the gradient is calculated
#' @export
#'
#' @rdname rmf_gradient
#' 
rmf_gradient.rmf_4d_array <- function(obj, dis, l, ...) {
  if(missing(l)) stop('Please specify a l argument')
  rmf_gradient(obj[,,,l], dis = dis, ...)
}

#' Calculate the internal time step sequence of a transient MODFLOW model
#' 
#' \code{rmf_time_steps} calculates the internal sequence of time steps of a transient MODFLOW model from either an \code{RMODFLOW} dis object or separate parameters
#' 
#' @param dis optional, an \code{RMODFLOW} dis object
#' @param perlen optional, only read when a \code{dis} object is not supplied; numeric vector of length \code{nper} specifying the stress period lengths
#' @param tsmult optional, only read when a \code{dis} object is not supplied; numeric vector of length \code{nper} specifying the time step multipliers
#' @param nstp optional, only read when a \code{dis} object is not supplied; numeric vector of length \code{nper} specifying the number of time steps in each stress period
#' @param incl_ss logical, only read when a \code{dis} object is supplied; should the lengths of steady-state stress periods in the \code{dis} object be incorporated in the calculation; defaults to TRUE 
#' 
#' @return a list holding the numeric vectors of the computed sequence of time step lengths and its cumulative sum
#' @export
#' @seealso \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?dis.htm}

rmf_time_steps = function(dis = NULL,
                          perlen = NULL,
                          tsmult = NULL,
                          nstp = NULL,
                          incl_ss = T){
  
  
  if(!is.null(dis)){
    if(incl_ss){
      perlen <- dis$perlen
      tsmult <- dis$tsmult
      nstp <- dis$nstp
      itmuni <- dis$itmuni
    } else {
      perlen <- dis$perlen[which(dis$sstr == 'TR')]
      tsmult <- dis$tsmult[which(dis$sstr == 'TR')]
      nstp <- dis$nstp[which(dis$sstr == 'TR')]
      itmuni <- dis$itmuni[which(dis$sstr == 'TR')]
    }
    
  } 
  
  its <- list()
  for(i in 1:length(perlen)){
    if(tsmult[i]==1) t1 = perlen[i]/nstp[i] else t1 = perlen[i]*((tsmult[i]-1)/((tsmult[i]^nstp[i])-1))   
    
    its[[i]] <-  t1
    
    if(nstp[i] > 1){
      for(j in 2:nstp[i]){
        its[[i]] <-  append(its[[i]], its[[i]][j-1]*tsmult[i])
      }
    }
  }
  
  its <-  list(tsl = unlist(its), cumsum = cumsum(unlist(its)))
  
  return(its)
  
}


#' Write a MODFLOW array to a separate file. 
#'
#' \code{rmf_write_array} writes a MODFLOW array to an output file. Binary and ASCII formats are supported
#'
#' @param array a 2D, 3D or 4D \code{rmf_array} to write.
#' @param file filename to write to
#' @param append logical; should the array be appended to the file
#' @param binary logical; should the array be written to a binary file
#' @param header logical; should a MODFLOW style header be written for the array (see 'Details'). Defaults to TRUE if binary is TRUE and FALSE otherwise.
#' @param dis optional \code{RMODFLOW} dis object. Used when \code{KPER}, \code{PERTIM} and \code{TOTIM} in the header should be exact.
#' @param desc character of maximum 16 characters. Used to set the \code{desc} element in the header. Default to \code{'HEAD'}
#' @param precision character; either \code{'single'} or \code{'double'}. Denotes the precision of the binary file.
#' @param xsection logical; does the array represent a NLAY x NCOL cross-section. See 'Details'.
#' 
#' @details the header file consists of the following elements:
#'  \code{KSTP}, \code{KPER},\code{PERTIM},\code{DESC},\code{NCOL}, \code{NROW}, \code{ILAY} and \code{FMTIN}
#'  (see \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?frequently_asked_questions.htm} for their respective meaning.)
#'  If the array is 2D or 3D, \code{KSTP}, \code{KPER},\code{PERTIM} are always set to 1; otherwise they are all equal to the index of the 4th dimension.
#'  
#'  The \code{DESC} element must be read but it not used by MODFLOW itself. Users may want to specify a different \code{DESC} value when using the array with postprocessing software.
#'  
#'  The \code{FMTIN} element is not written for binary files. For ASCII files, \code{RMODFLOW} sets it as \code{NCOL * F}. Note that the ASCII format
#'  is irrelevant when using free-format reading and writing.
#'  
#'  \code{xsection} can be set to TRUE if the array represents a cross-section, i.e. the ibound or strt array in the
#'   \code{bas} file. The user must make sure the array is of dimension NLAY * NCOL. The sole function of \code{xsection} is to 
#'   set the \code{ILAY} argument to -1 which promts MODFLOW to write slightly different information to the listing file. 
#'   \code{xsection} does not affect simulation results (assuming the array dimensions are correct)
#'  
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_array}}

rmf_write_array = function(array, file, append = FALSE, binary = FALSE, header = ifelse(binary, TRUE, FALSE), dis=NULL, desc = 'HEAD', precision = 'single', xsection = FALSE) {
  
  if(binary) { # binary
    if(header && is.integer(array)) warning("MODFLOW does not read a header line for a binary integer array. Consider setting header to FALSE", call. = FALSE)
    write_binary = function() {
      real_number_bytes <- ifelse(precision == 'single', 4, 8)
      size <- ifelse(is.integer(array), NA_integer_, real_number_bytes)
      ncell = prod(dim(array))
      desc = format(toupper(desc), width = 16, justify='right')
      
      if(is.null(dim(array))) {    # scalar
        if(header) {
          writeBin(1L, con=con) # KSTP
          writeBin(1L, con=con) # KPER
          writeBin(1, con=con, size = real_number_bytes) # PERTIM
          writeBin(1, con=con, size = real_number_bytes) # TOTIM
          writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
          writeBin(1L, con=con) # NCOL
          writeBin(1L, con=con) # NROW
          rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(1L, con=con)) # ILAY
        }
        writeBin(as.vector(array), con = con, size = size)
        
      } else if(length(dim(array))==2) {    # 2D
        if(header) {
          writeBin(1L, con=con) # KSTP
          writeBin(1L, con=con) # KPER
          writeBin(1, con=con, size = real_number_bytes) # PERTIM
          writeBin(1, con=con, size = real_number_bytes) # TOTIM
          writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
          writeBin(as.integer(dim(array)[2]), con=con) # NCOL
          writeBin(as.integer(dim(array)[1]), con=con) # NROW
          rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(1L, con=con)) # ILAY
        }
        writeBin(as.vector(aperm(array, c(2,1))), con = con, size = size)
        
        
      } else if(length(dim(array))==3) {    # 3D
        for(k in 1:dim(array)[3]) {
          if(header) {
            writeBin(1L, con=con) # KSTP
            writeBin(1L, con=con) # KPER
            writeBin(1, con=con, size = real_number_bytes) # PERTIM
            writeBin(1, con=con, size = real_number_bytes) # TOTIM
            writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
            writeBin(as.integer(dim(array)[2]), con=con) # NCOL
            writeBin(as.integer(dim(array)[1]), con=con) # NROW
            rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(as.integer(k), con=con)) # ILAY
          }
          writeBin(as.vector(aperm(array[,,k], c(2,1))), con = con, size = size)
        }
        
      } else if(length(dim(array))==4) {    # 4D
        
        if(header && is.null(dis)) warning('No dis object supplied; writing simplified header lines.', call. = FALSE)
        
        for(l in 1:dim(array)[4]) {
          for(k in 1:dim(array)[3]) {
            if(header) {
              if(is.null(dis)) {
                kper <- l
                kstp <- l
                totim <- sum(1:l)
                pertim <- l
              } else {
                kper <-  findInterval(l, cumsum(dis$nstp), left.open = T) + 1
                kstp <-  rmfi_ifelse0(kper > 1, l - cumsum(dis$nstp[kper-1]), l)
                totim <-  rmf_time_steps(dis)$cumsum[l]
                pertim <-  totim - rmf_time_steps(dis)$cumsum[cumsum(nstp)[kper-1]]
              }
              
              writeBin(as.integer(kstp), con=con) # KSTP
              writeBin(as.integer(kper), con=con) # KPER
              writeBin(pertim, con=con, size = real_number_bytes) # PERTIM
              writeBin(totim, con=con, size = real_number_bytes) # TOTIM
              writeChar(desc, con=con, nchars = 16, eos = NULL) # DESC
              writeBin(as.integer(dim(array)[2]), con=con) # NCOL
              writeBin(as.integer(dim(array)[1]), con=con) # NROW
              rmfi_ifelse0(xsection, writeBin(-1L, con=con),writeBin(as.integer(k), con=con)) # ILAY
            }
            writeBin(as.vector(aperm(array[,,k,l], c(2,1))), con = con, size = size)
          }
        }
      }
    }
    if(append) {
      con <-  file(file, open='ab')
    } else {
      con <-  file(file, open='wb')
    }
    
    try(write_binary())
    close(con)  
    
  } else { # ascii
    if(!append) close(file(file, open='w')) 
    
    if(length(dim(array))==3) {    # 3D
      for(k in 1:dim(array)[3]) {
        if(header) rmfi_write_variables(1, 1, 1, 1, format(desc, width=16, justify='right'), ncol(array), nrow(array), ifelse(xsection,-1,k), paste0('(',ncol(array),'F)'), file=file)
        write.table(array[,,k], file=file, col.names = F, row.names = F, append = TRUE)
      }
    } else if(length(dim(array))==4) {    # 4D
      if(header && is.null(dis)) warning('No dis object supplied; writing simplified header lines.', call. = FALSE)
      
      for(l in 1:dim(array)[4]) {
        for(k in 1:dim(array)[3]) {
          if(header) {
            if(is.null(dis)) {
              kper <- l
              kstp <- l
              totim <- sum(1:l)
              pertim <- l
            } else {
              kper <-  findInterval(l, cumsum(dis$nstp), left.open = T) + 1
              kstp <-  rmfi_ifelse0(kper > 1, l - cumsum(dis$nstp[kper-1]), l)
              totim <-  rmf_time_steps(dis)$cumsum[l]
              pertim <-  totim - rmf_time_steps(dis)$cumsum[cumsum(nstp)[kper-1]]
            }
            rmfi_write_variables(kper, kstp, totim, pertim, format(desc, width=16, justify='right'), ncol(array), nrow(array), ifelse(xsection,-1,k), paste0('(',ncol(array),'F)'), file=file)
          }
          write.table(array[,,k,l], file=file, col.names = F, row.names = F, append = TRUE)
        }
      }
    } else {
      if(header) rmfi_write_variables(1, 1, 1, 1, format(desc, width=16, justify='right'), ncol(array), nrow(array), ifelse(xsection, -1,1), paste0('(',ncol(array),'F)'), file=file)
      write.table(array, file=file, col.names = F, row.names = F, append = TRUE)
    }
    
  }
}

#' Generic function to export GIS raster layers from RMODFLOW arrays
#' 
#' @rdname rmf_export_raster
#' @export
rmf_export_raster <- function(...) {
  UseMethod('rmf_export_raster')
}

#' Generic function to export tables from RMODFLOW arrays
#' 
#' @rdname rmf_export_table
#' @export
rmf_export_table <- function(...) {
  UseMethod('rmf_export_table')
}

#' Generic function to export tables from RMODFLOW arrays
#' 
#' @rdname rmf_export_table
#' @export
rmf_export_table.rmf_4d_array <- function(array,
                                          k,
                                          l,
                                          dis,
                                          bas = NULL,
                                          mask = rmfi_ifelse0(is.null(bas),array*0+1,bas$ibound[,,1]),
                                          prj=NULL,
                                          crs=NULL,
                                          file='rmf_export.csv',
                                          type='csv') {
  if(type=='csv') {
    
    cell_coord <- cell_coordinates(dis)
    if(!is.null(prj)) {
      cell_coord <- rmf_convert_grid_to_xyz(x=c(cell_coord$x[,,k]),y=c(cell_coord$y[,,k]),prj=prj,dis=dis)
    } else {
      cell_coord <- data.frame(x = c(cell_coord$x[,,k]), y = c(cell_coord$y[,,k]))
    }
    write.csv(na.omit(data.frame(x = c(cell_coord$x), y = c(cell_coord$y), value = c(array[,,k,l]))), file = file, row.names = FALSE)
    
  } else {
    stop('Please provide valid type.', call. = FALSE)
  }
}

#' Generic function to export GIS vector layers from RMODFLOW arrays
#' 
#' @rdname rmf_export_vector
#' @export
rmf_export_vector <- function(...) {
  UseMethod('rmf_export_vector')
}

#' Generic function to export vectors
#' 
#' @rdname rmf_export_vector
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
    new_positions <- rmf_convert_grid_to_xyz(x=positions$x,y=positions$y,prj=prj)
    positions$x <- new_positions$x
    positions$y <- new_positions$y
  }
  if(!is.null(crs)) {
    positions <- rmfi_convert_coordinates(positions,from=sp::CRS(prj$projection),to=crs)
  }
  
  positions_matrix_x <- matrix(positions$x,nrow=length(ids),ncol=4,byrow=TRUE)
  positions_matrix_y <- matrix(positions$y,nrow=length(ids),ncol=4,byrow=TRUE)
  positions_matrix <- cbind(ids,positions_matrix_x, positions_matrix_y)
  create_polygon_from_row <- function(dat) sp::Polygons(list(sp::Polygon(data.frame(x=dat[2:5],y=dat[6:9]))), ID = dat[1])
  polygons_list <- apply(positions_matrix, 1, create_polygon_from_row)
  
  
  #   # takes too long
  #   polygons_list <- list(length=length(ids))
  #   for(i in 1:length(ids)) {
  #     polygons_list[[i]] <- Polygons(list(Polygon(positions[which(positions$id==ids[i]),c('x','y')])), ID = ids[i])
  #   }
  
  # apply mask!!
  # add i, j to data.frame
  
  SP <- sp::SpatialPolygons(polygons_list, proj4string=sp::CRS(prj$projection))
  DF <- data.frame(value = c(t(array*mask^2)), row.names = ids)
  if(include_ijk) {
    ijk <- convert_modflow_id_to_ijk(1:(dis$nrow*dis$ncol), dis)
    DF$i <- ijk$i
    DF$j <- ijk$j
    DF$k <- ijk$k
  }
  ids_to_keep <- row.names(na.omit(DF))
  SPDF <- sp::SpatialPolygonsDataFrame(SP[which(1:(dis$nrow*dis$ncol) %in% ids_to_keep)], na.omit(DF))
  rgdal::writeOGR(SPDF, dsn = '.', layer = file, driver = type, overwrite_layer = TRUE)
}

#' Generic function to get model performance measures
#' 
#' @rdname rmf_performance
#' @export
rmf_performance <- function(...) {
  UseMethod('rmf_performance')
}

#' Get model performance measures
#'
#' @param sim numeric vector with simulated values
#' @param obs numeric vector with observed values corresponding to sim
#' @param na_value numeric; flags values to remove from the calculations; defaults to -888
#' @param measures character vector with the required performance measures. Possible values are any of the measures present in \code{\link{hydroGOF::gof}} + 'ssq' (sum of squared errors)
#' @param ... arguments passes to \code{\link{hydroGOF::gof}}
#' 
#' @return data.frame with performance measures
#' 
#' @rdname rmf_performance
#' @method rmf_performance default
#' @export
#'
rmf_performance.default <- function(sim, obs, na_value = -888, measures = c('ssq', 'mse', 'mae', 'me', 'r2', 'nse', 'rmse', 'pbias', 'kge'), ...) {
  
  obsAndSims <- data.frame(sim=sim, obs=obs)[which(sim!=na_value),]
  
  perform <- rmfi_performance_measures(obsAndSims$obs,obsAndSims$sim, measures = measures, ...)
  return(perform)
  
}

#' Get model performance measures from a hpr object
#' 
#' @param hpr head predictions file object
#' @param hobdry value used to flag dry cells; defaults to -888
#' @param measures character vector with the required performance measures. Possible values are any of the measures present in \code{\link{hydroGOF::gof}} + 'ssq' (sum of squared errors)
#' @param ... arguments passes to \code{\link{hydroGOF::gof}}
#' @return data.frame with performance measures
#'
#' @rdname rmf_performance
#' @method rmf_performance hpr
#' @export
rmf_performance.hpr <- function(hpr, hobdry = -888, measures = c('ssq', 'mse', 'mae', 'me', 'r2', 'nse', 'rmse', 'pbias', 'kge'), ...) {
  obsAndSims <- data.frame(simulated=hpr$simulated, observed=hpr$observed,name=hpr$name)[which(hpr$simulated!=hobdry),]
  observations <- obsAndSims$observed
  predictions <- obsAndSims$simulated
  dry <- 0; if(hobdry %in% predictions) dry <- length(which(predictions == hobdry))
  if(dry > 0) predictions <- predictions[-which(predictions == hobdry)]
  names <- obsAndSims$name
  perform <- rmfi_performance_measures(observations,predictions, measures = measures, ...)
  return(perform)
}

#' Read a GMS 2D grid file
#' 
#' \code{read_gms_2d_grid} reads in a GMS 2D grid file and returns it as an \code{\link{RMODFLOW}} gms2dgrid object.
#' 
#' @param file filename
#' @return object of class gms2dgrid
#' @export
rmf_read_gms_2d_grid <- function(file = {cat('Please select gms 2d grid file ...\n'); file.choose()}) {
  grid2d <- list()
  grid2d.lines <- readr::read_lines(file)
  #2dgrid.lines <- remove.comments.from.lines(mlt.lines)
  grid2d.lines <- grid2d.lines[-1]    
  grid2d$objtype <- as.character(strsplit(grid2d.lines[1],'\"')[[1]][2])
  grid2d.lines <- grid2d.lines[-1]
  grid2d.lines <- grid2d.lines[-1]
  grid2d$nd <- as.numeric(strsplit(grid2d.lines[1],' ')[[1]][3])
  grid2d.lines <- grid2d.lines[-1]
  grid2d$nc <- as.numeric(strsplit(grid2d.lines[1],' ')[[1]][3])    
  grid2d.lines <- grid2d.lines[-1]
  grid2d$n <- as.character(strsplit(grid2d.lines[1],'\"')[[1]][2])    
  grid2d.lines <- grid2d.lines[-1]    
  grid2d.lines <- grid2d.lines[-1] 
  
  grid2d$nddata <- as.numeric(grid2d.lines[1:grid2d$nd])
  grid2d$ncdata <- as.numeric(grid2d.lines[(1+grid2d$nd):(grid2d$nd + grid2d$nc)])              
  
  class(grid2d) <- 'gms_2d_grid'
  return(grid2d)
}

#' Read a MODFLOW array from a separate file. 
#'
#' \code{rmf_read_array} reads a MODFLOW array from a separate file. Binary and ASCII formats are supported
#' 
#' @param file filename to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read (3th dimension); defaults to 1
#' @param nstp number of timesteps in the array that should be read (4th dimension); defaults to 1
#' @param binary logical; is the array read from a binary file.
#' @param integer logical; does the array hold integer values. Only used for binary files. Might not work optimally.
#' @param header logical; should a MODFLOW style header be read for the array (see 'Details'). Defaults to TRUE if binary is TRUE and FALSE otherwise.
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of the binary file.
#'
#' @details \code{nrow}, \code{ncol}, \code{nlay}, \code{nstp} have to be specified if header is FALSE. They are used to dimension the array.
#'  
#'  The \code{integer} flag is only used when reading binary files.
#'  
#'  The header file consists of the following elements:
#'  \code{KSTP}, \code{KPER},\code{PERTIM},\code{DESC},\code{NCOL}, \code{NROW}, \code{ILAY} and \code{FMTIN}
#'  (see \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?frequently_asked_questions.htm} for their respective meaning.)
#'  The \code{FMTIN} element is not read for binary files.
#'  If a header is read, the values are set as attributes to the array.
#' 
#' @return a rmf_array with optional attributes if a header was read.
#' @export
#' @seealso \code{\link{rmf_write_array}}

rmf_read_array = function(file, nrow = NULL, ncol = NULL, nlay=1, nstp=1, binary = F, integer = F, header = ifelse(binary, TRUE, FALSE), precision = 'single') {
  
  if(!header) {
    if(is.null(nrow) || is.null(ncol) || is.null(nlay) || is.null(nstp)) {
      stop('Either provide nrow, ncol, nlay and nstp or set header to TRUE', call. = FALSE)
    }
  }
  
  if(binary) { # Binary
    
    read_binary = function() {
      real_number_bytes <- ifelse(precision == 'single', 4, 8)
      type <- ifelse(integer, 'integer', 'numeric')
      if(header) {
        
        kstp <- readBin(con,what='integer',n=1)
        kper <- readBin(con,what='integer',n=1)
        pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
        totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
        desc <- readChar(con,nchars=16)
        
        stp_nr <- 0
        
        while(length(desc != 0)) {
          
          ncol <- readBin(con, what = 'integer', n = 1)
          nrow <- readBin(con, what = 'integer', n = 1)
          ilay <- abs(readBin(con, what = 'integer', n = 1)) # abs for XSECTION
          
          if(stp_nr == 0) { # initialize 3d array
            arr <- aperm(array(readBin(con,what=type,n = ncol * nrow, size = ifelse(integer, NA_integer_, real_number_bytes)),dim=c(ncol, nrow, 1)), c(2, 1, 3))
            kstp_attr <- kper_attr <- pertim_attr <- totim_attr <- desc_attr <- ncol_attr <- nrow_attr <- ilay_attr <- NULL
          } else { # read (abind drops attributes)
            arr <- abind::abind(arr, 
                                aperm(array(readBin(con,what=type,n = ncol * nrow, size = ifelse(integer, NA_integer_, real_number_bytes)),dim=c(ncol, nrow)), c(2, 1)),
                                along = 3)
          }
          
          # stp_nr only increases after each layer loop
          if(ilay == 1) {
            stp_nr <- stp_nr+1
            kstp_attr[stp_nr] <- kstp
            kper_attr[stp_nr] <- kper
            pertim_attr[stp_nr] <- pertim
            totim_attr[stp_nr] <- totim
            desc_attr[stp_nr] <- desc
            ncol_attr[stp_nr] <- ncol
            nrow_attr[stp_nr] <- nrow
          }
          # outside if-statement; so it will be equal to nlay; similar to rmf_read_hed
          ilay_attr[stp_nr] <- ilay 
          
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- readChar(con,nchars=16)
        }
        
      } else {
        arr <- array(NA, dim = c(nrow, ncol, nlay, nstp))
        
        for(stp_nr in 1:nstp) {
          for(ilay in 1:nlay) {
            arr[,,ilay,stp_nr] <- aperm(array(readBin(con,what=type,n = ncol * nrow, size = ifelse(integer, NA_integer_, real_number_bytes)),dim=c(ncol, nrow)), c(2, 1))
          }
        }
      } 
      return(arr)
    }
    con <- file(file,open='rb')
    arr = try(read_binary())
    close(con)
    
  } else { # ASCII
    lines <- readr::read_lines(file)
    
    if(header) {
      stp_nr <- 0
      
      while(length(lines) != 0) {
        variables <- rmfi_remove_empty_strings(strsplit(lines[1],' ')[[1]])
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        lines <- lines[-1]
        
        data_set <- rmfi_parse_array(lines,nrow,ncol,1, skip_header = TRUE)
        
        if(stp_nr == 0) { # initialize 3d array
          arr <- array(data_set$array, dim = c(dim(data_set$array), 1))
          kstp_attr <- kper_attr <- pertim_attr <- totim_attr <- desc_attr <- ncol_attr <- nrow_attr <- ilay_attr <- NULL
        } else { # read (abind drops attributes)
          arr <- abind::abind(arr, data_set$array, along = 3)
        }
        lines <- data_set$remaining_lines
        
        # stp_nr only increases after each layer loop
        if(ilay == 1) {
          stp_nr <- stp_nr+1
          kstp_attr[stp_nr] <- kstp
          kper_attr[stp_nr] <- kper
          pertim_attr[stp_nr] <- pertim
          totim_attr[stp_nr] <- totim
          desc_attr[stp_nr] <- desc
          ncol_attr[stp_nr] <- ncol
          nrow_attr[stp_nr] <- nrow
        }
        # outside if-statement; so it will be equal to nlay; similar to rmf_read_hed
        ilay_attr[stp_nr] <- ilay 
      }
      
    } else {
      arr <- array(NA, dim = c(nrow, ncol, nlay, nstp))
      
      for(stp_nr in 1:nstp)  {
        for(ilay in 1:nlay) {
          data_set <- rmfi_parse_array(lines,nrow,ncol,1, skip_header = TRUE)
          arr[,,ilay,stp_nr] <- data_set$array
          lines <- data_set$remaining_lines
        }
      }
    }
    if(integer) arr <- apply(arr, MARGIN = 1:length(dim(arr)), function(i) as.integer(i))
  }
  
  if(header) {
    # create list for each time step; abind to 4d array
    if(stp_nr > 1) {
      arr <- abind::abind(lapply(seq(1,dim(arr)[3],ilay), function(i) rmfi_ifelse0(ilay==1,arr,arr[,,i:(i+ilay-1)])), along = 4)
    }else {
      arr <- array(arr, dim = c(dim(arr), 1))
    }
  }
  nrow <- dim(arr)[1]
  ncol <- dim(arr)[2]
  nlay <- dim(arr)[3]
  nstp <- dim(arr)[4]
  
  # Set class of object (2darray; 3darray; 4darray)
  if(length(which(c(nrow,ncol,nlay,nstp) !=1 )) <= 1) {
    arr <- c(array(arr,dim=nrow*ncol*nlay*nstp))
  } else if(nrow !=1 && ncol !=1 && nlay == 1 && nstp == 1) {
    arr <- arr[,,1,1]
    class(arr) <- 'rmf_2d_array'   
  } else if(nstp != 1) {
    class(arr) <- 'rmf_4d_array'
  } else {
    arr <- arr[,,,1]
    class(arr) <- 'rmf_3d_array'
  }
  
  if(header) {
    
    attr(arr, 'dimnames') <- NULL
    attr(arr, 'kstp') <- kstp_attr
    attr(arr, 'kper') <- kper_attr
    attr(arr, 'pertim') <- pertim_attr
    attr(arr, 'totim') <- totim_attr
    attr(arr, 'desc') <- desc_attr
    attr(arr, 'ncol') <- ncol_attr
    attr(arr, 'nrow') <- nrow_attr
    attr(arr, 'ilay') <- ilay_attr
    
    no_data <- which(is.na(attr(arr, 'kstp')))
    if(length(no_data) != 0) {
      arr <- arr[,,,-no_data]
      attr(arr, 'kstp') <- attr(arr, 'kstp')[-no_data]
      attr(arr, 'kper') <- attr(arr, 'kper')[-no_data]
      attr(arr, 'pertim') <- attr(arr, 'pertim')[-no_data]
      attr(arr, 'totim') <- attr(arr, 'totim')[-no_data]
      attr(arr, 'desc') <- attr(arr, 'desc')[-no_data]
      attr(arr, 'ncol') <- attr(arr, 'ncol')[-no_data]
      attr(arr, 'nrow') <- attr(arr, 'nrow')[-no_data]
      attr(arr, 'ilay') <- attr(arr, 'ilay')[-no_data]
    }
  }
  
  return(arr)
  
}

#' Add rmf list class to data.frame and check if k, i and j columns are present
#' 
#' @param df data.frame that holds at least the k, i and j columns that specify cell indices as well as additional variables related to the boundary condition package.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @details 
#'      rmf_lists represent List Data input (and output) as used by MODFLOW. rmf_lists are used to define stress period input for boundary condition packages; 
#'      to define parameters and to read certain types of output in the the cell-by-cell budget file. A MODFLOW List Data can be viewed as discrete spatial features in contrast to the MODFLOW array data type which represents continuous data.
#'      
#'      A rmf_list is a dataframe with at least 3 integer columns k, i and j, that may contain repeated values.
#'      
#' @return an object of class \code{rmf_list} and \code{data.frame}
#' @export

rmf_create_list <-  function(df, kper = NULL) {
  
  df <- as.data.frame(df)
  if(any(!(c('k','i','j') %in% names(df)))) stop('Please set names of the kij columns to k, i and j', call. = FALSE)
  
  attr(df, 'kper') <- kper  
  class(df) <- c('rmf_list', class(df))
  return(df)
  
}

#' @export
as.data.frame.rmf_list <- function(obj) as.data.frame.data.frame(structure(obj, kper = NULL))

#' Read a projection file
#' 
#' \code{read_prj} reads in projection file and returns it as a prj object.
#' 
#' @param file filename; typically '*.prj'
#' @return object of class prj
#' @export
rmf_read_prj <- function(file = {cat('Please select prj file ...\n'); file.choose()}) {
  prj.lines <- readr::read_lines(file)
  prj <- list()
  prj$projection <- prj.lines[1]
  prj$origin <- as.numeric(RMODFLOW:::rmfi_remove_empty_strings(strsplit(prj.lines[2],' ')[[1]]))
  prj$rotation <- as.numeric(RMODFLOW:::rmfi_remove_empty_strings(strsplit(prj.lines[3],' ')[[1]])[1])
  if(length(prj.lines) > 3) prj$starttime <- as.POSIXct(prj.lines[4])
  class(prj) <- 'prj'
  return(prj)
}

#' Write an RMODFLOW projection file
#' 
#' \code{write.prj} writes a projection file
#' 
#' @param prj an \code{\link{RMODFLOW}} prj object
#' @param file filename to write to; typically '*.prj'
#' @export
rmf_write_prj <- function(prj,
                          file = {cat('Please select prj file to overwrite or provide new filename ...\n'); file.choose()}) {
  cat(paste0(prj$projection,'\n'), file=file)
  cat(paste0(paste0(prj$origin,collapse=' '),'\n'), file=file, append=TRUE)
  cat(paste0(prj$rotation,'\n'), file=file, append=TRUE)
  if(length(prj) > 3) cat(paste0(format(prj$starttime,format='%Y-%m-%d %H:%M:%S'),'\n'), file=file, append=TRUE)
}
