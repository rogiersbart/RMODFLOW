
#' Bilinear interpolation on a rectilinear grid
#'
#' @param x x coordinates of known points 
#' @param y y coordinates of known points
#' @param f values at known poins (length 4). Order: bottom-left, bottom-right, top-left, top-right; using R's column-major ordering
#' @param xout x coordinate of point to interpolate
#' @param yout y coordinate of point to interpolate
#'
#' @return single bilinear interpolated value 
#' @keywords internal
#'
rmfi_bilinear_intp <- function(x, y, f, xout, yout) {
  x2 <- max(x)
  x1 <- min(x)
  y2 <- max(y)
  y1 <- min(y)
  
  # a <- 1 / ((x2-x1)*(y2-y1))
  # value <- a * matrix(c(x2 - xout, xout - x1), nrow = 1) %*% matrix(f, nrow = 2) %*% matrix(c(y2 - yout, yout - y1), ncol = 1)
  
  wx <- (xout - x1) / (x2 - x1)
  wy <- (yout - y1) / (y2 - y1)
  
  value <- (1-wx)*wy*f[1] + wx*wy*f[2] + (1-wx)*(1-wy)*f[3] + wx*(1-wy)*f[4]
  return(c(value))
}

#' Trilinear interpolation on a rectilinear grid
#'
#' @param x x coordinates of known points 
#' @param y y coordinates of known points
#' @param z z coordinates of known points
#' @param f values at known poins (length 8). Order: upper:bottom-left, bottom-right, top-left, top-right; bottom:bottom-left, bottom-right, top-left, top-right; using R's column-major ordering
#' @param xout x coordinate of point to interpolate
#' @param yout y coordinate of point to interpolate
#' @param zout z coordinate of points to interpolate
#'
#' @return single trilinear interpolated value 
#' @keywords internal
#'
rmfi_trilinear_intp <- function(x, y, z, f, xout, yout, zout) {
  
  x2 <- max(x)
  x1 <- min(x)
  y2 <- max(y)
  y1 <- min(y)
  z2 <- max(z)
  z1 <- min(z)
  
  wx <- (xout - x1) / (x2 - x1)
  wy <- (yout - y1) / (y2 - y1)
  wz <- (zout - z1) / (z2 - z1)
  
  value <- (1-wx)*wy*(1-wz)*f[1] + wx*wy*(1-wz)*f[2] + (1-wx)*(1-wy)*(1-wz)*f[3] + wx*(1-wy)*(1-wz)*f[4] +
    (1-wx)*wy*wz*f[5] + wx*wy*wz*f[6] + (1-wx)*(1-wy)*wz*f[7] + wx*(1-wy)*(1-wz)*f[8]
  
  return(c(value))
}

#' Create sequence of confining bed indicators
#'
#' @param dis \code{RMODFLOW} dis object
#' @return vector of length \code{dis$nlay + confining beds} indicating indicating if the index represents a confining bed
#' @details When confining beds are present, \code{dis$botm} has \code{dis$nlay + number of confining beds} layers. 
#'          This functions returns a logical vector indicating which of those layers is a confining bed. This is useful 
#'          when handling calculations with \code{dis$botm}, e.g. calculating thicknesses.
#' @keywords internal
#'
rmfi_confining_beds <- function(dis) {
  nnlay <- dis$nlay + sum(dis$laycbd != 0)
  cbd <- rep(0, nnlay)
  cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
  return(cbd)
}

#' Convert data frame coordinates to another coordinate reference system
#' 
#' @param dat data frame with x and y (and/or z) coordinates
#' @param from coordinate reference system of the data
#' @param to target coordinate reference system
#' @return data frame with converted coordinates
#' @keywords internal
rmfi_convert_coordinates <- function(dat, from, to) {
  nrs <- which(!is.na(apply(dat, 1, sum)))
  if(is.na(sf::st_crs(from)) || is.na(sf::st_crs(to))) stop('crs can not be NA when transforming', call. = FALSE)
  converted_coords <- sf::st_transform(sf::st_as_sf(dat[nrs,], coords = colnames(dat), crs = sf::st_crs(from)), crs = sf::st_crs(to))
  converted_coords <- as.data.frame(sf::st_coordinates(converted_coords))
  colnames(converted_coords) <- colnames(dat)
  dat[nrs,] <- converted_coords
  
  return(dat)
}

#' Convert a huf object to an rmf_3d_array with the number of numerical layers per hydrogeological unit
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @param bas bas object, corresponding to the huf object; defaults to NULL
#' @return nlay rmf_3d_array
#' @keywords internal
rmfi_convert_huf_to_nlay <- function(huf, dis, bas = NULL) {
  nlay <- huf$top * 0
  huf_coordinates <- rmf_cell_coordinates(huf, dis = dis, include_faces = TRUE)
  if(any(dis$laycbd != 0)) {
    warning('Quasi-3D confining beds detected; adding their thickness to the overlying layer.')
    cbd <- rmfi_confining_beds(dis)
    if((dis$nlay + sum(dis$laycbd != 0)) > 1) dis$botm[,,which(cbd > 0) - 1] <- dis$botm[,,which(cbd > 0)]
    dis$laycbd <- rep(0, dis$nlay)
  }
  dis_coordinates <- rmf_cell_coordinates(dis, include_faces = TRUE)
  ibound <- rmfi_ifelse0(is.null(bas), dis$botm*0 + 1, abs(bas$ibound))
  for(i in 1:huf$nhuf) {
    for(j in 1:dis$nlay) {
      nlay[,,i] <- nlay[,,i] + (!(dis_coordinates$upper[,,j] < huf_coordinates$lower[,,i] | dis_coordinates$lower[,,j] > huf_coordinates$upper[,,i])) * ibound[,,j]
    }
  }
  return(nlay)
}

#' Set array input for a MODFLOW boundary condition package
#'
#' @param arg list of (1) \code{rmf_2d_array's} and/or rmf_parameter array objects or (2) a single nested \code{list} with \code{rmf_2d_array's} and/or rmf_parameter elements or (3) a \code{matrix}; defines the boundary condition input. 
#' @param dis dis object. If not explicitely suplied, the function will look in the arg argument for an object of class 'dis'.
#' @details typically, \code{arg} is \code{list(...)} where the ellipsis contains all the input \code{rmf_arrays} for the \code{rmf_create_*} function. When matrix elements are present, they are coerced to rmf_2d_arrays which are active for all stress-periods with a warning.
#' @return list with the parameters, input arrays and the kper argument
#' @keywords internal
#' @seealso \code{\link{rmfi_create_bc_list}}, \code{\link{rmfi_write_bc_list}}, \code{\link{rmfi_parse_bc_list}}, \code{\link{rmfi_parse_array_parameters}}, \code{\link{rmfi_write_array_parameters}}

rmfi_create_bc_array <- function(arg, dis) {
  
  # find dis
  if(missing(dis)) {
    dis_present <- vapply(arg, function(i) 'dis' %in% class(i), TRUE)
    if(any(dis_present)) {
      dis <- arg[dis_present][[1]]
      arg <- arg[!dis_present]
    } else {
      stop('Please provide a dis argument', call. = FALSE)
    }
  }
  
  # if arg is nested list, unnest
  if(length(arg) == 1 && inherits(arg[[1]], 'list')) arg <- arg[[1]] 
  # if matrix or 2d-array, make rmf_2d_array which is always active
  # arg <- lapply(arg, function(i) rmfi_ifelse0(inherits(i, 'matrix') && !(inherits(i, 'rmf_2d_array')), 
  #                                             {warning("Coercing matrix to rmf_2d_array; array active for all stress-periods.", call. = FALSE); rmf_create_array(i, kper = 1:dis$nper)},
  #                                             i) )
  
  
  # check for parameters and/or arrays and name them
  parameters <- arg[vapply(arg, function(i) inherits(i, 'rmf_parameter'), TRUE)]
  if(length(parameters) > 0) names(parameters) <- vapply(parameters, function(i) attr(i, 'parnam'), 'text')
  
  arrays <- arg[vapply(arg, function(i) !inherits(i, 'rmf_parameter'), TRUE)]
  if(length(arrays) > 0) {
    names(arrays) <- vapply(seq_along(arrays), function(i) rmfi_ifelse0(is.null(attr(arrays[[i]], 'parnam')), paste('array', i, sep = '_'), attr(arrays[[i]], 'parnam')), 'text')
  }
  
  if(any(vapply(c(parameters, arrays), function(i) is.null(attr(i, 'kper')), TRUE))) {
    stop('Please make sure all rmf_2d_array and rmf_parameter objects have a kper attribute', call. = FALSE)
  }
  
  # stress period data frame
  kper <- cbind(data.frame(kper = 1:dis$nper),
                matrix(FALSE, dis$nper, length(unique(c(names(parameters), names(arrays)))), dimnames = list(NULL, unique(c(names(parameters), names(arrays))))))
  
  if(length(parameters) > 0) {
    for(i in 1:length(parameters)) {
      if(is.null(attr(parameters[[i]], 'instnam'))) {
        kper[attr(parameters[[i]], 'parnam')] <-  c(1:dis$nper) %in% attr(parameters[[i]],'kper')
      } else {
        kper[c(1:dis$nper) %in% attr(parameters[[i]],'kper'), attr(parameters[[i]], 'parnam')] <-  attr(parameters[[i]],'instnam')
      }
    }
  }
  
  if(length(arrays) > 0) {
    for(i in 1:length(arrays)) {
      kper[names(arrays)[i]] <- c(1:dis$nper) %in% attr(arrays[[i]],'kper')
    }
  }
  
  # dimensions
  np <- 0
  parameter_values <- NULL
  instances <- NULL
  
  if(length(parameters) > 0) {
    np <- length(unique(names(parameters)))
  }
  
  # parameters
  if(length(parameters) > 0) {
    
    instances <- c(table(names(parameters)))
    instances[] <- vapply(seq_along(instances), function(i) rmfi_ifelse0(instances[i] < 2 && is.null(attr(parameters[[names(instances[i])]], 'instnam')), 0, instances[i]), 1)
    if(all(instances == 0)) instances <- NULL
    
    parameter_values <- vapply(parameters, function(i) attr(i, 'parval'), 1.0)
    if(!is.null(instances)) {
      parameter_values <- parameter_values[!duplicated(names(parameter_values))]
      
      # if parameter is time-varying, list all instances
      inst_l <- vapply(parameters, function(i) !is.null(attr(i, 'instnam')), TRUE)
      inst <- parameters[inst_l]
      p_names <- vapply(inst, function(i) attr(i, 'parnam'), 'text')
      unq_names <- unique(p_names)
      names(inst) <- vapply(inst, function(i) attr(i, 'instnam'), 'text')
      
      p_inst <- lapply(seq_along(unq_names), function(i) inst[which(p_names == unq_names[i])])
      names(p_inst) <- unq_names
      parameters <- c(parameters[!inst_l], p_inst)
    }
  }
  
  # check for wrong combinations of parameters and arrays in stress period
  # if parameters are defined: only parameters can be used for a stress period and there must be at least 1 parameter active
  #
  if(length(parameters) > 0) {
    if(length(arrays) > 0) {
      warning('Parameter and non-parameter recharge arrays present. Only parameter arrays will be used.', call. = FALSE)
      kper[,which(colnames(kper) %in% names(arrays))] <- FALSE
    }
    
    parm_df <- subset(kper, select = names(parameters))
    parm_err <- any(vapply(1:dis$nper, function(i) all(is.na(parm_df[i,]) | parm_df[i,] == FALSE), TRUE))
    if(parm_err) stop('If parameter arrays are provided, please make sure at least 1 parameter array is active during each stress period.', call. = FALSE)
  }
  # multiple non-parameter arrays can not be active for the same stress period
  if(length(arrays) > 0) {
    select <- rmfi_ifelse0(length(parameters > 0), names(kper) != names(parameters), names(kper))
    nparm_df <- subset(kper, select = select[-1])
    nparm_err <- vapply(1:dis$nper, function(i) sum(is.na(nparm_df[i,]) | nparm_df[i,] == TRUE) > 1, TRUE)
    if(any(nparm_err)) stop('There can be only 1 active non-parameter array per stress period. Stress period(s) ', paste0(which(nparm_err), collapse = ' '), ' have multiple active arrays.', call. = FALSE)
  }
  
  # check if any kper are active
  if(!(any(is.na(as.logical(c(unlist(kper[,-1]))))) || any(as.logical(c(unlist(kper[,-1])))))) warning('No boundary condition features are active. Perhaps reset the kper attribute?', call. = FALSE)
  
  # combine
  data <- c(parameters, arrays)
  return(list(np = np,
              instances = instances,
              parameter_values = parameter_values,
              data = data, kper = kper))
  
}

#' Set list input for a MODFLOW boundary condition package
#'
#' @param arg list of (1) rmf_list and/or rmf_parameter list objects or (2) a single nested \code{list} with rmf_list and/or rmf_parameter elements or (3) a \code{data.frame} that will be coerced to a rmf_list; defines the boundary condition input. 
#' @param dis dis object. If not explicitely suplied, the function will look in the arg argument for an object of class 'dis'.
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk)
#' @param aux optional character vector with the names of the auxiliary variables
#' @details typically, \code{arg} is \code{list(...)} where the ellipsis contains all the input \code{rmf_lists} for the \code{rmf_create_*} function. All elements should have corresponding columns.
#' 
#' @return list with the data, possible parameter values, dimensions and the kper data.frame
#' @keywords internal
#' @seealso \code{\link{rmfi_create_bc_array}}, \code{\link{rmfi_write_bc_list}}, \code{\link{rmfi_parse_bc_list}}

rmfi_create_bc_list <- function(arg, dis, varnames, aux = NULL) {
  
  var_cols <- 4:(3+length(varnames))
  
  # find dis
  if(missing(dis)) {
    dis_present <- vapply(arg, function(i) 'dis' %in% class(i), TRUE)
    if(any(dis_present)) {
      dis <- arg[dis_present][[1]]
      arg <- arg[!dis_present]
    } else {
      stop('Please provide a dis argument', call. = FALSE)
    }
  }
  
  # if arg is nested list, unnest
  if(length(arg) == 1 && inherits(arg[[1]], 'list')) arg <- arg[[1]] 
  # if data.frame, make rmf_list which is always active
  # arg <- lapply(arg, function(i) rmfi_ifelse0(inherits(i, 'data.frame') && !(inherits(i, 'rmf_list')), 
  #                                             {warning("Coercing data.frame to rmf_list; list active for all stress-periods", call. = FALSE); rmf_create_list(i, kper = 1:dis$nper)}, 
  #                                             i) )
  
  # check if all varnames are present (partial string matching)
  nms_check <- lapply(arg, function(i) pmatch(colnames(i), c('k', 'i', 'j', varnames)))
  if(any(vapply(nms_check, function(i) sum(!is.na(i)) != c(3 + length(varnames)), TRUE))) stop('Please make sure all rmf_list objects have columns k, i, j, ', paste(varnames, collapse = ', '), call. = FALSE)
  arg <- lapply(seq_along(arg), function(i) {x <- arg[[i]][,order(nms_check[[i]])];
                                             attributes(x) <- c(attributes(x), attributes(arg[[i]])[which(!(names(attributes(arg[[i]])) %in% names(attributes(x))))])
                                             x}) # re-order; reset dropped attributes
  arg <- lapply(seq_along(arg), function(i) setNames(arg[[i]], replace(colnames(arg[[i]]), var_cols, varnames)))
  
  # check for parameters and/or lists and name them
  parameters <- arg[vapply(arg, function(i) inherits(i, 'rmf_parameter'), TRUE)]
  if(length(parameters) > 0) names(parameters) <- vapply(parameters, function(i) attr(i, 'parnam'), 'text')
  lists <- arg[vapply(arg, function(i) !inherits(i, 'rmf_parameter'), TRUE)]
  if(length(lists) > 0) names(lists) <- paste('list', 1:length(lists), sep = '_')
  if(any(vapply(c(parameters, lists), function(i) is.null(attr(i, 'kper')), TRUE))) {
    stop('Please make sure all rmf_list and rmf_parameter objects have a kper attribute', call. = FALSE)
  }
  
  # stress period data frame
  kper <- cbind(data.frame(kper = 1:dis$nper),
                matrix(FALSE, dis$nper, length(unique(c(names(parameters), names(lists)))), dimnames = list(NULL, unique(c(names(parameters), names(lists))))))
  
  if(length(parameters) > 0) {
    for(i in 1:length(parameters)) {
      if(is.null(attr(parameters[[i]], 'instnam'))) {
        kper[attr(parameters[[i]], 'parnam')] <-  c(1:dis$nper) %in% attr(parameters[[i]],'kper')
      } else {
        kper[c(1:dis$nper) %in% attr(parameters[[i]],'kper'), attr(parameters[[i]], 'parnam')] <-  attr(parameters[[i]],'instnam')
      }
    }
  }
  
  if(length(lists) > 0) {
    for(i in 1:length(lists)) {
      kper[names(lists)[i]] <- c(1:dis$nper) %in% attr(lists[[i]],'kper')
    }
  }
  
  # dimensions
  np <- 0
  mxl <- 0
  itmp <- 0
  parameter_values <- NULL
  instances <- NULL
  
  if(length(parameters) > 0) {
    np <- length(unique(names(parameters)))
    mxl <- sum(vapply(parameters[!duplicated(names(parameters))], nrow, 1))
  } 
  
  find_mxact <- function(i) {
    nms <- names(kper)[-1]
    kper_names <- nms[which(kper[i,-1] == T)] 
    sum(unlist(lapply(parameters[kper_names], nrow))) +
      sum(unlist(lapply(lists[kper_names], nrow)))
  }
  mxact <- max(vapply(kper$kper, find_mxact, 1))
  
  
  # parameters
  if(length(parameters) > 0) {
    
    instances <- c(table(names(parameters)))
    instances[] <- vapply(seq_along(instances), function(i) rmfi_ifelse0(instances[i] < 2 && is.null(attr(parameters[[names(instances[i])]], 'instnam')), 0, instances[i]), 1)
    if(all(instances == 0)) instances <- NULL
    
    parameter_values <- vapply(parameters, function(i) attr(i, 'parval'), 1.0)
    if(!is.null(instances)) parameter_values <- parameter_values[!duplicated(names(parameter_values))]
    
    #check aux
    if(!is.null(aux)) {
      all_aux <- all(vapply(lists, function(i) all(aux %in% colnames(i)), TRUE))
      if(!all_aux) stop('Please make sure all AUX variables are defined in each rmf_list', call. = FALSE)
    }
    
    set_parm <- function(i) {
      instnam <- attr(i, 'instnam')
      i$parameter <- TRUE
      i$name <- attr(i, 'parnam')
      return(structure(i, instnam = instnam))
    }
    
    # set parameter df
    parameters <- lapply(parameters, set_parm)
    
    # time-varying
    if(any(vapply(parameters, function(i) !is.null(attr(i, 'instnam')), TRUE))) {
      parameters <- lapply(parameters, function(i) {rmfi_ifelse0(is.null(attr(i, 'instnam')), i$instance <-  NA, i$instance <-  attr(i, 'instnam')); i} )
    }
    
    parameters <- do.call(rbind, unname(parameters))
    
  }
  
  # lists
  if(length(lists) > 0) {
    
    #check aux
    if(!is.null(aux)) {
      all_aux <- all(vapply(lists, function(i) all(aux %in% colnames(i)), TRUE))
      if(!all_aux) stop('Please make sure all AUX variables are defined in each rmf_list', call. = FALSE)
    }
    
    # itmp
    itmp <- structure(vapply(lists, nrow, 1), names = names(lists))
    
    # set lists df
    lists <- lapply(lists, function(i) {i$parameter = FALSE; i})
    lists <- lapply(seq_along(lists), function(i) {lists[[i]]$name <- names(lists)[[i]]; lists[[i]]})
    lists <- do.call(rbind, unname(lists))
    if(length(parameters) > 0 && 'instance' %in% colnames(parameters))  lists$instance <-  NA
    
  }
  
  # check if any kper are active
  if(!(any(is.na(as.logical(c(unlist(kper[,-1]))))) || any(as.logical(c(unlist(kper[,-1])))))) warning('No boundary condition features are active. Perhaps reset the kper attribute?', call. = FALSE)
  
  # combine
  data <- structure(rbind(parameters, lists), kper = NULL)
  
  return(list(np = np,
              mxl = mxl,
              instances = instances,
              mxact = mxact,
              itmp = itmp,
              parameter_values = parameter_values,
              data = data,
              kper = kper))
  
}

#' Returns character lengths per value from a FORTRAN format
#'
#' @param format character; FORTRAN format within parentheses as read from a MODFLOW array header
#'
#' @return integer vector with the number of characters per value as specified by the FORTRAN format
#' @keywords internal
#'
rmfi_fortran_format <- function(format) {
  
  # take only what is within first and last parentheses
  if(grepl('\\(', format) && grepl('\\)', format)) {
    splt <- strsplit(format, '')[[1]]
    fp <- grep('\\(', splt)[1]
    lp <- grep('\\)', splt)
    lp <- lp[length(lp)]
    format <- paste0(splt[fp:lp], collapse = '')
  }
  
  # remove outer parentheses
  format <- trimws(toupper(format))
  format <- gsub('^\\(|\\)$','',format)
  
  # remove possible apostrophes
  format <- gsub('\"|\'|´|`','',format)
  
  # remove kP format
  format <- gsub('[0-9]*P', '', format)
  
  # split on commas for multiple formats
  format <- strsplit(format, split = ',')[[1]]
  
  # template
  template <- "^([0-9]*)([[:upper:]]+)([0-9]*)\\.?([0-9]*)"
  
  # function to get lengths
  get_lengths <- function(format) {
    
    # optional first multipliers
    split <- rmfi_remove_empty_strings(strsplit(format, split = '\\(|\\)')[[1]])
    format <- split[length(split)]
    split <- split[-length(split)]
    reps <- prod(as.numeric(split))

    # optional spaces
    # TODO spaces after format (allowed?)
    spaces <- strsplit(format, split='X')[[1]]
    format <- spaces[length(spaces)]
    spaces <- spaces[-length(spaces)]
    spaces <- sum(as.numeric(spaces))

    # format: repeats
    reps2 <- as.numeric(sub(template, "\\1", format))
    reps2[is.na(reps2)] <- 1L
    reps <- reps * reps2
    
    # format: lengths
    lengths <- as.numeric(sub(template, "\\3", format))
    lengths[is.na(lengths)] <- 1L
    lengths <- sum(lengths, spaces)
    
    return(rep(lengths, reps))
    
  }
  
  # apply function to every format and append results
  lengths <- unlist(lapply(format, get_lengths))
  
  return(lengths)
  
}

#' Calculate a geometric mean
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{rmfi_harmean}} and \code{\link{mean}}
#' @keywords internal
rmfi_geomean <- function(x, ...) {
  return(prod(x, ...) ^ (1 / length(x)))
}

#' Calculate a harmonic mean
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{mean}}
#' @seealso \code{\link{rmfi_geomean}} and \code{\link{mean}}
#' @keywords internal
rmfi_harmean <- function(x, ...) {
  return(1 / (mean(1 / x, ...)))
}

#' Conditional return
#' 
#' \code{rmfi_ifelse0} returns \code{yes} if \code{test} is \code{TRUE}. If \code{test} is \code{FALSE}, it returns \code{no}.
#' @param test an object which can be coerced to logical mode.
#' @param yes return value for \code{test==TRUE}
#' @param no return value for \code{test==FALSE}
#' @keywords internal
rmfi_ifelse0 <- function(test, yes, no) {
  if(test)   {
    return(yes)
  } else {
    return(no)
  }
}

#' List supported MODFLOW packages
#'
#' @param type character denoting type of packages to list; possible values are \code{'all' (default), 'basic', 'flow', 'boundary', 'solver', 'oc', 'sub', 'obs', 'swr', 'cfp', 'farm', 'cbc', 'output'}
#'
#' @return data.frame with ftype and rmf columns denoting the MODFLOW and \code{RMODFLOW} abbreviations for the requested packages
#' @keywords internal
#' @details 'cbc' returns all packages which allow a i*cbc flag to be set which is a flag and unit number for writing cell-by-cell flow data. 'output' lists all supported output types.
#' @note this function should be updated every time a new MODFLOW package is supported in \code{RMODFLOW}
rmfi_list_packages <- function(type = 'all') {
  
  # update rmfd_supported_packages in /data-raw/ when a new package is supported
  # NAM file is not in here but is supported

  df <- rmfd_supported_packages
  
  # Below is an exhaustive overview of all packages in MODFLOW-2005 & variants
  # basic
  basic <- c('dis', 'bas', 'nam', 'mlt', 'zon', 'pval', 'lgr')
  
  # flow packages
  flow <- c('bcf', 'lpf', 'huf', 'swi', 'hfb', 'uzf', 'upw', 'kdep', 'lvda')
  
  # boundary conditions
  boundary <- c('chd', 'fhb', 'rch', 'wel', 'drn', 'drt', 'ets', 'evt', 'ghb', 'lak', 'mnw1', 'mnw2', 'res', 'riv', 'sfr', 'str', 'bfh', 'rip')
  
  # solver
  solver <- c('de4', 'gmg', 'lmg', 'pcg', 'pcgn', 'sip', 'nwt')
  
  # output control
  oc <- c('gage', 'hyd', 'lmt', 'mnwi', 'oc')
  
  # subsidence
  sub <- c('ibs', 'sub', 'swt') 
  
  # observation
  obs <- c('chob', 'drob', 'gbob', 'hob', 'rvob', 'stob')
  
  # Surface-water routing
  swr <- c('swr', 'sswrlstrd', 'sswr_rdtabdata', 'sswr_rddro')
  
  # Conduit flow process
  cfp <- c('cfp', 'crch', 'coc')
  
  # Farm process
  farm <- c('fmp')
  
  # cbc (uzf and swi need to be set separately)
  cbc <- c('bcf', 'lpf', 'huf', 'upw', 'uzf', 'swi', 'fhb', 'rch', 'wel', 'drn', 'drt', 'ets', 'evt', 'ghb', 'lak', 'mnw1', 'mnw2', 'res', 'riv', 'sfr', 'str', 'ibs', 'sub', 'swt', 'swr', 'rip')
  
  # output
  if(type == 'output') {
    df <- rmfd_supported_output
  } else if(type != 'all') {
    # subset type
    df <- subset(df, rmf %in% get(type))
  }

  return(df)
  
}

#' Get an array specified by a control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param remaining_lines lines to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read
#' @param ndim dimensions of the array to read; either 1, 2 or 3. Denotes the if the returned array should be 1D, 2D or 3D.
#' @param fmt optional; character with a proper FORTRAN format enclosed in parentheses. Only used when skip_header = TRUE and a fixed-format array needs to be read, i.e. output arrays. 
#' @param skip_header optional; should the control record be skipped
#' @param nam a \code{RMODFLOW} nam object. Required when reading fixed-format or EXTERNAL arrays
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of binary files
#' @param file pathname to the MODFLOW input file which is currently being read. Required when reading fixed-format or OPEN/CLOSE arrays
#' @param integer logical; does the binary array hold integer values. Might not work optimally.
#' @param ... ignored
#' @return A list containing the array and the remaining text of the MODFLOW input file
#' @keywords internal
rmfi_parse_array <- function(remaining_lines,nrow,ncol,nlay, ndim, fmt = NULL,
                             skip_header = FALSE, nam = NULL, precision = "single", file = NULL, integer = FALSE, ...) {
  
  # Initialize array object
  array <- array(dim=c(nrow,ncol,nlay))
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(array))!=0)
  {
    for(k in 1:nlay) 
    { 
      fortranfmt <- FALSE
      
      # CONSTANT
      if(toupper(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]) == 'CONSTANT') {
        if(nlay==1) {
          array[1:length(array)] <- as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])[2])
          remaining_lines <- remaining_lines[-1]
        } else {
          array[,,k] <- matrix(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])[2]),nrow=nrow,ncol=ncol)
          remaining_lines <- remaining_lines[-1]
        }
      }
      # INTERNAL or without header
      else if(toupper(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]) %in% c('INTERNAL') | skip_header)
      {
        
        if(!skip_header) {
          cnst <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
          if(cnst == 0) cnst <-  1.0
          
          # format
          fmtin <- rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3]
          if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
            lengths <- rmfi_fortran_format(fmtin)
            fortranfmt <-  TRUE
          }
          remaining_lines <- remaining_lines[-1] 
        } else {
          if(!is.null(fmt)) {
            fortranfmt <-  TRUE
            lengths <- rmfi_fortran_format(fmt)
          }
          cnst <-  1.0
        }
        
        if(fortranfmt) {
          remaining_lines[1] <- paste(substring(remaining_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
          nPerLine <- length(lengths)
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          if(nLines > 1) remaining_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(remaining_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
          array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          
        } else {
          nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])))
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
        }
         remaining_lines <- remaining_lines[-c(1:nLines)]
      }
      # EXTERNAL
      else if(toupper(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]) == 'EXTERNAL')
      {
        nunit <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
        cnst <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3])
        if(cnst == 0) cnst <-  1.0
        fmtin <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[4])
        binary <- ifelse(toupper(fmtin) == "(BINARY)", TRUE, FALSE)
        
        if(is.null(nam)) stop('Please supply a nam object when reading EXTERNAL arrays', call. = FALSE)
        fname <-  nam$fname[which(nam$nunit == nunit)]
        direct <-  attr(nam, 'dir')
        absfile <- file.path(fname)
        if(!file.exists(absfile)) absfile <- file.path(direct, fname) # try full name
        if(!file.exists(absfile)) stop('Could not determine path to EXTERNAL file on unit number ', nunit, call. = FALSE)
        
        if(!binary) {
          if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
            lengths <- rmfi_fortran_format(fmtin)
            fortranfmt <-  TRUE
          }
          
          # if external file holds multiple arrays, remove the corresponding lines
          external_lines <-  readr::read_lines(absfile, lazy = FALSE)
          if(!is.null(attr(nam, as.character(nunit)))) external_lines <- external_lines[-c(1:attr(nam, as.character(nunit)))]
          
          if(fortranfmt) {
            external_lines[1] <- paste(substring(external_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
            nPerLine <- length(lengths)
            nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
            if(nLines > 1) external_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(external_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
            array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
            
          } else {
            nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
            nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
            array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          }
          
        } else {
          con <- file(absfile,open='rb')
          type <- ifelse(integer, 'integer', 'numeric')
          if(type=='integer') warning('Reading integer binary EXTERNAL array might not work optimally', call. = FALSE)
          real_number_bytes <- ifelse(precision == 'single', 4, 8)
          size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
          try({ 
            
            # if external file holds multiple arrays, remove the corresponding lines
            if(!is.null(attr(nam, as.character(nunit)))) {
              for(jj in 1:attr(nam, as.character(nunit))) {
                invisible(readBin(con, what = 'integer', n = 2))
                invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                invisible(readChar(con,nchars=16))
                nncol <- readBin(con, what = 'integer', n = 1)
                nnrow <- readBin(con, what = 'integer', n = 1)
                invisible(readBin(con, what = 'integer', n = 1))
                invisible(readBin(con,what='numeric',n = nncol * nnrow, size = real_number_bytes))
              }
            }
            # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
            if(!integer) {
              invisible(readBin(con, what = 'integer', n = 2))
              invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
              invisible(readChar(con,nchars=16))
              invisible(readBin(con, what = 'integer', n = 3))
            }
            
            array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
            nLines <-  1})
          
          close(con)
        }
        if(is.null(attr(nam, as.character(nunit)))) {
          attr(nam, as.character(nunit)) <- nLines
        } else {
          attr(nam, as.character(nunit)) <- attr(nam, as.character(nunit)) + nLines
        }
        remaining_lines <- remaining_lines[-1] 
      } 
      # OPEN/CLOSE
      else if(toupper(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[1]) == 'OPEN/CLOSE')
      {
        fname <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[2])
        cnst <-  as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[3])
        if(cnst == 0) cnst <-  1.0
        fmtin <-  as.character(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' ')[[1]])[4])
        binary <- ifelse(toupper(fmtin) == "(BINARY)", TRUE, FALSE)
        
        direct <-  dirname(file)
        absfile <- file.path(fname)
        if(!file.exists(absfile)) absfile <- file.path(direct, fname) # try full name
        if(!file.exists(absfile)) stop('Could not determine path to OPEN/CLOSE file with FNAME ', fname, call. = FALSE)
        
        if(!binary) {
          if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
            lengths <- rmfi_fortran_format(fmtin)
            fortranfmt <-  TRUE
          }
          external_lines <-  readr::read_lines(absfile, lazy = FALSE)
          
          if(fortranfmt) {
            external_lines[1] <- paste(substring(external_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
            nPerLine <- length(lengths)
            nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
            if(nLines > 1) external_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(external_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
            array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
            
          } else {
            nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
            nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
            array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          }
          
        } else {
          con <- file(absfile,open='rb')
          real_number_bytes <- ifelse(precision == 'single', 4, 8)
          type <- ifelse(integer, 'integer', 'numeric')
          size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
          try({     
            # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
            if(!integer) { 
              invisible(readBin(con, what = 'integer', n = 2))
              invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
              invisible(readChar(con,nchars=16))
              invisible(readBin(con, what = 'integer', n = 3))
            }
            array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
          })
          close(con)
        }
        remaining_lines <- remaining_lines[-1] 
        
      } else {
        # FIXED format
        header <- rmfi_parse_variables(remaining_lines[1], n = 3, format = 'fixed')
        locat <- as.numeric(header$variables[1])
        cnst <- as.numeric(header$variables[2])
        fmtin <- paste0(strsplit(rmfi_remove_comments_end_of_line(toupper(remaining_lines[1])), '')[[1]][21:40], collapse = '')
        fmtin <- trimws(as.character(fmtin))
        
        # CONSTANT
        if(locat == 0) { 
          array[,,k] <- cnst
          nLines <- 1
        } else {
          if(cnst == 0) cnst <-  1.0
          if(is.null(nam)) stop('Please supply a nam object when reading FIXED-FORMAT arrays', call. = FALSE)
          
          fname <- nam$fname[which(nam$nunit == abs(locat))]
          direct <- attr(nam, 'dir')
          
          absfile <- file.path(fname)
          if(!file.exists(absfile)) absfile <- file.path(direct, fname) # try full name
          if(!file.exists(absfile)) stop('Could not determine path to EXTERNAL file on unit number ', locat, call. = FALSE)
          ext_file <- TRUE
          
          # ASCII
          if(locat > 0) {
            if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
              lengths <- rmfi_fortran_format(fmtin)
              fortranfmt <-  TRUE
            }
            if(locat == nam$nunit[which(basename(nam$fname) == basename(file))] || normalizePath(absfile) == normalizePath(file)) { # read from current file
              ext_file <- FALSE
              remaining_lines <- remaining_lines[-1] 
              if(fortranfmt) {
                remaining_lines[1] <- paste(substring(remaining_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
                nPerLine <- length(lengths)
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                if(nLines > 1) remaining_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(remaining_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
                array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
                
              } else {
                nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])))
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
              }

            } else { # read from external file
              external_lines <-  readr::read_lines(absfile, lazy = FALSE)
              # remove lines of previous arrays
              if(!is.null(attr(nam, as.character(locat)))) external_lines <- external_lines[-c(1:attr(nam, as.character(locat)))]
              
              if(fortranfmt) {
                external_lines[1] <- paste(substring(external_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
                nPerLine <- length(lengths)
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                if(nLines > 1) external_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(external_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
                array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
                
              } else {
                nPerLine <- length(as.numeric(rmfi_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                array[,,k] <- cnst*matrix(as.numeric(rmfi_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
              }
            }
            
          } else if(locat < 0) { # read binary from external file
            con <- file(absfile,open='rb')
            real_number_bytes <- ifelse(precision == 'single', 4, 8)
            type <- ifelse(integer, 'integer', 'numeric')
            size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
            if(type=='integer') warning('Reading integer binary EXTERNAL array might not work optimally', call. = FALSE)
            
            try({          
              if(!is.null(attr(nam, as.character(locat)))) {
                for(jj in 1:attr(nam, as.character(locat))) {
                  invisible(readBin(con, what = 'integer', n = 2))
                  invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                  invisible(readChar(con,nchars=16))
                  nncol <- readBin(con, what = 'integer', n = 1)
                  nnrow <- readBin(con, what = 'integer', n = 1)
                  invisible(readBin(con, what = 'integer', n = 1))
                  invisible(readBin(con,what='numeric',n = nncol * nnrow, size = real_number_bytes))
                }
              }
              # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
              if(!integer) {
                invisible(readBin(con, what = 'integer', n = 2))
                invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                invisible(readChar(con,nchars=16))
                invisible(readBin(con, what = 'integer', n = 3))
              }
              
              array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
              nLines <-  1})
            
            close(con)
          }
          
          if(ext_file) {
            if(is.null(attr(nam, as.character(locat)))) {
              attr(nam, as.character(locat)) <- nLines
            } else {
              attr(nam, as.character(locat)) <- attr(nam, as.character(locat)) + nLines
            }
            nLines <- 1
          }

        }
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }   
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(ndim == 1) {
    array <- c(array(array,dim=nrow*ncol*nlay))
  } else if(ndim == 2) {
    array <- rmf_create_array(array[,,1], dim = c(nrow, ncol))
  } else if(ndim == 3) {
    array <- rmf_create_array(array, dim = c(nrow, ncol, nlay))
  } else {
    stop('ndim should be 1, 2 or 3')
  }
  
  # Return output of reading function 
  return(list(array=array,remaining_lines=remaining_lines))
}

#' Read MODFLOW array parameters for boundary-condition packages
#'
#' @param lines lines to read the parameter arrays from
#' @param dis \code{RMODFLOW} dis object
#' @param np numeric; number of parameters to read
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#'
#' @return A list containing the parameter arrays and the remaining text of the MODFLOW input file
#' @keywords internal
#' @seealso \code{\link{rmfi_write_array_parameters}}
#' 
rmfi_parse_array_parameters <- function(lines, dis, np, mlt = NULL, zon = NULL) {
  
  parm_list <- list()
  
  i <- 1
  while(i <= np){
    
    # data set 3
    data_set_3 <- rmfi_parse_variables(lines, character = TRUE)
    parnam <-   as.character(data_set_3$variables[1])
    parval <-  as.numeric(data_set_3$variables[3])
    nclu <- as.numeric(data_set_3$variables[4])
    p_tv <- NULL
    if(length(data_set_3$variables) > 4 && toupper(data_set_3$variables[5]) == 'INSTANCES'){
      p_tv <- TRUE
      numinst <- as.numeric(data_set_3$variables[6])
      arr <- list()
    } 
    lines <- data_set_3$remaining_lines
    rm(data_set_3)
    
    mltarr <- zonarr <- vector(mode = 'character', length = nclu)
    iz <- as.list(rep(0, nclu))
    
    # time-varying parameters
    if(!is.null(p_tv) && p_tv){
      
      inst_list <- list()
      
      # loop over instances
      for(j in 1:numinst){
        
        # data set 4a
        data_set_4a <- rmfi_parse_variables(lines, character = TRUE)
        instnam <- as.character(data_set_4a$variables)
        lines <-  data_set_4a$remaining_lines
        rm(data_set_4a)
        
        # loop over clusters
        for(k in 1:nclu) {
          # data set 4b
          data_set_4b <- rmfi_parse_variables(lines, character = TRUE)
          mltarr[k] <- data_set_4b$variables[1]
          zonarr[k] <- data_set_4b$variables[2]
          
          if(toupper(data_set_4b$variables[1]) != 'NONE') {
            if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
            
          }
          if(toupper(data_set_4b$variables[2]) != 'ALL') {
            if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
            
            # zero or character entry terminates IZ
            iz_vector <- suppressWarnings(as.numeric(data_set_4b$variables[3:length(data_set_4b$variables)]))
            iz[[k]] <- iz_vector[1:min(length(iz_vector), which(is.na(iz_vector))[1] - 1, which(iz_vector == 0)[1] - 1, na.rm = TRUE)]
          
          }
          lines <- data_set_4b$remaining_lines
          rm(data_set_4b)
        }
        inst_list[[j]] <- rmf_create_parameter(dis = dis, mlt = mlt, mltnam = mltarr, zon = zon, zonnam = zonarr, iz = iz, instnam = instnam,
                                                                   parval = parval, parnam = parnam, kper = 0)
        names(inst_list)[j] <- instnam
      }
      parm_list[[length(parm_list)+1]] <- inst_list
      names(parm_list)[length(parm_list)] <- parnam
      parm_list[[length(parm_list)]] <- lapply(parm_list[[length(parm_list)]], function(i) {attr(i, 'kper') <- NULL; return(i)})
      
    } else {
      # non time-varying
      # loop over clusters
      for(k in 1:nclu) {
        # data set 4b
        data_set_4b <- rmfi_parse_variables(lines, character = TRUE)
        mltarr[k] <- data_set_4b$variables[1]
        zonarr[k] <- data_set_4b$variables[2]
        
        if(toupper(data_set_4b$variables[1]) != 'NONE') {
          if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
          
        }
        if(toupper(data_set_4b$variables[2]) != 'ALL') {
          if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
          
          # zero or character entry terminates IZ
          iz_vector <- suppressWarnings(as.numeric(data_set_4b$variables[3:length(data_set_4b$variables)]))
          iz[[k]] <- iz_vector[1:min(length(iz_vector), which(is.na(iz_vector))[1] - 1, which(iz_vector == 0)[1], na.rm = TRUE)]
        }
        lines <- data_set_4b$remaining_lines
        rm(data_set_4b)
      }
      
      parm_list[[length(parm_list)+1]] <- rmf_create_parameter(dis = dis, mlt = mlt, mltnam = mltarr, zon = zon, zonnam = zonarr, iz = iz, instnam = NULL,
                                                               parval = parval, parnam = parnam, kper = 0)
      names(parm_list)[length(parm_list)] <- parnam
      attr(parm_list[[length(parm_list)]], 'kper') <- NULL
    }
    

    i <- i+1
  }
  
  
  return(list(parameters = parm_list, remaining_lines = lines))
}

#' Read comments
#' Internal function used in the read_* functions to read comments
#' @details removes empty comments and prevents copying of RMODFLOW header comment
#' @keywords internal
rmfi_parse_comments <- function(remaining_lines) {
  v <- paste("RMODFLOW, version",  packageDescription("RMODFLOW")$Version)
  comments <- NULL
  comment_tag <- substr(remaining_lines, 1, 1)
  comment_id <- which(comment_tag == "#")
  
  if(length(comment_id) > 0) {
    comments <- gsub('#', '', remaining_lines[comment_id])
    
    # remove empty comments
    empty <- which(nchar(trimws(comments)) == 0)
    if(length(empty) > 0) comments <- comments[-empty]
    
    # remove RMODFLOW header
    header <- grep(v, comments)
    if(length(header) > 0) comments <- comments[-header]
    
    remaining_lines <- remaining_lines[-comment_id]
  }

  return(list(comments = comments, remaining_lines = remaining_lines))
}

#' Reads a MODFLOW list
#'@param nlst number of list rows to read
#'@param l stress period number
#'@param varnames character vector; names of the variables starting from the 4th column (so after ijk). Length of varnames is used to dimension the dataframe
#'@param scalevar column name or integer; this column will be scaled
#'@param file the file that is being read; needed if list is specified through an OPEN/CLOSE statement
#'@param naux integer; number of auxiliary variables to read (which are always free format). Defaults to 0.
#'@param format either 'fixed' or 'free'
#'@param ... ignored
#'@keywords internal

rmfi_parse_list <-  function(remaining_lines, nlst, l = NULL, varnames, scalevar=4, file, naux = 0, format = 'free', precision = 'single', ...) {
  
  header <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),' |\t')[[1]])
  n <- 3 + length(varnames) 
  real_number_bytes <- ifelse(precision == 'single', 4, 8)
  scale <-  1.0
  df <- matrix(nrow=nlst, ncol=3+length(varnames))
  col_names <- c('k','i','j',varnames)
  
  # helper function
  read_list <- function(lines) {
    # if lines is of length 1, readr will assume it's a file connection and error out
    lines <- lines[1:nlst]
    if(nlst == 1) lines <- c(lines, '')
    
    # TODO atm aux can only be double
    if(format == 'fixed') {
      if(naux > 0) {
        widths <- readr::fwf_widths(c(rep(10, n - naux), NA))
        cols <- do.call(readr::cols_only, as.list(c(rep('i', 3), rep('d', n - naux - 3), 'c')))
        df <- as.data.frame(readr::read_fwf(I(lines), widths, col_types = cols, lazy = FALSE))
        
        df <- replace(df, which(is.na(df), arr.ind = TRUE), 0)
        
        # handle AUX variables which may be free format
        df[[ncol(df)]] <- gsub(',', ' ', df[[ncol(df)]])
        cols2 <- do.call(readr::cols_only, as.list(rep('d', naux)))
        lc <- as.data.frame(readr::read_table(I(df[[ncol(df)]]), col_names = FALSE, col_types = cols2))
        df <- cbind(df[-ncol(df)], lc)
        
      } else {
        widths <- readr::fwf_widths(c(rep(10, n)))
        cols <- do.call(readr::cols_only, as.list(c(rep('i', 3), rep('d', n - 3))))
        df <- as.data.frame(readr::read_fwf(I(lines), widths, col_types = cols, lazy = FALSE))
        
        df <- replace(df, which(is.na(df), arr.ind = TRUE), 0)
      }
      
    } else {
      lines <- gsub(',', ' ', lines)
      cols <- do.call(readr::cols_only, as.list(c(rep('i', 3), rep('d', n - 3))))
      # TODO unsuppress warnings;
      # reading in subset of columns without knowing all names not possible without warnings in readr
      # NOTE: this was for readr < 2.0.0; check again
      df <- as.data.frame(suppressWarnings(readr::read_table(I(lines), col_names = FALSE, col_types = cols)))
    }
    return(df[1:nlst,])
  }
  
  if(toupper(header[1]) == 'EXTERNAL') {
    if(is.null(nam)) stop('List is read on an EXTERNAL file. Please supply the nam object', call. = FALSE)
    remaining_lines <-  remaining_lines[-1]
    extfile <- file.path(attr(nam, 'dir'), nam$fname[which(nam$nunit==as.numeric(header[2]))])
    binary <-  ifelse(toupper(nam$ftype[which(nam$nunit==as.numeric(header[2]))]) == 'DATA(BINARY)', TRUE, FALSE)
    
    if(binary) {
      con <- file(extfile, open='rb')
      aa <-  readBin(con, what = 'numeric', n =nlst*(3+length(varnames)), size = real_number_bytes)
      close(con)
      df <-  matrix(aa, nrow=nlst, ncol=3+length(varnames), byrow = TRUE)
    } else {
      ext_lines <- readr::read_lines(extfile, lazy = FALSE)
      header <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(ext_lines[1]),' |\t')[[1]])
      if(toupper(header[1]) == 'SFAC') {
        scale <- as.numeric(header[2])
        ext_lines <- ext_lines[-1]
      }
      
      df <- read_list(ext_lines)
      
    }
    
  } else if(toupper(header[1]) == 'OPEN/CLOSE') {
    remaining_lines <-  remaining_lines[-1]
    extfile <- file.path(file, as.character(header[2]))
    binary <- rmfi_ifelse0(!is.na(header[3]) && toupper(header[3]) == "(BINARY)", TRUE, FALSE)
    
    if(binary) {
      con <- file(extfile, open='rb')
      aa <-  readBin(con, what = 'numeric', n =nlst*(3+length(varnames)), size = real_number_bytes)
      close(con)
      df <-  matrix(aa, nrow=nlst, ncol=3+length(varnames), byrow = TRUE)
    } else {
      
      ext_lines <- readr::read_lines(extfile, lazy = FALSE)
      
      header <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(ext_lines[1]),' |\t')[[1]])
      if(toupper(header[1]) == 'SFAC') {
        scale <- as.numeric(header[2])
        ext_lines <- ext_lines[-1]
      }
      
     df <- read_list(ext_lines)
       
    }
    
  } else if(toupper(header[1]) == 'SFAC') {
    remaining_lines <- remaining_lines[-1]
    scale <- as.numeric(header[2])
    
    df <- read_list(remaining_lines)
    remaining_lines <- remaining_lines[-c(1:nlst)]
    
  } else {
    
    df <- read_list(remaining_lines)
    remaining_lines <- remaining_lines[-c(1:nlst)]
  }
  
  df <- data.frame(df, stringsAsFactors = FALSE)
  colnames(df) <- col_names
  if(!is.null(l)) df$l <- l
  df <- rmf_create_list(df)
  if(scale != 1.0) df[[scalevar]] <- scale*df[[scalevar]]
  
  return(list(list = df, remaining_lines = remaining_lines))
  
}
#' Read modflow variables
#' If all are numbers, returns numeric, otherwise returns character vector
#' @param n integer; number of variables to be returned. Only used when format is \code{'fixed'}.  
#' @param nlay integer; number of layers for which values are to be read. Only used when format is \code{'free'} and a 1D(NLAY) variable is read which may be specified on multiple lines.
#' @param character logical; should a character vector be returned. Prevents conversion from character names to numeric. Defaults to FALSE. Useful if only characters are present on the line.
#' @param format character, either \code{'free'} or \code{'fixed'}. When 'fixed', reads 10-character fields and converts to numeric. Empty fields are set to zero.
#' @param ... ignored
#' @keywords internal
rmfi_parse_variables <- function(remaining_lines, n, nlay = NULL, character = FALSE, format = 'free', ...) {
  if(format == 'free') {
    variables <- rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),' |\t|,')[[1]])
    if(!is.null(nlay)) {
      while(length(variables) < nlay) { 
        remaining_lines <- remaining_lines[-1]
        variables <- append(variables, rmfi_remove_empty_strings(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),' |\t|,')[[1]]))
      }
    }
    if(!character && !any(is.na(suppressWarnings(as.numeric(variables))))) variables <- as.numeric(variables)
  } else if(format == 'fixed') { # every value has 10 characters; empty values are zero
    variables <- (unlist(lapply(seq(1,nchar(remaining_lines[1]), by=10), 
                                function(i) paste0(strsplit(rmfi_remove_comments_end_of_line(remaining_lines[1]),'')[[1]][i:(i+min(10, nchar(remaining_lines[1])-i+1)-1)], collapse=''))))
    variables <- lapply(strsplit(variables, " |\t"), rmfi_remove_empty_strings)
    variables[which(lengths(variables)==0)] <-  0 # empty values are set to 0
    variables <- unlist(variables)
    if(!character && !any(is.na(suppressWarnings(as.numeric(variables))))) {
      variables <- as.numeric(variables)
      if(length(variables) < n) variables <- c(variables, rep(0, n - length(variables))) # append 0's if values are missing
    }
  }
  return(list(variables=variables,remaining_lines=remaining_lines[-1]))
}

#' Model performance measures
#' @param measures any of the measures present in \code{\link{hydroGOF::gof}} + 'ssq' (sum of squared errors)
#' @param ... arguments passes to \code{\link{hydroGOF::gof}}
#' @keywords internal
rmfi_performance_measures <- function(observations, predictions,print=FALSE,measures = c('ssq', 'mse', 'mae', 'me', 'r2', 'nse', 'rmse', 'pbias', 'kge'), ...) {
  gof <- hydroGOF::gof(predictions, observations, ...)
  name <- c('Mean error', 'Mean absolute error', 'Mean squared error', 'Root mean squared error', 'Normalized root mean squared error',
            'Percent bias', 'Ratio of rmse to standard deviation of observations', 'Ratio of standard deviations', 'Nash-Sutcliffe efficiency', 
            'Modified Nash-Sutcliffe efficiency', 'Relative Nash-Sutcliffe efficiency','Index of agreement', 'Modified index of agreement', 
            'Relative index of agreement', 'Coefficient of persistance', 'Pearson product-moment correlation coefficient', 'Coefficient of determination',
            'R2 multiplied with slope of linear regression between sim and obs', 'Kling-Gupta efficiency', 'Volumetric efficiency')
  gof <- data.frame(measure = rownames(gof), value = c(gof), name = name)
  gof <- rbind(gof, data.frame(measure = 'SSQ', value = round(sum((predictions - observations)^2), digits = 2), name ='Sum of squared errors'))
  measures <- tolower(measures)
  if("pbias" %in% measures) measures <- replace(measures, which(measures == 'pbias'), 'pbias %')
  if("nrmse" %in% measures) measures <- replace(measures, which(measures == 'nmrse'), 'nrmse %')
  
  indx <- which(tolower(gof$measure) %in% measures)
  gof <- gof[indx, ]
  
  if(print){
    for(i in 1:nrow(gof)) {
      cat(paste(paste0(gof$measure[i], ': '), round(gof$value[i],2), paste0(' (',gof$name[i],')\n')))
    }
  }
  
  measures <- tolower(gof$measure)
  if("pbias %" %in% measures) measures <- replace(measures, which(measures == 'pbias %'), 'pbias')
  if("nrmse %" %in% measures) measures <- replace(measures, which(measures == 'nmrse %'), 'nrmse')
  gof <- setNames(data.frame(as.list(gof$value)), measures)
  return(gof)
}

#' Plot a RMODFLOW list-directed boundary condition object
#'
#' @param obj a \code{RMODFLOW} list-directed boundary condition object, i.e. riv, chd, wel, drn, ghb, and derivatives
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable single character or numeric indicating which column in the \code{rmf_list} object to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#'
#' @return ggplot2 object or layer
#' @keywords internal
#'
rmfi_plot_bc <- function(obj,
                         dis,
                         kper = NULL,
                         variable = 'id',
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         active_only = FALSE,
                         fun = sum,
                         add = FALSE,
                         ...) {
  
  if(is.null(kper)) {
    if(dis$nper > 1) warning('Setting kper to last stress-period', call. = FALSE)
    kper <- dis$nper
  }
  
  active_lists <- colnames(obj$kper)[-1]
  active_lists <- active_lists[which(obj$kper[kper,-1] == TRUE)] 
  
  obj <- subset(obj$data, name %in% active_lists)
  
  if(nrow(obj) == 0) {
    id <- class(obj)[which(class(obj) == 'rmf_package') - 1]
    if(add) {
      warning(paste0(id, ' object has no active features in stress-period ', kper, '. Returning NULL.'), call. = FALSE)
      return(NULL)
    } else {
      stop(paste0(id, ' object has no active features in stress-period ', kper, '.'), call. = FALSE)
    }
  }
  
  # rmf_plot.rmf_list
  rmf_plot(obj, dis = dis, variable = variable, active_only = active_only, i=i, j=j, k=k, fun = fun, add = add, ...)
  
}

#' Read input for a MODFLOW boundary condition package which uses list-directed input
#'
#' @param lines lines as returned from readr::read_lines
#' @param dis an \code{RMODFLOW} dis object
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk)
#' @param option optional named logical vector with the names of the options besides aux
#' @param scalevar integer, indicating which column is (possibly) scaled by SFAC
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.
#' @return list with (optional) comments, icb, option, aux and rmf_lists
#' @keywords internal
#' @seealso \code{\link{rmfi_create_bc_list}}, \code{\link{rmfi_write_bc_list}}
rmfi_parse_bc_list <- function(lines, dis, varnames, option, scalevar, ...) {
  
  rmf_lists <- list()
  
  # data set 0
  data_set_0 <-  rmfi_parse_comments(lines)
  comments <-  data_set_0$comments
  lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(lines, character = TRUE)
  
  if('PARAMETER' %in% toupper(data_set_1$variables)) {
    np_def <-  as.numeric(data_set_1$variables[2])
    lines <-  data_set_1$remaining_lines
  }  else {
    np_def <- 0
  }
  rm(data_set_1)
  
  # data set 2
  if(identical(c('shead', 'ehead'), tolower(varnames))) { # exception for CHD
    n <- 1 
    data_set_2 <-  rmfi_parse_variables(lines, n=n, ...)
    icb <- NULL
  } else {
    n <- 2
    data_set_2 <-  rmfi_parse_variables(lines, n=n, ...)
    icb <-  as.numeric(data_set_2$variables[2])
  }
  option[] <- FALSE
  aux <- NULL
  if(length(data_set_2$variables) > n) {
    if(!is.null(list(...)[["format"]]) && list(...)[['format']] == 'fixed') {
      data_set_2$variables <- c(rep("0", n), rmfi_parse_variables(paste0(strsplit(lines[1], '')[[1]][-c(1:10*n)], collapse = ''))$variables)
    }
    if(any(c(names(option), "AUX", "AUXILIARY") %in% toupper(data_set_2$variables[n+1:length(data_set_2$variables)]))) {
      option <- vapply(names(option), function(i) i %in% toupper(data_set_2$variables), TRUE)
      aux <- as.character(data_set_2$variables[grep('^AUX', toupper(data_set_2$variables))+1])
    }
    if(length(aux) == 0) aux <- NULL
  }
  lines <-  data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(np_def > 0){
    tv <- list()
    i <- 1
    while(i <= np_def){
      # data set 3
      data_set_3 <-  rmfi_parse_variables(lines, character = TRUE)
      p_name <-  as.character(data_set_3$variables[1])
      p_val <-  as.numeric(data_set_3$variables[3])
      p_nlst <- as.numeric(data_set_3$variables[4])
      p_tv <- NULL
      tv[[p_name]] <- FALSE
      if(length(data_set_3$variables) > 4 && toupper(data_set_3$variables[5]) == 'INSTANCES'){
        p_tv <- TRUE
        p_numinst <-  as.numeric(data_set_3$variables[6])
        tv[[p_name]] <- TRUE
      } 
      lines <- data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(p_tv) && p_tv){
        
        j=1
        while(j <= p_numinst){
          # data set 4a
          data_set_4a <- rmfi_parse_variables(lines, character = TRUE)
          instnam <- as.character(data_set_4a$variables)
          lines <-  data_set_4a$remaining_lines
          rm(data_set_4a)
          
          # data set 4b
          data_set_4b <- rmfi_parse_list(lines, nlst = p_nlst, varnames = rmfi_ifelse0(is.null(aux), varnames, c(varnames, aux)), naux = length(aux), scalevar = scalevar, file = file, ...)
          rmf_lists[[length(rmf_lists)+1]] <- rmf_create_parameter(data_set_4b$list, parnam = p_name, parval = p_val, instnam = instnam, kper = 0)
          
          lines <- data_set_4b$remaining_lines
          rm(data_set_4b)
          
          j <-  j+1
        } 
        
      } else {
        # non time-varying
        # data set 4b
        data_set_4b <- rmfi_parse_list(lines, nlst = p_nlst, varnames = rmfi_ifelse0(is.null(aux), varnames, c(varnames, aux)), naux = length(aux), scalevar = scalevar, file = file, ...)
        rmf_lists[[length(rmf_lists)+1]] <- rmf_create_parameter(data_set_4b$list, parnam = p_name, parval = p_val, kper = 0)
        
        lines <- data_set_4b$remaining_lines
        rm(data_set_4b)
      }
      
      i <-  i+1
    }
    rmf_lists <- lapply(rmf_lists, function(i) {attr(i, 'kper') <- NULL; return(i)})
  }
  
  # stress periods
  
  # function for setting kper attribute for parameters
  set_kper <- function(k, kper, p_name, i_name) {
    if(!is.null(attr(k, 'parnam')) && toupper(attr(k, 'parnam')) == toupper(p_name)) {
      if(!is.null(i_name)) {
        if(toupper(attr(k, "instnam")) == toupper(i_name)) attr(k, 'kper') <- c(attr(k, 'kper'), kper)
      } else {
        attr(k, 'kper') <- c(attr(k, 'kper'), kper)
      }
    }
    return(k)
  }
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 <-  rmfi_parse_variables(lines, n=2, ...)
    itmp <- as.numeric(data_set_5$variables[1])
    np <- ifelse(np_def > 0, as.numeric(data_set_5$variables[2]), 0)
    lines <- data_set_5$remaining_lines
    rm(data_set_5)
    
    if(itmp > 0){
      data_set_6 <- rmfi_parse_list(lines, nlst = itmp, varnames = rmfi_ifelse0(is.null(aux), varnames, c(varnames, aux)), naux = length(aux), scalevar = scalevar, file = file, ...)
      rmf_lists[[length(rmf_lists)+1]] <- structure(data_set_6$list, kper = i)
      # TODO : see if list already exists; then just add kper to attribute
      lines <-  data_set_6$remaining_lines
      rm(data_set_6)
    } else if(i > 1 && itmp < 0) {
      attr(rmf_lists[[length(rmf_lists)]], 'kper') <- c(attr(rmf_lists[[length(rmf_lists)]], 'kper'), i)
    }
    
    if(np > 0){
      for(j in 1:np){
        # data set 7
        data_set_7 <-  rmfi_parse_variables(lines, character = TRUE)
        p_name <-  as.character(data_set_7$variables[1])
        i_name <- rmfi_ifelse0(tv[[p_name]], as.character(data_set_7$variables[2]), NULL)
        
        rmf_lists <- lapply(rmf_lists, set_kper, p_name = p_name, i_name = i_name, kper = i)
        
        lines <- data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  
  return(list(comments = comments, icb = icb, option = option, aux = aux, rmf_lists = rmf_lists))
}

#' Remove comments at the end of a string
#' @param line A string.
#' @return The string, without the commented part.
#' @keywords internal
rmfi_remove_comments_end_of_line <- function(line) {
  if(grepl('#',line)) return(substr(line,1,regexpr('#',line)-1))
  else return(line)
}

#' Remove empty elements from a vector of strings.
#' @param vector_of_strings Vector of strings.
#' @return Vector of strings without the empty items.
#' @keywords internal
rmfi_remove_empty_strings <- function(vector_of_strings) {
  return(vector_of_strings[which(vector_of_strings!='')])
}

#' Reversed rainbow color palette
#' @keywords internal
rmfi_rev_rainbow <- function(...) {
  return(rev(rainbow(...)))
}

#' Calculate a weighted geometric mean
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @seealso \code{\link{rmfi_weighted_harmean}}, \code{\link{weighted.mean}}, \code{\link{rmfi_geomean}}, \code{\link{rmfi_harmean}} and \code{\link{mean}}
#' @keywords internal
rmfi_weighted_geomean <- function(x, w, ...) {
  return(prod(x^w, ...)^(1/sum(w)))
}

#' Calculate a weighted harmonic mean
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{sum}}
#' @seealso \code{\link{rmfi_weighted_geomean}}, \code{\link{weighted.mean}}, \code{\link{rmfi_harmean}} \code{\link{rmfi_geomean}} and \code{\link{mean}}
#' @keywords internal
rmfi_weighted_harmean <- function(x, w, ...) {
  return(sum(w)/(sum(w/x, ...)))
}

#' Write modflow array
#' Internal function used in the write_* functions for writing array datasets
#' @param external character vector with names corresponding to the dataset; used to write external arrays
#' @param fname  character vector with names corresponding to the dataset; used to write open/close arrays
#' @param binary character vector with names corresponding to the dataset; used to write external or open/close arrays
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of binary files
#' @param nam \code{\link{RMODFLOW}} nam object; used when writing external arrays
#' @param xsection logical; does the array represent a NLAY x NCOL cross-section. Passed to \code{rmf_write_array}
#' @param ... ignored
#' @details if the array should be written as integers, an integer array should be provided
rmfi_write_array <- function(array, file, cnstnt=1, iprn=-1, append=TRUE, external = NULL, fname = NULL, binary = NULL, precision = 'single', nam = NULL, xsection = FALSE, ...) {
  
  arrname <-  sub(x=sub(".*[$]","",deparse(substitute(array))),pattern = '[[].*', replacement='')
  external <- rmfi_ifelse0(is.null(external), FALSE, arrname %in% external)
  fname <- rmfi_ifelse0(is.null(fname), FALSE, arrname %in% fname)
  binary <- rmfi_ifelse0(is.null(binary), FALSE, arrname %in% binary)
  
  if(is.null(names(cnstnt))) {
    if(length(cnstnt) > 1)  stop('Please supply a single value or a named vector for cnstnt', call. = FALSE)
  } else {
    cnstnt <- ifelse(is.na(cnstnt[arrname]), 1, cnstnt[arrname])
  }
  
  if(is.null(names(iprn))) {
    if(length(iprn) > 1)  stop('Please supply a single value or a named vector for iprn', call. = FALSE)
  } else {
    iprn <- ifelse(is.na(iprn[arrname]), -1, iprn[arrname])
  }
  iprn <- as.integer(iprn)
  
  if(external) { # external
    if(is.null(nam)) stop('Please supply a nam object when writing EXTERNAL arrays', call. = FALSE)
    extfile <-  file.path(dirname(file), paste(arrname, 'ext', sep='.'))
    
    # set unit number in nam file
    found <-  FALSE
    nunit <- 200
    while(!found) {
      if(!(nunit %in%nam$nunit)) {
        nam <-  rbind(nam, list(ifelse(binary[arrname],"DATA(BINARY)","DATA"),nunit,extfile,NA))
        found <- TRUE
      } else {
        nunit <-  nunit+1
      }
    }
    nunit <- as.integer(nunit)
    
    if(!is.null(dim(array)) && length(dim(array)) > 2) {
      for(i in 1:dim(array)[3]) {
        cat(paste('EXTERNAL',nunit, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=ifelse(i == 1, append, TRUE))
      }
    } else {
      cat(paste('EXTERNAL',nunit, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
    }
    
    rmf_write_array(array = array, file = extfile, append = FALSE, binary = binary, header = ifelse(binary, ifelse(is.integer(array), FALSE,TRUE), FALSE), desc = 'HEAD', precision = precision, xsection = xsection)
    warning(paste('Please remember to add the external file to the nam file.\nftype =', ifelse(binary,"DATA(BINARY)","DATA"),
                  '\nnunit =', nunit, '\nfname =', extfile), call. = FALSE)
    #return(data.frame(ftype = ifelse(binary[arrname], 'DATA(BINARY)', 'DATA'), nunit=nunit, fname=extfile, options=NA))
    
  } else if(fname) { # open/close
    
    if(!is.null(dim(array)) && length(dim(array)) > 2) {
      for(i in 1:dim(array)[3]) {
        fname_i <- paste(paste(arrname, i, sep = '_'), 'in', sep = '.')
        direct <-  dirname(file)
        absfile <-  file.path(direct, fname_i)
        
        cat(paste('OPEN/CLOSE', fname_i, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
        rmf_write_array(array = array[,,i], file = absfile, append = FALSE, binary = binary, header = ifelse(binary, ifelse(is.integer(array), FALSE,TRUE), FALSE), desc = 'HEAD', precision = precision, xsection = xsection)
        
      }
    } else {
      fname_i <- paste(arrname, 'in', sep = '.')
      direct <-  dirname(file)
      absfile <-  file.path(direct, fname_i)
      cat(paste('OPEN/CLOSE', fname_i, cnstnt, ifelse(binary,"(binary)","(free)"), iprn, '\n', sep=' '), file=file, append=append)
      rmf_write_array(array = array, file = absfile, append = FALSE, binary = binary, header = ifelse(binary, ifelse(is.integer(array), FALSE,TRUE), FALSE), desc = 'HEAD', precision = precision, xsection = xsection)
      
    }
    
    
  } else { # not external or open/close
    if(is.null(dim(array))) {
      if(prod(c(array)[1] == c(array))==1) {
        cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
        cat(paste(paste(array, collapse=' '), '\n', sep=' '), file=file, append=TRUE)     
      }
    } else if(length(dim(array))==2) {
      if(prod(c(array)[1] == c(array))==1) {
        cat(paste('CONSTANT ',cnstnt * c(array)[1], '\n', sep=''), file=file, append=append)
      } else {
        cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=append)
        if(dim(array)[1] == 1) {
          cat(paste0(paste(array, collapse=' '),'\n'), file=file, append=TRUE)
        } else {
          write.table(array, file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE) 
        }
      }
    } else {
      for(i in 1:dim(array)[3])
      {
        if(prod(c(array[,,i])[1] == c(array[,,i]))==1) {
          cat(paste('CONSTANT ',cnstnt * c(array[,,i])[1], '\n', sep=''), file=file, append=ifelse(i == 1, append, TRUE))
        } else {
          cat(paste('INTERNAL ',cnstnt,' (free) ', iprn, '\n', sep=''), file=file, append=ifelse(i == 1, append, TRUE))
          if(dim(array)[1] == 1) {
            cat(paste0(paste(array[,,i], collapse=' '),'\n'), file=file, append=TRUE)
          } else {
            write.table(array[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
          }
        }
      }
    }
  }
}

#' Write MODFLOW array parameters for boundary-condition packages
#'
#' @param obj \code{RMODFLOW} object to write
#' @param arrays list of arrays to write
#' @param file filename to write to
#' @param type character; type of array parameter. Allowed values are \code{'bc'} for boundary-condition arrays and \code{'flow'} for flow package arrays
#' @param ... additional arguments passed to \code{rmfi_write_array} 
#'
#' @return NULL
#' @keywords internal
#' @seealso \code{\link{rmfi_parse_array_parameters}}
#' 
rmfi_write_array_parameters <- function(obj, arrays, file, partyp, ...) {
  
  parm_names <- names(obj$parameter_values)
  tv_parm <- structure(rep(F,obj$np), names = parm_names)
  
  for (i in 1:obj$np){
    
    p_name <- parm_names[i]
    arr <- arrays[[p_name]]
    
    tv_parm[i] <- (!is.null(obj$instances) && obj$instances[p_name] != 0)
    nclu <- ifelse(tv_parm[i], length(attr(arr[[1]], 'mlt')), length(attr(arr, 'mlt')))
    
    # headers
    rmfi_write_variables(p_name, toupper(partyp), obj$parameter_values[i], as.integer(nclu), ifelse(tv_parm[i], 'INSTANCES', ''), ifelse(tv_parm[i],  as.integer(obj$instances[p_name]), ''), file=file)
    
    # time-varying
    if(tv_parm[i]){
      instances <- names(arr)
      for (jj in 1:length(instances)){
        
        arr2 <- arr[[instances[jj]]]
        
        # instnam
        rmfi_write_variables(instances[jj], file=file)
        
        # clusters
        for (k in 1:nclu){
          rmfi_write_variables(attr(arr2, 'mlt')[k], attr(arr2, 'zon')[k], ifelse(toupper(attr(arr2, 'zon')[k]) == "ALL", '', as.integer(attr(arr2, 'iz')[[k]])), file=file)
        }
        rm(arr2)
      }
    } else { # non-time-varying
      for (k in 1:nclu){
        rmfi_write_variables(attr(arr, 'mlt')[k], attr(arr, 'zon')[k], ifelse(toupper(attr(arr, 'zon')[k]) == "ALL", '', as.integer(attr(arr, 'iz')[[k]])), file=file)
      }  
      rm(arr)
    }
  }
  
}

#'
#' Write a MODFLOW boundary condition package which uses list-directed input to a file
#'
#' @param file filename to write to
#' @param obj an \code{RMODFLOW} boundary condition rmf_package object with list directed input
#' @param dis an \code{RMODFLOW} dis object
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk)
#' @param header character; package name. Part of the header comment written to the output file
#' @param package character; acronym (often 3 letters) used by MODFLOW to name to package
#' @param partyp character; specifies the parameter type
#' @param ... arguments passed to \code{rmfi_write_variables} and \code{rmfi_write_list} when writing a fixed format file.

#' @return \code{NULL}
#' @keywords internal
#' @seealso \code{\link{rmfi_create_bc_list}}, \code{\link{rmfi_parse_bc_list}}
#' 

rmfi_write_bc_list <- function(file, obj, dis, varnames, header, package, partyp, ...) {

  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW', header, 'created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(obj)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(obj$np > 0) rmfi_write_variables('PARAMETER', as.integer(obj$np), as.integer(obj$mxl), file=file)
  
  # data set 2
  if(!is.null(list(...)[["format"]]) && list(...)[['format']] == 'fixed') {
    ds2 <- paste0(formatC(as.integer(c(obj$mxact, obj[[paste0('i',tolower(package), 'cb')]])), width = 10), collapse='')
  } else {
    ds2 <- as.integer(c(obj$mxact, obj[[paste0('i',tolower(package), 'cb')]]))
  }
  rmfi_write_variables(ds2, ifelse(obj$option['noprint'], 'NOPRINT', ''), rmfi_ifelse0((!is.null(obj$aux)), paste('AUX', obj$aux), ''), file=file)
  
  # parameters
  if(obj$np > 0) {
    parm_names <- names(obj$parameter_values)
    tv_parm <- structure(rep(FALSE,obj$np), names = parm_names)
    
    for (i in 1:obj$np){
      
      p_name <- parm_names[i]
      df <- subset(obj$data, name == p_name)
      
      tv_parm[i] <- (!is.null(obj$instances) && obj$instances[p_name] != 0)
      nlst <- unname(ifelse(tv_parm[i], nrow(df)/obj$instances[p_name], nrow(df)))
      
      # data set 3
      rmfi_write_variables(p_name, toupper(partyp), obj$parameter_values[i], as.integer(nlst), ifelse(tv_parm[i], 'INSTANCES', ''), ifelse(tv_parm[i],  as.integer(obj$instances[p_name]), ''), file=file)
      
      # time-varying
      if(tv_parm[i]){
        instances <- unique(df$instance)
        for (jj in 1:obj$instances[p_name]){
          
          df2 <- subset(df, instance == instances[jj])
          # data set 4a
          rmfi_write_variables(instances[jj], file=file)
          
          # data set 4b
          rmfi_write_list(df2, file = file, varnames = c(varnames, obj[['aux']]), aux = obj[['aux']], ...)
          rm(df2)
        }
      } else { # non-time-varying
        rmfi_write_list(df, file = file, varnames = c(varnames, obj[['aux']]), aux = obj[['aux']], ...)
        
      }  
      rm(df)
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    names_act <- colnames(obj$kper)[which(obj$kper[i,which(!is.na(obj$kper[i,]))] != FALSE)[-1]]
    if(i > 1 && identical(names_act, colnames(obj$kper)[which(obj$kper[i-1,which(!is.na(obj$kper[i-1,]))] != FALSE)[-1]])) {
      itmp <- -1 
    } else {
      list_names <- rmfi_ifelse0(obj$np > 0, names_act[!(names_act %in% parm_names)], names_act)
      if(length(list_names) > 0) {
        itmp <- sum(obj$itmp[list_names])
      } else {
        itmp <- 0
      }
    }
    
    if(obj$np > 0) {
      parm_names_active <- parm_names[parm_names %in% names_act]
      np <- length(parm_names_active)
    } else {
      np <- 0
    }
    
    rmfi_write_variables(itmp, np, file=file, integer = TRUE, ...)
    
    # data set 6
    if(itmp > 0){
      df <- subset(obj$data, name %in% list_names)
      rmfi_write_list(df, file = file, varnames = c(varnames, obj[['aux']]), aux = obj[['aux']], ...)
      rm(df)
    }
    
    # data set 7
    if(np > 0){
      for(j in 1:np){
        rmfi_write_variables(parm_names_active[j], ifelse(tv_parm[j], obj$kper[i,parm_names_active[j]], ''), file=file)
      }
    }
  }
  
  
}

#' Write a RMODFLOW list
#'
#' @param df \code{RMODFLOW} list
#' @param file filename to write to
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk) including optional auxiliary variables
#' @param aux character vector with the names of the auxiliary variables defined in \code{varnames}
#' @param format either \code{"free"} (default) or \code{"fixed"}
#' @param append logical
#' @param ... ignored
#' @return \code{NULL}
#' @keywords internal
rmfi_write_list <- function(df, file, varnames, aux = NULL, format = 'free', append = TRUE, ...) {
  
  naux <- length(aux)
  n <- length(varnames) - naux
  col_names <- c('k', 'i', 'j', varnames)
  df <- df[,col_names]
  df$k <- as.integer(df$k)
  df$i <- as.integer(df$i)
  df$j <- as.integer(df$j)

  if(format == 'fixed') {
    fmt <- paste0(c(rep('%10i', 3), rep('%10g', n), rep('%10g', naux)), collapse = '')
    dff <- do.call('sprintf', c(df, fmt))
    readr::write_lines(dff, file = file, append = append)
  } else {
    readr::write_delim(df, file = file, delim = ' ', col_names = FALSE, append = append)
  }
  
}

#' Write MODFLOW variables
#' Internal function used in the rmf_write_* functions for writing single line datasets
#' @param format either \code{'fixed'} or \code{'free'}.  Fixed format assumes fixed width character spaces for each value as determined by the width argument
#' @param width numeric vector with the character widths for each variable. If a single value, it is repeated.
#' @param integer logical; should all values be converted to integers? MODFLOW does not allow for exponents in integer values
#' @param iprn ignored
#' @keywords internal
rmfi_write_variables <- function(..., file, append=TRUE, width = 10, format = 'free', integer = FALSE, iprn = -1) {
  arg <- list(...)
  arg <- arg[vapply(arg, function(i) all(nchar(i) > 0), TRUE)] # removes empty elements
  if(integer) arg <- lapply(arg, as.integer)
  
  # sets integers in proper format since Type is converted to double when vectorized
  if(format == 'free') {
    if(integer) {
      arg <- lapply(arg, formatC)
    } else {
      arg <- lapply(arg, as.character)
    }
    arg <- unlist(arg)
    cat(paste0(paste(arg, sep = ' ', collapse = ' '), '\n'), file=file, append=append)
  } else if(format == 'fixed') { 
    arg <- unlist(lapply(arg, as.list), recursive = FALSE)
    if(length(width) == 1) width <- rep(width, length(arg)) 
    arg <- lapply(1:length(arg), function(i) rmfi_ifelse0(nchar(arg[[i]]) > width[i], formatC(arg[[i]], width = width[i]), paste0(paste0(rep(' ', width[i]-nchar(arg[[i]])), collapse = ''), as.character(arg[[i]]), collapse = '')))
    arg <- lapply(1:length(arg), function(i) paste0(strsplit(arg[[i]], '')[[1]][1:width[i]], collapse = ''))
    arg <- unlist(arg)
    cat(paste0(paste0(arg, collapse=''), '\n'), file=file, append=append)
  }
}
