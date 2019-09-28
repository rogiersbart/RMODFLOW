#' Create an \code{RMODFLOW} upw object
#' 
#' \code{rmf_create_upw} creates an \code{RMODFLOW} upw object.
#' 
#' @param dis RMODFLOW dis object
#' @param iupwcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param iphdry logical; indicating if head will be set to hdry when it's less than 1E-4 above the cell bottom; defaults to TRUE
#' @param noparcheck logical; should NOPARCHECK keyword be included?; defaults to FALSE
#' @param laytyp vector of flags for each layer, specifying layer type; defaults to all confined (0) except the first layer (1)
#' @param layavg vector of flags for each layer, specifying interblock transmissivity calculation method; defaults to 0 for each layer
#' @param chani vector of flags or horizontal anisotropies for each layer; defaults to 1 for each layer
#' @param layvka vector of flags for each layer, indicating whether vka is the vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to 0 for each layer
#' @param parameters list of \code{rmf_parameter} as created by \code{\link{rmf_create_parameter}}. See details; defaults to NULL
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk. If not read for a specific layer, set all values in that layer to NA.
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5. If not read for a specific layer, set all values in that layer to NA.
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15. If not read for a specific layer, set all values in that layer to NA.
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to 0. If not read for a specific layer, set all values in that layer to NA.
#' @return Object of class upw
#' @details Flow variables are any of \code{HK, HANI, VK, VANI, SS, SY and VKCB}. A single variable can be specified either through the use of parameters or by using direct array input.
#'          When a flow variable for a specific layer is specified using parameters, all flow variables of the type must be specified by parameters. E.g. if a flow parameter defines HK for layer 1, HK must be defined for all layers using flow parameters instead of direct array input.
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @export
#' @seealso \code{\link{rmf_create_parameter}}, \code{\link{rmf_read_upw}}, \code{\link{rmf_write_upw}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_create_upw <- function(dis,
                           iupwcb = 0,
                           hdry = -888,
                           iphdry = TRUE,
                           noparcheck = FALSE,
                           laytyp = rmfi_ifelse0(dis$nlay == 1, 1, c(1,rep(0, dis$nlay - 1))),
                           layavg = rep(0, dis$nlay),
                           chani = rep(1.0, dis$nlay),
                           layvka = rep(0, dis$nlay),
                           parameters = NULL,
                           hk = rmf_create_array(0.0001, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           hani = rmfi_ifelse0(all(chani <= 0), hk*0 + 1, NULL),
                           vka = rmfi_ifelse0(all(layvka == 0), hk, NULL),
                           ss = rmfi_ifelse0('TR' %in% dis$sstr, hk*0 + 1e-5, NULL),
                           sy = rmfi_ifelse0('TR' %in% dis$sstr && all(laytyp > 0), hk*0 + 0.15, NULL),
                           vkcb = NULL) {
  
  upw <- list()
  
  # data set 0
  # to provide comments, use ?comment on the resulting upw object
  
  # data set 1
  upw$iupwcb <- iupwcb
  upw$hdry <- hdry
  upw$npupw <- 0
  upw$iphdry <- iphdry
  upw$noparcheck <- noparcheck
  
  # data set 2
  upw$laytyp <- laytyp
  
  # data set 3
  upw$layavg <- layavg
  
  # data set 4
  upw$chani <- chani
  
  # data set 5
  upw$layvka <- layvka
  
  # data set 6
  upw$laywet <- rep(0, dis$nlay)
  
  # data set 7-8
  types <- NULL
  if(!is.null(parameters)) {
    upw$npupw <- length(parameters)
    # error check
    if(any(vapply(parameters, function(i) is.null(attr(i, 'partyp')) || is.null(attr(i, 'layer')) || is.null(attr(i, 'parnam')) || is.null(attr(i, 'parval')), TRUE))) {
      stop('Please make sure all parameters have a parnam, parval, partyp and layer attribute', call. = FALSE)
    }
    
    types <- toupper(unique(vapply(parameters, function(i) attr(i, 'partyp'), 'text')))
    upw$parameters <- list()
    upw$parameter_values <- NULL
    for(i in 1:upw$npupw) {
      attrb <- attributes(parameters[[i]])
      parnam <- attrb$parnam
      upw$parameter_values[parnam] <- attrb$parval
      
      upw$parameters[[parnam]] <- parameters[[i]]
      upw$parameters[[parnam]][which(is.na(upw$parameters[[parnam]]))] <- 0
      
      if(is.null(upw[[tolower(attrb$partyp)]])) upw[[tolower(attrb$partyp)]] <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))
      upw[[tolower(attrb$partyp)]][,,unique(attrb$layer)] <- upw[[tolower(attrb$partyp)]][,,unique(attrb$layer)] + c(upw$parameters[[parnam]])
      
    }
    
    # check for every type if all layers are defined
    layer_check <- list()
    for(i in 1:upw$npupw) {
      layer_check[[toupper(attr(upw$parameters[[i]], 'partyp'))]] <- append(layer_check[[toupper(attr(upw$parameters[[i]], 'partyp'))]], c(unlist(attr(upw$parameters[[i]], 'layer'))))
    }
    # to prevent false positive since last layer can not have a confining bed
    if('VKCB' %in% names(layer_check)) layer_check[['VKCB']] <- append(layer_check[['VKCB']], dis$nlay)
    
    layer_check <- structure(vapply(seq_along(layer_check), function(i) isTRUE(all.equal(sort(layer_check[[i]]), 1:dis$nlay)), TRUE), names = names(layer_check))
    if(any(!layer_check)) stop(paste('Parameters are used to define ', names(layer_check)[!layer_check],', but not all layers are defined through parameters.'), call. = FALSE)
    
  } 
  
  # data set 9-14
  if(!("HK" %in% types)) {
    if(is.null(hk)) stop('Please specify a hk argument', call. = FALSE)
    upw$hk <- rmf_create_array(hk, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(!("HANI" %in% types) && any(upw$chani <= 0)) {
    if(is.null(hani)) stop('Please specify a hani argument', call. = FALSE)
    upw$hani <- rmf_create_array(hani, dim = c(dis$nrow, dis$ncol, dis$nlay))
  } 
  if(!("VK" %in% types | "VANI" %in% types)) {
    if(is.null(vka)) stop('Please specify a vka argument', call. = FALSE)
    upw$vka <- rmf_create_array(vka, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(!("SS" %in% types) && 'TR' %in% dis$sstr) {
    if(is.null(ss)) stop('Please specify a ss argument', call. = FALSE)
    upw$ss <- rmf_create_array(ss, dim = c(dis$nrow, dis$ncol, dis$nlay))
  } 
  if(!("SY" %in% types) && 'TR' %in% dis$sstr && any(upw$laytyp != 0)) {
    if(is.null(sy)) stop('Please specify a sy argument', call. = FALSE)
    upw$sy <- rmf_create_array(sy, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(!("VKCB" %in% types) && any(dis$laycbd != 0)) {
    if(is.null(vkcb)) stop('Please specify a vkcb argument', call. = FALSE)
    upw$vkcb <- rmf_create_array(vkcb, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  
  class(upw) <- c('upw','rmf_package')
  return(upw)
}



#' Read a MODFLOW-NWT Upstream Weighting file
#' 
#' \code{rmf_read_upw} reads in a MODFLOW-NWT upstream weighting file and returns it as an \code{\link{RMODFLOW}} upw object.
#' 
#' @param file filename; typically '*.upw'
#' @param dis an \code{RMODFLOW} dis object
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are 'free' format and INTERNAL or CONSTANT.
#' @return object of class upw
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @export
#' @seealso \code{\link{rmf_write_upw}}, \code{\link{rmf_create_upw}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_read_upw <- function(file = {cat('Please select upw file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         mlt = NULL,
                         zon = NULL,
                         ...) {
  
  upw_lines <- readr::read_lines(file)
  upw <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(upw_lines)
  comment(upw) <- data_set_0$comments
  upw_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(upw_lines)
  upw$iupwcb <- as.numeric(data_set_1$variables[1])
  upw$hdry <- as.numeric(data_set_1$variables[2])
  upw$npupw <- as.numeric(data_set_1$variables[3])
  upw$ihdry <- as.numeric(data_set_1$variables[4]) != 0
  upw$noparcheck <- 'NOPARCHECK' %in% data_set_1$variables
  upw_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  upw$laytyp <- as.numeric(rmfi_parse_variables(upw_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  upw_lines <- upw_lines[-1]
  
  # data set 3
  upw$layavg <- as.numeric(rmfi_parse_variables(upw_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  upw_lines <- upw_lines[-1]
  
  # data set 4
  upw$chani <- as.numeric(rmfi_parse_variables(upw_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  upw_lines <- upw_lines[-1]
  
  # data set 5
  upw$layvka <- as.numeric(rmfi_parse_variables(upw_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  upw_lines <- upw_lines[-1]
  
  # data set 6
  upw$laywet <- as.numeric(rmfi_parse_variables(upw_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  upw_lines <- upw_lines[-1]
  
  # data set 7-8
  types <- NULL
  if(upw$npupw > 0) {
    
    upw$parameters <- list()
    upw$parameter_values <- NULL
    
    for(i in 1:upw$npupw) {
      data_set_8 <- rmfi_parse_variables(upw_lines, character = TRUE)
      parnam <- data_set_8$variables[1]
      partyp <- data_set_8$variables[2]
      parval <- as.numeric(data_set_8$variables[3])
      nclu <- as.numeric(data_set_8$variables[4])
      upw_lines <- data_set_8$remaining_lines
      rm(data_set_8)
      
      ds9 <- list(layer = NULL, mltarr = NULL, zonarr = NULL, iz = list())
      for(j in 1:nclu) {
        data_set_9 <- rmfi_parse_variables(upw_lines, character = TRUE)
        ds9$layer[j] <- as.numeric(data_set_9$variables[1])
        ds9$mltarr[j] <- data_set_9$variables[2]
        ds9$zonarr[j] <- data_set_9$variables[3]
        # zero or character entry terminates IZ
        if(ds9$zonarr[j] == 'ALL') {
          ds9$iz[[j]] <- NULL
        } else {
          iz <- suppressWarnings(as.numeric(data_set_9$variables[4:length(data_set_9$variables)]))
          ds9$iz[[j]] <- iz[1:min(length(iz), which(is.na(iz))[1] - 1, which(iz == 0)[1] - 1, na.rm = TRUE)]
        }
        upw_lines <- data_set_9$remaining_lines
        
        if(toupper(data_set_9$variables[2]) != 'NONE') {
          if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
        }
        if(toupper(data_set_9$variables[3]) != 'ALL') {
          if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
        }
        rm(data_set_9)
        
      }
      
      upw$parameter_values[parnam] <- parval
      upw$parameters[[parnam]] <- rmf_create_parameter(dis = dis, parnam = parnam, partyp = partyp, parval = parval, layer = ds9$layer, mltnam = ds9$mltarr, zonnam = ds9$zonarr, iz = ds9$iz, mlt = mlt, zon = zon)
      upw$parameters[[parnam]][which(is.na(upw$parameters[[parnam]]))] <- 0
      
      if(is.null(upw[[tolower(partyp)]])) upw[[tolower(partyp)]] <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))
      upw[[tolower(partyp)]][,,unique(ds9$layer)] <- upw[[tolower(partyp)]][,,unique(ds9$layer)] + c(upw$parameters[[parnam]])
      
      if(!(toupper(partyp) %in% types)) types <- append(types, toupper(partyp))
    }
  }
  
  # data set 9-14
  if(!('HK' %in% types)) upw$hk <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(upw$chani <= 0) && !('HANI' %in% types)) upw$hani <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(!('VK' %in% upw$partyp || 'VANI' %in% types))upw$vka <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(dis$sstr == 'TR')) {
    if(!('SS' %in% types)) upw$ss <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
    if(any(upw$laytyp != 0) && !('SY' %in% types)) upw$sy <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  }     
  if(any(dis$laycbd != 0) && !("VKCB" %in% types)) upw$vkcb <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))

  for(k in 1:dis$nlay) {
    
    # data set 9
    if('HK' %in% types) {
      upw_lines <- upw_lines[-1]  
    } else {
      data_set_10 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
      upw_lines <- data_set_10$remaining_lines
      upw$hk[,,k] <- data_set_10$array
      rm(data_set_10)
    }
    
    # data set 10
    if(upw$chani[k] <= 0) {
      if('HANI' %in% types) {
        upw_lines <- upw_lines[-1]  
      } else {
        data_set_11 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
        upw_lines <- data_set_11$remaining_lines
        upw$hani[,,k] <- data_set_11$array
        rm(data_set_11)
      }
    }
    
    # data set 11
    if('VK' %in% types || 'VANI' %in% types) {
      upw_lines <- upw_lines[-1]  
    } else {
      data_set_12 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
      upw_lines <- data_set_12$remaining_lines
      upw$vka[,,k] <- data_set_12$array
      rm(data_set_12)
    }
    
    # data set 12
    if('TR' %in% dis$sstr) {
      if('SS' %in% types) {
        upw_lines <- upw_lines[-1]  
      } else {
        data_set_13 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
        upw_lines <- data_set_13$remaining_lines
        upw$ss[,,k] <- data_set_13$array
        rm(data_set_13)
      }
    }
    
    # data set 13
    if('TR' %in% dis$sstr && upw$laytyp[k] != 0) {
      if('SY' %in% types) {
        upw_lines <- upw_lines[-1]  
      } else {
        data_set_14 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
        upw_lines <- data_set_14$remaining_lines
        upw$sy[,,k] <- data_set_14$array
        rm(data_set_14)
      }
    }
    
    # data set 14
    if(dis$laycbd[k] != 0) {
      if('VKCB' %in% types) {
        upw_lines <- upw_lines[-1]  
      } else {
        data_set_15 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
        upw_lines <- data_set_15$remaining_lines
        upw$vkcb[,,k] <- data_set_15$array
        rm(data_set_15)
      }
    }
  }
  
  class(upw) <- c('upw','rmf_package')
  return(upw)
}

#' Write a MODFLOW-NWT upstream weighting file
#' 
#' @param upw an \code{\link{RMODFLOW}} upw object
#' @param file filename to write to; typically '*.upw'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @return \code{NULL}
#' @export
rmf_write_upw <- function(upw,
                          file = {cat('Please select upw file to overwrite or provide new filename ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          iprn=-1, 
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Upstream Weighting Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(upw)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(upw$iupwcb,upw$hdry,upw$npupw, ifelse(upw$iphdry, 1, 0), file=file)
  
  # data set 2
  rmfi_write_variables(upw$laytyp, file = file)
  
  # data set 3
  rmfi_write_variables(upw$layavg, file = file)
  
  # data set 4
  rmfi_write_variables(upw$chani, file = file)
  
  # data set 5
  rmfi_write_variables(upw$layvka, file = file)
  
  # data set 6
  rmfi_write_variables(upw$laywet, file = file)
  
  # data set 7-8
  types <- NULL
  if(upw$npupw > 0) {
    for(i in 1:upw$npupw) {
      types <- append(types, attr(upw$parameters[[i]], 'partyp'))
      rmfi_write_variables(attr(upw$parameters[[i]], 'parnam'), attr(upw$parameters[[i]], 'partyp'),attr(upw$parameters[[i]], 'parval'),length(attr(upw$parameters[[i]], 'mlt')), file = file)
      for(j in 1:length(attr(upw$parameters[[i]], 'mlt'))) {
        rmfi_write_variables(attr(upw$parameters[[i]], 'layer')[j],attr(upw$parameters[[i]], 'mlt')[j], attr(upw$parameters[[i]], 'zon')[j], rmfi_ifelse0(attr(upw$parameters[[i]], 'zon')[j] == "ALL", NULL, attr(upw$parameters[[i]], 'iz')[[j]]), file=file)
      } 
    }
  }
  
  
  # data set 9-14
  for(k in 1:dis$nlay) {
    
    # data set 9
    if('HK' %in% types) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(upw$hk[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 10
    if(upw$chani[k] <= 0) {
      if('HANI' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$hani[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 11
    if('VK' %in% types|| 'VANI' %in% types) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(upw$vka[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 12
    if('TR' %in% dis$sstr) {
      if('SS' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$ss[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 13
    if('TR' %in% dis$sstr && upw$laytyp[k] != 0) {
      if('SY' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$sy[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 14
    if(dis$laycbd[k] != 0) {
      if('VKCB' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$vkcb[,,k], file = file, iprn = iprn, ...)
      }
    }
  }
}


