#' Create an \code{RMODFLOW} upw object
#' 
#' \code{rmf_create_upw} creates an \code{RMODFLOW} upw object.
#' 
#' @param dis RMODFLOW dis object
#' @param iupwcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param npupw number of upw parameters; defaults to 0
#' @param iphdry logical; indicating if head will be set to hdry when it's less than 1E-4 above the cell bottom; defaults to TRUE
#' @param laytyp vector of flags for each layer, specifying layer type; defaults to all confined (0) except the first layer (1)
#' @param layavg vector of flags for each layer, specifying interblock transmissivity calculation method; defaults to 0 for each layer
#' @param chani vector of flags or horizontal anisotropies for each layer; defaults to 1 for each layer
#' @param layvka vector of flags for each layer, indicating whether vka is the vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to 0 for each layer
#' @param parnam vector of parameter names; names should not be more than 10 characters, are not case sensitive, and should be unique
#' @param partyp vector of parameter types; the upw parameter types are HK, HANI, VK, VANI, SS, SY, or VKCB
#' @param parval vector of parameter values
#' @param nclu vector with the number of clusters required for each parameter
#' @param mltarr matrix of multiplier array names, with dis$nlay rows and upw$npupw columns; cells with non-occurring layer-parameter combinations should be NA
#' @param zonarr matrix of zone array names, with dis$nlay rows and upw$npupw columns; cells with non-occurring layer-parameter combinations should be NA
#' @param iz character matrix of zone number combinations separated by spaces, with dis$nlay rows and upw$npupw columns; cells with non-occurring layer-parameter combinations should be NA; if zonarr is "ALL", iz should be ""
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk. If not read for a specific layer, set all values in that layer to NA.
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5. If not read for a specific layer, set all values in that layer to NA.
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15. If not read for a specific layer, set all values in that layer to NA.
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to 0. If not read for a specific layer, set all values in that layer to NA.
#' @return Object of class upw
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @export
#' @seealso \code{\link{rmf_read_upw}}, \code{\link{rmf_write_upw}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_create_upw <- function(dis = rmf_create_dis(),
                           iupwcb = 0,
                           hdry = -888,
                           npupw = 0,
                           iphdry = TRUE,
                           laytyp = ifelse(dis$nlay == 1, list(1), list(c(1,rep(0, dis$nlay - 1))))[[1]],
                           layavg = laytyp * 0,
                           chani = rep(1, dis$nlay),
                           layvka = rep(0, dis$nlay),
                           parnam = NULL,
                           partyp = NULL,
                           parval = NULL,
                           nclu = NULL,
                           mltarr = NULL,
                           zonarr = NULL,
                           iz = NULL,
                           hk = rmf_create_array(0.0001, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           hani = rmf_create_array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           vka = hk,
                           ss = rmf_create_array(1E-5, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           sy = rmf_create_array(0.15, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           vkcb = rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))) {

  upw <- NULL
  
  # data set 0
  # to provide comments, use ?comment on the resulting upw object
  
  # data set 1
  upw$iupwcb <- iupwcb
  upw$hdry <- hdry
  upw$npupw <- npupw
  upw$iphdry <- iphdry
  
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
  upw$parnam <- parnam
  upw$partyp <- partyp
  upw$parval <- parval
  upw$nclu <- nclu
  upw$mltarr <- mltarr
  upw$zonarr <- zonarr
  upw$iz <- iz
  
  # data set 9-14
  if(!("HK" %in% upw$partyp)) upw$hk <- rmf_create_array(hk,
                                                         dim = rmfi_ifelse0(length(dim(hk)) > 2, dim(hk), c(dim(hk),1)))
  if(!("HANI" %in% upw$partyp) && any(upw$chani <= 0)) upw$hani <- rmf_create_array(hani,
                                                                                    dim = rmfi_ifelse0(length(dim(hani)) > 2, dim(hani), c(dim(hani),1)))
  if(!("VK" %in% upw$partyp | "VANI" %in% upw$partyp)) upw$vka <- rmf_create_array(vka,
                                                                                   dim = rmfi_ifelse0(length(dim(vka)) > 2, dim(vka), c(dim(vka),1)))
  if(!("SS" %in% upw$partyp) && 'TR' %in% dis$sstr) upw$ss <- rmf_create_array(ss,
                                                                               dim = rmfi_ifelse0(length(dim(ss)) > 2, dim(ss), c(dim(ss),1)))
  if(!("SY" %in% upw$partyp) && 'TR' %in% dis$sstr && any(upw$laytyp != 0)) upw$sy <- rmf_create_array(sy,
                                                                                                       dim = rmfi_ifelse0(length(dim(sy)) > 2, dim(sy), c(dim(sy),1)))
  if(!("VKCB" %in% upw$partyp) && any(dis$laycbd != 0)) upw$vkcb <- rmf_create_array(vkcb,
                                                                                     dim = rmfi_ifelse0(length(dim(vkcb)) > 2, dim(vkcb), c(dim(vkcb),1)))

  class(upw) <- c('upw','rmf_package')
  return(upw)
}

#' Read a MODFLOW-NWT Upstream Weighting file
#' 
#' \code{rmf_read_upw} reads in a MODFLOW-NWT upstream weighting file and returns it as an \code{\link{RMODFLOW}} upw object.
#' 
#' @param file filename; typically '*.upw'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are 'free' format and INTERNAL or CONSTANT.
#' @return object of class upw
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @export
#' @seealso \code{\link{rmf_write_upw}}, \code{\link{rmf_create_upw}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_read_upw <- function(file = {cat('Please select upw file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
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
  upw$iphdry <- as.numeric(data_set_1$variables[4])
  upw_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  upw$laytyp <- as.numeric(rmfi_parse_variables(upw_lines)$variables)[1:dis$nlay]
  upw_lines <- upw_lines[-1]
  
  # data set 3
  upw$layavg <- as.numeric(rmfi_parse_variables(upw_lines)$variables)[1:dis$nlay]
  upw_lines <- upw_lines[-1]
  
  # data set 4
  upw$chani <- as.numeric(rmfi_parse_variables(upw_lines)$variables)[1:dis$nlay]
  upw_lines <- upw_lines[-1]
  
  # data set 5
  upw$layvka <- as.numeric(rmfi_parse_variables(upw_lines)$variables)[1:dis$nlay]
  upw_lines <- upw_lines[-1]
  
  # data set 6
  upw$laywet <- as.numeric(rmfi_parse_variables(upw_lines)$variables)[1:dis$nlay]
  upw_lines <- upw_lines[-1]
  
  # data set 7-8
  if(upw$npupw > 0) {
    upw$parnam <- vector(mode='character',length=upw$npupw)
    upw$partyp <- vector(mode='character',length=upw$npupw)
    upw$parval <- vector(mode='numeric',length=upw$npupw)
    upw$nclu <- vector(mode='numeric',length=upw$npupw)
    upw$mltarr <- matrix(nrow=dis$nlay, ncol=upw$npupw)
    upw$zonarr <- matrix(nrow=dis$nlay, ncol=upw$npupw)
    upw$iz <- matrix(nrow=dis$nlay, ncol=upw$npupw)
    for(i in 1:upw$npupw) {
      data_set_7 <- rmfi_parse_variables(upw_lines)
      upw_lines <- data_set_7$remaining_lines
      upw$parnam[i] <- data_set_7$variables[1]
      upw$partyp[i] <- data_set_7$variables[2]
      upw$parval[i] <- as.numeric(data_set_7$variables[3])
      upw$nclu[i] <- as.numeric(data_set_7$variables[4])
      for(j in 1:upw$nclu[i]) {
        data_set_8 <- rmfi_parse_variables(upw_lines)
        upw_lines <- data_set_8$remaining_lines
        k <- as.numeric(data_set_8$variables[1])
        upw$mltarr[k,i] <- data_set_8$variables[2]
        upw$zonarr[k,i] <- data_set_8$variables[3]
        upw$iz[k,i] <- paste(data_set_8$variables[-c(1:3)],collapse=' ')
        rm(data_set_8)
      } 
      rm(data_set_7)
    }
  }
  
  # data set 9-14
  if(!('HK' %in% upw$partyp)) upw$hk <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(upw$chani <= 0) && !('HANI' %in% upw$partyp)) upw$hani <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(!('VK' %in% upw$partyp || 'VANI' %in% upw$partyp))upw$vka <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(dis$sstr == 'TR')) {
    if(!('SS' %in% upw$partyp)) upw$ss <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
    if(any(upw$laytyp != 0) && !('SY' %in% upw$partyp)) upw$sy <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  }     
  if(any(dis$laycbd != 0) && !("VKCB" %in% upw$partyp)) upw$vkcb <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  
  for(k in 1:dis$nlay) {
    
    # data set 9
    if(is.null(upw$hk)) {
      upw_lines <- upw_lines[-1]  
    } else {
      data_set_10 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
      upw_lines <- data_set_10$remaining_lines
      upw$hk[,,k] <- data_set_10$array
      rm(data_set_10)
    }
    
    # data set 10
    if(upw$chani[k] <= 0) {
      if(is.null(upw$hani)) {
        upw_lines <- upw_lines[-1]  
      } else {
        data_set_11 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
        upw_lines <- data_set_11$remaining_lines
        upw$hani[,,k] <- data_set_11$array
        rm(data_set_11)
      }
    }
    
    # data set 11
    if(is.null(upw$vka)) {
      upw_lines <- upw_lines[-1]  
    } else {
      data_set_12 <- rmfi_parse_array(upw_lines,dis$nrow,dis$ncol,1, file = file, ...)
      upw_lines <- data_set_12$remaining_lines
      upw$vka[,,k] <- data_set_12$array
      rm(data_set_12)
    }
    
    # data set 12
    if('TR' %in% dis$sstr) {
      if(is.null(upw$ss)) {
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
      if(is.null(upw$sy)) {
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
      if(is.null(upw$vkcb)) {
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
  if(upw$npupw != 0) {
    for(i in 1:upw$npupw) {
      rmfi_write_variables(upw$parnam[i],upw$partyp[i],upw$parval[i],upw$nclu[i], file = file)
      layers <- which(!is.na(upw$mltarr[,i]))
      for(j in 1:upw$nclu[i]) {
        rmfi_write_variables(layers[j],upw$mltarr[layers[j],i],upw$zonarr[layers[j],i],upw$iz[layers[j],i], file=file)
      } 
    }
  }
  
  # data set 9-14
  for(k in 1:dis$nlay) {
    
    # data set 9
    if('HK' %in% upw$partyp) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(upw$hk[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 10
    if(upw$chani[k] <= 0) {
      if('HANI' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$hani[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 11
    if('VK' %in% upw$partyp | 'VANI' %in% upw$partyp) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(upw$vka[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 12
    if('TR' %in% dis$sstr) {
      if('SS' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$ss[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 13
    if('TR' %in% dis$sstr & upw$laytyp[k] != 0) {
      if('SY' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$sy[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 14
    if(dis$laycbd[k] != 0) {
      if('VKCB' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$vkcb[,,k], file = file, iprn = iprn, ...)
      }
    }
  }
}


