#' Create an \code{RMODFLOW} lpf object
#' 
#' \code{rmf_create_lpf} creates an \code{RMODFLOW} lpf object.
#' 
#' @param dis RMODFLOW dis object
#' @param ilpfcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param nplpf number of lpf parameters; defaults to 0
#' @param storagecoefficient logical; should STORAGECOEFFICIENT keyword be included?; defaults to FALSE
#' @param constantcv logical; should CONSTANTCV keyword be included?; defaults to FALSE
#' @param thickstrt logical; should THICKSTRT keyword be included?; defaults to FALSE
#' @param nocvcorrection logical; should NOCVCORRECTION keyword be included?; defaults to FALSE
#' @param novfc logical; should NOVFC keyword be included?; defaults to FALSE
#' @param noparcheck logical; should NOPARCHECK keyword be included?; defaults to FALSE
#' @param laytyp vector of flags for each layer, specifying layer type; defaults to all confined (0) except the first layer (1)
#' @param layavg vector of flags for each layer, specifying interblock transmissivity calculation method; defaults to 0 for each layer
#' @param chani vector of flags or horizontal anisotropies for each layer; defaults to 1 for each layer
#' @param layvka vector of flags for each layer, indicating whether vka is the vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to 0 for each layer
#' @param laywet vector of flags for each layer, indicating if wetting is active; defaults to 0 for each layer
#' @param wetfct is a factor that is included in the calculation of the head that is initially established at a cell when it is converted from dry to wet; defaults to 0.1
#' @param iwetit is the iteration interval for attempting to wet cells; defaults to 1
#' @param ihdwet is a flag that determines which equation is used to define the initial head at cells that become wet; defaults to 0
#' @param parnam vector of parameter names; names should not be more than 10 characters, are not case sensitive, and should be unique
#' @param partyp vector of parameter types; the lpf parameter types are HK, HANI, VK, VANI, SS, SY, or VKCB
#' @param parval vector of parameter values
#' @param nclu vector with the number of clusters required for each parameter
#' @param mltarr matrix of multiplier array names, with dis$nlay rows and lpf$nplpf columns; cells with non-occurring layer-parameter combinations should be NA
#' @param zonarr matrix of zone array names, with dis$nlay rows and lpf$nplpf columns; cells with non-occurring layer-parameter combinations should be NA
#' @param iz character matrix of zone number combinations separated by spaces, with dis$nlay rows and lpf$nplpf columns; cells with non-occurring layer-parameter combinations should be NA; if zonarr is "ALL", iz should be ""
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk. If not read for a specific layer, set all values in that layer to NA.
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5. If not read for a specific layer, set all values in that layer to NA.
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15. If not read for a specific layer, set all values in that layer to NA.
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to 0. If not read for a specific layer, set all values in that layer to NA.
#' @param wetdry 3d array with a wetting threshold and flag indicating which neighboring cells can cause a cell to become wet; defaults to -0.01. If not read for a specific layer, set all values in that layer to NA.
#' @return Object of class lpf
#' @export
#' @seealso \code{\link{rmf_read_lpf}}, \code{\link{rmf_write_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
rmf_create_lpf <- function(dis = rmf_create_dis(),
                         ilpfcb = 0,
                         hdry = -888,
                         nplpf = 0,
                         storagecoefficient = FALSE,
                         constantcv = FALSE,
                         thickstrt = FALSE,
                         nocvcorrection = FALSE,
                         novfc = FALSE,
                         noparcheck = FALSE,
                         laytyp = ifelse(dis$nlay == 1, list(1), list(c(1,rep(0, dis$nlay - 1))))[[1]],
                         layavg = laytyp * 0,
                         chani = rep(1, dis$nlay),
                         layvka = rep(0, dis$nlay),
                         laywet = rep(0, dis$nlay),
                         wetfct = 0.1,
                         iwetit = 1,
                         ihdwet = 0,
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
                         vkcb = rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                         wetdry = rmf_create_array(-0.01, dim = c(dis$nrow, dis$ncol, dis$nlay))) {
    
  lpf <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting lpf object
  
  # data set 1
    lpf$ilpfcb <- ilpfcb
    lpf$hdry <- hdry
    lpf$nplpf <- nplpf
    lpf$storagecoefficient <- storagecoefficient
    lpf$constantcv <- constantcv
    lpf$thickstrt <- thickstrt
    lpf$nocvcorrection <- nocvcorrection
    lpf$novfc <- novfc
    lpf$noparcheck <- noparcheck
  
  # data set 2
    lpf$laytyp <- laytyp
  
  # data set 3
    lpf$layavg <- layavg
  
  # data set 4
    lpf$chani <- chani
  
  # data set 5
    lpf$layvka <- layvka
  
  # data set 6
    lpf$laywet <- laywet
  
  # data set 7
    if(!as.logical(prod(lpf$laywet==0))) {
      lpf$wetfct <- wetfct
      lpf$iwetit <- iwetit
      lpf$ihdwet <- ihdwet
    }
  
  # data set 8-9
    lpf$parnam <- parnam
    lpf$partyp <- partyp
    lpf$parval <- parval
    lpf$nclu <- nclu
    lpf$mltarr <- mltarr
    lpf$zonarr <- zonarr
    lpf$iz <- iz

  # data set 10-16
    if(!("HK" %in% lpf$partyp)) lpf$hk <- rmf_create_array(hk,
                                                           dim = rmfi_ifelse0(length(dim(hk)) > 2, dim(hk), c(dim(hk),1)))
    if(!("HANI" %in% lpf$partyp) && any(lpf$chani <= 0)) lpf$hani <- rmf_create_array(hani,
                                                                                      dim = rmfi_ifelse0(length(dim(hani)) > 2, dim(hani), c(dim(hani),1)))
    if(!("VK" %in% lpf$partyp | "VANI" %in% lpf$partyp)) lpf$vka <- rmf_create_array(vka,
                                                                                     dim = rmfi_ifelse0(length(dim(vka)) > 2, dim(vka), c(dim(vka),1)))
    if(!("SS" %in% lpf$partyp) && 'TR' %in% dis$sstr) lpf$ss <- rmf_create_array(ss,
                                                                                 dim = rmfi_ifelse0(length(dim(ss)) > 2, dim(ss), c(dim(ss),1)))
    if(!("SY" %in% lpf$partyp) && 'TR' %in% dis$sstr && any(lpf$laytyp != 0)) lpf$sy <- rmf_create_array(sy,
                                                                                                         dim = rmfi_ifelse0(length(dim(sy)) > 2, dim(sy), c(dim(sy),1)))
    if(!("VKCB" %in% lpf$partyp) && any(dis$laycbd != 0)) lpf$vkcb <- rmf_create_array(vkcb,
                                                                                       dim = rmfi_ifelse0(length(dim(vkcb)) > 2, dim(vkcb), c(dim(vkcb),1)))
    if(any(lpf$laywet != 0) && any(lpf$laytyp != 0)) lpf$wetdry <- rmf_create_array(wetdry,
                                                                                    dim = rmfi_ifelse0(length(dim(wetdry)) > 2, dim(wetdry), c(dim(wetdry),1)))
    
  class(lpf) <- c('lpf','rmf_package')
  return(lpf)
}

#' @describeIn rmf_create_lpf Deprecated function name
#' @export
create_lpf <- function(...) {
  .Deprecated(new = "rmf_create_lpf", old = "create_lpf")
  rmf_create_lpf(...)
}

#' Read a MODFLOW layer-property flow file
#' 
#' \code{read_lpf} reads in a MODFLOW layer property file and returns it as an \code{\link{RMODFLOW}} lpf object.
#' 
#' @param file filename; typically '*.lpf'.
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class lpf
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_lpf}}, \code{\link{rmf_create_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
rmf_read_lpf <- function(file = {cat('Please select lpf file ...\n'); file.choose()}, 
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         ...) {
  
  lpf_lines <- readr::read_lines(file)
  lpf <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(lpf_lines)
  comment(lpf) <- data_set_0$comments
  lpf_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(lpf_lines)
  lpf$ilpfcb <- as.numeric(data_set_1$variables[1])
  lpf$hdry <- as.numeric(data_set_1$variables[2])
  lpf$nplpf <- as.numeric(data_set_1$variables[3])
  lpf$storagecoefficient <- 'STORAGECOEFFICIENT' %in% data_set_1$variables
  lpf$constantcv <- 'CONSTANTCV' %in% data_set_1$variables
  lpf$thickstrt <- 'THICKSTRT' %in% data_set_1$variables
  lpf$nocvcorrection <- 'NOCVCORRECTION' %in% data_set_1$variables
  lpf$novfc <- 'NOVFC' %in% data_set_1$variables
  lpf$noparcheck <- 'NOPARCHECK' %in% data_set_1$variables
  lpf_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  lpf$laytyp <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay]
  lpf_lines <- lpf_lines[-1]
  
  # data set 3
  lpf$layavg <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay]
  lpf_lines <- lpf_lines[-1]
  
  # data set 4
  lpf$chani <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay]
  lpf_lines <- lpf_lines[-1]
  
  # data set 5
  lpf$layvka <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay]
  lpf_lines <- lpf_lines[-1]
  
  # data set 6
  lpf$laywet <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay]
  lpf_lines <- lpf_lines[-1]
  
  # data set 7
  if(!as.logical(prod(lpf$laywet==0))) {
    data_set_7 <- rmfi_parse_variables(lpf_lines) 
    lpf$wetfct <- as.numeric(data_set_7$variables[1])
    lpf$iwetit <- as.numeric(data_set_7$variables[2])
    lpf$ihdwet <- as.numeric(data_set_7$variables[3])
    lpf_lines <- data_set_7$remaining_lines
    rm(data_set_7)
  }
  
  # data set 8-9
  if(lpf$nplpf > 0) {
    lpf$parnam <- vector(mode='character',length=lpf$nplpf)
    lpf$partyp <- vector(mode='character',length=lpf$nplpf)
    lpf$parval <- vector(mode='numeric',length=lpf$nplpf)
    lpf$nclu <- vector(mode='numeric',length=lpf$nplpf)
    lpf$mltarr <- matrix(nrow=dis$nlay, ncol=lpf$nplpf)
    lpf$zonarr <- matrix(nrow=dis$nlay, ncol=lpf$nplpf)
    lpf$iz <- matrix(nrow=dis$nlay, ncol=lpf$nplpf)
    for(i in 1:lpf$nplpf) {
      data_set_8 <- rmfi_parse_variables(lpf_lines)
      lpf_lines <- data_set_8$remaining_lines
      lpf$parnam[i] <- data_set_8$variables[1]
      lpf$partyp[i] <- data_set_8$variables[2]
      lpf$parval[i] <- as.numeric(data_set_8$variables[3])
      lpf$nclu[i] <- as.numeric(data_set_8$variables[4])
      for(j in 1:lpf$nclu[i]) {
        data_set_9 <- rmfi_parse_variables(lpf_lines)
        lpf_lines <- data_set_9$remaining_lines
        k <- as.numeric(data_set_9$variables[1])
        lpf$mltarr[k,i] <- data_set_9$variables[2]
        lpf$zonarr[k,i] <- data_set_9$variables[3]
        lpf$iz[k,i] <- paste(data_set_9$variables[-c(1:3)],collapse=' ')
        rm(data_set_9)
      }
      rm(data_set_8) 
    }
  }
  
  # data set 10-16
  if(!('HK' %in% lpf$partyp)) lpf$hk <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(lpf$chani <= 0) && !('HANI' %in% lpf$partyp)) lpf$hani <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(!('VK' %in% lpf$partyp || 'VANI' %in% lpf$partyp))lpf$vka <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(dis$sstr == 'TR')) {
    if(!('SS' %in% lpf$partyp)) lpf$ss <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
    if(any(lpf$laytyp != 0) && !('SY' %in% lpf$partyp)) lpf$sy <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  }     
  if(any(dis$laycbd != 0) && !("VKCB" %in% lpf$partyp)) lpf$vkcb <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(lpf$laywet != 0) && any(lpf$laytyp != 0)) lpf$wetdry <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  for(k in 1:dis$nlay) {
    
    # data set 10
    if(is.null(lpf$hk)) {
      lpf_lines <- lpf_lines[-1]  
    } else {
      data_set_10 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
      lpf_lines <- data_set_10$remaining_lines
      lpf$hk[,,k] <- data_set_10$array
      rm(data_set_10)
    }
    
    # data set 11
    if(lpf$chani[k] <= 0) {
      if(is.null(lpf$hani)) {
        lpf_lines <- lpf_lines[-1]  
      } else {
        data_set_11 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
        lpf_lines <- data_set_11$remaining_lines
        lpf$hani[,,k] <- data_set_11$array
        rm(data_set_11)
      }
    }
    
    # data set 12
    if(is.null(lpf$vka)) {
      lpf_lines <- lpf_lines[-1]  
    } else {
      data_set_12 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
      lpf_lines <- data_set_12$remaining_lines
      lpf$vka[,,k] <- data_set_12$array
      rm(data_set_12)
    }
    
    # data set 13
    if('TR' %in% dis$sstr) {
      if(is.null(lpf$ss)) {
        lpf_lines <- lpf_lines[-1]  
      } else {
        data_set_13 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
        lpf_lines <- data_set_13$remaining_lines
        lpf$ss[,,k] <- data_set_13$array
        rm(data_set_13)
      }
    }
    
    # data set 14
    if('TR' %in% dis$sstr && lpf$laytyp[k] != 0) {
      if(is.null(lpf$sy)) {
        lpf_lines <- lpf_lines[-1]  
      } else {
        data_set_14 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
        lpf_lines <- data_set_14$remaining_lines
        lpf$sy[,,k] <- data_set_14$array
        rm(data_set_14)
      }
    }
    
    # data set 15
    if(dis$laycbd[k] != 0) {
      if(is.null(lpf$vkcb)) {
        lpf_lines <- lpf_lines[-1]  
      } else {
        data_set_15 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
        lpf_lines <- data_set_15$remaining_lines
        lpf$vkcb[,,k] <- data_set_15$array
        rm(data_set_15)
      }
    }
    
    # data set 16
    if(lpf$laywet[k] != 0 & lpf$laytyp[k] != 0) {
      data_set_16 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
      lpf_lines <- data_set_16$remaining_lines
      lpf$wetdry[,,k] <- data_set_16$array
      rm(data_set_16)
    }     
  }
  
  class(lpf) <- c('lpf','rmf_package')
  return(lpf)
}

#' @describeIn rmf_read_lpf Deprecated function name
#' @export
read_lpf <- function(...) {
  .Deprecated(new = "rmf_read_lpf", old = "read_lpf")
  rmf_read_lpf(...)
}



#' Write a MODFLOW layer-property flow file
#' 
#' @param lpf an \code{\link{RMODFLOW}} lpf object
#' @param file filename to write to; typically '*.lpf'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
rmf_write_lpf <- function(lpf,
                          file = {cat('Please select lpf file to overwrite or provide new filename ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          iprn=-1,
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Layer-Property Flow Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(lpf)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(lpf$ilpfcb,lpf$hdry,lpf$nplpf,ifelse(lpf$storagecoefficient,'STORAGECOEFFICIENT',''),ifelse(lpf$constantcv,'CONSTANTCV',''),ifelse(lpf$thickstrt,'THICKSTRT',''),ifelse(lpf$nocvcorrection,'NOCVCORRECTION',''),ifelse(lpf$novfc,'NOVFC',''),ifelse(lpf$noparcheck,'NOPARCHECK',''), file=file)
  
  # data set 2
  rmfi_write_variables(lpf$laytyp, file = file)
  
  # data set 3
  rmfi_write_variables(lpf$layavg, file = file)
  
  # data set 4
  rmfi_write_variables(lpf$chani, file = file)
  
  # data set 5
  rmfi_write_variables(lpf$layvka, file = file)
  
  # data set 6
  rmfi_write_variables(lpf$laywet, file = file)
  
  # data set 7
  if(!as.logical(prod(lpf$laywet==0))) {
    rmfi_write_variables(lpf$wetfct,lpf$iwetit,lpf$ihdwet, file = file)
  }
  
  # data set 8-9
  if(lpf$nplpf != 0) {
    for(i in 1:lpf$nplpf) {
      rmfi_write_variables(lpf$parnam[i],lpf$partyp[i],lpf$parval[i],lpf$nclu[i], file = file)
      layers <- which(!is.na(lpf$mltarr[,i]))
      for(j in 1:lpf$nclu[i]) {
        rmfi_write_variables(layers[j],lpf$mltarr[layers[j],i],lpf$zonarr[layers[j],i],lpf$iz[layers[j],i], file=file)
      } 
    }
  }
  
  # data set 10-16
  for(k in 1:dis$nlay) {
    
    # data set 10
    if('HK' %in% lpf$partyp) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(lpf$hk[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 11
    if(lpf$chani[k] <= 0) {
      if('HANI' %in% lpf$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$hani[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 12
    if('VK' %in% lpf$partyp | 'VANI' %in% lpf$partyp) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(lpf$vka[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 13
    if('TR' %in% dis$sstr) {
      if('SS' %in% lpf$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$ss[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 14
    if('TR' %in% dis$sstr & lpf$laytyp[k] != 0) {
      if('SY' %in% lpf$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$sy[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 15
    if(dis$laycbd[k] != 0) {
      if('VKCB' %in% lpf$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$vkcb[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 16
    if(lpf$laywet[k] != 0 & lpf$laytyp[k] != 0) {
      rmfi_write_array(lpf$wetdry[,,k], file = file, iprn = iprn, ...)
    }     
  }
}

#' @describeIn rmf_write_lpf Deprecated function name
#' @export
write_lpf <- function(...) {
  .Deprecated(new = "rmf_write_lpf", old = "write_lpf")
  rmf_write_lpf(...)
}
