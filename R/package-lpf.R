#' Create an \code{RMODFLOW} lpf object
#' 
#' \code{rmf_create_lpf} creates an \code{RMODFLOW} lpf object.
#' 
#' @param dis RMODFLOW dis object
#' @param ilpfcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
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
#' @param parameters list of \code{rmf_parameter} as created by \code{\link{rmf_create_parameter}}. See details; defaults to NULL
#' @param hk 3d array with hydraulic conductivity along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param hani 3d array with the ratio of hydraulic conductivity along columns to that along rows; defaults to 1. If not read for a specific layer, set all values in that layer to NA.
#' @param vka 3d array with vertical hydraulic conductivity or the ratio of horizontal to vertical; defaults to hk. If not read for a specific layer, set all values in that layer to NA.
#' @param ss 3d array with specific storage; only required when there are transient stress periods; defaults to 1E-5. If not read for a specific layer, set all values in that layer to NA.
#' @param sy 3d array with specific yield; only required when there are transient stress periods; defaults to 0.15. If not read for a specific layer, set all values in that layer to NA.
#' @param vkcb 3d array with vertical hydraulic conductivity of quasi-three-dimensional confining beds; defaults to NULL. If not read for a specific layer, set all values in that layer to NA.
#' @param wetdry 3d array with a wetting threshold and flag indicating which neighboring cells can cause a cell to become wet; defaults to NULL. If not read for a specific layer, set all values in that layer to NA.
#' @details Flow variables are any of \code{HK, HANI, VK, VANI, SS, SY and VKCB}. A single variable can be specified either through the use of parameters or by using direct array input.
#'          When a flow variable for a specific layer is specified using parameters, all flow variables of the type must be specified by parameters. E.g. if a flow parameter defines HK for layer 1, HK must be defined for all layers using flow parameters instead of direct array input.
#' @return Object of class lpf
#' @export
#' @seealso \code{\link{rmf_create_parameter}}, \code{\link{rmf_read_lpf}}, \code{\link{rmf_write_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}
rmf_create_lpf <- function(dis,
                           ilpfcb = 0,
                           hdry = -888,
                           storagecoefficient = FALSE,
                           constantcv = FALSE,
                           thickstrt = FALSE,
                           nocvcorrection = FALSE,
                           novfc = FALSE,
                           noparcheck = FALSE,
                           laytyp = rmfi_ifelse0(dis$nlay == 1, 1, c(1,rep(0, dis$nlay - 1))),
                           layavg = rep(0, dis$nlay),
                           chani = rep(1.0, dis$nlay),
                           layvka = rep(0, dis$nlay),
                           laywet = rep(0, dis$nlay),
                           wetfct = 0.1,
                           iwetit = 1,
                           ihdwet = 0,
                           parameters = NULL,
                           hk = rmf_create_array(0.0001, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           hani = rmfi_ifelse0(all(chani <= 0), hk*0 + 1, NULL),
                           vka = rmfi_ifelse0(all(layvka == 0), hk, NULL),
                           ss = rmfi_ifelse0('TR' %in% dis$sstr, hk*0 + 1e-5, NULL),
                           sy = rmfi_ifelse0('TR' %in% dis$sstr && all(laytyp > 0), hk*0 + 0.15, NULL),
                           vkcb = NULL,
                           wetdry = NULL) {
    
  lpf <- list()
  
  # data set 0
  # to provide comments, use ?comment on the resulting lpf object
  
  # data set 1
  lpf$ilpfcb <- ilpfcb
  lpf$hdry <- hdry
  lpf$nplpf <- 0
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
  if(any(lpf$laywet != 0)) {
    lpf$wetfct <- wetfct
    lpf$iwetit <- iwetit
    lpf$ihdwet <- ihdwet
  }
  
  # data set 8-9
  types <- NULL
  if(!is.null(parameters)) {
    lpf$nplpf <- length(parameters)
    # error check
    if(any(vapply(parameters, function(i) is.null(attr(i, 'partyp')) || is.null(attr(i, 'layer')) || is.null(attr(i, 'parnam')) || is.null(attr(i, 'parval')), TRUE))) {
      stop('Please make sure all parameters have a parnam, parval, partyp and layer attribute', call. = FALSE)
    }
    
    types <- toupper(unique(vapply(parameters, function(i) attr(i, 'partyp'), 'text')))
    lpf$parameters <- list()
    lpf$parameter_values <- NULL
    for(i in 1:lpf$nplpf) {
      attrb <- attributes(parameters[[i]])
      parnam <- attrb$parnam
      lpf$parameter_values[parnam] <- attrb$parval
      
      lpf$parameters[[parnam]] <- parameters[[i]]
      if(is.null(lpf[[tolower(attrb$partyp)]])) lpf[[tolower(attrb$partyp)]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay))
      lpf[[tolower(attrb$partyp)]][,,unique(attrb$layer)] <- c(parameters[[i]])
      
    }
    
    # check for every type if all layers are defined
    layer_check <- list()
    for(i in 1:lpf$nplpf) {
        layer_check[[toupper(attr(lpf$parameters[[i]], 'partyp'))]] <- append(layer_check[[toupper(attr(lpf$parameters[[i]], 'partyp'))]], c(unlist(attr(lpf$parameters[[i]], 'layer'))))
    }
    # to prevent false positive since last layer can not have a confining bed
    if('VKCB' %in% names(layer_check)) layer_check[['VKCB']] <- append(layer_check[['VKCB']], dis$nlay)
    
    layer_check <- structure(vapply(seq_along(layer_check), function(i) isTRUE(all.equal(sort(layer_check[[i]]), 1:dis$nlay)), TRUE), names = names(layer_check))
    if(any(!layer_check)) stop(paste('Parameters are used to define ', names(layer_check)[!layer_check],', but not all layers are defined through parameters.'), call. = FALSE)
    
  } 
  
  # data set 10-16
  if(!("HK" %in% types)) {
    if(is.null(hk)) stop('Please specify a hk argument', call. = FALSE)
    lpf$hk <- rmf_create_array(hk, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(!("HANI" %in% types) && any(lpf$chani <= 0)) {
    if(is.null(hani)) stop('Please specify a hani argument', call. = FALSE)
    lpf$hani <- rmf_create_array(hani, dim = c(dis$nrow, dis$ncol, dis$nlay))
  } 
  if(!("VK" %in% types | "VANI" %in% types)) {
    if(is.null(vka)) stop('Please specify a vka argument', call. = FALSE)
    lpf$vka <- rmf_create_array(vka, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(!("SS" %in% types) && 'TR' %in% dis$sstr) {
    if(is.null(ss)) stop('Please specify a ss argument', call. = FALSE)
    lpf$ss <- rmf_create_array(ss, dim = c(dis$nrow, dis$ncol, dis$nlay))
  } 
  if(!("SY" %in% types) && 'TR' %in% dis$sstr && any(lpf$laytyp != 0)) {
    if(is.null(sy)) stop('Please specify a sy argument', call. = FALSE)
    lpf$sy <- rmf_create_array(sy, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(!("VKCB" %in% types) && any(dis$laycbd != 0)) {
    if(is.null(vkcb)) stop('Please specify a vkcb argument', call. = FALSE)
    lpf$vkcb <- rmf_create_array(vkcb, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  if(any(lpf$laywet != 0) && any(lpf$laytyp != 0)) {
    if(is.null(wetdry)) stop('Please specify a wetdry argument', call. = FALSE)
    lpf$wetdry <- rmf_create_array(wetdry, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }
  
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
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class lpf
#' @export
#' @seealso \code{\link{rmf_write_lpf}}, \code{\link{rmf_create_lpf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lpf.htm}

rmf_read_lpf <- function(file = {cat('Please select lpf file ...\n'); file.choose()}, 
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         mlt = NULL,
                         zon = NULL,
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
  lpf$laytyp <- as.numeric(rmfi_parse_variables(lpf_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  lpf_lines <- lpf_lines[-1]
  
  # data set 3
  lpf$layavg <- as.numeric(rmfi_parse_variables(lpf_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  lpf_lines <- lpf_lines[-1]
  
  # data set 4
  lpf$chani <- as.numeric(rmfi_parse_variables(lpf_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  lpf_lines <- lpf_lines[-1]
  
  # data set 5
  lpf$layvka <- as.numeric(rmfi_parse_variables(lpf_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  lpf_lines <- lpf_lines[-1]
  
  # data set 6
  lpf$laywet <- as.numeric(rmfi_parse_variables(lpf_lines, nlay = dis$nlay)$variables[1:dis$nlay])
  lpf_lines <- lpf_lines[-1]
  
  # data set 7
  if(any(lpf$laywet != 0)) {
    data_set_7 <- rmfi_parse_variables(lpf_lines) 
    lpf$wetfct <- as.numeric(data_set_7$variables[1])
    lpf$iwetit <- as.numeric(data_set_7$variables[2])
    lpf$ihdwet <- as.numeric(data_set_7$variables[3])
    lpf_lines <- data_set_7$remaining_lines
    rm(data_set_7)
  }
  
  # data set 8-9
  types <- NULL
  if(lpf$nplpf > 0) {
    
    lpf$parameters <- list()
    lpf$parameter_values <- NULL
    
    for(i in 1:lpf$nplpf) {
      data_set_8 <- rmfi_parse_variables(lpf_lines, character = TRUE)
      parnam <- data_set_8$variables[1]
      partyp <- data_set_8$variables[2]
      parval <- as.numeric(data_set_8$variables[3])
      nclu <- as.numeric(data_set_8$variables[4])
      lpf_lines <- data_set_8$remaining_lines
      rm(data_set_8)

      ds9 <- list(layer = NULL, mltarr = NULL, zonarr = NULL, iz = list())
      for(j in 1:nclu) {
        data_set_9 <- rmfi_parse_variables(lpf_lines, character = TRUE)
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
        lpf_lines <- data_set_9$remaining_lines
        
        if(toupper(data_set_9$variables[2]) != 'NONE') {
          if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
        }
        if(toupper(data_set_9$variables[3]) != 'ALL') {
          if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
        }
        rm(data_set_9)
        
      }
      
      lpf$parameter_values[parnam] <- parval
      lpf$parameters[[parnam]] <- rmf_create_parameter(dis = dis, parnam = parnam, partyp = partyp, parval = parval, layer = ds9$layer, mltnam = ds9$mltarr, zonnam = ds9$zonarr, iz = ds9$iz, mlt = mlt, zon = zon)
      
      if(is.null(lpf[[tolower(partyp)]])) lpf[[tolower(partyp)]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay))
      lpf[[tolower(partyp)]][,,unique(ds9$layer)] <- c(lpf$parameters[[parnam]])
      
      if(!(toupper(partyp) %in% types)) types <- append(types, toupper(partyp))
    }
  }
  
  # data set 10-16
  if(!('HK' %in% types)) lpf$hk <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(lpf$chani <= 0) && !('HANI' %in% types)) lpf$hani <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(!('VK' %in% lpf$partyp || 'VANI' %in% types))lpf$vka <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(dis$sstr == 'TR')) {
    if(!('SS' %in% types)) lpf$ss <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
    if(any(lpf$laytyp != 0) && !('SY' %in% types)) lpf$sy <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  }     
  if(any(dis$laycbd != 0) && !("VKCB" %in% types)) lpf$vkcb <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(any(lpf$laywet != 0) && any(lpf$laytyp != 0)) lpf$wetdry <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  
  for(k in 1:dis$nlay) {
    
    # data set 10
    if('HK' %in% types) {
      lpf_lines <- lpf_lines[-1]  
    } else {
      data_set_10 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
      lpf_lines <- data_set_10$remaining_lines
      lpf$hk[,,k] <- data_set_10$array
      rm(data_set_10)
    }
    
    # data set 11
    if(lpf$chani[k] <= 0) {
      if('HANI' %in% types) {
        lpf_lines <- lpf_lines[-1]  
      } else {
        data_set_11 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
        lpf_lines <- data_set_11$remaining_lines
        lpf$hani[,,k] <- data_set_11$array
        rm(data_set_11)
      }
    }
    
    # data set 12
    if('VK' %in% types || 'VANI' %in% types) {
      lpf_lines <- lpf_lines[-1]  
    } else {
      data_set_12 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
      lpf_lines <- data_set_12$remaining_lines
      lpf$vka[,,k] <- data_set_12$array
      rm(data_set_12)
    }
    
    # data set 13
    if('TR' %in% dis$sstr) {
      if('SS' %in% types) {
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
      if('SY' %in% types) {
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
      if('VKCB' %in% types) {
        lpf_lines <- lpf_lines[-1]  
      } else {
        data_set_15 <- rmfi_parse_array(lpf_lines,dis$nrow,dis$ncol,1, file = file, ...)
        lpf_lines <- data_set_15$remaining_lines
        lpf$vkcb[,,k] <- data_set_15$array
        rm(data_set_15)
      }
    }
    
    # data set 16
    if(lpf$laywet[k] != 0 && lpf$laytyp[k] != 0) {
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
                          iprn = -1,
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
  if(any(lpf$laywet != 0)) {
    rmfi_write_variables(lpf$wetfct, lpf$iwetit, lpf$ihdwet, file = file)
  }
  
  # data set 8-9
  types <- NULL
  if(lpf$nplpf > 0) {
    for(i in 1:lpf$nplpf) {
      types <- append(types, attr(lpf$parameters[[i]], 'partyp'))
      rmfi_write_variables(attr(lpf$parameters[[i]], 'parnam'), attr(lpf$parameters[[i]], 'partyp'),attr(lpf$parameters[[i]], 'parval'),length(attr(lpf$parameters[[i]], 'mlt')), file = file)
      for(j in 1:length(attr(lpf$parameters[[i]], 'mlt'))) {
        rmfi_write_variables(attr(lpf$parameters[[i]], 'layer')[j],attr(lpf$parameters[[i]], 'mlt')[j], attr(lpf$parameters[[i]], 'zon')[j], rmfi_ifelse0(attr(lpf$parameters[[i]], 'zon')[j] == "ALL", NULL, attr(lpf$parameters[[i]], 'iz')[[j]]), file=file)
      } 
    }
  }
  
  # data set 10-16
  for(k in 1:dis$nlay) {
    
    # data set 10
    if('HK' %in% types) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(lpf$hk[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 11
    if(lpf$chani[k] <= 0) {
      if('HANI' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$hani[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 12
    if('VK' %in% types|| 'VANI' %in% types) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(lpf$vka[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 13
    if('TR' %in% dis$sstr) {
      if('SS' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$ss[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 14
    if('TR' %in% dis$sstr && lpf$laytyp[k] != 0) {
      if('SY' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$sy[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 15
    if(dis$laycbd[k] != 0) {
      if('VKCB' %in% types) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(lpf$vkcb[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 16
    if(lpf$laywet[k] != 0 && lpf$laytyp[k] != 0) {
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
