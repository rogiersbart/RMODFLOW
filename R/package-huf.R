#' Create an \code{RMODFLOW} huf object
#' 
#' \code{rmf_create_huf} creates an \code{RMODFLOW} huf object.
#' 
#' @param dis RMODFLOW dis object
#' @param ihufcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param nhuf number of hydrogeological units (hgu's)
#' @param iohufheads flag and unit number indicating whether interpolated heads should be written. Defaults to 0 (interpolated heads are not written). See details.
#' @param iohufflows flag and unit number indicating whether interpolated cell-by-cell flows should be written. Defaults to 0 (interpolated flows are not written). See details.
#' @param lthuf vector length \code{dis$nlay}, specifying layer type; defaults to all confined (0) except the first layer (1)
#' @param laywt vector length \code{dis$nlay},, indicating if wetting is active; defaults to 0 for each layer
#' @param wetfct is a factor that is included in the calculation of the head that is initially established at a cell when it is converted from dry to wet; defaults to 0.1
#' @param iwetit is the iteration interval for attempting to wet cells; defaults to 1
#' @param ihdwet is a flag that determines which equation is used to define the initial head at cells that become wet; defaults to 0
#' @param wetdry 3d array with a wetting threshold and flag indicating which neighboring cells can cause a cell to become wet; defaults to NULL. If not read for a specific layer, set all values in that layer to NA.
#' @param hgunam character vector of length nhuf specifying the names of the hgu's
#' @param top 3d array with the top elevations of each hgu (hgu is the third dimension)
#' @param thck 3d array with the thicknesses of each hgu (hgu is the third dimension)
#' @param hguhani vector of flags indicating whether HANI parameters (0) or hguhani (> 0) is used to specify horizontal anisotropies for each hgu; defaults to 1 for each hgu
#' @param hguvani vector of flags indicating whether vk parameters will be specified (0) or HGUVANI is used for vertical anisotropy (> 0). VANI parameters may overwrite the the latter value; defaults to 0 for each hgu
#' @param parameters list of \code{rmf_parameter} as created by \code{\link{rmf_create_parameter}}. See details; defaults to NULL
#' @param hk (named) vector with hydraulic conductivity along rows; one value per hgu specified in the order of \code{hgunam}; defaults to 0.0001. See details.
#' @param hani (named) vector with the ratio of hydraulic conductivity along columns to that along rows; one value per hgu specified in the order of \code{hgunam}; defaults to 1. See details.
#' @param vka (named) vector with vertical hydraulic conductivity or the ratio of horizontal to vertical conductivity depending on the value of hguvani; one value per hgu specified in the order of \code{hgunam}; defaults to NULL. See details.
#' @param ss (named) vector with specific storage; only required when there are transient stress periods; one value per hgu specified in the order of \code{hgunam}; defaults to 1e-5. See details.
#' @param sy (named) vector with specific yield; only required when there are transient stress periods; one value per hgu specified in the order of \code{hgunam}; defaults to 0.15. See details.
#' @param sytp numeric value defining sytp; only required when there are transient stress periods and all layers are confined; defaults to NULL. See details.
#' @details If iohufheads (iohufflows) > 0, heads (cell-by-cell flows)  will be interpolated to each hgu and saved, resulting in a single value per hgu at every row-column combination.
#'          Flow variables are any of \code{HK, HANI, VK, VANI, SS, SY and SYTP}. Variables can be specified either through the use of parameters or direct vector input through hk, hani, vk, ss, sy and sytp arguments.
#'          Specifying directly with parameters is useful if the parametrization is complex. If only a single value per hgu per partyp is needed, the direct vector input can be used.
#'          If any parameters are specified, all flow variables have to be specified by parameters. If hk, hani, vk, ss, sy or sytp are not named, the hgu number will be added as a suffix to the type (e.g. 'HK_1'). 
#'          Flow variable SYTP can not be specified per hgu, only for all hgu's. Using direct vector input, VANI van not be specified, only VK. VANI can be set through hguvani.
#'          
#' @return Object of class huf
#' @export
#' @seealso \code{\link{rmf_create_parameter}}, \code{\link{rmf_read_huf}}, \code{\link{rmf_write_huf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?huf.htm}
rmf_create_huf <- function(dis,
                           ihufcb = 0,
                           hdry = -888,
                           nhuf,
                           iohufheads = 0,
                           iohufflows = 0,
                           lthuf = rmfi_ifelse0(dis$nlay == 1, 1, c(1,rep(0, dis$nlay - 1))),
                           laywt = rep(0, dis$nlay),
                           wetfct = 0.1,
                           iwetit = 1,
                           ihdwet = 0,
                           wetdry = NULL,
                           hgunam,
                           top,
                           thck,
                           hguhani = rep(1.0, nhuf),
                           hguvani = rep(0, nhuf),
                           parameters = NULL,
                           hk = rep(0.0001, nhuf),
                           hani = rep(1, nhuf),
                           vk = hk,
                           ss = rep(1e-5, nhuf),
                           sy = rep(0.15, nhuf),
                           sytp = NULL) {
  
  huf <- list()
  
  # data set 0
  # to provide comments, use ?comment on the resulting huf object
  
  # data set 1
  huf$ihufcb <- ihufcb
  huf$hdry <- hdry
  huf$nhuf <- nhuf
  huf$nphuf <- 0
  huf$iohufheads <- iohufheads
  huf$iohufflows <- iohufflows
  
  # data set 2
  huf$lthuf <- lthuf
  
  # data set 3
  huf$laywt <- laywt

  # data set 4
  if(any(huf$laywet != 0)) {
    huf$wetfct <- wetfct
    huf$iwetit <- iwetit
    huf$ihdwet <- ihdwet
    
    # data set 5
    huf$wetdry <- wetdry
  }
  
  # data set 6
  huf$hgunam <- hgunam
  
  # data set 7
  huf$top <- top
  
  # data set 8
  huf$thck <- thck
  
  # data set 9
  huf$hguhani <- hguhani
  huf$hguvani <- hguvani
  
  # data set 10-11
  if(!is.null(parameters)) {
    huf$nphuf <- length(parameters)
    # error check
    if(any(vapply(parameters, function(i) is.null(attr(i, 'partyp')) || is.null(attr(i, 'hgunam')) || is.null(attr(i, 'parnam')) || is.null(attr(i, 'parval')), TRUE))) {
      stop('Please make sure all parameters have a parnam, parval, partyp and hgunam attribute')
    }
    
    huf$parameters <- list()
    huf$parameter_values <- NULL
    for(i in 1:huf$nphuf) {
      attrb <- attributes(parameters[[i]])
      parnam <- attrb$parnam
      huf$parameter_values[parnam] <- attrb$parval
      huf$parameters[[parnam]] <- parameters[[i]]
    }
  } else {
    
    # simple parameters
    
    create_huf_parameter <- function(value, index, partyp, hgunam){
      if(is.null(names(value))) names(value) <- paste(partyp, index, sep = '_')
      rmf_create_parameter(dis = dis, parnam = names(value), parval = value, partyp = partyp, hgunam = hgunam)
    }
    
    # hk
    hk_p <- lapply(1:huf$nhuf, function(i) create_huf_parameter(value = hk[i], index = i, partyp = 'HK', hgunam = huf$hgunam[i]))
    huf$parameters <- hk_p
    
    # hani
    if(any(huf$hguhani == 0)) {
      hani_p <- lapply(1:huf$nhuf, function(i) rmfi_ifelse0(huf$hguhani[i] == 0, create_huf_parameter(value = hani[i], index = i, partyp = 'HANI', hgunam = huf$hgunam[i]), NULL))
      huf$parameters <- c(huf$parameters, hani_p[huf$hguhani == 0])
    }
    
    # vk
    if(any(huf$hguvani == 0)) {
      vk_p <- lapply(1:huf$nhuf, function(i) rmfi_ifelse0(huf$hguvani[i] == 0, create_huf_parameter(value = vk[i], index = i, partyp = 'VK', hgunam = huf$hgunam[i]), NULL))
      huf$parameters <- c(huf$parameters, vk_p[huf$hguvani == 0])
    }
      
    # ss
    if('TR' %in% dis$sstr) {
      ss_p <- lapply(1:huf$nhuf, function(i) create_huf_parameter(value = ss[i], index = i, partyp = 'SS', hgunam = huf$hgunam[i]))
      huf$parameters <- c(huf$parameters, ss_p)
    }
      
    # sy
    if('TR' %in% dis$sstr && any(huf$lthuf != 0)) {
      sy_p <- lapply(1:huf$nhuf, function(i) rmfi_ifelse0(huf$lthuf[i] != 0, create_huf_parameter(value = ss[i], index = i, partyp = 'SY', hgunam = huf$hgunam[i]), NULL))
      huf$parameters <- c(huf$parameters, ss_p[huf$lthuf != 0])
    }
      
    # sytp
    if('TR' %in% dis$sstr && all(huf$lthuf == 0) && !is.null(sytp)) {
      if(is.null(names(sytp))) names(sytp) <- 'SYTP'
      sytp_p <- create_huf_parameter(value = sytp, index = 0, partyp = 'SYTP', hgunam = 'SYTP')
      huf$parameters <- c(huf$parameters, sytp_p)
    }
        
    huf$nphuf <- length(huf$parameters)
    huf$parameter_values <- vapply(huf$parameters, function(i) attr(i, 'parval'), 1.0)
    names(huf$parameter_values) <- vapply(huf$parameters, function(i) attr(i, 'parnam'), "name")
  } 
  
  # data set 12
  # not supported yet
  
  class(huf) <- c('huf','rmf_package')
  return(huf)
}


#' Read a MODFLOW hydrogeologic unit flow file
#' 
#' \code{read_huf} reads in a MODFLOW hydrogeologic unit flow file and returns it as an \code{\link{RMODFLOW}} huf object.
#' 
#' @param file filename; typically '*.huf'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class huf
#' @export
rmf_read_huf <- function(file = {cat('Please select huf file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         mlt = NULL,
                         zon = NULL,
                         ...) {
  
  huf_lines <- readr::read_lines(file)
  huf <- list()

  # data set 0
    data_set_0 <- rmfi_parse_comments(huf_lines)
    comment(huf) <- data_set_0$comments
    huf_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    data_set_1 <- rmfi_parse_variables(huf_lines)
    huf$ihufcb <- as.numeric(data_set_1$variables[1])
    huf$hdry <- as.numeric(data_set_1$variables[2])
    huf$nhuf <- as.numeric(data_set_1$variables[3])
    huf$nphuf <- as.numeric(data_set_1$variables[4])
    huf$iohufheads <- ifelse(is.na(data_set_1$variables[5]) || any(is.na(suppressWarnings(as.numeric(data_set_1$variables[5])))) , 0, as.numeric(data_set_1$variables[5]))
    huf$iohufflows <- ifelse(is.na(data_set_1$variables[6]) || any(is.na(suppressWarnings(as.numeric(data_set_1$variables[5:6])))), 0, as.numeric(data_set_1$variables[6]))
    huf_lines <- data_set_1$remaining_lines
    rm(data_set_1)
  
  # data set 2
    data_set_2 <- rmfi_parse_variables(huf_lines, nlay = dis$nlay)
    huf$lthuf <- as.numeric(data_set_2$variables[1:dis$nlay])
    huf_lines <- data_set_2$remaining_lines
    rm(data_set_2)
  
  # data set 3   
    data_set_3 <- rmfi_parse_variables(huf_lines, nlay = dis$nlay)
    huf$laywt <- as.numeric(data_set_3$variables[1:dis$nlay])
    huf_lines <- data_set_3$remaining_lines
    rm(data_set_3)
  
  # data set 4
    if(any(huf$laywt > 0)) {
      data_set_4 <- rmfi_parse_variables(huf_lines)
      huf$wetfct <- as.numeric(data_set_4$variables[1])
      huf$iwetit <- as.numeric(data_set_4$variables[2])
      huf$ihdwet <- as.numeric(data_set_4$variables[3])
      huf_lines <- data_set_4$remaining_lines
      rm(data_set_4)
      
      # data set 5
      data_set_5 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol,sum(which(huf$laywt!=0)), file = file, ...)
      huf$wetdry <- rmf_create_array(dim = c(dis$nrow, dis$ncol, dis$nlay))
      huf$wetdry[,,which(huf$laywt != 0)] <- data_set_5$array
      huf_lines <- data_set_5$remaining_lines
      rm(data_set_5)
    }
  
  # data set 6-8
    huf$hgunam <- vector(mode='character',length=huf$nhuf)
    huf$top <- rmf_create_array(dim=c(dis$nrow, dis$ncol, huf$nhuf))
    huf$thck <- rmf_create_array(dim=c(dis$nrow, dis$ncol, huf$nhuf))
    for(i in 1:huf$nhuf) {
      data_set_6 <- rmfi_parse_variables(huf_lines)
      huf$hgunam[i] <- as.character(data_set_6$variables[1])
      huf_lines <- data_set_6$remaining_lines
      rm(data_set_6)
      
      data_set_7 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol, 1, file = file, ...)
      huf$top[,,i] <- data_set_7$array
      huf_lines <- data_set_7$remaining_lines
      rm(data_set_7)
      
      data_set_8 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol, 1, file = file, ...)
      huf$thck[,,i] <- data_set_8$array
      huf_lines <- data_set_8$remaining_lines
      rm(data_set_8)
    }

  # data set 9
    huf$hguhani <- vector(mode='numeric',length=huf$nhuf)   
    huf$hguvani <- vector(mode='numeric',length=huf$nhuf)
    data_set_9 <- rmfi_parse_variables(huf_lines)
    if(data_set_9$variables[1] == 'ALL') {
      huf$hguhani <- rep(as.numeric(data_set_9$variables[2]),huf$nhuf)
      huf$hguvani <- rep(as.numeric(data_set_9$variables[3]),huf$nhuf)
      huf_lines <- data_set_9$remaining_lines
    } else {
      k <- which(huf$hgunam == data_set_9$variables[1])
      huf$hguhani[k] <- as.numeric(data_set_9$variables[2])
      huf$hguvani[k] <- as.numeric(data_set_9$variables[3])
      huf_lines <- data_set_9$remaining_lines
      for(i in 2:huf$nhuf) {
        data_set_9 <- rmfi_parse_variables(huf_lines)
        k <- which(huf$hgunam == data_set_9$variables[1])
        huf$hguhani[k] <- as.numeric(data_set_9$variables[2])
        huf$hguvani[k] <- as.numeric(data_set_9$variables[3])
        huf_lines <- data_set_9$remaining_lines
      }      
    }
    rm(data_set_9)
  
  # data set 10-11
    types <- NULL
    if(huf$nphuf > 0) {
      
      huf$parameters <- list()
      huf$parameter_values <- NULL
      
      for(i in 1:huf$nphuf) {
        data_set_10 <- rmfi_parse_variables(huf_lines)
        parnam <- data_set_10$variables[1]
        partyp <- data_set_10$variables[2]
        parval <- as.numeric(data_set_10$variables[3])
        nclu <- as.numeric(data_set_10$variables[4])
        huf_lines <- data_set_10$remaining_lines
        rm(data_set_10)
        
        ds11 <- list(layer = NULL, mltarr = NULL, zonarr = NULL, iz = NULL)
        for(j in 1:nclu) {
          data_set_11 <- rmfi_parse_variables(huf_lines)
          ds11$hgunam[j] <- data_set_11$variables[1]
          ds11$mltarr[j] <- data_set_11$variables[2]
          ds11$zonarr[j] <- data_set_11$variables[3]
          # zero or character entry terminates IZ
          if(ds11$zonarr[j] == 'ALL') {
            ds11$iz[[j]] <- NULL
          } else {
            iz <- suppressWarnings(as.numeric(data_set_11$variables[4:length(data_set_11$variables)]))
            ds11$iz[[j]] <- iz[1:min(length(iz), which(is.na(iz))[1] - 1, which(iz == 0)[1] - 1, na.rm = TRUE)]
          }
          huf_lines <- data_set_11$remaining_lines
          
          if(toupper(data_set_11$variables[2]) != 'NONE') {
            if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
          }
          if(toupper(data_set_11$variables[3]) != 'ALL') {
            if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
          }
          rm(data_set_11)
          
        }
        
        huf$parameter_values[parnam] <- parval
        huf$parameters[[parnam]] <- rmf_create_parameter(dis = dis, parnam = parnam, partyp = partyp, parval = parval, hgunam = ds11$hgunam, mltnam = ds11$mltarr, zonnam = ds11$zonarr, iz = ds11$iz, mlt = mlt, zon = zon)
        
        # if(is.null(huf[[tolower(partyp)]])) huf[[tolower(partyp)]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay))
        # huf[[tolower(partyp)]][,,unique(ds11$layer)] <- c(huf$parameters[[parnam]])
        # 
        # if(!(toupper(partyp) %in% types)) types <- append(types, toupper(partyp))
      }
    }
    
  # data set 12
    # These are print options, not implemented yet...
  
  class(huf) <- c('huf','rmf_package')
  return(huf)
}

#' @describeIn rmf_read_huf Deprecated function name
#' @export
read_huf <- function(...) {
  .Deprecated(new = "rmf_read_huf", old = "read_huf")
  rmf_read_huf(...)
}

#' Write a MODFLOW hydrogeologic unit flow file
#' 
#' @param huf an \code{\link{RMODFLOW}} huf object
#' @param file filename to write to; typically '*.huf'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
rmf_write_huf <- function(huf,
                          file = {cat('Please select huf file to overwrite or provide new filename ...\n'); file.choose()},
                          iprn=-1,
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Hydrogeologic Unit Flow Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(huf)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(huf$ihufcb,huf$hdry,huf$nhuf,huf$nphuf,huf$iohufheads,huf$iohufflows, file = file)
  
  # data set 2
  rmfi_write_variables(huf$lthuf, file=file)
  
  # data set 3
  rmfi_write_variables(huf$laywt, file=file)
  
  # data set 4
  if(any(huf$laywt > 0)) {
    rmfi_write_variables(huf$wetfct, huf$iwetit, huf$ihdwet, file = file)
    
    # data set 5
    if(dim(huf$wetdry)[3]>0) {
      rmfi_write_array(huf$wetdry, file = file, iprn = iprn, ...) 
    }
  }
  
  # data set 6-8
  for(i in 1:huf$nhuf) {
    rmfi_write_variables(huf$hgunam[i], file=file)   
    rmfi_write_array(huf$top[,,i], file = file, iprn = iprn, ...)
    rmfi_write_array(huf$thck[,,i], file = file, iprn = iprn, ...)
  }
  
  # data set 9
  for(i in 1:huf$nhuf) {
    rmfi_write_variables(huf$hgunam[i],huf$hguhani[i],huf$hguvani[i], file=file)
  }
  
  # data set 10-11
  for(i in 1:huf$nphuf) {
    attrb <- attributes(huf$parameters[[i]])
    rmfi_write_variables(attrb$parnam, attrb$parval,attrb$parval, length(attrb$mlt), file=file)
    for(j in 1:length(attrb$mlt)) {
      rmfi_write_variables(attrb$hgunam[j], attrb$mlt[j], attrb$zon[j], rmfi_ifelse0(attrb$zon[j] != 'ALL', attrb$iz[[j]], ''), file=file)      
    }
  }
  
  # data set 12
  # Print options, not implemented
}

#' @describeIn rmf_write_huf Deprecated function name
#' @export
write_huf <- function(...) {
  .Deprecated(new = "rmf_write_huf", old = "write_huf")
  rmf_write_huf(...)
}

#' Read a MODFLOW hydraulic conductivity depth-dependence capability file
#' 
#' \code{read_kdep} reads in a MODFLOW Hydraulic-Conductivity Depth-Dependence Capability file and returns it as an \code{\link{RMODFLOW}} kdep object.
#' 
#' @param file Filename; typically *.kdep
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param huf hydrogeologic unit file object; defaults to that with the same filename but with extension '.huf'
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class kdep
#' @export
rmf_read_kdep <- function(file = {cat('Please select kdep file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          huf = {cat('Please select corresponding huf file ...\n'); rmf_read_huf(file.choose(), dis = dis)},
                          ...) {
  
  kdep_lines <- readr::read_lines(file)
  kdep <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(kdep_lines)
  comments <- data_set_0$comments
  kdep_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(kdep_lines)
  kdep$npkdep <- as.numeric(data_set_1$variables[1])
  kdep$ifkdep <- as.numeric(data_set_1$variables[2])
  kdep_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  if(kdep$ifkdep > 0) {
    data_set2 <- rmfi_parse_array(kdep_lines,dis$nrow,dis$ncol,1, file = file, ...)
    kdep_lines <- data_set2$remaining_lines
    kdep$rs <- data_set2$array
    rm(data_set2)
  }
  
  # data set 3-4
  kdep$parnam <- vector(mode='character',length=kdep$npkdep)
  kdep$partyp <- vector(mode='character',length=kdep$npkdep)
  kdep$parval <- vector(mode='numeric',length=kdep$npkdep)
  kdep$nclu <- vector(mode='numeric',length=kdep$npkdep)
  kdep$mltarr <- matrix(nrow=huf$nhuf, ncol=kdep$npkdep)
  kdep$zonarr <- matrix(nrow=huf$nhuf, ncol=kdep$npkdep)
  kdep$iz <- matrix(nrow=huf$nhuf, ncol=kdep$npkdep)
  for(i in 1:kdep$npkdep) {
    dat <- rmfi_parse_variables(kdep_lines)
    line.split <- dat$variables
    kdep_lines <- dat$remaining_lines
    kdep$parnam[i] <- line.split[1]
    kdep$partyp[i] <- line.split[2]
    kdep$parval[i] <- as.numeric(line.split[3])
    kdep$nclu[i] <- as.numeric(line.split[4])
    for(j in 1:kdep$nclu[i]) {
      dat <- rmfi_parse_variables(kdep_lines)
      line.split <- dat$variables
      kdep_lines <- dat$remaining_lines
      k <- which(huf$hgunam == line.split[1])
      kdep$mltarr[k,i] <- line.split[2]
      kdep$zonarr[k,i] <- line.split[3]
      kdep$iz[k,i] <- paste(line.split[-c(1:3)],collapse=' ')
    } 
  }
  
  comment(kdep) <- comments
  class(kdep) <- c('kdep','rmf_package')
  return(kdep)
}

#' @describeIn rmf_read_kdep Deprecated function name
#' @export
read_kdep <- function(...) {
  .Deprecated(new = "rmf_read_kdep", old = "read_kdep")
  rmf_read_kdep(...)
}
