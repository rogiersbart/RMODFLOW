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
#'          Flow variable SYTP can not be specified per hgu, only for all hgu's. Using direct vector input, VANI can not be specified, only VK. VANI can be set through hguvani.
#'          
#'          \code{\link{rmf_convert_huf_to_grid}} can be used to convert parameters defined on the HUF grid to the numerical grid
#'          
#' @return Object of class huf
#' @export
#' @seealso \code{\link{rmf_create_parameter}}, \code{\link{rmf_convert_huf_to_grid}}, \code{\link{rmf_read_huf}}, \code{\link{rmf_write_huf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?huf.htm}
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
  huf$lthuf <- rmfi_ifelse0(length(lthuf) == 1, rep(lthuf, dis$nlay), lthuf)
  
  # data set 3
  huf$laywt <- rmfi_ifelse0(length(laywt) == 1, rep(laywt, dis$nlay), laywt)

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
  huf$hguhani <- rmfi_ifelse0(length(hguhani) == 1, rep(hguhani, huf$nhuf), hguhani)
  huf$hguvani <- rmfi_ifelse0(length(hguvani) == 1, rep(hguvani, huf$nhuf), hguvani)
  
  # data set 10-11
  if(!is.null(parameters)) {
    huf$nphuf <- length(parameters)
    # error check
    if(any(vapply(parameters, function(i) is.null(attr(i, 'partyp')) || is.null(attr(i, 'hgunam')) || is.null(attr(i, 'parnam')) || is.null(attr(i, 'parval')), TRUE))) {
      stop('Please make sure all parameters have a parnam, parval, partyp and hgunam attribute', call. = FALSE)
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
    if(length(hk) == 1) hk <- rep(hk, huf$nhuf)
    hk_p <- lapply(1:huf$nhuf, function(i) create_huf_parameter(value = hk[i], index = i, partyp = 'HK', hgunam = huf$hgunam[i]))
    huf$parameters <- hk_p
    
    # hani
    if(any(huf$hguhani == 0)) {
      if(length(hani) == 1) hani <- rep(hani, huf$nhuf)
      hani_p <- lapply(1:huf$nhuf, function(i) rmfi_ifelse0(huf$hguhani[i] == 0, create_huf_parameter(value = hani[i], index = i, partyp = 'HANI', hgunam = huf$hgunam[i]), NULL))
      huf$parameters <- c(huf$parameters, hani_p[huf$hguhani == 0])
    }
    
    # vk
    if(any(huf$hguvani == 0)) {
      if(length(vk) == 1) vk <- rep(vk, huf$nhuf)
      vk_p <- lapply(1:huf$nhuf, function(i) rmfi_ifelse0(huf$hguvani[i] == 0, create_huf_parameter(value = vk[i], index = i, partyp = 'VK', hgunam = huf$hgunam[i]), NULL))
      huf$parameters <- c(huf$parameters, vk_p[huf$hguvani == 0])
    }
      
    # ss
    if('TR' %in% dis$sstr) {
      if(length(ss) == 1) ss <- rep(ss, huf$nhuf)
      ss_p <- lapply(1:huf$nhuf, function(i) create_huf_parameter(value = ss[i], index = i, partyp = 'SS', hgunam = huf$hgunam[i]))
      huf$parameters <- c(huf$parameters, ss_p)
    }
      
    # sy
    if('TR' %in% dis$sstr && any(huf$lthuf != 0)) {
      if(length(sy) == 1) sy <- rep(sy, huf$nhuf)
      sy_p <- lapply(1:huf$nhuf, function(i) rmfi_ifelse0(huf$lthuf[i] != 0, create_huf_parameter(value = sy[i], index = i, partyp = 'SY', hgunam = huf$hgunam[i]), NULL))
      huf$parameters <- c(huf$parameters, sy_p[huf$lthuf != 0])
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
#'
#' @details \code{\link{rmf_convert_huf_to_grid}} can be used to convert parameters defined on the HUF grid to the numerical grid
#' @return object of class huf
#' @export
#' @seealso \code{\link{rmf_convert_huf_to_grid}}, \code{\link{rmf_create_huf}}, \code{\link{rmf_write_huf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?huf.htm}
rmf_read_huf <- function(file = {cat('Please select huf file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         mlt = NULL,
                         zon = NULL,
                         ...) {
  
  huf_lines <- readr::read_lines(file, lazy = FALSE)
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
      data_set_5 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol,sum(which(huf$laywt!=0)), ndim = 3, file = file, ...)
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
      data_set_6 <- rmfi_parse_variables(huf_lines, character = TRUE)
      huf$hgunam[i] <- as.character(data_set_6$variables[1])
      huf_lines <- data_set_6$remaining_lines
      rm(data_set_6)
      
      data_set_7 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol, 1, ndim = 2, file = file, ...)
      huf$top[,,i] <- data_set_7$array
      huf_lines <- data_set_7$remaining_lines
      rm(data_set_7)
      
      data_set_8 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol, 1, ndim = 2, file = file, ...)
      huf$thck[,,i] <- data_set_8$array
      huf_lines <- data_set_8$remaining_lines
      rm(data_set_8)
    }

  # data set 9
    huf$hguhani <- vector(mode='numeric',length=huf$nhuf)   
    huf$hguvani <- vector(mode='numeric',length=huf$nhuf)
    data_set_9 <- rmfi_parse_variables(huf_lines, character = TRUE)
    if(toupper(data_set_9$variables[1]) == 'ALL') {
      huf$hguhani <- rep(as.numeric(data_set_9$variables[2]),huf$nhuf)
      huf$hguvani <- rep(as.numeric(data_set_9$variables[3]),huf$nhuf)
      huf_lines <- data_set_9$remaining_lines
    } else {
      k <- which(huf$hgunam == data_set_9$variables[1])
      huf$hguhani[k] <- as.numeric(data_set_9$variables[2])
      huf$hguvani[k] <- as.numeric(data_set_9$variables[3])
      huf_lines <- data_set_9$remaining_lines
      for(i in 2:huf$nhuf) {
        data_set_9 <- rmfi_parse_variables(huf_lines, character = TRUE)
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
        data_set_10 <- rmfi_parse_variables(huf_lines, character = TRUE)
        parnam <- data_set_10$variables[1]
        partyp <- data_set_10$variables[2]
        parval <- as.numeric(data_set_10$variables[3])
        nclu <- as.numeric(data_set_10$variables[4])
        huf_lines <- data_set_10$remaining_lines
        rm(data_set_10)
        
        ds11 <- list(layer = NULL, mltarr = NULL, zonarr = NULL, iz = list())
        for(j in 1:nclu) {
          data_set_11 <- rmfi_parse_variables(huf_lines, character = TRUE)
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

#' Write a MODFLOW hydrogeologic unit flow file
#' 
#' @param huf an \code{\link{RMODFLOW}} huf object
#' @param file filename to write to; typically '*.huf'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_convert_huf_to_grid}}, \code{\link{rmf_create_huf}}, \code{\link{rmf_read_huf}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?huf.htm}
rmf_write_huf <- function(huf,
                          file = {cat('Please select huf file to overwrite or provide new filename ...\n'); file.choose()},
                          iprn=-1,
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Hydrogeologic Unit Flow Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(huf)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(as.integer(huf$ihufcb),huf$hdry,as.integer(huf$nhuf),as.integer(huf$nphuf),as.integer(huf$iohufheads),as.integer(huf$iohufflows), file = file)
  
  # data set 2
  rmfi_write_variables(huf$lthuf, file=file, integer = TRUE)
  
  # data set 3
  rmfi_write_variables(huf$laywt, file=file, integer = TRUE)
  
  # data set 4
  if(any(huf$laywt > 0)) {
    rmfi_write_variables(huf$wetfct, as.integer(huf$iwetit), as.integer(huf$ihdwet), file = file)
    
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
    rmfi_write_variables(attrb$parnam, attrb$partyp, attrb$parval, as.integer(length(attrb$mlt)), file=file)
    for(j in 1:length(attrb$mlt)) {
      rmfi_write_variables(attrb$hgunam[j], attrb$mlt[j], attrb$zon[j], rmfi_ifelse0(attrb$zon[j] != 'ALL', as.integer(attrb$iz[[j]]), ''), file=file)      
    }
  }
  
  # data set 12
  # Print options, not implemented
}

#' Create an \code{RMODFLOW} kdep object
#' 
#' \code{rmf_create_kdep} creates an \code{RMODFLOW} kdep object.
#' 
#' @param parameters either a single \code{rmf_parameter} or a list of \code{rmf_parameters} specifying the depth-dependency coefficients. See details.
#' @param rs optional 2d array specifying the reference elevation surface
#' @param dis \code{RMODFLOW} dis object. Only used if \code{rs} is supplied.
#'
#' @return a \code{RMODFLOW} kdep object
#' @export
#' @details All parameters should have a hgunam attribute and their partyp attribute set to 'KDEP'.
#'          The KDEP package can only be used in conjunction with the HUF package.
#'          Note that the parameters are defined on the HUF grid, not the numerical grid
#'          \code{\link{rmf_convert_huf_to_grid}} can be used to convert parameters defined on the HUF grid to the numerical grid
#' @seealso \code{link{rmf_convert_huf_to_grid}}, \code{\link{rmf_read_kdep}}, \code{\link{rmf_write_kdep}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?kdep.htm}
rmf_create_kdep <- function(parameters,
                            rs = NULL,
                            dis) {
  
  if(length(parameters) == 1 && inherits(parameters[[1]], 'list') && !(inherits(parameters[[1]], 'rmf_parameter'))) parameters <- parameters[[1]]
  
  kdep <- list()
  
  # data set 0
  # to provide comments, use ?comment on the resulting kdep object
  
  # data set 1
  kdep$npkdep <- length(parameters)
  kdep$ifkdep <- ifelse(is.null(rs), 0, 1)
  
  # data set 2
  if(!is.null(rs)) kdep$rs <- rmf_create_array(rs, dim = c(dis$nrow, dis$ncol))
  
  # data set 3 & 4
  # error check
  if(any(vapply(parameters, function(i) is.null(attr(i, 'partyp')) || is.null(attr(i, 'hgunam')) || is.null(attr(i, 'parnam')) || is.null(attr(i, 'parval')), TRUE))) {
    stop('Please make sure all parameters have a parnam, parval, partyp and hgunam attribute', call. = FALSE)
  }
  
  kdep$parameters <- list()
  kdep$parameter_values <- NULL
  for(i in 1:kdep$npkdep) {
    attrb <- attributes(parameters[[i]])
    parnam <- attrb$parnam
    kdep$parameter_values[parnam] <- attrb$parval
    kdep$parameters[[parnam]] <- parameters[[i]]
  }

  class(kdep) <- c('kdep', 'rmf_package')
  return(kdep)
  
}

#' Read a MODFLOW hydraulic conductivity depth-dependence capability file
#' 
#' \code{rmf_read_kdep} reads in a MODFLOW Hydraulic-Conductivity Depth-Dependence Capability file and returns it as an \code{RMODFLOW} kdep object.
#' 
#' @param filename filename; typically *.kdep
#' @param dis \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class kdep
#' @details Note that the parameters are defined on the HUF grid, not the numerical grid
#'          \code{\link{rmf_convert_huf_to_grid}} can be used to convert parameters defined on the huf grid to the numerical grid
#' @export
#' @seealso \code{\link{rmf_convert_huf_to_grid}}, \code{\link{rmf_create_kdep}}, \code{\link{rmf_write_kdep}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?kdep.htm}

rmf_read_kdep <- function(file = {cat('Please select kdep file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          ...) {
  
  kdep_lines <- readr::read_lines(file, lazy = FALSE)
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
    data_set2 <- rmfi_parse_array(kdep_lines,dis$nrow,dis$ncol,1, ndim = 2, file = file, ...)
    kdep_lines <- data_set2$remaining_lines
    kdep$rs <- data_set2$array
    rm(data_set2)
  }
  
  # data set 3-4
  for(i in 1:kdep$npkdep) {
    data_set_3 <- rmfi_parse_variables(kdep_lines)
    parnam <- data_set_3$variables[1]
    partyp <- data_set_3$variables[2]
    parval <- as.numeric(data_set_3$variables[3])
    nclu <- as.numeric(data_set_3$variables[4])
    kdep_lines <- data_set_3$remaining_lines
    rm(data_set_3)
    
    ds4 <- list(layer = NULL, mltarr = NULL, zonarr = NULL, iz = list())
    for(j in 1:nclu) {
      data_set_4 <- rmfi_parse_variables(kdep_lines)
      ds4$hgunam[j] <- data_set_4$variables[1]
      ds4$mltarr[j] <- data_set_4$variables[2]
      ds4$zonarr[j] <- data_set_4$variables[3]
      # zero or character entry terminates IZ
      if(toupper(ds4$zonarr[j]) == 'ALL') {
        ds4$iz[[j]] <- NULL
      } else {
        iz <- suppressWarnings(as.numeric(data_set_4$variables[4:length(data_set_4$variables)]))
        ds4$iz[[j]] <- iz[1:min(length(iz), which(is.na(iz))[1] - 1, which(iz == 0)[1] - 1, na.rm = TRUE)]
      }
      kdep_lines <- data_set_4$remaining_lines
      
      if(toupper(data_set_4$variables[2]) != 'NONE') {
        if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
      }
      if(toupper(data_set_4$variables[3]) != 'ALL') {
        if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
      }
      rm(data_set_4)
      
    }
    
    kdep$parameter_values[parnam] <- parval
    kdep$parameters[[parnam]] <- rmf_create_parameter(dis = dis, parnam = parnam, partyp = partyp, parval = parval, hgunam = ds4$hgunam, mltnam = ds4$mltarr, zonnam = ds4$zonarr, iz = ds4$iz, mlt = mlt, zon = zon)
    
    # if(is.null(kdep[[tolower(partyp)]])) kdep[[tolower(partyp)]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay))
    # kdep[[tolower(partyp)]][,,unique(ds4$layer)] <- c(kdep$parameters[[parnam]])
    # 
    # if(!(toupper(partyp) %in% types)) types <- append(types, toupper(partyp))
  }
  
  comment(kdep) <- comments
  class(kdep) <- c('kdep','rmf_package')
  return(kdep)
}

#' Write a MODFLOW hydraulic conductivity depth-dependence capability file
#' 
#' @param kdep an \code{RMODFLOW} kdep object
#' @param file filename to write to; typically '*.kdep'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#'
#' @return \code{NULL}
#' @export
#'
#' @seealso \code{\link{rmf_create_kdep}}, \code{\link{rmf_read_kdep}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?kdep.htm}
rmf_write_kdep <- function(kdep,
                           file = {cat('Please select kdep file to overwrite or provide new filename ...\n'); file.choose()},
                           iprn = -1,
                           ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Hydraulic Conductivity Depth-Dependence Capability Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(kdep)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(kdep$npkdep, kdep$ifkdep, file = file, integer = TRUE)
  
  # data set 2
  if(kdep$ifkdep > 0) rmfi_write_array(kdep$rs, file = file, iprn = iprn, ...) 

  # data set 3-4
  for(i in 1:kdep$npkdep) {
    attrb <- attributes(kdep$parameters[[i]])
    rmfi_write_variables(attrb$parnam, attrb$partyp, attrb$parval, as.integer(length(attrb$mlt)), file=file)
    for(j in 1:length(attrb$mlt)) {
      rmfi_write_variables(attrb$hgunam[j], attrb$mlt[j], attrb$zon[j], rmfi_ifelse0(attrb$zon[j] != 'ALL', as.integer(attrb$iz[[j]]), ''), file=file)      
    }
  }
  
}

#' Create an \code{RMODFLOW} lvda object
#' 
#' \code{rmf_create_lvda} creates an \code{RMODFLOW} lvda object.
#'
#' @param parameters either a single \code{rmf_parameter} or a list of \code{rmf_parameters} specifying the angle between the grid axis and the principal direction of horizontal hydraulic conductivity. See details.
#' @param dis \code{RMODFLOW} dis object
#'
#' @return a \code{RMODFLOW} lvda object
#' @export
#' @details All parameters should have a layer attribute and their partyp attribute set to 'LVDA'.
#'          The LVDA package can only be used in conjunction with the HUF package.
#'          Note that the parameters are defined on the numerical grid, not the HUF grid
#' @seealso \code{\link{rmf_read_lvda}}, \code{\link{rmf_write_lvda}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lvda.htm}

rmf_create_lvda <- function(parameters, dis) {
  
  if(length(parameters) == 1 && inherits(parameters[[1]], 'list') && !(inherits(parameters[[1]], 'rmf_parameter'))) parameters <- parameters[[1]]
  
  lvda <- list()
  
  # data set 0
  # to provide comments, use ?comment on the resulting lvda object
  
  # data set 1
  lvda$nplvda <- length(parameters)
  
  # data set 2 & 3
  # error check
  if(any(vapply(parameters, function(i) is.null(attr(i, 'partyp')) || is.null(attr(i, 'layer')) || is.null(attr(i, 'parnam')) || is.null(attr(i, 'parval')), TRUE))) {
    stop('Please make sure all parameters have a parnam, parval, partyp and layer attribute', call. = FALSE)
  }
  
  lvda$parameters <- list()
  lvda$parameter_values <- NULL
  lvda$lvda <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))
  for(i in 1:lvda$nplvda) {
    attrb <- attributes(parameters[[i]])
    parnam <- attrb$parnam
    lvda$parameter_values[parnam] <- attrb$parval
    lvda$parameters[[parnam]] <- parameters[[i]]
    
    lvda$lvda[,,unique(attrb$layer)] <- lvda$lvda[,,unique(attrb$layer)] + c(lvda$parameters[[parnam]])
  }

  class(lvda) <- c('lvda', 'rmf_package')
  return(lvda)
  
}

#' Read a MODFLOW model-layer variable-direction horizontal anisotropy capability file
#' 
#' \code{rmf_read_lvda} reads in a MODFLOW Model-Layer Variable-Direction Horizontal Anisotropy Capability file and returns it as an \code{RMODFLOW} lvda object.
#' 
#' @param filename filename; typically *.lvda
#' @param dis \code{RMODFLOW} dis object
#' @param ... ignored
#' @details Note that the parameters are defined on the numerical grid, not the HUF grid
#' @return object of class lvda
#' @export
#' @seealso \code{\link{rmf_create_lvda}}, \code{\link{rmf_write_lvda}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lvda.htm}
rmf_read_lvda <- function(file, 
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          ...) {
  
  
  lvda_lines <- readr::read_lines(file, lazy = FALSE)
  lvda <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(lvda_lines)
  comments <- data_set_0$comments
  lvda_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(lvda_lines)
  lvda$nplvda <- as.numeric(data_set_1$variables[1])
  lvda_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  lvda$lvda <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))
  # data set 2-3
  for(i in 1:lvda$nplvda) {
    data_set_2 <- rmfi_parse_variables(lvda_lines)
    parnam <- data_set_2$variables[1]
    partyp <- data_set_2$variables[2]
    parval <- as.numeric(data_set_2$variables[3])
    nclu <- as.numeric(data_set_2$variables[4])
    lvda_lines <- data_set_2$remaining_lines
    rm(data_set_2)
    
    ds3 <- list(layer = NULL, mltarr = NULL, zonarr = NULL, iz = list())
    for(j in 1:nclu) {
      data_set_3 <- rmfi_parse_variables(lvda_lines)
      ds3$layer[j] <- data_set_3$variables[1]
      ds3$mltarr[j] <- data_set_3$variables[2]
      ds3$zonarr[j] <- data_set_3$variables[3]
      # zero or character entry terminates IZ
      if(toupper(ds3$zonarr[j]) == 'ALL') {
        ds3$iz[[j]] <- NULL
      } else {
        iz <- suppressWarnings(as.numeric(data_set_3$variables[4:length(data_set_3$variables)]))
        ds3$iz[[j]] <- iz[1:min(length(iz), which(is.na(iz))[1] - 1, which(iz == 0)[1] - 1, na.rm = TRUE)]
      }
      lvda_lines <- data_set_3$remaining_lines
      
      if(toupper(data_set_3$variables[2]) != 'NONE') {
        if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
      }
      if(toupper(data_set_3$variables[3]) != 'ALL') {
        if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
      }
      rm(data_set_3)
      
    }
    
    lvda$parameter_values[parnam] <- parval
    lvda$parameters[[parnam]] <- rmf_create_parameter(dis = dis, parnam = parnam, partyp = partyp, parval = parval, layer = ds3$layer, mltnam = ds3$mltarr, zonnam = ds3$zonarr, iz = ds3$iz, mlt = mlt, zon = zon)
    
    # if(is.null(lvda[[tolower(partyp)]])) lvda[[tolower(partyp)]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay))
    # lvda[[tolower(partyp)]][,,unique(ds3$layer)] <- c(lvda$parameters[[parnam]])
    # 
    # if(!(toupper(partyp) %in% types)) types <- append(types, toupper(partyp))
    
    lvda$lvda[,,unique(ds3$layer)] <- lvda$lvda[,,unique(ds3$layer)] + c(lvda$parameters[[parnam]])
    
  }
  
  comment(lvda) <- comments
  class(lvda) <- c('lvda','rmf_package')
  return(lvda)
  
}


#' Write a MODFLOW model-layer variable-direction horizontal anisotropy capability file
#' 
#' @param lvda an \code{RMODFLOW} lvda object
#' @param file filename to write to; typically '*.lvda'
#' @param ... ignored
#'
#' @return \code{NULL}
#' @export
#'
#' @seealso \code{\link{rmf_create_lvda}}, \code{\link{rmf_read_lvda}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?lvda.htm}
rmf_write_lvda <- function(lvda,
                           file = {cat('Please select lvda file to overwrite or provide new filename ...\n'); file.choose()},
                           ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Model-Layer Variable-Direction Horizontal Anisotropy Capability Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(lvda)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(lvda$nplvda, file = file, integer = TRUE)
  
  # data set 3-4
  for(i in 1:lvda$nplvda) {
    attrb <- attributes(lvda$parameters[[i]])
    rmfi_write_variables(attrb$parnam, attrb$partyp, attrb$parval, as.integer(length(attrb$mlt)), file=file)
    for(j in 1:length(attrb$mlt)) {
      rmfi_write_variables(as.integer(attrb$layer[j]), attrb$mlt[j], attrb$zon[j], rmfi_ifelse0(attrb$zon[j] != 'ALL', as.integer(attrb$iz[[j]]), ''), file=file)      
    }
  }
  
}
