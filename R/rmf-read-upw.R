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

