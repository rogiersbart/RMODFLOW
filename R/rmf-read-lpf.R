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
    lpf$layavg <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay])
    lpf_lines <- lpf_lines[-1]
  
  # data set 4
    lpf$chani <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay])
    lpf_lines <- lpf_lines[-1]
  
  # data set 5
    lpf$layvka <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay])
    lpf_lines <- lpf_lines[-1]
    
  # data set 6
    lpf$laywet <- as.numeric(rmfi_parse_variables(lpf_lines)$variables)[1:dis$nlay])
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
