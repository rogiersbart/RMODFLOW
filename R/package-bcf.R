#' Create an \code{RMODFLOW} bcf object
#' 
#' \code{rmf_create_bcf} creates an \code{RMODFLOW} bcf object
#'
#' @param dis \code{RMODFLOW} dis object
#' @param ibcfcb flag and unit number for writing cell-by-cell flow terms; defaults to 0
#' @param hdry head assigned to cells that are converted to dry cells; defaults to -888
#' @param iwdflg wetting capability flag; defaults to 0
#' @param wetfct factor included in the calculation of the initial head when a cell is converted from dry to wet; defaults to 1
#' @param iwetit iteration interval for attempting to wet cells; defaults to 1
#' @param ihdwet flag determining which equation is used to define initial heads at cells that are converted from dry to wet; defaults to 0
#' @param layavg numeric vector of length \code{dis$nlay} determining which method of calculating interblock transmissivity to use for each layer; defaults to 0 for 3 layers
#' @param laycon numeric vector of length \code{dis$nlay} determining which layer type (LAYCON) to use for each layer; defaults to 0 for 3 layer
#' @param trpy numeric vector of length \code{dis$nlay} determining the horizontal anisotropy of each layer; defaults to 1 for 3 layers
#' @param sf1 numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the primary storage coefficient for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL
#' @param tran numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the transmissivity along rows for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to 0.001 for each cell
#' @param hy numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the hydraulic conductivity along rows for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL
#' @param vcont numeric 3D array of dimensions \code{dis$nrow x dis$ncol x (dis$nlay-1)} specifying the vertical hydraulic conductivity divided by the thickness between the node in the cell and the node in the cell below. If not read for a specific layer, set all values in that layer to NA; defaults to 1e-5 for each cell
#' @param sf2 numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the secondary storage coefficient for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL    
#' @param wetdry numeric 3D array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the flag and wetting threshold for each cell. If not read for a specific layer, set all values in that layer to NA; defaults to NULL
#' 
#' @return \code{RMODFLOW} bcf object
#' @export
#' @seealso \code{\link{rmf_read_bcf}}, \code{\link{rmf_write_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_create_bcf <-  function(dis,
                          ibcfcb = 0,
                          hdry = -888,
                          iwdflg = 0,
                          wetfct = 1,
                          iwetit = 1,
                          ihdwet = 0,
                          layavg = rep(0, dis$nlay),
                          laycon = rep(0, dis$nlay),
                          trpy = rep(1, dis$nlay),
                          sf1 = 1e-5,
                          tran = 0.001,
                          hy = 0.0001,
                          vcont = 1e-5,
                          sf2 = 0.15,
                          wetdry = NULL
                          ){
  
  bcf <-  list()
  
  # data set 0
  # to provide comments, use ?comment on resulting bcf object
  
  # data set 1
  bcf$ibcfcb <- ibcfcb
  bcf$hdry <- hdry
  bcf$iwdflg <- iwdflg
  bcf$wetfct <- wetfct
  bcf$iwetit <- iwetit
  bcf$ihdwet <- ihdwet
  
  # data set 2
  bcf$layavg <- layavg
  bcf$laycon <- laycon
  
  # data set 3
  bcf$trpy <- trpy
  
  # data set 4
  if('TR' %in% dis$sstr) bcf$sf1 <- rmf_create_array(sf1, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 5
  if(any(c(0,2) %in% bcf$laycon)) bcf$tran <- rmf_create_array(tran, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 6
  if(any(c(1,3) %in% bcf$laycon)) bcf$hy <- rmf_create_array(hy, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 7
  if(dis$nlay > 1) bcf$vcont <- rmf_create_array(vcont, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 8
  if('TR' %in% dis$sstr && any(c(2,3) %in% bcf$laycon)) bcf$sf2 <- rmf_create_array(sf2, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  # data set 9
  if(bcf$iwdflg != 0 && any(c(1,3) %in% bcf$laycon)) bcf$wetdry <- rmf_create_array(wetdry, dim = c(dis$nrow, dis$ncol, dis$nlay))
  
  class(bcf) <- c('bcf', 'rmf_package')
  return(bcf)
}

#' Read a MODFLOW bcf file
#' 
#' \code{rmf_read_bcf} reads in a MODFLOW block-centered flow file and returns it as an \code{RMODFLOW} bcf object
#' 
#' @param file filename; typically '*_bcf'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_array} and \code{rmfi_parse_variables}. Can be ignored when input is 'free' format and arrays are INTERNAL or CONSTANT.
#' @return an \code{RMODFLOW} bcf object
#' @export
#' @seealso \code{\link{rmf_write_bcf}}, \code{\link{rmf_create_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_read_bcf <- function(file = {cat('Please select bcf file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                        ...){
  
  bcf <-  list()
  bcf_lines <-  readr::read_lines(file)
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(bcf_lines)
  comment(bcf) <-  data_set_0$comments
  bcf_lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(bcf_lines, n = 6, ...)
  bcf$ibcfcb <- rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  bcf$hdry <- rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  bcf$iwdflg <- rmfi_ifelse0(is.na(data_set_1$variables[3]), 0, as.numeric(data_set_1$variables[3]))
  bcf$wetfct <- rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4]))
  bcf$iwetit <- rmfi_ifelse0(is.na(data_set_1$variables[5]), 0, as.numeric(data_set_1$variables[5]))
  bcf$ihdwet <- rmfi_ifelse0(is.na(data_set_1$variables[6]), 0, as.numeric(data_set_1$variables[6]))
  bcf_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  counter <- 0
  format <- ifelse('format' %in% names(list(...)), get('format'), 'free')
  
  while(counter != dis$nlay) {
    if(format == 'free') {
      data_set_2 <- rmfi_parse_variables(bcf_lines, ...)
      bcf_lines <- data_set_2$remaining_lines
      data_set_2 <- data_set_2$variables
    } else if(format == 'fixed') {
      data_set_2 <- (unlist(lapply(seq(1,nchar(bcf_lines[1]), by=2), 
                                   function(i) paste0(strsplit(rmfi_remove_comments_end_of_line(bcf_lines[1]),'')[[1]][i:(i+1)], collapse=''))))
      data_set_2 <- lapply(strsplit(data_set_2, " |t"), rmfi_remove_empty_strings)
      data_set_2[which(lengths(data_set_2)==0)] = 0 # empty values are set to 0
      bcf_lines <- bcf_lines[-1]
    }
    data_set_2 <- strsplit(as.character(data_set_2), split = '')
    data_set_2 <- lapply(data_set_2, function(i) rmfi_ifelse0(length(unlist(i)) == 1, c("0",unlist(i)), i))
    
    while(counter != length(data_set_2) && counter != dis$nlay) {
      counter <- counter+1
      bcf$layavg[counter] <- as.numeric(data_set_2[[counter]][1])
      bcf$laycon[counter] <- as.numeric(data_set_2[[counter]][2])
    }
    rm(data_set_2)
  }
  rm(counter)
  
  
  # data set 3
  data_set_3 <- rmfi_parse_array(bcf_lines, 1, dis$nlay, 1, ndim = 1, file = file, ...)
  bcf$trpy <- data_set_3$array
  bcf_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # data set 4-9
  bcf$wetdry <- bcf$sf2 <- bcf$vcont <- bcf$hy <- bcf$tran <- bcf$sf1 <- rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  
  for(i in 1:dis$nlay){
    
    # data set 4
    if('TR' %in% dis$sstr){
      data_set_4 <- rmfi_parse_array(bcf_lines, nrow=dis$nrow, ncol = dis$ncol, nlay=1, file = file, ...)
      bcf$sf1[,,i] <- data_set_4$array
      bcf_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    # data set 5
    if(bcf$laycon[i] %in% c(0,2)){
      data_set_5 <- rmfi_parse_array(bcf_lines, nrow=dis$nrow, ncol=dis$ncol, nlay=1, file = file, ...)
      bcf$tran[,,i] <- data_set_5$array
      bcf_lines <- data_set_5$remaining_lines
      rm(data_set_5)
    }
    
    # data set 6
    if(bcf$laycon[i] %in% c(1,3)){
      data_set_6 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$hy[,,i] <- data_set_6$array
      bcf_lines <- data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    # data set 7
    if(i != dis$nlay){
      data_set_7 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$vcont[,,i] <- data_set_7$array
      bcf_lines <- data_set_7$remaining_lines
      rm(data_set_7)
    }
    
    # data set 8
    if(('TR' %in% dis$sstr) && bcf$laycon[i] %in% c(2,3)){
      data_set_8 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$sf2[,,i] <- data_set_8$array
      bcf_lines <- data_set_8$remaining_lines
      rm(data_set_8)
    }
    
    # data set 9
    if((bcf$iwdflg != 0) && (bcf$laycon[i] %in% c(1,3))){
      data_set_9 <- rmfi_parse_array(bcf_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, ...)
      bcf$wetdry[,,i] <- data_set_9$array
      bcf_lines <- data_set_9$remaining_lines
      rm(data_set_9)
    } 
  }
  
  if(all(is.na(bcf$sf1))) bcf$sf1 <- NULL
  if(all(is.na(bcf$tran))) bcf$tran <- NULL
  if(all(is.na(bcf$hy))) bcf$hy <- NULL
  if(all(is.na(bcf$sf2))) bcf$sf2 <- NULL
  if(all(is.na(bcf$wetdry))) bcf$wetdry <- NULL
  
  class(bcf) <- c('bcf', 'rmf_package')
  return(bcf)
  
}

#' Write a MODFLOW block-centered flow file
#' 
#' \code{rmf_write_bcf} writes a MODFLOW block-centered flow file based on an \code{RMODFLOW} bcf object
#'
#' @param bcf an \code{RMODFLOW} bcf object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.bcf'
#' @param ... arguments passed to \code{rmfi_write_array} and \code{rmfi_write_variables}. Can be ignored when format is free and arrays are INTERNAL or CONSTANT.
#'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_bcf}}, \code{\link{rmf_create_bcf}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?bcf.htm}

rmf_write_bcf <- function(bcf, 
                          dis =  {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          file = {cat('Please select bcf file to overwrite or provide new filename ...\n'); file.choose()}, 
                          ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Block-Centered Flow Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(lpf)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(bcf$ibcfcb, bcf$hdry, bcf$iwdflg, bcf$wetfct, bcf$iwetit, bcf$ihdwet, file = file, ...)
  
  # data set 2 - should write 40 characters if format is fixed
  fmt <- ifelse('format' %in% names(list(...)), get('format'), 'free')
  ltype <- paste(bcf$layavg, bcf$laycon, sep='')
  
  if(fmt == 'free') {
    rmfi_write_variables(ltype, file=file)
  } else if(fmt == "fixed") {
    counter <- 0
    while((dis$nlay-counter) > 20) {
      rmfi_write_variables(paste0(ltype[1:20], collapse=''), file=file)
      ltype <- ltype[-c(1:20)]
      counter <- counter+20
    }
    rmfi_write_variables(paste0(ltype, collapse=''), file=file)
  }
  
  # data set 3
  rmfi_write_array(bcf$trpy, file=file, ...)
  
  # data set 4-9
  for(i in 1:dis$nlay){
    if('TR' %in% dis$sstr) rmfi_write_array(bcf$sf1[,,i], file=file, ...)
    if(bcf$laycon[i] %in% c(0,2)) rmfi_write_array(bcf$tran[,,i], file=file, ...)
    if(bcf$laycon[i] %in% c(1,3)) rmfi_write_array(bcf$hy[,,i], file=file, ...)
    if(i != dis$nlay) rmfi_write_array(bcf$vcont[,,i], file=file, ...)
    if(('TR' %in% dis$sstr) && bcf$laycon[i] %in% c(2,3)) rmfi_write_array(bcf$sf2[,,i], file=file, ...)
    if((bcf$iwdflg != 0) && (bcf$laycon[i] %in% c(1,3))) rmfi_write_array(bcf$wetdry[,,i], file=file, ...)
  }
}
