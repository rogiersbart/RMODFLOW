#' Create an \code{RMODFLOW} bas object
#' 
#' \code{rmf_create_bas} creates an \code{RMODFLOW} bas object.
#' 
#' @param dis RMODFLOW dis object
#' @param xsection logical; is the model a 1-row cross-section?
#' @param chtoch logical; should flow between adjacent constant-head cells be calculated?
#' @param free logical; is free format used?
#' @param printtime logical; should start, end and elapsed times be written to the global output file?
#' @param showprogress logical; should progress information be displayed?
#' @param stoperror logical; should the model be stopped based on budget percent discrepancy?
#' @param stoper numeric; threshold budget percent discrepancy
#' @param ibound 3d array specifying active (1), inactive (0) or constant head (0) status of all cells
#' @param strt 3d array specifying starting heads
#' @return Object of class bas
#' @export
#' @seealso \code{\link{rmf_read_bas}}, \code{\link{rmf_write_bas}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?bas.htm}
rmf_create_bas <- function(dis,
                           xsection = FALSE,
                           chtoch = FALSE,
                           free = TRUE,
                           printtime = FALSE,
                           showprogress = FALSE,
                           stoperror = FALSE,
                           stoper = 1,
                           ibound = rmf_create_array(1L, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           hnoflo = -999,
                           strt = rmf_create_array(0, dim = c(dis$nrow, dis$ncol, dis$nlay))) {
      
  bas <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting bas object
  
  # data set 1
    bas$xsection <- xsection
    if(bas$xsection) warning('XSECTION: assuming ibound and strt arrays are of dimensions NLAY x NCOL', call. = FALSE)
    bas$chtoch <- chtoch
    bas$free <- free
    bas$printtime <- printtime
    bas$showprogress <- showprogress
    bas$stoperror <- stoperror
    bas$stoper <- stoper
    
  # data set 2
    bas$ibound <- rmf_create_array(apply(ibound, MARGIN = 1:length(dim(ibound)), function(i) as.integer(i)),
                                   dim = rmfi_ifelse0(bas$xsection, c(dis$nlay, dis$ncol), c(dis$nrow, dis$ncol, dis$nlay)),
                                   dimlabels = rmfi_ifelse0(bas$xsection, c("k", "j"), c("i", "j", "k")))
  
  # data set 3
    bas$hnoflo <- hnoflo
  
  # data set 4
    bas$strt <- rmf_create_array(strt, dim = rmfi_ifelse0(bas$xsection, c(dis$nlay, dis$ncol), c(dis$nrow, dis$ncol, dis$nlay)),
                                 dimlabels = rmfi_ifelse0(bas$xsection, c("k", "j"), c("i", "j", "k")))
  
  class(bas) <- c('bas','rmf_package')
  return(bas)
}

#' Read a MODFLOW basic file
#' 
#' \code{rmf_read_bas} reads in a MODFLOW basic file and returns it as an \code{\link{RMODFLOW}} bas object.
#' 
#' @param file filename; typically '*.bas'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param ... arguments passed to \code{rmfi_parse_array} and \code{rmfi_parse_variables}. Can be ignored when input is 'free' format and input arrays are INTERNAL or CONSTANT.
#' @return object of class bas
#' @export
#' @seealso \code{\link{rmf_write_bas}}, \code{\link{rmf_create_bas}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?bas6.htm}
rmf_read_bas <- function(file = {cat('Please select bas file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         ...) {
  
  bas <- list()
  bas_lines <- readr::read_lines(file)
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(bas_lines)
  comment(bas) <- data_set_0$comments
  bas_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(bas_lines, format = 'free')
  bas$xsection <- 'XSECTION' %in% toupper(data_set_1$variables)
  bas$chtoch <- 'CHTOCH' %in% toupper(data_set_1$variables)
  bas$free <- 'FREE' %in% toupper(data_set_1$variables)
  bas$printtime <- 'PRINTTIME' %in% toupper(data_set_1$variables)
  bas$showprogress <- 'SHOWPROGRESS' %in% toupper(data_set_1$variables)
  bas$stoperror <- 'STOPERROR' %in% toupper(data_set_1$variables)
  if(bas$stoperror) bas$stoper <- as.numeric(data_set_1$variables[match('stoperror',data_set_1$variables)+1]) else bas$stoper <- as.numeric(NA)
  bas_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 <- rmfi_parse_array(bas_lines,nrow=ifelse(bas$xsection,dis$nlay,dis$nrow),ncol=dis$ncol,nlay=ifelse(bas$xsection,1,dis$nlay), file = file, integer = TRUE, ...)
  bas$ibound <- rmf_create_array(apply(data_set_2$array, MARGIN = 1:length(dim(data_set_2$array)), function(i) as.integer(i)),
                                 dim = rmfi_ifelse0(bas$xsection, c(dis$nlay, dis$ncol), c(dis$nrow, dis$ncol, dis$nlay)),
                                 dimlabels = rmfi_ifelse0(bas$xsection, c("k", "j"), c("i", "j", "k")))
  bas_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  # data set 3
  data_set_3 <- rmfi_parse_variables(bas_lines, n = 1, format = ifelse(bas$free, 'free', 'fixed'))
  bas$hnoflo <- rmfi_ifelse0(is.na(data_set_3$variables[1]), 0, as.numeric(data_set_3$variables[1]))
  bas_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # data set 4
  data_set_4 <- rmfi_parse_array(bas_lines,ifelse(bas$xsection,dis$nlay,dis$nrow),dis$ncol,ifelse(bas$xsection,1,dis$nlay), file = file, ...)
  bas$strt <- rmf_create_array(data_set_4$array, dim = rmfi_ifelse0(bas$xsection, c(dis$nlay, dis$ncol), c(dis$nrow, dis$ncol, dis$nlay)),
                               dimlabels = rmfi_ifelse0(bas$xsection, c("k", "j"), c("i", "j", "k")))
  bas_lines <- data_set_4$remaining_lines
  rm(data_set_4)
  
  class(bas) <- c('bas','rmf_package')
  return(bas)
}

#' Write a MODFLOW basic file
#' 
#' \code{rmf_write_bas} writes a MODFLOW basic file based on an \code{\link{RMODFLOW}} bas object.
#' 
#' @param bas an \code{\link{RMODFLOW}} bas object
#' @param file filename to write to; typically '*.bas'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
rmf_write_bas <- function(bas,
                          file = {cat('Please select bas file to overwrite or provide new filename ...\n'); file.choose()},
                          iprn=-1, 
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Basic Package created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(bas)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  options <- NULL
  if(bas$xsection) {
    options <- paste(options, 'XSECTION ',sep='')
    warning('XSECTION: assuming ibound and strt arrays are of dimensions NLAY x NCOL', call. = FALSE)
  }
  if(bas$chtoch) options <- paste(options, 'CHTOCH ',sep='')
  if(bas$free) options <- paste(options, 'FREE ',sep='')
  if(bas$printtime) options <- paste(options, 'PRINTTIME ',sep='')
  if(bas$showprogress) options <- paste(options, 'SHOWPROGRESS ',sep='')
  if(bas$stoperror) options <- paste(options,'STOPERROR ',bas$stoper,sep='')
  rmfi_write_variables(options, file=file)
  
  # data set 2
  rmfi_write_array(bas$ibound, file = file, iprn = iprn, xsection = bas$xsection, ...)
  
  # data set 3
  rmfi_write_variables(as.character(bas$hnoflo), file=file, format = ifelse(bas$free, 'free', 'fixed'))
  
  # data set 4
  rmfi_write_array(bas$strt, file = file, iprn = iprn, xsection = bas$xsection, ...)
  
}
