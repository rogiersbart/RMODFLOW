#' Create an \code{RMODFLOW} dis object
#' 
#' \code{rmf_create_dis} creates an \code{RMODFLOW} dis object.
#' 
#' @param nlay number of layers; defaults to 3
#' @param nrow number of rows; defaults to 10
#' @param ncol number of columns; defaults to 10
#' @param nper number of stress periods; defaults to 1
#' @param itmuni time unit; defaults to 1 (seconds)
#' @param lenuni length unit; defaults to 2 (metres)
#' @param laycbd vector of Quasi-3D confining bed flags; defaults to 0 for all layers
#' @param delr vector of cell widths along rows; defaults to 100 for all columns
#' @param delc vector of cell widths along columns; defaults to 100 for all rows
#' @param top matrix with the top elevation of layer 1; defaults to 0 for all nrow x ncol cells
#' @param botm 3D array with the bottom elevations of all layers and Quasi-3D confining beds; defaults to nlay layers, equally spaced between 0 (top first layer) and -100 (bottom last layer)
#' @param perlen vector of stress period lengths 
#' @param nstp vector of stress period time steps
#' @param tsmult vector of successive time step length multipliers
#' @param sstr character vector with steady state ('SS') or transient ('TR') stress period indicator
#' @return Object of class dis
#' @export
#' @seealso \code{\link{rmf_read_dis}}, \code{\link{rmf_write_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
rmf_create_dis <- function(nlay = 3,
                           nrow = 10,
                           ncol = 10,
                           nper = 1,
                           itmuni = 1,
                           lenuni = 2,
                           laycbd = rep(0, nlay),
                           delr = 100,
                           delc = 100,
                           top = matrix(0, nrow = nrow, ncol = ncol),
                           botm = array(rep(seq(0,-10 * (nlay + sum(laycbd != 0)),length = nlay + sum(laycbd != 0) + 1)[2:(nlay + sum(laycbd != 0) + 1)], each = nrow * ncol), dim = c(nrow, ncol, nlay + sum(laycbd != 0))),
                           perlen = rep(1, nper),
                           nstp = rep(1, nper),
                           tsmult = rep(1, nper),
                           sstr = c('SS', rep('TR', nper - 1))) {
  dis <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting dis object
    
  # data set 1
    dis$nlay <- nlay
    dis$nrow <- nrow
    dis$ncol <- ncol
    dis$nper <- nper
    dis$itmuni <- itmuni
    dis$lenuni <- lenuni
  
  # data set 2
    dis$laycbd <- laycbd
    if(dis$laycbd[dis$nlay] != 0) {
      warning("Setting laycbd for the bottom layer to zero.", call. = FALSE)
      dis$laycbd[dis$nlay] <- 0
    }
  
  # data set 3
    if(length(delr) == 1) delr <- rep(delr, ncol)
    dis$delr <- delr
  # data set 4
    if(length(delc) == 1) delc <- rep(delc, nrow)
    dis$delc <- delc
  
  # data set 5
    dis$top <- rmf_create_array(top, dim = c(dis$nrow, dis$ncol))

  # data set 6
    dis$botm <- rmf_create_array(botm, dim = c(dis$nrow, dis$ncol, dis$nlay + length(which(dis$laycbd != 0))))

  # data set 7
    dis$perlen <- perlen
    dis$nstp <- nstp
    dis$tsmult <- tsmult
    dis$sstr <- sstr
  
  #comment(dis) <- comments
  class(dis) <- c('dis','rmf_package')
  return(dis)
}

#' @describeIn rmf_create_dis Deprecated function name
#' @export
create_dis <- function(...) {
  .Deprecated(new = "rmf_create_dis", old = "create_dis")
  rmf_create_dis(...)
}

#' Read a MODFLOW discretization file
#' 
#' \code{rmf_read_dis} reads in a MODFLOW discretization file and returns it as an \code{\link{RMODFLOW}} dis object.
#' 
#' @param file filename; typically '*.dis'
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class dis
#' @export
#' @seealso \code{\link{rmf_write_dis}}, \code{\link{rmf_create_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
rmf_read_dis <- function(file = {cat('Please select dis file ...\n'); file.choose()}, ...) {
  
  dis_lines <- readr::read_lines(file)
  dis <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(dis_lines)
  comment(dis) <- data_set_0$comments
  dis_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(dis_lines)
  dis$nlay <- as.numeric(data_set_1$variables[1])
  dis$nrow <- as.numeric(data_set_1$variables[2])
  dis$ncol <- as.numeric(data_set_1$variables[3])
  dis$nper <- as.numeric(data_set_1$variables[4])
  dis$itmuni <- as.numeric(data_set_1$variables[5])
  dis$lenuni <- as.numeric(data_set_1$variables[6])
  dis_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 <- rmfi_parse_variables(dis_lines, nlay = dis$nlay)
  dis$laycbd <- as.numeric(data_set_2$variables[1:dis$nlay])
  dis_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  if(dis$laycbd[dis$nlay] != 0) {
    warning("Setting laycbd for the bottom layer to zero.", call. = FALSE)
    dis$laycbd[dis$nlay] <- 0
  }
  
  # data set 3
  data_set_3 <- rmfi_parse_array(dis_lines, 1, dis$ncol, 1, ndim = 1, file = file, ...)
  dis$delr <- data_set_3$array
  dis_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # data set 4
  data_set_4 <- rmfi_parse_array(dis_lines, 1, dis$nrow, 1, ndim = 1, file = file, ...)
  dis$delc <- data_set_4$array
  dis_lines <- data_set_4$remaining_lines
  rm(data_set_4)
  
  # data set 5
  data_set_5 <- rmfi_parse_array(dis_lines,dis$nrow,dis$ncol,1, ndim = 2, file = file, ...)
  dis_lines <- data_set_5$remaining_lines
  dis$top <- data_set_5$array
  rm(data_set_5)
  
  # data set 6
  data_set_6 <- rmfi_parse_array(dis_lines,dis$nrow,dis$ncol,dis$nlay+length(which(dis$laycbd != 0)), file = file, ...)
  dis_lines <- data_set_6$remaining_lines
  dis$botm <- rmf_create_array(data_set_6$array, dim = c(dis$nrow, dis$ncol, dis$nlay+length(which(dis$laycbd != 0))))
  rm(data_set_6)
  
  # data set 7
  for(i in 1:dis$nper) {
    data_set_7 <- rmfi_parse_variables(dis_lines)
    dis$perlen[i] <- as.numeric(data_set_7$variables[1])
    dis$nstp[i] <- as.numeric(data_set_7$variables[2])
    dis$tsmult[i] <- as.numeric(data_set_7$variables[3])
    dis$sstr[i] <- data_set_7$variables[4]
    dis_lines <- data_set_7$remaining_lines
    rm(data_set_7)
  }
  
  class(dis) <- c('dis','rmf_package')
  return(dis)
}

#' @describeIn rmf_read_dis Deprecated function name
#' @export
read_dis <- function(...) {
  .Deprecated(new = "rmf_read_dis", old = "read_dis")
  rmf_read_dis(...)
}

#' Write a MODFLOW discretization file
#' 
#' @param dis an \code{\link{RMODFLOW}} dis object
#' @param file filename to write to; typically '*.dis'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_dis}}, \code{\link{rmf_create_dis}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm}
rmf_write_dis <- function(dis,
                          file = {cat('Please select dis file to overwrite or provide new filename ...\n'); file.choose()},
                          iprn=-1,
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Discretization File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(dis)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(dis$nlay,dis$nrow,dis$ncol,dis$nper,dis$itmuni,dis$lenuni,file=file)
  #  cat(paste(dis$nlay,dis$nrow,dis$ncol,dis$nper,dis$itmuni,dis$lenuni, '\n', sep=' '), file=file, append=TRUE)
  
  # data set 2
  rmfi_write_variables(dis$laycbd,file=file)
  #  cat(paste(paste(dis$laycbd, collapse=' '), '\n', sep=' '), file=file, append=TRUE)
  
  # data set 3
  rmfi_write_array(dis$delr,file=file,iprn=iprn, ...)  
  
  # data set 4
  rmfi_write_array(dis$delc,file=file,iprn=iprn, ...)
  
  # data set 5
  rmfi_write_array(dis$top,file=file,iprn=iprn, ...)
  
  # data set 6
  rmfi_write_array(dis$botm,file=file,iprn=iprn, ...)
  
  # data set 7
  for(i in 1:dis$nper) {
    rmfi_write_variables(dis$perlen[i],dis$nstp[i],dis$tsmult[i],dis$sstr[i], file=file)  
  }
}

#' @describeIn rmf_write_dis Deprecated function name
#' @export
write_dis <- function(...) {
  .Deprecated(new = "rmf_write_dis", old = "write_dis")
  rmf_write_dis(...)
}
