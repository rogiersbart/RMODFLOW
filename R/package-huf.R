#' Read a MODFLOW hydrogeologic unit flow file
#' 
#' \code{read_huf} reads in a MODFLOW hydrogeologic unit flow file and returns it as an \code{\link{RMODFLOW}} huf object.
#' 
#' @param file filename; typically '*.huf'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return object of class huf
#' @importFrom readr read_lines
#' @export
rmf_read_huf <- function(file = {cat('Please select huf file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         ...) {
  
  huf_lines <- read_lines(file)
  huf <- list()

  # data set 0
    data_set_0 <- rmfi_parse_comments(huf_lines)
    comment(huf) <- data_set_0$comments
    huf_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    data_set_1 <- rmfi_parse_variables(huf_lines)
    huf$ihufcb <- data_set_1$variables[1]
    huf$hdry <- data_set_1$variables[2]
    huf$nhuf <- data_set_1$variables[3]
    huf$nphuf <- data_set_1$variables[4]
    huf$iohufheads <- ifelse(is.na(data_set_1$variables[5]),0,data_set_1$variables[5])
    huf$iohufflows <- ifelse(is.na(data_set_1$variables[6]),0,data_set_1$variables[6])
    huf_lines <- data_set_1$remaining_lines
    rm(data_set_1)
  
  # data set 2
    data_set_2 <- rmfi_parse_variables(huf_lines)
    huf$lthuf <- data_set_2$variables
    huf_lines <- data_set_2$remaining_lines
    rm(data_set_2)
  
  # data set 3   
    data_set_3 <- rmfi_parse_variables(huf_lines)
    huf$laywt <- data_set_3$variables
    huf_lines <- data_set_3$remaining_lines
    rm(data_set_3)
  
  # data set 4
    if(sum(huf$laywt > 0)) {
      data_set_4 <- rmfi_parse_variables(huf_lines)
      huf$wetfct <- data_set_4$variables[1]
      huf$iwetit <- data_set_4$variables[2]
      huf$ihdwet <- data_set_4$variables[3]
      huf_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
  
  # data set 5
    data_set_5 <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol,sum(which(huf$laywt!=0)), file = file, ...)
    huf$wetdry <- data_set_5$array
    huf_lines <- data_set_5$remaining_lines
    rm(data_set_5)
  
  # data set 6-8
    huf$hgunam <- vector(mode='character',length=huf$nhuf)
    huf$top <- rmf_create_array(dim=c(dis$nrow, dis$ncol, huf$nhuf))
    huf$thck <- rmf_create_array(dim=c(dis$nrow, dis$ncol, huf$nhuf))
    for(i in 1:huf$nhuf) {
      data_set <- rmfi_parse_variables(huf_lines)
      huf$hgunam[i] <- data_set$variables[1]
      huf_lines <- data_set$remaining_lines
      data_set <- rmfi_parse_array(huf_lines,dis$nrow,dis$ncol,2, file = file, ...)
      huf_lines <- data_set$remaining_lines
      huf$top[,,i] <- data_set$array[,,1]
      huf$thck[,,i] <- data_set$array[,,2]  
    }
    rm(data_set)
  
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
    huf$parnam <- vector(mode='character',length=huf$nphuf)
    huf$partyp <- vector(mode='character',length=huf$nphuf)
    huf$parval <- vector(mode='numeric',length=huf$nphuf)
    huf$nclu <- vector(mode='numeric',length=huf$nphuf)
    huf$mltarr <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    huf$zonarr <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    huf$iz <- matrix(nrow=huf$nhuf, ncol=huf$nphuf)
    for(i in 1:huf$nphuf) {
      data_set_10 <- rmfi_parse_variables(huf_lines)
      huf_lines <- data_set_10$remaining_lines
      huf$parnam[i] <- data_set_10$variables[1]
      huf$partyp[i] <- data_set_10$variables[2]
      huf$parval[i] <- as.numeric(data_set_10$variables[3])
      huf$nclu[i] <- as.numeric(data_set_10$variables[4])
      for(j in 1:huf$nclu[i]) {
        data_set_11 <- rmfi_parse_variables(huf_lines)
        huf_lines <- data_set_11$remaining_lines
        k <- which(huf$hgunam == data_set_11$variables[1])
        huf$mltarr[k,i] <- data_set_11$variables[2]
        huf$zonarr[k,i] <- data_set_11$variables[3]
        huf$iz[k,i] <- paste(data_set_11$variables[-c(1:3)],collapse=' ')
      } 
    }
    rm(data_set_10, data_set_11)
  
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
  if(sum(huf$laywt > 0)) {
    rmfi_write_variables(huf$wetfct, huf$iwetit, huf$ihdwet, file = file)
  }
  
  # data set 5
  if(dim(huf$wetdry)[3]>0) {
    rmfi_write_array(huf$wetdry, file = file, iprn = iprn, ...) 
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
    rmfi_write_variables(huf$parnam[i],huf$partyp[i],huf$parval[i],huf$nclu[i], file=file)
    hgunams <- which(!is.na(huf$mltarr[,i]))
    for(j in 1:huf$nclu[i]) {
      rmfi_write_variables(huf$hgunam[hgunams[j]],huf$mltarr[hgunams[j],i],huf$zonarr[hgunams[j],i],huf$iz[hgunams[j],i], file=file)      
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
#' @importFrom readr read_lines
#' @export
rmf_read_kdep <- function(file = {cat('Please select kdep file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          huf = {cat('Please select corresponding huf file ...\n'); rmf_read_huf(file.choose(), dis = dis)},
                          ...) {
  
  kdep_lines <- read_lines(file)
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