#' Write a MODFLOW-NWT upstream weighting file
#' 
#' @param upw an \code{\link{RMODFLOW}} upw object
#' @param file filename to write to; typically '*.upw'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @note upw input structure is nearly identical to lpf but calculations are done differently. Differences include the addition of the iphdry value and the ommision of optional keywords. Layer wetting capabilities are also not supported by upw.
#' @note upw must be used with the Newton solver. See also \code{\link{rmf_create_nwt}}.
#' @return \code{NULL}
#' @export
rmf_write_upw <- function(upw,
                          file = {cat('Please select upw file to overwrite or provide new filename ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          iprn=-1, 
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Upstream Weighting Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(upw)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(upw$iupwcb,upw$hdry,upw$npupw, ifelse(upw$iphdry, 1, 0), file=file)
  
  # data set 2
  rmfi_write_variables(upw$laytyp, file = file)
  
  # data set 3
  rmfi_write_variables(upw$layavg, file = file)
  
  # data set 4
  rmfi_write_variables(upw$chani, file = file)
  
  # data set 5
  rmfi_write_variables(upw$layvka, file = file)
  
  # data set 6
  rmfi_write_variables(upw$laywet, file = file)
  
  # data set 7-8
  if(upw$npupw != 0) {
    for(i in 1:upw$npupw) {
      rmfi_write_variables(upw$parnam[i],upw$partyp[i],upw$parval[i],upw$nclu[i], file = file)
      layers <- which(!is.na(upw$mltarr[,i]))
      for(j in 1:upw$nclu[i]) {
        rmfi_write_variables(layers[j],upw$mltarr[layers[j],i],upw$zonarr[layers[j],i],upw$iz[layers[j],i], file=file)
      } 
    }
  }
  
  # data set 9-14
  for(k in 1:dis$nlay) {
    
    # data set 9
    if('HK' %in% upw$partyp) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(upw$hk[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 10
    if(upw$chani[k] <= 0) {
      if('HANI' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$hani[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 11
    if('VK' %in% upw$partyp | 'VANI' %in% upw$partyp) {
      cat(paste0(iprn,'\n'),file=file,append=TRUE)
    } else {
      rmfi_write_array(upw$vka[,,k], file = file, iprn = iprn, ...)
    }
    
    # data set 12
    if('TR' %in% dis$sstr) {
      if('SS' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$ss[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 13
    if('TR' %in% dis$sstr & upw$laytyp[k] != 0) {
      if('SY' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$sy[,,k], file = file, iprn = iprn, ...)
      }
    }
    
    # data set 14
    if(dis$laycbd[k] != 0) {
      if('VKCB' %in% upw$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        rmfi_write_array(upw$vkcb[,,k], file = file, iprn = iprn, ...)
      }
    }
  }
}

