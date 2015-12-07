#' Write a MODFLOW layer-property flow file
#' 
#' @param lpf an \code{\link{RMODFLOW}} lpf object
#' @param file filename to write to; typically '*.lpf'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_lpf <- function(lpf, file, IPRN=-1)
{
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Layer-Property Flow Package created by RMODFLOW, version',v,'\n'), file = file)
    cat(paste('#', comment(lpf)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    write_variables(lpf$ILPFCB,lpf$HDRY,lpf$NPLPF,ifelse(lpf$STORAGECOEFFICIENT,'STORAGECOEFFICIENT',''),ifelse(lpf$CONSTANTCV,'CONSTANTCV',''),ifelse(lpf$THICKSTRT,'THICKSTRT',''),ifelse(lpf$NOCVCORRECTION,'NOCVCORRECTION',''),ifelse(lpf$NOVFC,'NOVFC',''),ifelse(lpf$NOPARCHECK,'NOPARCHECK',''), file=file)
    
  # data set 2
    write_variables(lpf$LAYTYP, file = file)
    
  # data set 3
    write_variables(lpf$LAYAVG, file = file)
    
  # data set 4
    write_variables(lpf$CHANI, file = file)
    
  # data set 5
    write_variables(lpf$LAYVKA, file = file)
    
  # data set 6
    write_variables(lpf$LAYWET, file = file)
    
  # data set 7
    if(!as.logical(prod(lpf$LAYWET==0))) {
      write_variables(lpf$WETFCT,lpf$IWETIT,lpf$IHDWET, file = file)
    }
    
  # data set 8-9
    for(i in 1:lpf$NPLPF) {
      write_variables(lpf$PARNAM[i],lpf$PARTYP[i],lpf$Parval[i],lpf$NCLU[i], file = file)
      layers <- which(!is.na(lpf$Mltarr[,i]))
      for(j in 1:lpf$NCLU[i]) {
        write_variables(layers[j],lpf$Mltarr[layers[j],i],lpf$Zonarr[layers[j],i],lpf$IZ[layers[j],i], file=file)
      } 
    }
    
  # data set 10-16
    for(k in 1:dis$NLAY) {
      
    # data set 10
      if('HK' %in% lpf$PARTYP) {
        cat(paste0(IPRN,'\n'),file=file,append=TRUE)
      } else {
        write_array(lpf$HK[,,k], file = file, IPRN = IPRN)
      }
      
    # data set 11
      if(lpf$CHANI[k] <= 0) {
        if('HANI' %in% lpf$PARTYP) {
          cat(paste0(IPRN,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$HANI[,,k], file = file, IPRN = IPRN)
        }
      }
      
    # data set 12
      if('VK' %in% lpf$PARTYP | 'VANI' %in% lpf$PARTYP) {
        cat(paste0(IPRN,'\n'),file=file,append=TRUE)
      } else {
        write_array(lpf$VKA[,,k], file = file, IPRN = IPRN)
      }
      
    # data set 13
      if('TR' %in% dis$SSTR) {
        if('SS' %in% lpf$PARTYP) {
          cat(paste0(IPRN,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$Ss[,,k], file = file, IPRN = IPRN)
        }
      }
      
    # data set 14
      if('TR' %in% dis$SSTR & lpf$LAYTYP[k] != 0) {
        if('SY' %in% lpf$PARTYP) {
          cat(paste0(IPRN,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$Sy[,,k], file = file, IPRN = IPRN)
        }
      }
      
    # data set 15
      if(dis$LAYCBD[k] != 0) {
        if('VKCB' %in% lpf$PARTYP) {
          cat(paste0(IPRN,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$VKCB[,,k], file = file, IPRN = IPRN)
        }
      }
      
    # data set 16
      if(lpf$LAYWET[k] != 0 & lpf$LAYTYP[k] != 0) {
        write_array(lpf$WETDRY[,,k], file = file, IPRN = IPRN)
      }     
    }
}
