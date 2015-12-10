#' Write a MODFLOW layer-property flow file
#' 
#' @param lpf an \code{\link{RMODFLOW}} lpf object
#' @param file filename to write to; typically '*.lpf'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_lpf <- function(lpf, file, iprn=-1)
{
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Layer-Property Flow Package created by RMODFLOW, version',v,'\n'), file = file)
    cat(paste('#', comment(lpf)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    write_variables(lpf$ilpfcb,lpf$hdry,lpf$nplpf,ifelse(lpf$storagecoefficient,'STORAGECOEFFICIENT',''),ifelse(lpf$constantcv,'CONSTANTCV',''),ifelse(lpf$thickstrt,'THICKSTRT',''),ifelse(lpf$nocvcorrection,'NOCVCORRECTION',''),ifelse(lpf$novfc,'NOVFC',''),ifelse(lpf$noparcheck,'NOPARCHECK',''), file=file)
    
  # data set 2
    write_variables(lpf$laytyp, file = file)
    
  # data set 3
    write_variables(lpf$layavg, file = file)
    
  # data set 4
    write_variables(lpf$chani, file = file)
    
  # data set 5
    write_variables(lpf$layvka, file = file)
    
  # data set 6
    write_variables(lpf$laywet, file = file)
    
  # data set 7
    if(!as.logical(prod(lpf$laywet==0))) {
      write_variables(lpf$wetfct,lpf$iwetit,lpf$ihdwet, file = file)
    }
    
  # data set 8-9
    for(i in 1:lpf$nplpf) {
      write_variables(lpf$parnam[i],lpf$partyp[i],lpf$parval[i],lpf$nclu[i], file = file)
      layers <- which(!is.na(lpf$mltarr[,i]))
      for(j in 1:lpf$nclu[i]) {
        write_variables(layers[j],lpf$mltarr[layers[j],i],lpf$zonarr[layers[j],i],lpf$iz[layers[j],i], file=file)
      } 
    }
    
  # data set 10-16
    for(k in 1:dis$NLAY) {
      
    # data set 10
      if('HK' %in% lpf$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        write_array(lpf$hk[,,k], file = file, iprn = iprn)
      }
      
    # data set 11
      if(lpf$chani[k] <= 0) {
        if('HANI' %in% lpf$partyp) {
          cat(paste0(iprn,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$hani[,,k], file = file, iprn = iprn)
        }
      }
      
    # data set 12
      if('VK' %in% lpf$partyp | 'VANI' %in% lpf$partyp) {
        cat(paste0(iprn,'\n'),file=file,append=TRUE)
      } else {
        write_array(lpf$vka[,,k], file = file, iprn = iprn)
      }
      
    # data set 13
      if('TR' %in% dis$sstr) {
        if('SS' %in% lpf$partyp) {
          cat(paste0(iprn,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$ss[,,k], file = file, iprn = iprn)
        }
      }
      
    # data set 14
      if('TR' %in% dis$sstr & lpf$laytyp[k] != 0) {
        if('SY' %in% lpf$partyp) {
          cat(paste0(iprn,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$sy[,,k], file = file, iprn = iprn)
        }
      }
      
    # data set 15
      if(dis$laycbd[k] != 0) {
        if('VKCB' %in% lpf$partyp) {
          cat(paste0(iprn,'\n'),file=file,append=TRUE)
        } else {
          write_array(lpf$vkcb[,,k], file = file, iprn = iprn)
        }
      }
      
    # data set 16
      if(lpf$laywet[k] != 0 & lpf$laytyp[k] != 0) {
        write_array(lpf$wetdry[,,k], file = file, iprn = iprn)
      }     
    }
}
