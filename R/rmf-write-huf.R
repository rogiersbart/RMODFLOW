#' Write a MODFLOW hydrogeologic unit flow file
#' 
#' @param huf an \code{\link{RMODFLOW}} huf object
#' @param file filename to write to; typically '*.huf'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmf_write_huf <- function(huf,
                      file = {cat('Please select huf file to overwrite or provide new filename ...\n'); file.choose()},
                      iprn=-1) {
  
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
      rmfi_write_array(huf$wetdry, file = file, iprn = iprn) 
    }
  
  # data set 6-8
    for(i in 1:huf$nhuf) {
      rmfi_write_variables(huf$hgunam[i], file=file)   
      rmfi_write_array(huf$top[,,i], file = file, iprn = iprn)
      rmfi_write_array(huf$thck[,,i], file = file, iprn = iprn)
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
write_huf <- function(...) {
  .Deprecated(new = "rmf_write_huf", old = "write_huf")
  rmf_write_huf(...)
}
