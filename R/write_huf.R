#' Write a MODFLOW hydrogeologic unit flow file
#' 
#' @param huf an \code{\link{RMODFLOW}} huf object
#' @param file filename to write to; typically '*.huf'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_huf <- function(huf, file, IPRN=-1)
{
  # Data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Hydrogeologic Unit Flow Package created by RMODFLOW, version',v,'\n'), file = file)
    cat(paste('#', comment(huf)), sep='\n', file=file, append=TRUE)
    
  # Data set 1
    write_modflow_variables(huf$IHUFCB,huf$HDRY,huf$NHUF,huf$NPHUF,huf$IOHUFHEADS,huf$IOHUFFLOWS, file = file)
  
  # Data set 2
    write_modflow_variables(huf$LTHUF, file=file)
  
  # Data set 3
    write_modflow_variables(huf$LAYWT, file=file)
  
  # Data set 4
    if(sum(huf$LAYWT > 0)) {
      write_modflow_variables(huf$WETFCT, huf$IWETIT, huf$IHDWET, file = file)
    }

  # Data set 5
    if(dim(huf$WETDRY)[3]>0) {
      write_modflow_array(huf$WETDRY, file = file, IPRN = IPRN) 
    }
  
  # Data set 6-8
    for(i in 1:huf$NHUF) {
      write_modflow_variables(huf$HGUNAM[i], file=file)   
      write_modflow_array(huf$TOP[,,i], file = file, IPRN = IPRN)
      write_modflow_array(huf$THCK[,,i], file = file, IPRN = IPRN)
    }
  
  # Data set 9
    for(i in 1:huf$NHUF) {
      write_modflow_variables(huf$HGUNAM[i],huf$HGUHANI[i],huf$HGUVANI[i], file=file)
    }
  
  # Data set 10-11
    for(i in 1:huf$NPHUF) {
      write_modflow_variables(huf$PARNAM[i],huf$PARTYP[i],huf$Parval[i],huf$NCLU[i], file=file)
      HGUNAMs <- which(!is.na(huf$Mltarr[,i]))
      for(j in 1:huf$NCLU[i]) {
        write_modflow_variables(huf$HGUNAM[HGUNAMs[j]],huf$Mltarr[HGUNAMs[j],i],huf$Zonarr[HGUNAMs[j],i],huf$IZ[HGUNAMs[j],i], file=file)      
      }
    }
  
  # Data set 12
  # Print options, not implemented
}
