#' Write a MODFLOW hydrogeologic unit flow file
#' 
#' @param huf an \code{\link{RMODFLOW}} huf object
#' @param file filename to write to; typically '*.huf'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_huf <- function(huf, file, IPRN=-1)
{
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Hydrogeologic Unit Flow Package created by RMODFLOW, version',v,'\n'), file = file)
    cat(paste('#', comment(huf)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    write_variables(huf$IHUFCB,huf$HDRY,huf$NHUF,huf$NPHUF,huf$IOHUFHEADS,huf$IOHUFFLOWS, file = file)
  
  # data set 2
    write_variables(huf$LTHUF, file=file)
  
  # data set 3
    write_variables(huf$LAYWT, file=file)
  
  # data set 4
    if(sum(huf$LAYWT > 0)) {
      write_variables(huf$WETFCT, huf$IWETIT, huf$IHDWET, file = file)
    }

  # data set 5
    if(dim(huf$WETDRY)[3]>0) {
      write_array(huf$WETDRY, file = file, IPRN = IPRN) 
    }
  
  # data set 6-8
    for(i in 1:huf$NHUF) {
      write_variables(huf$HGUNAM[i], file=file)   
      write_array(huf$TOP[,,i], file = file, IPRN = IPRN)
      write_array(huf$THCK[,,i], file = file, IPRN = IPRN)
    }
  
  # data set 9
    for(i in 1:huf$NHUF) {
      write_variables(huf$HGUNAM[i],huf$HGUHANI[i],huf$HGUVANI[i], file=file)
    }
  
  # data set 10-11
    for(i in 1:huf$NPHUF) {
      write_variables(huf$PARNAM[i],huf$PARTYP[i],huf$Parval[i],huf$NCLU[i], file=file)
      HGUNAMs <- which(!is.na(huf$Mltarr[,i]))
      for(j in 1:huf$NCLU[i]) {
        write_variables(huf$HGUNAM[HGUNAMs[j]],huf$Mltarr[HGUNAMs[j],i],huf$Zonarr[HGUNAMs[j],i],huf$IZ[HGUNAMs[j],i], file=file)      
      }
    }
  
  # data set 12
  # Print options, not implemented
}
