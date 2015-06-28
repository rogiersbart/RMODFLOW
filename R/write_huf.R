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
  cat(paste('# MODFLOW Hydrogeologic Unit Flow Package created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(huf)), sep='\n', file=file, append=TRUE)
  
  # Data set 1
  cat(paste(huf$IHUFCB,huf$HDRY,huf$NHUF,huf$NPHUF,huf$IOHUFHEADS,huf$IOHUFFLOWS, '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 2
  cat(paste(paste(huf$LTHUF, collapse=' '), '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 3
  cat(paste(paste(huf$LAYWT, collapse=' '), '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 4
  if(sum(huf$LAYWT > 0)) cat(paste(huf$WETFCT, huf$IWETIT, huf$IHDWET, '\n', sep=' '), file=file, append=TRUE)

  # Data set 5
  if(dim(huf$WETDRY)[3]>0)
  {
    for(i in 1:dim(huf$WETDRY)[3])
    {
      cat(paste('INTERNAL 1.0 (free) ',IPRN, '\n', sep=''), file=file, append=TRUE)
      write.table(huf$WETDRY[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }    
  }
  
  # Data set 6-8
  for(i in 1:huf$NHUF)
  {
    cat(paste(huf$HGUNAM[i], '\n', sep=' '), file=file, append=TRUE)   
    cat(paste('INTERNAL 1.0 (free) ',IPRN, '\n', sep=''), file=file, append=TRUE)
    write.table(huf$TOP[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)     
    cat(paste('INTERNAL 1.0 (free) ',IPRN, '\n', sep=''), file=file, append=TRUE)
    write.table(huf$THCK[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE) 
  }
  
  # Data set 9
  for(i in 1:huf$NHUF) cat(paste(huf$HGUNAM[i],huf$HGUHANI[i],huf$HGUVANI[i], '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 10-11
  for(i in 1:huf$NPHUF)
  {
    cat(paste(huf$PARNAM[i],huf$PARTYP[i],huf$Parval[i],huf$NCLU[i], '\n', sep=' '), file=file, append=TRUE)
    HGUNAMs <- which(!is.na(huf$Mltarr[,i]))
    for(j in 1:huf$NCLU[i])
    {
      cat(paste(huf$HGUNAM[HGUNAMs[j]],huf$Mltarr[HGUNAMs[j],i],huf$Zonarr[HGUNAMs[j],i],huf$IZ[HGUNAMs[j],i], '\n', sep=' '), file=file, append=TRUE)      
    }
  }
  
  # Data set 12
  # Print options, not implemented
}