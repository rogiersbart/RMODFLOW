################################################################################
### write.pval ##################################################################
################################################################################
write.pval <- function(pval, file, info='No further information provided')
{
  # Data set 0
  cat('# MODFLOW Parameter Value File created in R\n', file=file)
  cat(paste('#', info, '\n'), file=file, append=TRUE)
  
  # Data set 1
  cat(paste(pval$NP, '\n', sep=''), file=file, append=TRUE)
  
  # Data set 2
  for(i in 1:pval$NP)
  {
    cat(paste(pval$PARNAM[i], ' ', pval$Parval[i],'\n', sep=''), file=file, append=TRUE)
  }  
}