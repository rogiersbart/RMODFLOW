#' Write a MODFLOW file
#' 
#' @export
write_pval <- function(pval, file)
{
  # Data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Parameter Value File created by RMODFLOW, version',v,'at',date(),'\n'), file=file)
    cat(paste('#', comment(pval)), sep='\n', file=file, append=TRUE)
    
  # Data set 1
    cat(paste(pval$NP, '\n', sep=''), file=file, append=TRUE)
  
  # Data set 2
    for(i in 1:pval$NP)
    {
      cat(paste(pval$PARNAM[i], ' ', pval$Parval[i],'\n', sep=''), file=file, append=TRUE)
    }  
}