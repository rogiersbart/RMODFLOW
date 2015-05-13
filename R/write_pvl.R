#' Write a MODFLOW file
#' 
#' @export
write_pvl <- function(pvl, file)
{
  # Data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Parameter Value File created by RMODFLOW, version',v,'at',date(),'\n'), file=file)
    cat(paste('#', comment(pvl)), sep='\n', file=file, append=TRUE)
    
  # Data set 1
    cat(paste(pvl$NP, '\n', sep=''), file=file, append=TRUE)
  
  # Data set 2
    for(i in 1:pvl$NP)
    {
      cat(paste(pvl$PARNAM[i], ' ', pvl$Parval[i],'\n', sep=''), file=file, append=TRUE)
    }  
}