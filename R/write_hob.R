#' Write a MODFLOW head observations file
#' 
#' @param hob an \code{\link{RMODFLOW}} hob object
#' @param file filename to write to; typically '*.hob'
#' @return \code{NULL}
#' @export
write_hob <- function(hob, file)
{
  # Data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Head-Observation Package created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(hob)), sep='\n', file=file, append=TRUE)
    
  # Data set 1
    write_modflow_variables(hob$NH, hob$MOBS, hob$MAXM, hob$IUHOBSV, hob$HOBDRY, ifelse(hob$NOPRINT,'NOPRINT',''), file=file)
  
  # Data set 2
    # write_modflow_variables(hob$TOMULTH, ifelse(is.na(hob$EVH),1,hob$EVH), file=file) # MODFLOW-2000
    write_modflow_variables(hob$TOMULTH, file=file)
  
  # Data set 3 - 6
    for(i in 1:length(hob$LAYER)) {
      write_modflow_variables(hob$OBSNAM[[i]][1], hob$LAYER[i], hob$ROW[i], hob$COLUMN[i], ifelse(length(hob$IREFSP[[i]]) > 1,-length(hob$IREFSP[[i]]),hob$IREFSP[[i]]), hob$TOFFSET[[i]][1],hob$ROFF[i], hob$COFF[i], hob$HOBS[[i]][1],file=file) 
      if(hob$LAYER[i] < 0) {
        write_modflow_variables(paste(hob$MLAY[[i]],hob$PR[[i]],collapse=' '), file=file)
      }
      if(length(hob$IREFSP[[i]]) > 1) {
        
        # Data set 5
          write_modflow_variables(hob$ITT[i], file=file)
        
        # Data set 6
          for(j in 1:length(hob$IREFSP[[i]])) {
            write_modflow_variables(hob$OBSNAM[[i]][j], hob$IREFSP[[i]][j], hob$TOFFSET[[i]][j],hob$HOBS[[i]][j], file=file)
          }
      }
    }
}  
