#' Write a MODFLOW head observations file
#' 
#' @param hob an \code{\link{RMODFLOW}} hob object
#' @param file filename to write to; typically '*.hob'
#' @return \code{NULL}
#' @export
write_hob <- function(hob, file) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Head-Observation Package created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(hob)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    write_modflow_variables(hob$nh, hob$mobs, hob$maxm, hob$iuhobsv, hob$hobdry, ifelse(hob$noprint,'NOPRINT',''), file=file)
  
  # data set 2
    # write_modflow_variables(hob$tomulth, ifelse(is.na(hob$evh),1,hob$evh), file=file) # MODFLOW-2000
    write_modflow_variables(hob$tomulth, file=file)
  
  # data set 3 - 6
    for(i in 1:length(hob$layer)) {
      write_modflow_variables(hob$obsnam[[i]][1], hob$layer[i], hob$row[i], hob$column[i], ifelse(length(hob$irefsp[[i]]) > 1,-length(hob$irefsp[[i]]),hob$irefsp[[i]]), hob$toffset[[i]][1],hob$roff[i], hob$coff[i], hob$hobs[[i]][1],file=file) 
      if(hob$layer[i] < 0) {
        write_modflow_variables(paste(hob$mlay[[i]],hob$pr[[i]],collapse=' '), file=file)
      }
      if(length(hob$irefsp[[i]]) > 1) {
        
        # data set 5
          write_modflow_variables(hob$itt[i], file=file)
        
        # data set 6
          for(j in 1:length(hob$irefsp[[i]])) {
            write_modflow_variables(hob$obsnam[[i]][j], hob$irefsp[[i]][j], hob$toffset[[i]][j],hob$hobs[[i]][j], file=file)
          }
      }
    }
}  
