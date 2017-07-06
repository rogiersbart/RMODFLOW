#' Write a MODFLOW head observations file
#' 
#' @param hob an \code{\link{RMODFLOW}} hob object
#' @param file filename to write to; typically '*.hob'
#' @return \code{NULL}
#' @export
rmf_write_hob <- function(hob,
                      file = {cat('Please select hob file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
    v <- packageDescription("RMODFLOW")$Version
    cat(paste('# MODFLOW Head-Observation Package created by RMODFLOW, version',v,'\n'), file=file)
    cat(paste('#', comment(hob)), sep='\n', file=file, append=TRUE)
    
  # data set 1
    rmfi_write_variables(hob$nh, hob$mobs, hob$maxm, hob$iuhobsv, hob$hobdry, ifelse(hob$noprint,'NOPRINT',''), file=file)
  
  # data set 2
    # rmfi_write_variables(hob$tomulth, ifelse(is.na(hob$evh),1,hob$evh), file=file) # MODFLOW-2000
    rmfi_write_variables(hob$tomulth, file=file)
  
  # data set 3 - 6
    i <- 1
    while(i <= length(hob$layer)) {
      rmfi_write_variables(hob$obsnam[i], hob$layer[i], hob$row[i], hob$column[i], hob$irefsp[i], hob$toffset[i][1],hob$roff[i], hob$coff[i], hob$hobs[i],file=file) 
      if(hob$layer[i] < 0) {
        rmfi_write_variables(paste(hob$mlay[[i]],hob$pr[[i]],collapse=' '), file=file)
      }
      if(hob$irefsp[i] < 0) {
        
        # data set 5
          rmfi_write_variables(hob$itt[i], file=file)
        
        # data set 6
          for(j in 1:abs(hob$irefsp[i])) {
            i <- i + 1
            rmfi_write_variables(hob$obsnam[i], hob$irefsp[i], hob$toffset[i],hob$hobs[i], file=file)
          }
      }
      i <- i + 1
    }
}  

#' @describeIn rmf_write_hob Deprecated function name
#' @export
write_hob <- function(...) {
  .Deprecated(new = "rmf_write_hob", old = "write_hob")
  rmf_write_hob(...)
}
