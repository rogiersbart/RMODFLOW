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
    write_modflow_variables(hob$TOMULTH, ifelse(is.na(hob$EVH),1,hob$EVH), file=file)
  
  # Data set 3 - 6
    if(class(hob$IREFSP)=='list') {
      IREFSP <- hob$IREFSP   
      hob$IREFSP <- NULL
      for(i in 1:hob$NH) hob$IREFSP[i] <- -length(IREFSP[[i]])
    }  
    for(nr in 1:hob$NH) {
      write_modflow_variables(hob$OBSNAM[[nr]][1], hob$LAYER[nr], hob$ROW[nr], hob$COLUMN[nr], hob$IREFSP[nr], hob$TOFFSET[[nr]][1],hob$ROFF[nr], hob$COFF[nr], hob$HOBS[[nr]][1],ifelse(is.na(hob$STATISTIC[nr]),'',hob$STATISTIC[nr]),ifelse(is.na(hob$STATFLAG[nr]),'',hob$STATFLAG[[nr]][1]),ifelse(is.na(hob$PLOTSYMBOL[[nr]][1]),'',hob$PLOTSYMBOL[[nr]][1]),file=file)
      if(hob$LAYER[nr] < 0) {
        line <- NULL
        for(layerNr in 1:abs(hob$LAYER[nr])) {
          line <- append(line,hob$MLAY[[nr]][layerNr])
          line <- append(line,hob$PR[[nr]][layerNr])
        }
        write_modflow_variables(line, file=file)
      }
      if(hob$IREFSP[nr] < 0) {
        # Data set 5
          write_modflow_variables(hob$ITT, file=file)
        
        # Data set 6
          for(time in 1:abs(hob$IREFSP[nr])) {
            write_modflow_variables(hob$OBSNAM[[nr]][time], IREFSP[[nr]][time], hob$TOFFSET[[nr]][time],hob$HOBS[[nr]][time], hob$STATh[[nr]][time], hob$STATdd[[nr]][time],hob$STATFLAG[[nr]][time], hob$PLOTSYMBOL[[nr]][time], file=file)
          }
      }       
    }  
}  
