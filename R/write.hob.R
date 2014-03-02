################################################################################
### write.hob ##################################################################
################################################################################
write.hob <- function(hob, file, info='No further information provided')
{
  # Data set 0
  cat('# MODFLOW Head-Observation Package created in R\n', file=file)
  cat(paste('#', info, '\n'), file=file, append=TRUE)
  
  # Data set 1
  cat(paste(hob$NH, hob$MOBS, hob$MAXM, hob$IUHOBSV, hob$HOBDRY, ifelse(hob$NOPRINT,'NOPRINT \n','\n'), sep=' '), file=file, append=TRUE)
  
  # Data set 2
  cat(paste(hob$TOMULTH, ifelse(is.na(hob$EVH),1,hob$EVH), '\n', sep=' '), file=file, append=TRUE)
  
  # Data set 3 - 6
  if(class(hob$IREFSP)=='list')
  {
    IREFSP <- hob$IREFSP   
    hob$IREFSP <- NULL
    for(i in 1:hob$NH) hob$IREFSP[i] <- -length(IREFSP[[i]])
  }  
  for(nr in 1:hob$NH)
  {
    cat(paste(hob$OBSNAM[[nr]][1], hob$LAYER[nr], hob$ROW[nr], hob$COLUMN[nr], hob$IREFSP[nr], hob$TOFFSET[[nr]][1],
              hob$ROFF[nr], hob$COFF[nr], hob$HOBS[[nr]][1],
              ifelse(is.na(hob$STATISTIC[nr]),'',hob$STATISTIC[nr]),
              ifelse(is.na(hob$STATFLAG[nr]),'',hob$STATFLAG[[nr]][1]),
              ifelse(is.na(hob$PLOTSYMBOL[[nr]][1]),'',hob$PLOTSYMBOL[[nr]][1]),
              '\n', sep=' '), file=file, append=TRUE)
    if(hob$LAYER[nr] < 0)
    {
      line <- NULL
      for(layerNr in 1:abs(hob$LAYER[nr]))
      {
        line <- append(line,hob$MLAY[[nr]][layerNr])
        line <- append(line,hob$PR[[nr]][layerNr])
      }
      cat(paste(line,collapse=' '), file=file, append=TRUE);cat('\n', file=file, append=TRUE)
    }
    if(hob$IREFSP[nr] < 0)
    {
      # Data set 5
      cat(paste(hob$ITT, '\n', sep=' '), file=file, append=TRUE)
      
      # Data set 6
      for(time in 1:abs(hob$IREFSP[nr]))
      {
        cat(paste(hob$OBSNAM[[nr]][time], IREFSP[[nr]][time], hob$TOFFSET[[nr]][time],
                  hob$HOBS[[nr]][time], hob$STATh[[nr]][time], hob$STATdd[[nr]][time],
                  hob$STATFLAG[[nr]][time], hob$PLOTSYMBOL[[nr]][time], '\n', sep=' '), file=file, append=TRUE)
      }
    }       
  }  
}  