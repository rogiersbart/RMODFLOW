#' Read a MODFLOW head observations file
#' 
#' \code{read_hob} reads in a MODFLOW head observations file and returns it as an \code{\link{RMODFLOW}} hob object.
#' 
#' @param file Filename; typically "*.hob"
#' @return Object of class hob
#' @export
read_hob <- function(file)
{
  hob.lines <- scan(file, what=character(), sep='\n')
  hob <- NULL
  
  # Data set 0
    comments <- get_comments_from_lines(hob.lines)
    hob.lines <- remove_comments_from_lines(hob.lines)
  
  # Data set 1
    line.split <- split_line_words(hob.lines[1]); hob.lines <- hob.lines[-1]
    hob$NH <- as.numeric(line.split[1])
    hob$MOBS <- as.numeric(line.split[2])
    hob$MAXM <- as.numeric(line.split[3])
    hob$IUHOBSV <- as.numeric(line.split[4])
    hob$HOBDRY <- as.numeric(line.split[5])
    hob$NOPRINT <- F
    if(length(line.split) > 5) if(line.split[6]=='NOPRINT') hob$NOPRINT <- TRUE
  
  # Data set 2
    line.split <- split_line_numbers(hob.lines[1]); hob.lines <- hob.lines[-1]
    hob$TOMULTH <- line.split[1]
    hob$EVH <- line.split[2]
  
  # Data set 3 - 6
    hob$OBSNAM <- NULL; hob$LAYER <- NULL; hob$ROW <- NULL; hob$COLUMN <- NULL; hob$IREFSP <- NULL; hob$TOFFSET <- NULL
    hob$ROFF <- NULL; hob$COFF <- NULL; hob$HOBS <- NULL; hob$STATISTIC <- NULL; hob$STATFLAG <- NULL; hob$PLOTSYMBOL <- NULL
    hob$STATh <- NULL; hob$STATdd <- NULL; IREFSP <- NULL
    hob$MLAY <- NULL; hob$PR <- NULL
    for(nr in 1:hob$NH)
    {
      line.split <- split_line_words(hob.lines[1]); hob.lines <- hob.lines[-1]
      hob$LAYER[nr] <- as.numeric(line.split[2])
      hob$ROW[nr] <- as.numeric(line.split[3])
      hob$COLUMN[nr] <- as.numeric(line.split[4])
      hob$IREFSP[nr] <- as.numeric(line.split[5])
      hob$ROFF[nr] <- as.numeric(line.split[7])
      hob$COFF[nr] <- as.numeric(line.split[8])
      if(hob$IREFSP[nr] >= 0)
      {
        hob$OBSNAM[nr] <- line.split[1]
        hob$TOFFSET[nr] <- as.numeric(line.split[6])
        hob$HOBS[nr] <- as.numeric(line.split[9])
        hob$STATISTIC[nr] <- as.numeric(line.split[10])
        hob$STATFLAG[nr] <- as.numeric(line.split[11])
        hob$PLOTSYMBOL[nr] <- as.numeric(line.split[12]) 
      }
      if(hob$LAYER[nr] < 0)
      {
        line.split <- split_line_numbers(hob.lines[1]); hob.lines <- hob.lines[-1]
        for(layerNr in 1:abs(hob$LAYER[nr]))
        {
          hob$MLAY[[nr]][layerNr] <- line.split[(2*layerNr)-1]
          hob$PR[[nr]][layerNr] <- line.split[2*layerNr]
        }
      }
      if(hob$IREFSP[nr] < 0)
      {
        # Data set 5
        line.split <- split_line_numbers(hob.lines[1]); hob.lines <- hob.lines[-1]
        hob$ITT <- line.split[1]    
        # Data set 6
        line.split <- split_line_numbers(hob.lines[1]); hob.lines <- hob.lines[-1]
        for(time in 1:abs(hob$IREFSP))
        {
          hob$OBSNAM[[nr]][time] <- line.split[1]
          IREFSP[[nr]][time] <- line.split[2]
          hob$TOFFSET[[nr]][time] <- as.numeric(line.split[3])
          hob$HOBS[[nr]][time] <- as.numeric(line.split[4])
          hob$STATh[[nr]][time] <- as.numeric(line.split[5])
          hob$STATdd[[nr]][time] <- as.numeric(line.split[6])
          hob$STATFLAG[[nr]][time] <- as.numeric(line.split[7])
          hob$PLOTSYMBOL[[nr]][time] <- as.numeric(line.split[8]) 
        }
      }       
    }
    if(length(IREFSP) != 0) hob$IREFSP <- IREFSP
  
  comment(hob) <- comments
  class(hob) <- c('hob','modflow_package')
  return(hob)
}