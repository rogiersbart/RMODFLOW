#' Read a MODFLOW head observations file
#' 
#' \code{read_hob} reads in a MODFLOW head observations file and returns it as an \code{\link{RMODFLOW}} hob object.
#' 
#' @param file filename; typically '*.hob'
#' @return object of class hob
#' @importFrom readr read_lines
#' @export
read_hob <- function(file = {cat('Please select hob file ...\n'); file.choose()}) {
  
  hob_lines <- read_lines(file)
  hob <- NULL
  
  # data set 0
    data_set_0 <- read_modflow_comments(hob_lines)
    comment(hob) <- data_set_0$comments
    hob_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    line.split <- split_line_words(hob_lines[1]); hob_lines <- hob_lines[-1]
    hob$nh <- as.numeric(line.split[1])
    hob$mobs <- as.numeric(line.split[2])
    hob$maxm <- as.numeric(line.split[3])
    hob$iuhobsv <- as.numeric(line.split[4])
    hob$hobdry <- as.numeric(line.split[5])
    hob$noprint <- F
    if(length(line.split) > 5) if(line.split[6]=='NOPRINT') hob$noprint <- TRUE
  
  # data set 2
    line.split <- split_line_numbers(hob_lines[1]); hob_lines <- hob_lines[-1]
    hob$tomulth <- line.split[1]
    hob$evh <- line.split[2]
  
  # data set 3 - 6
    hob$obsnam <- NULL; hob$layer <- NULL; hob$row <- NULL; hob$column <- NULL; hob$irefsp <- NULL; hob$toffset <- NULL
    hob$roff <- NULL; hob$coff <- NULL; hob$hobs <- NULL; hob$statistic <- NULL; hob$statflag <- NULL; hob$plotsymbol <- NULL
    hob$stath <- NULL; hob$statdd <- NULL; irefsp <- NULL
    hob$mlay <- NULL; hob$pr <- NULL
    for(nr in 1:hob$nh) {
      line.split <- split_line_words(hob_lines[1]); hob_lines <- hob_lines[-1]
      hob$layer[nr] <- as.numeric(line.split[2])
      hob$row[nr] <- as.numeric(line.split[3])
      hob$column[nr] <- as.numeric(line.split[4])
      hob$irefsp[nr] <- as.numeric(line.split[5])
      hob$roff[nr] <- as.numeric(line.split[7])
      hob$coff[nr] <- as.numeric(line.split[8])
      if(hob$irefsp[nr] >= 0) {
        hob$obsnam[nr] <- line.split[1]
        hob$toffset[nr] <- as.numeric(line.split[6])
        hob$hobs[nr] <- as.numeric(line.split[9])
        hob$statistic[nr] <- as.numeric(line.split[10])
        hob$statflag[nr] <- as.numeric(line.split[11])
        hob$plotsymbol[nr] <- as.numeric(line.split[12]) 
      }
      if(hob$layer[nr] < 0) {
        line.split <- split_line_numbers(hob_lines[1]); hob_lines <- hob_lines[-1]
        for(layerNr in 1:abs(hob$layer[nr])) {
          hob$mlay[[nr]][layerNr] <- line.split[(2*layerNr)-1]
          hob$pr[[nr]][layerNr] <- line.split[2*layerNr]
        }
      }
      if(hob$irefsp[nr] < 0) {
        # data set 5
        line.split <- split_line_numbers(hob_lines[1]); hob_lines <- hob_lines[-1]
        hob$itt <- line.split[1]    
        # data set 6
        line.split <- split_line_numbers(hob_lines[1]); hob_lines <- hob_lines[-1]
        for(time in 1:abs(hob$irefsp)) {
          hob$obsnam[[nr]][time] <- line.split[1]
          irefsp[[nr]][time] <- line.split[2]
          hob$toffset[[nr]][time] <- as.numeric(line.split[3])
          hob$hobs[[nr]][time] <- as.numeric(line.split[4])
          hob$stath[[nr]][time] <- as.numeric(line.split[5])
          hob$statdd[[nr]][time] <- as.numeric(line.split[6])
          hob$statflag[[nr]][time] <- as.numeric(line.split[7])
          hob$plotsymbol[[nr]][time] <- as.numeric(line.split[8]) 
        }
      }       
    }
    if(length(irefsp) != 0) hob$irefsp <- irefsp
  
  class(hob) <- c('hob','modflow_package')
  return(hob)
}
