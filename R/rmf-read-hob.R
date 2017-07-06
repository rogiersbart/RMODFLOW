#' Read a MODFLOW head observations file
#' 
#' \code{read_hob} reads in a MODFLOW head observations file and returns it as an \code{\link{RMODFLOW}} hob object.
#' 
#' @param file filename; typically '*.hob'
#' @return object of class hob
#' @importFrom readr read_lines
#' @export
rmf_read_hob <- function(file = {cat('Please select hob file ...\n'); file.choose()}) {
  
  hob_lines <- readr::read_lines(file)
  hob <- list()
  
  # data set 0
    data_set_0 <- RMODFLOW:::rmfi_parse_comments(hob_lines)
    comment(hob) <- data_set_0$comments
    hob_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    dat <- rmfi_parse_variables(hob_lines)
    line.split <- dat$variables
    hob_lines <- dat$remaining_lines
    hob$nh <- as.numeric(line.split[1])
    hob$mobs <- as.numeric(line.split[2])
    hob$maxm <- as.numeric(line.split[3])
    hob$iuhobsv <- as.numeric(line.split[4])
    hob$hobdry <- as.numeric(line.split[5])
    hob$noprint <- F
    if(length(line.split) > 5) if(line.split[6]=='NOPRINT') hob$noprint <- TRUE
  
  # data set 2
    data_set_2 <- rmfi_parse_variables(hob_lines)
    hob$tomulth <- as.numeric(data_set_1$variables[1])
    hob$evh <- as.numeric(data_set_1$variables[2])
    hob_lines <- data_set_1$remaining_lines
    rm(data_set_2)
    
  # data set 3 - 6
    hob$obsnam <- hob$obsloc <- hob$layer <- hob$row <- hob$column <- hob$irefsp <- hob$toffset <- hob$roff <- hob$coff <- hob$hobs <- hob$statistic <- hob$statflag <- hob$plotsymbol <- hob$stath <- hob$statdd <- rep(NA, hob$nh)
    hob$mlay <- hob$pr <- list()
    obsnam <- obsloc <- layer <- row <- column <- irefsp <- toffset <- roff <- coff <- hobs <- statistic <- statflag <- plotsymbol <- stath <- statdd <- NA
    mlay <- pr <- NA
    nr <- 1
    while(nr <= hob$nh) {
      
      dat <- rmfi_parse_variables(hob_lines)
      line.split <- dat$variables
      hob_lines <- dat$remaining_lines
      obsnam <- line.split[1]
      obsloc <- obsnam
      layer <- as.numeric(line.split[2])
      row <- as.numeric(line.split[3])
      column <- as.numeric(line.split[4])
      irefsp <- as.numeric(line.split[5])
      toffset <- as.numeric(line.split[6])
      roff <- as.numeric(line.split[7])
      coff <- as.numeric(line.split[8])
      hobs <- as.numeric(line.split[9])
      statistic <- as.numeric(line.split[10])
      statflag <- as.numeric(line.split[11])
      plotsymbol <- as.numeric(line.split[12])
      if(irefsp >= 0) {
        hob$obsnam[nr] <- obsnam
        hob$obsloc[nr] <- obsloc
        hob$toffset[nr] <- toffset
        hob$hobs[nr] <- hobs
        hob$statistic[nr] <- statistic
        hob$statflag[nr] <- statflag
        hob$plotsymbol[nr] <- plotsymbol
        hob$layer[nr] <- layer
        hob$row[nr] <- row
        hob$column[nr] <- column
        hob$irefsp[nr] <- irefsp
        hob$roff[nr] <- roff
        hob$coff[nr] <- coff
      }
      if(layer < 0) {
        dat <- rmfi_parse_variables(hob_lines)
        line.split <- dat$variables
        hob_lines <- dat$remaining_lines
        for(layerNr in 1:abs(hob$layer[nr])) {
          mlay[layerNr] <- line.split[(2*layerNr)-1]
          pr[layerNr] <- line.split[2*layerNr]
        }
        hob$mlay[[nr]] <- mlay
        hob$pr[[nr]] <- pr
      }
      if(irefsp < 0) {
        # data set 5
        dat <- rmfi_parse_variables(hob_lines)
        line.split <- dat$variables
        hob_lines <- dat$remaining_lines
        hob$itt <- line.split[1]    
        # data set 6
        for(ntime in 1:abs(irefsp)) {
          dat <- rmfi_parse_variables(hob_lines)
          line.split <- dat$variables
          hob_lines <- dat$remaining_lines
          hob$obsnam[nr] <- line.split[1]
          hob$obsloc[nr] <- obsloc
          hob$irefsp[nr] <- as.numeric(line.split[2])
          hob$toffset[nr] <- as.numeric(line.split[3])
          hob$hobs[nr] <- as.numeric(line.split[4])
          hob$stath[nr] <- as.numeric(line.split[5])
          hob$statdd[nr] <- as.numeric(line.split[6])
          hob$statflag[nr] <- as.numeric(line.split[7])
          hob$plotsymbol[nr] <- as.numeric(line.split[8]) 
          if(layer < 0) {
            hob$mlay[[nr]] <- mlay
            hob$pr[[nr]] <- pr
          }
          hob$statistic[nr] <- statistic
          hob$layer[nr] <- layer
          hob$row[nr] <- row
          hob$column[nr] <- column
          hob$roff[nr] <- roff
          hob$coff[nr] <- coff
          if(ntime < abs(irefsp)) nr <- nr + 1
        }
      }   
      
      nr <- nr + 1
    }
    # if(length(irefsp) != 0) hob$irefsp <- irefsp
  
  class(hob) <- c('hob','rmf_package')
  return(hob)
}

#' @describeIn rmf_read_hob Deprecated function name
read_hob <- function(...) {
  .Deprecated(new = "rmf_read_hob", old = "read_hob")
  rmf_read_hob(...)
}

#' @describeIn rmf_read_hob Compatible with default ModelMuse file extensions
rmf_read_ob_hob <- function(...) {
  rmf_read_hpr(...)
}
