#' Create an \code{RMODFLOW} hob object
#' 
#' \code{rmf_create_hob} creates an RMODFLOW hob object
#' 
#' @param locations data.frame with the name, x and y coordinates, and top and bottom of the monitoring well filter
#' @param time_series data.frame with the name of the monitoring well filter and the observation time and head
#' @param dis RMODFLOW dis object
#' @param hydraulic_conductivity 3d array with hydraulic conductivity values, for calculating the weights for a multilayer observation
#' @param prj RMODFLOW prj object; provide if coordinates in locations are real world coordinates
#' @param iuhobsv file unit for saving observation data in a file; the file unit should be included as DATA type in the name file; defaults to 0 (\emph{i.e.} no observation output file will be written)
#' @param hobdry value of the simulated equivalent for a dry cell used in the observation output file
#' @param noprint logical; if TRUE, the input and output are not printed in the listing file
#' @param TOMULT time offset multiplier, to convert the time unit in time_series to the MODFLOW time unit
#' @param unique_obsnam logical; should an ID number be added to obsnam for filters with observations at different times? Defaults to FALSE.
#' @return RMODFLOW hob object
#' @export
#' @seealso \code{\link{read.hob}} and \code{\link{write.hob}}
rmf_create_hob <- function(locations,
                           time_series,
                           dis,
                           hydraulic_conductivity = array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           iuhobsv = 0,
                           hobdry = -888,
                           noprint = FALSE,
                           tomulth = 1,
                           itt = 1,
                           unique_obsnam = FALSE,
                           prj = NULL) {
      
  hob <- NULL
  
  # data set 0
    # comments should be provided with ?comment
  
  # data set 1
    hob$nh <- nrow(time_series)
    hob$mobs <- NA # set later
    hob$maxm <- NA # set later
    hob$iuhobsv <- iuhobsv
    hob$hobdry <- hobdry
    hob$noprint <- noprint
  
  # data set 2
    hob$tomulth <- tomulth
    # hob$evh # MODFLOW-2000
  
  # data set 3-6
  
    hob$obsnam <- hob$toffset <- hob$hobs <- hob$mlay <- hob$pr <- hob$irefsp <- list()
    locations_top <- cbind(locations[,c('x','y','top')], convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$top, dis = dis, prj = prj, output = 'ijk'), convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$top, dis = dis, prj = prj, output = 'off')[, c('roff', 'coff')])
    locations_bottom <- cbind(locations[,c('x','y','bottom')], convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$bottom, dis = dis, prj = prj, output = 'ijk'), convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$bottom, dis = dis, prj = prj, output = 'off'))
    if(locations_bottom$loff == -0.5) locations_bottom$k <- locations_bottom$k - 1
  
    for(i in 1:nrow(locations)) {
      
      hob$row[i] <- locations_top$i[i]
      hob$column[i] <- locations_top$j[i]
      first_greater_than <- function(x, y) which(y > x)[1]
      if(sum(time_series$name == locations$name[i]) > 1) hob$irefsp[[i]] <- -sum(time_series$name == locations$name[i])
      if(sum(time_series$name == locations$name[i]) == 1) hob$irefsp[[i]] <- first_greater_than(time_series$time[which(time_series$name == locations$name[i])],cumsum(dis$perlen))
      hob$roff[i] <- locations_top$roff[i]
      hob$coff[i] <- locations_top$coff[i]
      if(locations_top$k[i] == locations_bottom$k[i]) {
        hob$layer[i] <- locations_top$k[i]
      } else {
        hob$layer[i] <- - (locations_bottom$k[i] - locations_top$k[i] + 1)
      }
      hob$mlay[[i]] <- hob$pr[[i]] <- NA
      if(hob$layer[i] < 0) {
        hob$mlay[[i]] <- locations_top$k[i]:locations_bottom$k[i]
        length_in_cell <- rep(NA, abs(hob$layer[[i]]))
        for(j in 1:abs(hob$layer[[i]])) {
          layer_tops <- cell_coordinates(dis, include_faces = TRUE)$upper
          length_in_cell[j] <- min(locations$top[i], layer_tops[locations_top$i[i],locations_top$j[i],hob$mlay[[i]][j]]) - max(locations$bottom[i], dis$botm[locations_top$i[i],locations_top$j[i],hob$mlay[[i]][j]])
        }
        hob$pr[[i]] <- hydraulic_conductivity[locations_top$i[i],locations_top$j[i],hob$mlay[[i]]] * length_in_cell
        hob$pr[[i]] <- hob$pr[[i]]/sum(hob$pr[[i]])
      }
      if(hob$irefsp[[i]] > 0) {
        hob$toffset[[i]] <- time_series$time[which(time_series$name == locations$name[i])] - c(0,cumsum(dis$perlen)[1:(dis$nper-1)])[hob$irefsp[[i]]]
        hob$hobs[[i]] <- time_series$head[which(time_series$name == locations$name[i])]
        hob$obsnam[[i]] <- locations$name[i]
      } else {
        hob$itt[i] <- ifelse(length(itt)==1,itt,itt[i])
        if(unique_obsnam) {
          hob$obsnam[[i]] <- paste(locations$name[i], c(1:abs(hob$irefsp[[i]])), sep = '_')
        } else {
          hob$obsnam[[i]] <- rep(locations$name[i], abs(hob$irefsp[[i]]))
        }        
        hob$irefsp[[i]] <- apply(array(time_series$time[which(time_series$name == locations$name[i])],dim=c(length(time_series$time[which(time_series$name == locations$name[i])]))),1,first_greater_than,cumsum(dis$perlen))
        hob$toffset[[i]] <- time_series$time[which(time_series$name == locations$name[i])] - c(0,cumsum(dis$perlen)[1:(dis$nper-1)])[hob$irefsp[[i]]]
        hob$hobs[[i]] <- time_series$head[which(time_series$name == locations$name[i])]
      }
  }
  hob$mobs <- 0
  for(i in which(hob$layer < 0)) hob$mobs <- hob$mobs + length(hob$irefsp[[i]])
  hob$maxm <- abs(min(hob$layer[which(hob$layer <= 1)]))
  return(hob)
}

#' Read a MODFLOW head observations file
#' 
#' \code{read_hob} reads in a MODFLOW head observations file and returns it as an \code{\link{RMODFLOW}} hob object.
#' 
#' @param file filename; typically '*.hob'
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return object of class hob
#' @export
rmf_read_hob <- function(file = {cat('Please select hob file ...\n'); file.choose()}, ...) {
  
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
  hob$iuhobsv <- ifelse(is.na(as.numeric(line.split[4])), 0, as.numeric(line.split[4]))
  hob$hobdry <- ifelse(is.na(as.numeric(line.split[5])), -888, as.numeric(line.split[5]))
  hob$noprint <- F
  if(length(line.split) > 5) if(toupper(line.split[6])=='NOPRINT') hob$noprint <- TRUE
  
  # data set 2
  dat <- rmfi_parse_variables(hob_lines, n = 1, ...)
  hob$tomulth <- as.numeric(dat$variables[1])
  # hob$evh <- as.numeric(dat$variables[2]) # MODFLOW-2000
  hob_lines <- dat$remaining_lines
  rm(dat)
  
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

#' @describeIn rmf_read_hob Compatible with default ModelMuse file extensions
#' @export
rmf_read_ob_hob <- function(...) {
  rmf_read_hpr(...)
}

#' Write a MODFLOW head observations file
#' 
#' @param hob an \code{\link{RMODFLOW}} hob object
#' @param file filename to write to; typically '*.hob'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' @return \code{NULL}
#' @export
rmf_write_hob <- function(hob,
                          file = {cat('Please select hob file to overwrite or provide new filename ...\n'); file.choose()}, ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Head-Observation Package created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(hob)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(hob$nh, hob$mobs, hob$maxm, hob$iuhobsv, hob$hobdry, ifelse(hob$noprint,'NOPRINT',''), file=file)
  
  # data set 2
  # rmfi_write_variables(hob$tomulth, ifelse(is.na(hob$evh) || is.null(hob$env),1,hob$evh), file=file) # MODFLOW-2000
  rmfi_write_variables(hob$tomulth, file=file, ...)
  
  # data set 3 - 6
  i <- 1
  while(i <= length(hob$layer)) {
    for (k in 1:length(hob$obsnam[[i]])){
      
      rmfi_write_variables(hob$obsnam[[i]][k], hob$layer[i], hob$row[i], hob$column[i], hob$irefsp[[i]][k], hob$toffset[[i]][k],hob$roff[i], hob$coff[i], hob$hobs[[i]][k],file=file) 
      
      if(hob$layer[i] < 0) {
        rmfi_write_variables(paste(hob$mlay[[i]],hob$pr[[i]],collapse=' '), file=file)
      }
      if(hob$irefsp[[i]][k] < 0) {
        
        # data set 5
        rmfi_write_variables(hob$itt[i], file=file)
        
        # data set 6
        for(j in 1:abs(hob$irefsp[[i]][k])) {
          k <- k + 1
          rmfi_write_variables(hob$obsnam[[i]][k], hob$irefsp[[i]][i], hob$toffset[[i]][k],hob$hobs[[i]][k], file=file)
        }
      }
    }
    i <- i + 1
  }
}  

#' Read a MODFLOW head predictions file
#' 
#' \code{rmf_read_hpr} reads in a MODFLOW head predictions file and returns it as an \code{\link{RMODFLOW}} hpr object.
#' 
#' @param file filename; typically '*.hpr'
#' @return object of class hpr
#' @export
rmf_read_hpr <- function(file = {cat('Please select hpr file ...\n'); file.choose()}) {
  # TODO use readr
  hpr <- read.table(file, header = TRUE, stringsAsFactors = FALSE)
  colnames(hpr) <- c('simulated', 'observed', 'name')
  hpr$residual <- hpr$simulated - hpr$observed
  class(hpr) <- c('hpr','data.frame')
  return(hpr)
}

#' @describeIn rmf_read_hpr Compatible with default ModelMuse file extension
#' @export
rmf_read_hob_out <- function(...) {
  rmf_read_hpr(...)
}
