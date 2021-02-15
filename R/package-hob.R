#' Create an \code{RMODFLOW} hob object
#' 
#' \code{rmf_create_hob} creates an RMODFLOW hob object
#' 
#' @param locations data.frame or \code{sf POINT} object with the name, x and y coordinates (if data.frame), and top and bottom of the monitoring well filter
#' @param time_series data.frame with the name of the monitoring well filter and the observation time and head
#' @param dis RMODFLOW dis object
#' @param hydraulic_conductivity 3d array with hydraulic conductivity values, for calculating the weights for a multilayer observations
#' @param prj RMODFLOW prj object; provide if coordinates in \code{locations} are real world coordinates
#' @param iuhobsv file unit (> 0) for saving observation data in a file; the file unit should be included as DATA type in the name file; defaults to 665
#' @param hobdry value of the simulated equivalent for a dry cell used in the observation output file
#' @param noprint logical; if TRUE, the input and output are not printed in the listing file
#' @param tomult time offset multiplier, to convert the time unit in \code{time_series} to the MODFLOW time unit
#' @param itt integer flag, either a single value or a value for every observation well as defined in \code{locations}. Identifies whether hydraulic heads (1; default) or changes in hydraulic heads (2) are used as observations. See details.
#' @param unique_obsnam logical; should an ID number be added to obsnam for filters with observations at different times? Defaults to FALSE.
#' @details Regardless of the value of \code{itt}, observed heads should be specified in \code{time_series}. MODFLOW will calculate the changes in hydraulic head internally.
#' The \code{locations} data.frame must have columns named \code{name, x, y, top, bottom}.
#' The \code{time_series} data.frame must have columns named \code{name, time, head}.
#' @return Object of class hob
#' @export
#' @seealso \code{\link{rmf_read_hob}}, \code{\link{rmf_write_hob}} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hob.htm}
rmf_create_hob <- function(locations,
                           time_series,
                           dis,
                           hydraulic_conductivity = array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                           prj = rmf_get_prj(dis),
                           iuhobsv = 665,
                           hobdry = -888,
                           noprint = FALSE,
                           tomulth = 1,
                           itt = 1,
                           unique_obsnam = FALSE) {
  
  # locations is sf object
  if(inherits(locations, 'sf')) {
    geom <- unique(sf::st_geometry_type(locations))
    if(length(geom) > 1 || geom != 'POINT') stop('A locations sf object should have geometry type POINT for all features', call. = FALSE)
    if(!is.na(sf::st_crs(locations))) {
      if(is.null(prj) || sf::st_crs(locations) != sf::st_crs(prj$crs)) stop('When locations is an sf object with a crs, prj should be provided with the same crs', call. = FALSE)
    } else {
      if(!is.null(prj) && !is.na(sf::st_crs(prj$crs))) stop('prj has a crs defined whereas locations does not', call. = FALSE) 
    }
    coords <- setNames(as.data.frame(sf::st_coordinates(locations)), c('x', 'y'))
    locations <- cbind(sf::st_set_geometry(locations, NULL), coords)
  }
  
  # error checks in locations & time_series
  if(!all(c('x', 'y', 'name', 'top', 'bottom') %in% colnames(locations))) stop('locations object must have columns x, y, name, top and bottom', call. = FALSE)
  if(!all(c('time', 'name', 'head') %in% colnames(time_series))) stop('time_series data.frame must have columns time, name and head', call. = FALSE)
  
  # set locations tops and bottoms
  locations_top <- cbind(locations[,c('x','y','top','name')], suppressWarnings(rmf_convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$top, dis = dis, prj = prj, output = c('ijk', 'off'))))
  locations_bottom <- cbind(locations[,c('x','y','bottom','name')], suppressWarnings(rmf_convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$bottom, dis = dis, prj = prj, output = c('ijk', 'off'))))
  if(any(is.na(locations_top)) || any(is.na(locations_bottom))) {
    na_id <- unique(c(which(is.na(locations_top), arr.ind = TRUE)[, 1], which(is.na(locations_bottom), arr.ind = TRUE)[, 1]))
    locations_top <- locations_top[-na_id, ]
    locations_bottom <- locations_bottom[-na_id, ]
    na_names <- locations$name[na_id]
    locations <- locations[-na_id, ]
    time_series <- subset(time_series, !(name %in% na_names))
    warning('Removing observations outside the grid domain: ', paste(na_names, collapse = ' '), call. = FALSE)
    if(nrow(locations_top) == 0 || nrow(locations_bottom) == 0 || nrow(locations) == 0) stop('No observations inside grid domain', call. = FALSE)
  }
  if(!setequal(locations$name, time_series$name)) {
    na_names <- setdiff(union(locations$name, time_series$name), intersect(locations$name, time_series$name))
    warning('Following observations are not present in both locations and time series data.frames and are therefore removed: ', 
            paste(na_names, collapse= ' '), call. = FALSE)
    locations <- subset(locations, !(name %in% na_names))
    time_series <- subset(time_series, !(name %in% na_names))
    locations_top <- subset(locations_top, !(name %in% na_names))
    locations_bottom <- subset(locations_bottom, !(name %in% na_names))
    if(nrow(time_series) == 0) stop('time series is empty', call. = FALSE)
    if(nrow(locations) == 0) stop('locations is empty', call. = FALSE)
  }

  locations_top$k <- ifelse(locations_top$loff == 0.5, locations_top$k + 1, locations_top$k)
  locations_bottom$k <- ifelse(locations_bottom$loff == -0.5, locations_bottom$k - 1, locations_bottom$k)  
  
  # hob
  hob <- list()
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
  df <- list()
  df$obsnam <- df$layer <- df$row <- df$column <- df$nrefsp <- df$irefsp <- df$toffset <- df$roff <- df$coff <- df$hobs <- df$itt <- rep(NA, hob$nh)
  df$mlay <- df$pr <- list()
  
  for(i in 1:nrow(locations)) {
    # locations
    ts_id <- which(time_series$name == locations$name[i])
    df$row[ts_id] <- locations_top$i[i]
    df$column[ts_id] <- locations_top$j[i]
    first_greater_than <- function(x, y) which(y >= x)[1]
    if(length(ts_id) > 1) df$irefsp[ts_id] <- -length(ts_id)
    if(length(ts_id) == 1) df$irefsp[ts_id] <- first_greater_than(time_series$time[ts_id], cumsum(dis$perlen))
    df$nrefsp[ts_id] <- length(ts_id)
    df$roff[ts_id] <- locations_top$roff[i]
    df$coff[ts_id] <- locations_top$coff[i]
    if(locations_top$k[i] == locations_bottom$k[i]) {
      df$layer[ts_id] <- locations_top$k[i]
      m_lay <- 1
    } else {
      m_lay <- - (locations_bottom$k[i] - locations_top$k[i] + 1)
      df$layer[ts_id] <- m_lay
    }
    
    # multiple layers
    # TODO set proper pr
    df$mlay[ts_id] <- df$pr[ts_id] <- NA
    if(m_lay < 0) {
      df$mlay[ts_id] <- lapply(df$mlay[ts_id], function(x) x <- locations_top$k[i]:locations_bottom$k[i]) 
      length_in_cell <- rep(NA, abs(df$layer[ts_id[1]]))
      for(j in 1:abs(df$layer[ts_id[1]])) {
        m_val <- df$mlay[[ts_id[1]]][j]
        layer_tops <- rmf_cell_coordinates(dis, include_faces = TRUE)$upper
        length_in_cell[j] <- min(locations$top[i], layer_tops[locations_top$i[i],locations_top$j[i], m_val]) - max(locations$bottom[i], dis$botm[locations_top$i[i],locations_top$j[i], m_val])
      }
      df$pr[ts_id] <- lapply(df$pr[ts_id], function(x) x <- hydraulic_conductivity[locations_top$i[i],locations_top$j[i],df$mlay[[ts_id[1]]]] * length_in_cell) 
      df$pr[ts_id] <- lapply(df$pr[ts_id], function(x) x <- df$pr[[ts_id[1]]]/sum(df$pr[[ts_id[1]]]))
    } 
    
    # time series
    if(df$irefsp[ts_id[1]] > 0) {
      df$toffset[ts_id] <- time_series$time[ts_id] - c(0,cumsum(dis$perlen)[1:(dis$nper-1)])[df$irefsp[ts_id]]
      df$hobs[ts_id] <- time_series$head[ts_id]
      df$obsnam[ts_id] <- locations$name[i]
      df$itt[ts_id] <- 1
    } else {
      df$itt[ts_id] <- ifelse(length(itt)==1,itt,itt[i])
      if(unique_obsnam) {
        df$obsnam[ts_id] <- paste(locations$name[i], c(1:abs(df$nrefsp[ts_id[1]])), sep = '_')
      } else {
        df$obsnam[ts_id] <- rep(as.character(locations$name[i]), abs(df$nrefsp[ts_id[1]]))
      }    
      df$irefsp[ts_id] <- apply(array(time_series$time[ts_id],dim=c(length(time_series$time[ts_id]))),1,first_greater_than,cumsum(dis$perlen))
      df$toffset[ts_id] <- time_series$time[ts_id] - c(0,cumsum(dis$perlen)[1:(dis$nper-1)])[df$irefsp[ts_id]]
      df$hobs[ts_id] <- time_series$head[ts_id]
    }
  }
  hob$mobs <- 0
  if(any(df$layer < 0)) hob$mobs <- length(which(df$layer < 0))
  hob$maxm <- abs(min(df$layer[which(df$layer <= 1)]))
  
  if(hob$mobs == 0) {
    df$mlay <- df$layer
    df$pr <- rep(1, hob$nh)
  } else {
    s_lay <- which(df$layer > 0)
    df$mlay[s_lay] <- df$layer[s_lay]
    df$pr[s_lay] <- 1
  }
  
  # Data
  hob$data <- data.frame(obsnam = df$obsnam, layer = I(df$mlay), pr = I(df$pr), row = df$row, column = df$column, nrefsp = df$nrefsp, irefsp = df$irefsp,
                         itt = df$itt, toffset = df$toffset, roff = df$roff, coff = df$coff, hobs = df$hobs, stringsAsFactors = FALSE)
  class(hob) <- c('hob','rmf_package')
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
#' @seealso \code{\link{rmf_create_hob}}, \code{\link{rmf_write_hob}} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hob.htm}
rmf_read_hob <- function(file = {cat('Please select hob file ...\n'); file.choose()}, ...) {
  
  hob_lines <- readr::read_lines(file)
  hob <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(hob_lines)
  comment(hob) <- data_set_0$comments
  hob_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(hob_lines)
  hob$nh <- as.numeric(data_set_1$variables[1])
  hob$mobs <- as.numeric(data_set_1$variables[2])
  hob$maxm <- as.numeric(data_set_1$variables[3])
  hob$iuhobsv <- ifelse(is.na(as.numeric(data_set_1$variables[4])), 0, as.numeric(data_set_1$variables[4]))
  hob$hobdry <- ifelse(is.na(as.numeric(data_set_1$variables[5])), -888, as.numeric(data_set_1$variables[5]))
  hob$noprint <- FALSE
  if(length(data_set_1$variables) > 5) if(toupper(data_set_1$variables[6])=='NOPRINT') hob$noprint <- TRUE
  hob_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 <- rmfi_parse_variables(hob_lines, n = 1, ...)
  hob$tomulth <- as.numeric(data_set_2$variables[1])
  # hob$evh <- as.numeric(dat$variables[2]) # MODFLOW-2000
  hob_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  # data set 3 - 6
  df <- list()
  df$obsnam <- df$obsloc <- df$layer <- df$row <- df$column <- df$nrefsp <- df$irefsp <- df$toffset <- df$roff <- df$coff <- df$hobs <- df$statistic <- df$statflag <- df$plotsymbol <- df$stath <- df$statdd <- df$itt <- rep(NA, hob$nh)
  df$mlay <- df$pr <- list()
  obsnam <- obsloc <- layer <- row <- column <- irefsp <- toffset <- roff <- coff <- hobs <- statistic <- statflag <- plotsymbol <- stath <- statdd <- NA
  mlay <- pr <- NA
  nr <- 1
  
  while(nr <= hob$nh) {
    
    # data set 3
    data_set_3 <- rmfi_parse_variables(hob_lines, character = TRUE)
    obsnam <- as.character(data_set_3$variables[1])
    obsloc <- obsnam
    layer <- as.numeric(data_set_3$variables[2])
    row <- as.numeric(data_set_3$variables[3])
    column <- as.numeric(data_set_3$variables[4])
    irefsp <- as.numeric(data_set_3$variables[5])
    toffset <- as.numeric(data_set_3$variables[6])
    roff <- as.numeric(data_set_3$variables[7])
    coff <- as.numeric(data_set_3$variables[8])
    hobs <- as.numeric(data_set_3$variables[9])
    statistic <- as.numeric(data_set_3$variables[10])
    statflag <- as.numeric(data_set_3$variables[11])
    plotsymbol <- as.numeric(data_set_3$variables[12])
    if(irefsp >= 0) {
      df$obsnam[nr] <- obsnam
      df$obsloc[nr] <- obsloc
      df$toffset[nr] <- toffset
      df$hobs[nr] <- hobs
      df$statistic[nr] <- statistic
      df$statflag[nr] <- statflag
      df$plotsymbol[nr] <- plotsymbol
      df$layer[nr] <- layer
      df$row[nr] <- row
      df$column[nr] <- column
      df$nrefsp[nr] <- 1
      df$irefsp[nr] <- irefsp
      df$roff[nr] <- roff
      df$coff[nr] <- coff
    }
    hob_lines <- data_set_3$remaining_lines
    rm(data_set_3)
    
    # data set 4
    if(layer < 0) {
      data_set_4 <- rmfi_parse_variables(hob_lines)
      for(layerNr in 1:abs(layer)) {
        mlay[layerNr] <- data_set_4$variables[(2*layerNr)-1]
        pr[layerNr] <- data_set_4$variables[2*layerNr]
      }
      df$mlay[[nr]] <- mlay[1:abs(layer)]
      df$pr[[nr]] <- pr[1:abs(layer)]
      hob_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    df$itt[nr] <- 1
    if(irefsp < 0) {
      
      # data set 5
      data_set_5 <- rmfi_parse_variables(hob_lines)
      itt <- as.numeric(data_set_5$variables[1]) 
      hob_lines <- data_set_5$remaining_lines
      rm(data_set_5)
      
      # data set 6
      for(ntime in 1:abs(irefsp)) {
        df$nrefsp[nr] <- abs(irefsp)
        df$itt[nr] <- itt
        data_set_6 <- rmfi_parse_variables(hob_lines, character = TRUE)
        df$obsnam[nr] <- as.character(data_set_6$variables[1])
        df$obsloc[nr] <- obsloc
        df$irefsp[nr] <- as.numeric(data_set_6$variables[2])
        df$toffset[nr] <- as.numeric(data_set_6$variables[3])
        df$hobs[nr] <- as.numeric(data_set_6$variables[4])
        df$stath[nr] <- as.numeric(data_set_6$variables[5])
        df$statdd[nr] <- as.numeric(data_set_6$variables[6])
        df$statflag[nr] <- as.numeric(data_set_6$variables[7])
        df$plotsymbol[nr] <- as.numeric(data_set_6$variables[8]) 
        if(layer < 0) {
          df$mlay[[nr]] <- mlay[1:abs(layer)]
          df$pr[[nr]] <- pr[1:abs(layer)]
        }
        df$statistic[nr] <- statistic
        df$layer[nr] <- layer
        df$row[nr] <- row
        df$column[nr] <- column
        df$roff[nr] <- roff
        df$coff[nr] <- coff
        if(ntime < abs(irefsp)) nr <- nr + 1
        hob_lines <- data_set_6$remaining_lines
        rm(data_set_6)
      }
    }   
    
    nr <- nr + 1
  }
  # if(length(irefsp) != 0) df$irefsp <- irefsp
  
  if(hob$mobs == 0) {
    df$mlay <- df$layer
    df$pr <- rep(1, hob$nh)
  } else {
    s_lay <- which(df$layer > 0)
    df$mlay[s_lay] <- df$layer[s_lay]
    df$pr[s_lay] <- 1
  }
  
  # Data
  hob$data <- data.frame(obsnam = df$obsnam, layer = I(df$mlay), pr = I(df$pr), row = df$row, column = df$column, nrefsp = df$nrefsp, irefsp = df$irefsp,
                         itt = df$itt, toffset = df$toffset, roff = df$roff, coff = df$coff, hobs = df$hobs, stringsAsFactors = FALSE)
  
  class(hob) <- c('hob','rmf_package')
  return(hob)
}

#' @describeIn rmf_read_hob Compatible with default ModelMuse file extensions
#' @export
rmf_read_ob_hob <- function(...) {
  rmf_read_hob(...)
}

#' Write a MODFLOW head observations file
#' 
#' @param hob a \code{\link{RMODFLOW}} hob object
#' @param file filename to write to; typically '*.hob'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_create_hob}}, \code{\link{rmf_read_hob}} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hob.htm}
rmf_write_hob <- function(hob,
                          file = {cat('Please select hob file to overwrite or provide new filename ...\n'); file.choose()}, ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Head-Observation Package created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(hob)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(as.integer(hob$nh), as.integer(hob$mobs), as.integer(hob$maxm), as.integer(hob$iuhobsv), hob$hobdry, ifelse(hob$noprint,'NOPRINT',''), file=file)
  
  # data set 2
  # rmfi_write_variables(hob$tomulth, ifelse(is.na(hob$evh) || is.null(hob$env),1,hob$evh), file=file) # MODFLOW-2000
  rmfi_write_variables(hob$tomulth, file=file, ...)
  
  # data set 3 - 6
  i <- 1
  while(i <= hob$nh) {
    # data set 3
    rmfi_write_variables(hob$data$obsnam[i], as.integer(ifelse(length(hob$data$layer[[i]]) > 1, -length(hob$data$layer[[i]]), hob$data$layer[[i]])), as.integer(hob$data$row[i]), as.integer(hob$data$column[i]),
                         as.integer(ifelse(hob$data$nrefsp[i] > 1, -hob$data$nrefsp[i], hob$data$irefsp[i])), hob$data$toffset[i], hob$data$roff[i], hob$data$coff[i], hob$data$hobs[i],
                         file=file)
    
    # data set 4
    if(length(hob$data$layer[[i]]) > 1) {
      rmfi_write_variables(paste(as.integer(hob$data$layer[[i]]), hob$data$pr[[i]], collapse = ' '), file = file)
    }
    
    # data set 5 - 6
    if(hob$data$nrefsp[i] > 1) {
      # data set 5
      rmfi_write_variables(hob$data$itt[i], file = file, integer = TRUE)
      
      for(j in 1:hob$data$nrefsp[i]) {
        # data set 6
        id <- i + j - 1
        rmfi_write_variables(hob$data$obsnam[id], as.integer(hob$data$irefsp[id]),  hob$data$toffset[id],  hob$data$hobs[id], file = file)
      }
      i <- i + hob$data$nrefsp[i]
    } else {
      i <- i + 1
    }
  }
}  
