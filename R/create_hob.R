#' Create an \code{RMODFLOW} hob object
#' 
#' \code{create_hob} creates an RMODFLOW hob object
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
create_hob <- function(locations,
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
    # hob$EVH # MODFLOW-2000
  
  # data set 3-6
  
    hob$obsnam <- hob$toffset <- hob$hobs <- hob$mlay <- hob$pr <- hob$irefsp <- list()
    locations_top <- cbind(locations[,c('x','y','top')],convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$top, dis = dis, prj = prj)[,c('i','j','k','roff','coff')])
    locations_bottom <- cbind(locations[,c('x','y','bottom')],convert_xyz_to_grid(x = locations$x, y = locations$y, z = locations$bottom, dis = dis, prj = prj)[,c('i','j','k','roff','coff','loff')])
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
