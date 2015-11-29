#' Create an \code{RMODFLOW} hob object
#' 
#' \code{create_hob} creates an RMODFLOW hob object
#' 
#' @param locations data.frame with the name, x and y coordinates, and top and bottom of the monitoring well filter
#' @param time_series data.frame with the name of the monitoring well filter and the observation time and head
#' @param dis RMODFLOW dis object
#' @param hydraulic_conductivity 3d array with hydraulic conductivity values, for calculating the weights for a multilayer observation
#' @param prj RMODFLOW prj object; provide if coordinates in locations are real world coordinates
#' @param IUHOBSV file unit for saving observation data in a file; the file unit should be included as DATA type in the name file; defaults to 0 (\emph{i.e.} no observation output file will be written)
#' @param HOBDRY value of the simulated equivalent for a dry cell used in the observation output file
#' @param NOPRINT logical; if TRUE, the input and output are not printed in the listing file
#' @param TOMULT time offset multiplier, to convert the time unit in time_series to the MODFLOW time unit
#' @param unique_obsnam logical; should an ID number be added to OBSNAM for filters with observations at different times? Defaults to FALSE.
#' @return RMODFLOW hob object
#' @export
#' @seealso \code{\link{read.hob}} and \code{\link{write.hob}}
create_hob <- function(locations,
                       time_series,
                       dis,
                       hydraulic_conductivity = array(1, dim = c(dis$NROW, dis$NCOL, dis$NLAY)),
                       IUHOBSV = 0,
                       HOBDRY = -888,
                       NOPRINT = FALSE,
                       TOMULTH = 1,
                       unique_obsnam = FALSE,
                       prj = NULL) {
  
  hob <- NULL
  
  # Data set 0
    # comments should be provided with ?comment
  
  # Data set 1
    hob$NH <- nrow(time_series)
    hob$MOBS <- NA # set later
    hob$MAXM <- NA # set later
    hob$IUHOBSV <- IUHOBSV
    hob$HOBDRY <- HOBDRY
    hob$NOPRINT <- NOPRINT
  
  # Data set 2
    hob$TOMULTH <- TOMULTH
    # hob$EVH # MODFLOW-2000
  
  # Data set 3-6
  
    hob$OBSNAM <- hob$TOFFSET <- hob$HOBS <- hob$MLAY <- hob$PR <- hob$IREFSP <- list()
    locations_top <- cbind(locations[,c('x','y','top')],convert_real_to_dis(x = locations$x, y = locations$y, z = locations$top, dis = dis, prj = prj)[,c('i','j','k','roff','coff')])
    locations_bottom <- cbind(locations[,c('x','y','bottom')],convert_real_to_dis(x = locations$x, y = locations$y, z = locations$bottom, dis = dis, prj = prj)[,c('i','j','k','roff','coff')])
    for(i in 1:nrow(locations)) {
      
      hob$ROW[i] <- locations_top$i[i]
      hob$COLUMN[i] <- locations_top$j[i]
      first_greater_than <- function(x, y) which(y > x)[1]
      if(sum(time_series$name == locations$name[i]) > 1) hob$IREFSP[[i]] <- -sum(time_series$name == locations$name[i])
      if(sum(time_series$name == locations$name[i]) == 1) hob$IREFSP[[i]] <- first_greater_than(time_series$time[which(time_series$name == locations$name[i])],cumsum(dis$PERLEN))
      hob$ROFF[i] <- locations_top$roff[i]
      hob$COFF[i] <- locations_top$coff[i]
      if(locations_top$k[i] == locations_bottom$k[i]) {
        hob$LAYER[i] <- locations_top$k[i]
      } else {
        hob$LAYER[i] <- - (locations_bottom$k[i] - locations_top$k[i] + 1)
      }
      hob$MLAY[[i]] <- hob$PR[[i]] <- NA
      if(hob$LAYER[i] < 0) {
        hob$MLAY[[i]] <- locations_top$k[i]:locations_bottom$k[i]
        length_in_cell <- rep(NA, abs(hob$LAYER[[i]]))
        for(j in 1:abs(hob$LAYER[[i]])) {
          layer_tops <- cell_coordinates(dis, include_faces = TRUE)$upper
          length_in_cell[j] <- min(locations$top[i], layer_tops[locations_top$i[i],locations_top$j[i],hob$MLAY[[i]][j]]) - max(locations$bottom[i], dis$BOTM[locations_top$i[i],locations_top$j[i],hob$MLAY[[i]][j]])
        }
        hob$PR[[i]] <- hydraulic_conductivity[locations_top$i[i],locations_top$j[i],hob$MLAY[[i]]] * length_in_cell
        hob$PR[[i]] <- hob$PR[[i]]/sum(hob$PR[[i]])
      }
      if(hob$IREFSP[[i]] > 0) {
        hob$TOFFSET[[i]] <- time_series$time[which(time_series$name == locations$name[i])] - c(0,cumsum(dis$PERLEN)[1:(dis$NPER-1)])[hob$IREFSP[[i]]]
        hob$HOBS[[i]] <- time_series$head[which(time_series$name == locations$name[i])]
        hob$OBSNAM[[i]] <- locations$name[i]
      } else {
        hob$ITT[i] <- 1 # to do: enable suppport for ITT = 2!
        if(unique_obsnam) {
          hob$OBSNAM[[i]] <- paste(locations$name[i], c(1:abs(hob$IREFSP[[i]])), sep = '_')
        } else {
          hob$OBSNAM[[i]] <- rep(locations$name[i], abs(hob$IREFSP[[i]]))
        }        
        hob$IREFSP[[i]] <- apply(array(time_series$time[which(time_series$name == locations$name[i])],dim=c(length(time_series$time[which(time_series$name == locations$name[i])]))),1,first_greater_than,cumsum(dis$PERLEN))
        hob$TOFFSET[[i]] <- time_series$time[which(time_series$name == locations$name[i])] - c(0,cumsum(dis$PERLEN)[1:(dis$NPER-1)])[hob$IREFSP[[i]]]
        hob$HOBS[[i]] <- time_series$head[which(time_series$name == locations$name[i])]
      }
  }
  
  hob$MOBS <- length(which(hob$LAYER < 0))
  hob$MAXM <- abs(min(hob$LAYER[which(hob$LAYER <= 1)]))

  return(hob)
}
