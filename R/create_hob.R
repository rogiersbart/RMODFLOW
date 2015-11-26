#' Create an \code{RMODFLOW} hob object
#' 
#' \code{create_hob} creates an RMODFLOW hob object
#' 
#' @param xFilter x coordinates of the monitoring well filter
#' @param yFilter y coordinates of the monitoring well filter
#' @param topFilter z coordinate of the top of the filter
#' @param bottomFilter z coordinate of the bottom of the filter
#' @param head
#' @param i
#' @param j
#' @param xGrid
#' @param yGrid
#' @param multiLayer
#' @param dis
#' @param useTransmissivity
#' @param huf
#' @param mlt
#' @return Object of class hob
#' @export
#' @seealso \code{\link{read.hob}} and \code{\link{write.hob}}
create_hob <- function(
  # points in space
    name, 
    x,
    y,
    z=NULL,
    top=NULL,
    bottom=NULL,
  # points in time
    head_name,
    head,
    time = x * 0,
    STATISTIC = rep(1, length(x)),
    STAT_FLAG = rep(0, length(x)),
    PLOT_SYMBOL = rep(1, length(x)),
  # other parameters
    dis,
    MAXM = dis$NLAY,
    IUHOBSV = 0,
    HOBDRY = -999,
    NOPRINT = TRUE,
    TOMULTH = 1,
    prj = NULL,
    useTransmissivity=F,
    huf=NA,
    mlt=NA) {
  
  hob <- NULL
  
#   # Data set 0
#     # comments should be provided with ?comment
#   
#   # Data set 1
#     hob$NH <- length(x)
#     hob$MOBS <- ifelse(is.null(top),0,length(which(!is.na(top))))
#     hob$MAXM <- MAXM
#     hob$IUHOBSV <- IUHOBSV
#     hob$HOBDRY <- HOBDRY
#     hob$NOPRINT <- NOPRINT
#   
#   # Data set 2
#     hob$TOMULT <- TOMULTH
#     # hob$EVH # MODFLOW-2000
#   
#   # Data set 3-6
#   
#     # loop over name
#       {
#   hob$OBSNAM <- name
#   locations <- convert_real_to_dis(x = x, y = y, z = z, dis = dis, prj = prj)
#   hob$LAYER <- locations$k
#   hob$ROW <- locations$i
#   hob$COLUMN <- locations$j
#   first_greater_than <- function(x, y) which(y > x)[1]
#   hob$IREFSP <- apply(array(time,dim=c(length(time))),1,first_greater_than,cumsum(dis$PERLEN))
#   hob$ROFF <- locations$roff
#   hob$COFF <- locations$coff
#   hob$TOFFSET <- time - c(0,cumsum(dis$PERLEN)[1:(dis$NPER-1)])[hob$IREFSP]
#   hob$HOBS <- head
#   hob$STATISTIC <- STATISTIC
#   hob$STAT_FLAG <- STAT_FLAG
#   hob$PLOT_SYMBOL <- PLOT_SYMBOL
#   
#   ROW <- NULL
#   COLUMN <- NULL
#   LAYER <- NULL
#   ROFF <- NULL
#   COFF <- NULL
#   for(h in 1:nrow(xyz)) {
#     nr <- which.min(points.distance(xyz$x[h],xyz$y[h],xyzGrid$x,xyzGrid$y))
#     ROW[h] <- xyzGrid$i[nr]
#     COLUMN[h] <- xyzGrid$j[nr]
#     ROFF[h] <- -(xyz$y[h]-xyzGrid$y[nr])/50
#     COFF[h] <- (xyz$x[h]-xyzGrid$x[nr])/50
#     LAYER[h] <- which(newDIS$BOTM[ROW[h],COLUMN[h],] <= xyz$z[h])[1]
#   }
#   
#   # new MLAY and PR objects
#   for(h in 1:newHOB$NH) {
#     # get layers and thicknesses from newDIS file
#     layerBottoms <- newDIS$BOTM[hobData$ROW[h],hobData$COLUMN[h],]
#     layerTops <- newDIS$BOTM[hobData$ROW[h],hobData$COLUMN[h],]
#     layerTops[2:length(layerTops)] <- layerTops[1:(length(layerTops)-1)]
#     layerTops[1] <- newDIS$TOP[hobData$ROW[h],hobData$COLUMN[h]]
#     filterTop <- hobData$FilterTop[h]
#     filterBottom <- hobData$FilterBottom[h]
#     newHOB$MLAY[[h]] <- which(layerBottoms < filterTop & layerTops > filterBottom)
#     thicknesses <- NULL
#     layerCenters <- NULL
#     for(i in 1:length(newHOB$MLAY[[h]])) {
#       lay <- newHOB$MLAY[[h]][i]
#       thicknesses[i] <- min(filterTop,layerTops[lay]) - max(filterBottom, layerBottoms[lay])
#       layerCenters[i] <- mean(layerTops[lay],layerBottoms[lay])
#     }
#     # get kvalues from newHUF and newMLT files
#     kvalues <- NULL
#     hufTops <- newHUF$TOP[hobData$ROW[h],hobData$COLUMN[h],]
#     for(i in 1:length(newHOB$MLAY[[h]])) {
#       hufNr <- which(hufTops >= layerCenters[i])[1]
#       kvalues[i] <- newMLT$RMLT[[(2*i)-1]][hobData$ROW[h],hobData$COLUMN[h]]
#     }
#     
#     # weighting according to transmissivity
#     newHOB$PR[[h]] <- thicknesses * kvalues
#     newHOB$PR[[h]] <- newHOB$PR[[h]]/sum(newHOB$PR[[h]])
#   }
#   
#   # new MAXM, MOBS and LAYER objects
#   lengths <- NULL
#   for(h in 1:newHOB$NH) lengths[h] <- length(newHOB$MLAY[[h]])
#   newHOB$MAXM <- max(lengths)
#   newHOB$MOBS <- length(which(lengths > 1))
#   newHOB$LAYER <- -lengths
#   singleLayers <- which(newHOB$LAYER==-1)
#   for(i in singleLayers) newHOB$LAYER[i] <- newHOB$MLAY[[i]]  
#   
# }
#   
  # update hob$MAXM?
  return(hob)
}
