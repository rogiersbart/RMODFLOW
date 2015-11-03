#' Create an \code{RMODFLOW} hob object
#' 
#' \code{create.hob} creates in a MODFLOW budget ascii file created from the binary file (typically *.bud) using \code{Read_budget.exe} and returns it as an \code{\link{RMODFLOW}} bud object.
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
create_hob <- function(xFilter,yFilter,topFilter,bottomFilter,head,i,j,xGrid,yGrid,multiLayer=F,
                       dis, useTransmissivity=F, huf=NA, mlt=NA)
{
#   hob <- NULL
#   hob$NH <- length(xFilter)
#   hob$MOBS <- 
#   hob$MAXM <- 
#   hob$IUHOBSV <- 
#   hob$HOBDRY <-
#   hob$NOPRINT <-
#   hob$TOMULT
#   hob$EVH
#   hob$LAYER
#   hob$ROW
#   hob$COLUMN
#   hob$IREFSP
#   hob$ROFF
#   hob$COFF
#   hob$OBSNAM
#   hob$TOFFSET
#   hob$HOBS
#   hob$STATISTIC
#   hob$STATFLAG
#   hob$PLOTSYMBOL  
#   
#   
#   ROW <- NULL
#   COLUMN <- NULL
#   LAYER <- NULL
#   ROFF <- NULL
#   COFF <- NULL
#   for(h in 1:nrow(xyz))
#   {
#     nr <- which.min(points.distance(xyz$x[h],xyz$y[h],xyzGrid$x,xyzGrid$y))
#     ROW[h] <- xyzGrid$i[nr]
#     COLUMN[h] <- xyzGrid$j[nr]
#     ROFF[h] <- -(xyz$y[h]-xyzGrid$y[nr])/50
#     COFF[h] <- (xyz$x[h]-xyzGrid$x[nr])/50
#     LAYER[h] <- which(newDIS$BOTM[ROW[h],COLUMN[h],] <= xyz$z[h])[1]
#   }
#   
#   
#   # new MLAY and PR objects
#   for(h in 1:newHOB$NH)
#   {
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
#     for(i in 1:length(newHOB$MLAY[[h]]))
#     {
#       lay <- newHOB$MLAY[[h]][i]
#       thicknesses[i] <- min(filterTop,layerTops[lay]) - max(filterBottom, layerBottoms[lay])
#       layerCenters[i] <- mean(layerTops[lay],layerBottoms[lay])
#     }
#     # get kvalues from newHUF and newMLT files
#     kvalues <- NULL
#     hufTops <- newHUF$TOP[hobData$ROW[h],hobData$COLUMN[h],]
#     for(i in 1:length(newHOB$MLAY[[h]]))
#     {
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
  return(hob)
}
