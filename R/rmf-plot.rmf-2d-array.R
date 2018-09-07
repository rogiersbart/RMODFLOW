#' Plot a MODFLOW 2D array
#' 
#' \code{rmf_plot.rmf_2d_array} plots a MODFLOW 2D array.
#' 
#' @param array an object of class rmf_2d_array
#' @param dis discretization file object
#' @param bas basic file object; optional
#' @param mask a 2D array with 0 or F indicating inactive cells; optional; defaults to having all cells active or, if bas is provided, the first layer of bas$ibound
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'factor', 'grid' or 'contour'
#' @param levels labels that should be used on the factor legend; if NULL the array factor levels are used
#' @param gridlines logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param height_exaggeration height exaggeration for 3D plot; optional
#' @param binwidth binwidth for contour plot; defaults to 1/20 of zlim
#' @param label logical; should labels be added to contour plot
#' @param prj projection file object
#' @param crs coordinate reference system for the plot
#' @param alpha transparency value; defaults to 1
#' @param plot3d logical; should a 3D plot be made
#' @param height 2D array for specifying the 3D plot z coordinate
#' @param title plot title
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask)
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_2d_array
#' @export
#' @import ggplot2 directlabels akima rgl quadprog
rmf_plot.rmf_2d_array <- function(array,
                                       dis,
                                       bas = NULL,
                                       mask = rmfi_ifelse0(is.null(bas), array*0+1, {warning('Using first ibound layer as mask.', call. = FALSE)}),
                                       colour_palette = rmfi_rev_rainbow,
                                       zlim = range(array[as.logical(mask)], finite=TRUE),
                                       nlevels = 7,
                                       type = 'fill',
                                       levels = NULL,
                                       gridlines = FALSE,
                                       add = FALSE,
                                       height_exaggeration = 100,
                                       binwidth=round(diff(zlim)/20),
                                       label=TRUE,
                                       prj=NULL,
                                       crs=NULL,
                                       alpha=1,
                                       plot3d=FALSE,
                                       height=NULL,
                                       title = NULL,
                                       crop = FALSE) {

  
  if(plot3d) {
    xyz <- cell_coordinates(dis)
    x <- xyz$x[,,1]
    y <- xyz$y[,,1]
    if(!is.null(prj)) {
      xyz <- rmf_convert_grid_to_xyz(x=c(x),y=c(y),prj=prj)
      x[,] <- xyz$x
      y[,] <- xyz$y
    }
    z <- t(height)*height_exaggeration
    if(!add) rgl::open3d()
    colorlut <- colorRampPalette(colour_palette(nlevels))(25) # height color lookup table
    col <- colorlut[ round(approx(seq(zlim[1],zlim[2],length=25+1),seq(0.5,25+0.5,length=25+1),xout=c(t(array)),rule=2)$y) ] # assign colors to heights for each point
    alpha <- rep(1,length(col))
    alpha[which(c(t(mask))==0)] <- 0
    if(type=='fill') rgl::surface3d(t(x),t(y),z,color=col,alpha=alpha,back='lines',smooth=FALSE) 
    if(type=='grid') rgl::surface3d(t(x),t(y),z,front='lines',alpha=alpha,back='lines',smooth=FALSE) 
  } else {
    xy <- expand.grid(cumsum(dis$delr)-dis$delr/2,sum(dis$delc)-(cumsum(dis$delc)-dis$delc/2))
    names(xy) <- c('x','y')
    mask[which(mask==0)] <- NA
    if(type %in% c('fill','factor','grid')) {
      ids <- factor(1:(dis$nrow*dis$ncol))
      xWidth <- rep(dis$delr,dis$nrow)
      yWidth <- rep(dis$delc,each=dis$ncol)
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c(t(array*mask^2)))
      if(!is.null(prj)) {
        new_positions <- rmf_convert_grid_to_xyz(x=positions$x,y=positions$y,prj=prj)
        positions$x <- new_positions$x
        positions$y <- new_positions$y
      }
      if(!is.null(crs)) {
        positions <- rmfi_convert_coordinates(positions,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))
      }
      datapoly <- merge(values, positions, by=c("id"))
      if(crop) datapoly <- na.omit(datapoly)
    }
    if(type=='fill') {  
      if(add) {
        return(list(geom_polygon(aes(x=x,y=y,fill=value, group=id),data=datapoly,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
          scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim, na.value = NA))) 
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
          geom_polygon(aes(fill=value, group=id),alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
          scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim,  na.value = NA) +
          coord_equal() + ggtitle(title))
      }
    } else if(type=='factor') {  
      if(add) {
        return(list(geom_polygon(aes(x=x,y=y,fill=factor(value), group=id),data=datapoly,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    scale_fill_discrete('value',labels=rmfi_ifelse0(is.null(levels),levels(factor(datapoly$value)),levels), na.value = NA)))
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
                 geom_polygon(aes(fill=factor(value), group=id),alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 scale_fill_discrete('value',labels=rmfi_ifelse0(is.null(levels),levels(factor(datapoly$value)),levels), na.value = NA) +
                 coord_equal() + ggtitle(title))
      }
    } else if(type=='grid') {  
      if(add) {
        return(geom_polygon(aes(x=x,y=y,group=id),data=datapoly,alpha=alpha,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA))
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
                 geom_polygon(aes(group=id),alpha=alpha,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA) +
                 coord_equal() + ggtitle(title))
      }
    } else if(type=='contour') {
      if(!is.null(prj)) {
        new_xy <- rmf_convert_grid_to_xyz(x=xy$x,y=xy$y,prj=prj)
        xy$x <- new_xy$x
        xy$y <- new_xy$y
      }
      if(!is.null(crs)) {
        xy <- RMODFLOW:::rmfi_convert_coordinates(xy,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))
      }
      xy$z <- c(t(array*mask^2))
      xyBackup <- xy
      xy <- na.omit(xy)
      xy <- interp(xy$x,xy$y,xy$z,xo=seq(min(xy$x),max(xy$x),length=ceiling(sum(dis$delr)/min(dis$delr))),yo=seq(min(xy$y),sum(max(xy$y)),length=ceiling(sum(dis$delc)/min(dis$delc))))
      xy$x <- rep(xy$x,ceiling(sum(dis$delc)/min(dis$delc)))
      xy$y <- rep(xy$y,each=ceiling(sum(dis$delr)/min(dis$delr)))
      xy$z <- c(xy$z)
      xy <- as.data.frame(xy)
      xy <- xy[which(xy$z >= zlim[1] & xy$z <= zlim[2]),]
      closestGridPoints <- apply(xy[,c('x','y')],1,function(x) which.min((x[1]-xyBackup$x)^2 + (x[2]-xyBackup$y)^2))
      xy$z[which(is.na(xyBackup$z[closestGridPoints]))] <- NA
      if(crop) {
        xlim = c(min(xy$x, na.rm = T), max(xy$x, na.rm = T))
        ylim = c(min(xy$y, na.rm = T), max(xy$xy, na.rm = T))
      } else {
        xlim = c(min(xyBackup$x, na.rm = T), max(xyBackup$x, na.rm = T))
        ylim = c(min(xyBackup$y, na.rm = T), max(xyBackup$y, na.rm = T))
      }
      rm(xyBackup)
      if(add) {
        if(label) return(list(stat_contour(aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth),geom_dl(aes(x=x, y=y, z=z, label=..level.., colour=..level..),data=xy,method="top.pieces", stat="contour")))
        if(!label) return(stat_contour(aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth))
      } else {
        if(label) {
          return(ggplot(xy, aes(x=x, y=y)) + stat_contour(aes(z=z, colour = ..level..),binwidth=binwidth) +geom_dl(aes(z=z, label=..level.., colour=..level..),method="top.pieces", stat="contour") + coord_equal(xlim = xlim, ylim = ylim) +theme(legend.position="none") + ggtitle(title))
        } else {
          return(ggplot(xy, aes(x=x, y=y)) + stat_contour(aes(z=z,colour = ..level..),binwidth=binwidth) + coord_equal(xlim = xlim, ylim = ylim) + ggtitle(title))
        }
      }
    } else {
      stop('Please provide valid plot type.', call. = FALSE)
    }
  }
}

#' @describeIn rmf_plot.rmf_2d_array Deprecated function name
#' @export
plot.rmf_2d_array <- function(...) {
  .Deprecated(new = "rmf_plot.rmf_2d_array", old = "plot.rmf_2d_array")
  rmf_plot.rmf_2d_array(...)
}
