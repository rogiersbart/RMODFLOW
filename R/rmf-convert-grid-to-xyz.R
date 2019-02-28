#' Convert modflow coordinates to real world coordinates
#' 
#' @param x modflow x coordinate
#' @param y modflow y coordinate
#' @param z modflow z coordinate
#' @param i modflow row number
#' @param j modflow column number
#' @param k modflow layer number
#' @param roff modflow row offset
#' @param coff modflow column offset
#' @param loff modflow layer offset
#' @param prj prj object
#' @param dis dis object
#' @details Provide either xyz or ijk
#'   If xyz is provided, it is reprojected using the optional prj object.
#'   
#'   Optional roff, coff and loff values can be supplied along the i, j and k indices.
#'   
#'   k indices should not represent Quasi-3D confining beds. If a real world coordinate should be obtained from a Quasi-3D confining bed,
#'   set the k index to the overlying model layer and supply a loff value (relative to the thickness of the overlying model layer)
#'   
#' @return data frame with real world x and y (and optionally z) coordinates
#' @export
rmf_convert_grid_to_xyz <- function(x = NULL,
                                    y = NULL,
                                    z = NULL,
                                    i = NULL,
                                    j = NULL,
                                    k = NULL,
                                    roff = NULL,
                                    coff = NULL,
                                    loff = NULL,
                                    prj = NULL,
                                    dis = NULL) {
  if(!is.null(x)) {
    if(!is.null(prj)) {
      if(length(prj$origin) <= 2) prj$origin <-  c(prj$origin, 0)
      # counterclockwise rotation is negative. Now corresponds to rmf_convert_xyz_to_grid
      angle <- atan(y/x)*180/pi+prj$rotation
      angle[which(is.na(angle))] <- 90-prj$rotation
      s <- sqrt(x^2+y^2)
      x <- prj$origin[1]+ cos(angle*pi/180)*s
      y <- prj$origin[2]+ sin(angle*pi/180)*s
      if(!is.null(z)) z <- prj$origin[3]+z
    }
    ifelse(!is.null(z),return(data.frame(x=x,y=y,z=z)),return(data.frame(x=x,y=y)))
 
   } else if(!is.null(i)) {
    if(is.null(dis)) stop('Please provide a dis object')
    y_grid <- c(cumsum(rev(dis$delc))-rev(dis$delc)/2)[(dis$nrow-i+1)]
    x_grid <- c(cumsum(dis$delr)-dis$delr/2)[j]
    
    if(!is.null(k)) {
      
      # set thicknesses of cells
      if(any(k > dis$nlay)) stop('k is greater than dis$nlay')
      thck <- dis$botm
      nnlay <- ifelse(length(dim(dis$botm)) > 2, dim(dis$botm)[3], 1)
      if(nnlay > 1) thck[,,2:nnlay] <- dis$botm[,,(2:nnlay)-1]-dis$botm[,,2:nnlay]
      thck[,,1] <- dis$top - dis$botm[,,1]
      
      #  vectorize this:
      
      # adjust botm for presence of confining beds
      df <- data.frame(i=i,j=j,k=k)
      if(!is.null(loff)) df$loff <- loff
      cbd <- rep(0, nnlay)
      cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
      
      for(x in 1:nrow(df)) {
        k_adj <- ifelse(df$k[x] == 1, df$k[x], df$k[x] + sum((dis$laycbd != 0)[1:(df$k[x]-1)]))
        cell_thchk <- sum(thck[df$i[x],df$j[x],k_adj:nnlay])
        # calculate z of cell center; normalize with bottom of model (which can never be a confined bed)
        df$z[x] <- cell_thchk-thck[,,!cbd][df$i[x],df$j[x],df$k[x]]/2 + dis$botm[df$i[x],df$j[x],nnlay]
        if(!is.null(loff)) df$z[x] <- df$z[x] - df$loff[x]*thck[df$i[x],df$j[x],df$k[x]]
        
      }
      z_grid <- df$z
      rm(df)
    }
    
    # offset
    if(!is.null(roff)) y_grid <- y_grid - roff*dis$delc[i]
    if(!is.null(coff)) x_grid <- x_grid + coff*dis$delr[j]
    
    x <- x_grid
    y <- y_grid
    if(!is.null(k)) z <- z_grid
    
    if(!is.null(prj)) {
      if(length(prj$origin) <= 2) prj$origin = c(prj$origin, 0)
      s <- sqrt(x_grid^2+y_grid^2)
      angle <- asin(y_grid/s)*180/pi - prj$rotation
      x_grid <- cos(angle*pi/180)*s
      y_grid <- sin(angle*pi/180)*s
      x <- prj$origin[1] + x_grid
      y <- prj$origin[2] + y_grid
      if(!is.null(k)) {
        z <- prj$origin[3] + z_grid
      }
    }
    
    dat <- data.frame(x=x,y=y)
    if(!is.null(k)) dat <- data.frame(x=x,y=y,z=z)
    return(dat)
  }
}                                                                           

#' @describeIn rmf_convert_grid_to_xyz Deprecated function name
#' @export
convert_grid_to_xyz <- function(...) {
  .Deprecated(new = "rmf_convert_grid_to_xyz", old = "convert_grid_to_xyz")
  rmf_convert_grid_to_xyz(...)
}
