#' Convert a parameter defined on the HUF grid to the numerical grid
#' 
#' @param values vector of parameter values, in the order of \code{hgunam}
#' @param huf huf object
#' @param dis dis object
#' @param mask masking 3d array, typically the \code{ibound} array, to speed up grid conversion; defaults to including all cells
#' @param type type of averaging that should be performed; either arithmetic (default), harmonic or geometric
#' @return 3d array
#' @export
rmf_convert_huf_to_grid <- function(values,
                                    huf,
                                    dis,
                                    mask = dis$top / dis$top,
                                    type = 'arithmetic') {
  num_grid_array <- dis$botm*NA
  huf$botm <- huf$thck*NA
  huf$botm[,,1] <- huf$top[,,1]-huf$thck[,,1]
  for(k in 2:dim(huf$thck)[3]) {
    huf$botm[,,k] <- huf$botm[,,k-1]-huf$thck[,,k]
  } 
  dis$top <- array(c(dis$top,dis$botm[,,1:(dis$nlay-1)]),dim=c(dis$nrow,dis$ncol,dis$nlay))
  i <- rep(1:dis$nrow,dis$ncol*dis$nlay)
  j <- rep(rep(1:dis$ncol,each=dis$nrow),dis$nlay)
  k <- rep(1:dis$nlay,each=dis$nrow*dis$ncol)
  num_grid_array[which(mask==0)] <- 0
  get_weighted_mean <- function(cell) {
        iCell <- i[cell]
        jCell <- j[cell]
        kCell <- k[cell]
        cell_top <- dis$top[iCell,jCell,kCell]
        cell_botm <- dis$botm[iCell,jCell,kCell]
        thck <- pmin(huf$top[iCell,jCell,],cell_top) - pmax(huf$botm[iCell,jCell,],cell_botm)
        thck[which(thck < 0)] <- 0
        if(type=='arithmetic') return(weighted.mean(values,thck))
        if(type=='harmonic') return(rmfi_weighted_harmean(values,thck))
        if(type=='geometric') return(rmfi_weighted_geomean(values,thck))
  }
  weighted_means <- lapply(which(mask!=0),get_weighted_mean)
  num_grid_array[which(mask!=0)] <- weighted_means
  num_grid_array <- array(num_grid_array,dim=c(dis$nrow,dis$ncol,dis$nlay))
  return(as.rmf_3d_array(num_grid_array))
}

#' @describeIn rmf_convert_huf_to_grid Deprecated function name
convert_huf_to_grid <- function(...) {
  .Deprecated(new = "rmf_convert_huf_to_grid", old = "convert_huf_to_grid")
  rmf_convert_huf_to_grid(...)
}
