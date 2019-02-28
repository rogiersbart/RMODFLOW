#' Get cell dimensions from a dis object
#' 
#' @param dis dis object
#' @param hed hed object, used for calculating the saturated thickness; if not specified, the regular cell thickness is returned
#' @param include_volume logical; should the cell volumes be included?
#' @param include_faces logical; should face areas be included?
#' @return list with cell dimension 3d arrays
#' @rdname rmf_cell_dimensions
#' @method rmf_cell_dimensions dis
#' @export
rmf_cell_dimensions.dis <- function(dis,
                                    hed = NULL,
                                    include_volume = FALSE,
                                    include_faces = FALSE) {
  cell_dimensions <- list()
  # remove the confining beds
  nnlay <- dis$nlay + length(which(dis$laycbd != 0))
  cbd <- rep(0, nnlay)
  cbd[cumsum(dis$laycbd+1)[dis$laycbd != 0]] <- 1
  if (is.null(hed) | ifelse(is.null(hed),FALSE,dim(hed)[4] == 1)) {
    cell_top <- dis$botm
    cell_top[,,1] <- dis$top
    if(nnlay > 1) cell_top[,,2:nnlay] <- dis$botm[,,c(1:(nnlay-1))]
    if(!is.null(hed)) {
      cell_top[which(cell_top > hed[,,,1])] <- hed[which(cell_top > hed[,,,1])]
    }
    cell_dimensions$z <- rmf_create_array(cell_top - dis$botm)[,,!cbd]
    cell_dimensions$x <- rmf_create_array(rep(dis$delc,dis$ncol*dis$nlay),dim=c(dis$nrow,dis$ncol,dis$nlay))
    cell_dimensions$y <- rmf_create_array(rep(dis$delr, dis$nlay, each = dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay))
    if(include_volume) cell_dimensions$volume <- rmf_create_array(with(cell_dimensions, x * y * z))
    if(include_faces) {
      cell_dimensions$front <- rmf_create_array(with(cell_dimensions, x * z))
      cell_dimensions$back <- cell_dimensions$front
      cell_dimensions$left <- rmf_create_array(with(cell_dimensions, y * z))
      cell_dimensions$right <- cell_dimensions$left
      cell_dimensions$lower <- rmf_create_array(with(cell_dimensions, x * y))
      cell_dimensions$upper <- cell_dimensions$lower
    }  
  } else {
    cell_top <- rmf_create_array(dim = c(dis$nrow, dis$ncol, nnlay, sum(dis$nstp)))
    cell_top[,,1,] <- dis$top
    if(nnlay > 1) cell_top[,,2:nnlay,] <- dis$botm[,,c(1:(nnlay-1))]
    cell_top[which(cell_top > hed[,,,])] <- hed[which(cell_top > hed[,,,])] 
    cell_dimensions$z <- rmf_create_array(cell_top - dis$botm)[,,!cbd]
    cell_dimensions$x <- rmf_create_array(rep(dis$delc,dis$ncol*dis$nlay),dim=c(dis$nrow,dis$ncol,dis$nlay))
    cell_dimensions$y <- rmf_create_array(rep(dis$delr, dis$nlay, each = dis$nrow),dim=c(dis$nrow,dis$ncol,dis$nlay))
    if(include_volume) cell_dimensions$volume <- rmf_create_array(with(cell_dimensions, x * y * z))
    if(include_faces) {
      cell_dimensions$front <- rmf_create_array(with(cell_dimensions, x * z))
      cell_dimensions$back <- cell_dimensions$front
      cell_dimensions$left <- rmf_create_array(with(cell_dimensions, y * z))
      cell_dimensions$right <- cell_dimensions$left
      cell_dimensions$lower <- rmf_create_array(with(cell_dimensions, x * y))
      cell_dimensions$upper <- cell_dimensions$lower
    }
  }
  return(cell_dimensions)
}
