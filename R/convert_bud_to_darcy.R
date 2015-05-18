#' Convert bud object fluxes to darcy velocities
#' 
#' @param values Vector of parameter values, corresponding to HGUNAM
#' @param huf huf object
#' @param dis dis object
#' @return object with four 3d arrays: right, front, lower and magnitude
#' @export
convert_bud_to_darcy <- function(bud,dis)
{
  thck <- dis$BOTM
  thck[,,1] <- dis$TOP-dis$BOTM[,,1]
  for(i in 2:dis$NLAY) thck[,,i] <- dis$BOTM[,,i-1]-dis$BOTM[,,i]
  delc <- rep(dis$DELC,dis$NCOL)
  delr <- rep(dis$DELR,each=dis$NROW)
  darcy <- list()
  darcy$right <- aperm(array(bud$FLOW_RIGHT_FACE[[1]][[1]]$data,dim=c(dis$NCOL,dis$NROW,dis$NLAY)),c(2,1,3))/delc/thck
  darcy$front <- aperm(array(bud$FLOW_FRONT_FACE[[1]][[1]]$data,dim=c(dis$NCOL,dis$NROW,dis$NLAY)),c(2,1,3))/delr/thck
  darcy$lower <- aperm(array(bud$FLOW_LOWER_FACE[[1]][[1]]$data,dim=c(dis$NCOL,dis$NROW,dis$NLAY)),c(2,1,3))/delc/delr
  darcy$magnitude <- sqrt(darcy$right^2 + darcy$front^2 + darcy$lower^2)
  class(darcy$right) <- 'modflow_3d_array'
  class(darcy$front) <- 'modflow_3d_array'
  class(darcy$lower) <- 'modflow_3d_array'
  class(darcy$magnitude) <- 'modflow_3d_array'
  return(darcy)
}