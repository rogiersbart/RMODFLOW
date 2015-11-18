#' Convert bud object fluxes to darcy velocities
#' 
#' @param bud bud object
#' @param dis dis object
#' @param hed hed object; optional; if specified, the saturated cell thickness is used
#' @param stress_period stress period; defaults to 1
#' @param time_step time step; defaults to 1
#' @return list of 3d arrays: right, front, lower, left, back, upper, qx, qy, qz and q; all represent darcy velocities: the first six at the different cell faces, the last four represent the components and magnitude at the cell center
#' @export
convert_bud_to_darcy <- function(bud, dis, hed = NULL, stress_period = 1, time_step = 1) {
  
  thck <- cell_thickness(dis = dis, hed = hed)
  delc <- rep(dis$DELC,dis$NCOL)
  delr <- rep(dis$DELR,each=dis$NROW)
  darcy <- list()
  darcy$right <- aperm(array(bud$FLOW_RIGHT_FACE[[stress_period]][[time_step]],dim=c(dis$NCOL,dis$NROW,dis$NLAY)),c(2,1,3))/delc/thck
  darcy$front <- -aperm(array(bud$FLOW_FRONT_FACE[[stress_period]][[time_step]],dim=c(dis$NCOL,dis$NROW,dis$NLAY)),c(2,1,3))/delr/thck
  darcy$lower <- -aperm(array(bud$FLOW_LOWER_FACE[[stress_period]][[time_step]],dim=c(dis$NCOL,dis$NROW,dis$NLAY)),c(2,1,3))/delc/delr
  darcy$left <- darcy$back <- darcy$upper <- darcy$right * 0
  darcy$left[,c(2:dis$NCOL),] <- darcy$right[,c(1:(dis$NCOL-1)),]
  darcy$back[c(2:dis$NROW),,] <- darcy$front[c(1:(dis$NROW-1)),,]
  darcy$upper[,,c(2:dis$NLAY)] <- darcy$lower[,,c(1:(dis$NLAY-1))]
  if('RECHARGE' %in% names(bud)) {
    if(attributes(bud$RECHARGE[[stress_period]][[time_step]])$ITYPE == 4) darcy$upper[,,1] <- -bud$RECHARGE[[stress_period]][[time_step]]/delc/delr
    # to do: add functionality for different RECHARGE ITYPE, with recharge in different layers
  }
  if('DRAINS' %in% names(bud)) {
    bud$DRAINS[[stress_period]][[time_step]] <- cbind(bud$DRAINS[[stress_period]][[time_step]],convert_id_to_ijk(bud$DRAINS[[stress_period]][[time_step]]$ICELL, dis = dis))
    for(i in 1:nrow(bud$DRAINS[[stress_period]][[time_step]])) {
      darcy$upper[bud$DRAINS[[stress_period]][[time_step]][i,'i'],bud$DRAINS[[stress_period]][[time_step]][i,'j'],bud$DRAINS[[stress_period]][[time_step]][i,'k']] <- darcy$upper[bud$DRAINS[[stress_period]][[time_step]][i,'i'],bud$DRAINS[[stress_period]][[time_step]][i,'j'],bud$DRAINS[[stress_period]][[time_step]][i,'k']] - bud$DRAINS[[stress_period]][[time_step]][i,'value']/delc[bud$DRAINS[[stress_period]][[time_step]][i,'i']]/delr[bud$DRAINS[[stress_period]][[time_step]][i,'j']]
    }    
  }
  if('RIVER_LEAKAGE' %in% names(bud)) {
    bud$RIVER_LEAKAGE[[stress_period]][[time_step]] <- cbind(bud$RIVER_LEAKAGE[[stress_period]][[time_step]],convert_id_to_ijk(bud$RIVER_LEAKAGE[[stress_period]][[time_step]]$ICELL, dis = dis))
    for(i in 1:nrow(bud$RIVER_LEAKAGE[[stress_period]][[time_step]])) {
      darcy$upper[bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'i'],bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'j'],bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'k']] <- darcy$upper[bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'i'],bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'j'],bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'k']] - bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'value']/delc[bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'i']]/delr[bud$RIVER_LEAKAGE[[stress_period]][[time_step]][i,'j']]
    }   
  }
  darcy$qx <- (darcy$right + darcy$left) / 2
  darcy$qy <- (darcy$front + darcy$back) / 2
  darcy$qz <- (darcy$lower + darcy$upper) / 2
  darcy$q <- sqrt((darcy$qx)^2 + (darcy$qy)^2 + (darcy$qz)^2)
  class(darcy$right) <- 'modflow_3d_array'
  class(darcy$front) <- 'modflow_3d_array'
  class(darcy$lower) <- 'modflow_3d_array'
  class(darcy$left) <- 'modflow_3d_array'
  class(darcy$back) <- 'modflow_3d_array'
  class(darcy$upper) <- 'modflow_3d_array'
  class(darcy$qx) <- 'modflow_3d_array'
  class(darcy$qy) <- 'modflow_3d_array'
  class(darcy$qz) <- 'modflow_3d_array'
  class(darcy$q) <- 'modflow_3d_array'
  return(darcy)
}
