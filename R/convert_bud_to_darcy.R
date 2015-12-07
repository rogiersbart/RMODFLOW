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
  thck <- cell_dimensions(dis = dis, hed = hed)$z
  delc <- rep(dis$delc,dis$ncol)
  delr <- rep(dis$delr,each=dis$nrow)
  darcy <- list()
  darcy$right <- bud$flow_right_face[[stress_period]][[time_step]]
  darcy$front <- -bud$flow_front_face[[stress_period]][[time_step]]
  darcy$lower <- -bud$flow_lower_face[[stress_period]][[time_step]]/delc/delr
  darcy$left <- darcy$back <- darcy$upper <- darcy$right * 0
  darcy$left[,c(2:dis$ncol),] <- darcy$right[,c(1:(dis$ncol-1)),]
  darcy$back[c(2:dis$nrow),,] <- darcy$front[c(1:(dis$nrow-1)),,]
  darcy$upper[,,c(2:dis$nlay)] <- darcy$lower[,,c(1:(dis$nlay-1))]
  darcy$right <- darcy$right/delc/thck
  darcy$left <- darcy$left/delc/thck
  darcy$front <- darcy$front/delr/thck
  darcy$back <- darcy$back/delr/thck
  
  if('RECHARGE' %in% names(bud)) {
    if(attributes(bud$recharge[[stress_period]][[time_step]])$itype == 4) darcy$upper[,,1] <- -bud$recharge[[stress_period]][[time_step]]/delc/delr
    # to do: add functionality for different recharge itype, with recharge in different layers
  }
  if('DRAINS' %in% names(bud)) {
    bud$drains[[stress_period]][[time_step]] <- cbind(bud$drains[[stress_period]][[time_step]],convert_modflow_id_to_ijk(bud$drains[[stress_period]][[time_step]]$ICELL, dis = dis))
    for(i in 1:nrow(bud$drains[[stress_period]][[time_step]])) {
      darcy$upper[bud$drains[[stress_period]][[time_step]][i,'i'],bud$drains[[stress_period]][[time_step]][i,'j'],bud$drains[[stress_period]][[time_step]][i,'k']] <- darcy$upper[bud$drains[[stress_period]][[time_step]][i,'i'],bud$drains[[stress_period]][[time_step]][i,'j'],bud$drains[[stress_period]][[time_step]][i,'k']] - bud$drains[[stress_period]][[time_step]][i,'value']/delc[bud$drains[[stress_period]][[time_step]][i,'i']]/delr[bud$drains[[stress_period]][[time_step]][i,'j']]
    }    
  }
  if('RIVER_LEAKAGE' %in% names(bud)) {
    bud$river_leakage[[stress_period]][[time_step]] <- cbind(bud$river_leakage[[stress_period]][[time_step]],convert_modflow_id_to_ijk(bud$river_leakage[[stress_period]][[time_step]]$ICELL, dis = dis))
    for(i in 1:nrow(bud$river_leakage[[stress_period]][[time_step]])) {
      darcy$upper[bud$river_leakage[[stress_period]][[time_step]][i,'i'],bud$river_leakage[[stress_period]][[time_step]][i,'j'],bud$river_leakage[[stress_period]][[time_step]][i,'k']] <- darcy$upper[bud$river_leakage[[stress_period]][[time_step]][i,'i'],bud$river_leakage[[stress_period]][[time_step]][i,'j'],bud$river_leakage[[stress_period]][[time_step]][i,'k']] - bud$river_leakage[[stress_period]][[time_step]][i,'value']/delc[bud$river_leakage[[stress_period]][[time_step]][i,'i']]/delr[bud$river_leakage[[stress_period]][[time_step]][i,'j']]
    }   
  }
  darcy$qx <- (darcy$right + darcy$left) / 2
  darcy$qy <- (darcy$front + darcy$back) / 2
  darcy$qz <- (darcy$lower + darcy$upper) / 2
  darcy$q <- sqrt((darcy$qx)^2 + (darcy$qy)^2 + (darcy$qz)^2)
  class(darcy$right) <- '3d_array'
  class(darcy$front) <- '3d_array'
  class(darcy$lower) <- '3d_array'
  class(darcy$left) <- '3d_array'
  class(darcy$back) <- '3d_array'
  class(darcy$upper) <- '3d_array'
  class(darcy$qx) <- '3d_array'
  class(darcy$qy) <- '3d_array'
  class(darcy$qz) <- '3d_array'
  class(darcy$q) <- '3d_array'
  return(darcy)
}
