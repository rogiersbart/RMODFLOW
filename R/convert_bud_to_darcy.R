#' Convert bud object fluxes to darcy velocities
#' 
#' @param bud bud object
#' @param dis dis object
#' @param hed hed object; optional; if specified, the saturated cell thickness is used
#' @return list of 4d arrays: right, front, lower, left, back, upper, qx, qy, qz and q; all represent darcy velocities: the first six at the different cell faces, the last four represent the components and magnitude at the cell center
#' @export
convert_bud_to_darcy <- function(bud, dis, hed = NULL) {
  thck <- create_rmodflow_array(cell_dimensions(dis = dis, hed = hed)$z,dim=dim(bud$flow_right_face))
  delc <- create_rmodflow_array(rep(dis$delc,dis$ncol),dim=dim(bud$flow_right_face))
  delr <- create_rmodflow_array(rep(dis$delr,each=dis$nrow),dim=dim(bud$flow_right_face))
  darcy <- list()
  darcy$right <- bud$flow_right_face
  darcy$front <- -bud$flow_front_face
  darcy$lower <- -bud$flow_lower_face/delc/delr
  darcy$left <- darcy$back <- darcy$upper <- darcy$right * 0
  darcy$left[,c(2:dis$ncol),,] <- darcy$right[,c(1:(dis$ncol-1)),,]
  darcy$back[c(2:dis$nrow),,,] <- darcy$front[c(1:(dis$nrow-1)),,,]
  darcy$upper[,,c(2:dis$nlay),] <- darcy$lower[,,c(1:(dis$nlay-1)),]
  darcy$right <- darcy$right/delc/thck
  darcy$left <- darcy$left/delc/thck
  darcy$front <- darcy$front/delr/thck
  darcy$back <- darcy$back/delr/thck
  
  for (kper in 1:dis$nper) {
    for (kstp in 1:dis$nstp[kper]) {
      if ('recharge' %in% names(bud)) {
        if(attributes(bud$recharge[[kper]][[kstp]])$itype == 4) darcy$upper[,,1,(c(0,cumsum(dis$nstp))[kper]+kstp)] <- -bud$recharge[[kper]][[kstp]]/delc[,,1,1]/delr[,,1,1]
        # to do: add functionality for different recharge itype, with recharge in different layers
      }
      if('drains' %in% names(bud)) {
        bud$drains[[kper]][[kstp]] <- cbind(bud$drains[[kper]][[kstp]],convert_id_to_ijk(bud$drains[[kper]][[kstp]]$icell, dis = dis, type = 'modflow'))
        for(i in 1:nrow(bud$drains[[kper]][[kstp]])) {
          darcy$upper[bud$drains[[kper]][[kstp]][i,'i'],bud$drains[[kper]][[kstp]][i,'j'],bud$drains[[kper]][[kstp]][i,'k'],(c(0,cumsum(dis$nstp))[kper]+kstp)] <- darcy$upper[bud$drains[[kper]][[kstp]][i,'i'],bud$drains[[kper]][[kstp]][i,'j'],bud$drains[[kper]][[kstp]][i,'k'],(c(0,cumsum(dis$nstp))[kper]+kstp)] - bud$drains[[kper]][[kstp]][i,'value']/delc[bud$drains[[kper]][[kstp]][i,'i']]/delr[bud$drains[[kper]][[kstp]][i,'j']]
        }    
      }
      if('river_leakage' %in% names(bud)) {
        bud$river_leakage[[kper]][[kstp]] <- cbind(bud$river_leakage[[kper]][[kstp]],convert_id_to_ijk(bud$river_leakage[[kper]][[kstp]]$icell, dis = dis, type = 'modflow'))
        for(i in 1:nrow(bud$river_leakage[[kper]][[kstp]])) {
          darcy$upper[bud$river_leakage[[kper]][[kstp]][i,'i'],bud$river_leakage[[kper]][[kstp]][i,'j'],bud$river_leakage[[kper]][[kstp]][i,'k'],(c(0,cumsum(dis$nstp))[kper]+kstp)] <- darcy$upper[bud$river_leakage[[kper]][[kstp]][i,'i'],bud$river_leakage[[kper]][[kstp]][i,'j'],bud$river_leakage[[kper]][[kstp]][i,'k'],(c(0,cumsum(dis$nstp))[kper]+kstp)] - bud$river_leakage[[kper]][[kstp]][i,'value']/delc[bud$river_leakage[[kper]][[kstp]][i,'i']]/delr[bud$river_leakage[[kper]][[kstp]][i,'j']]
        }   
      }
    }
  }
  darcy$qx <- (darcy$right + darcy$left) / 2
  darcy$qy <- (darcy$front + darcy$back) / 2
  darcy$qz <- (darcy$lower + darcy$upper) / 2
  darcy$q <- sqrt((darcy$qx)^2 + (darcy$qy)^2 + (darcy$qz)^2)
  for(i in 1:length(darcy)) attributes(darcy[[i]]) <- list(dim=attr(darcy[[i]],'dim'),class=attr(darcy[[i]],'class'))
  return(darcy)
}
