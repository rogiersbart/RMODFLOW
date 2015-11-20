#' Convert a huf to a dis object (for plotting)
#' 
#' @param huf huf object
#' @param dis dis object, corresponding to the huf object
#' @return dis object containing the layers of the huf object
#' @export
convert_huf_to_dis <- function(huf, dis) {
  new_dis <- create_dis(NLAY = huf$NHUF,
                        NROW = dis$NROW,
                        NCOL = dis$NCOL,
                        DELR = dis$DELR,
                        DELC = dis$DELC,
                        TOP = huf$TOP[,,1],
                        BOTM = huf$TOP - huf$THCK)
  
  new_dis$TOP[which(new_dis$TOP > dis$TOP)] <- dis$TOP[which(new_dis$TOP > dis$TOP)]
  for(k in 1:huf$NHUF) {
    new_dis$BOTM[,,k][which(new_dis$BOTM[,,k] > dis$TOP)] <- dis$TOP[which(new_dis$BOTM[,,k] > dis$TOP)]
    new_dis$BOTM[,,k][which(new_dis$BOTM[,,k] < dis$BOTM[,,dis$NLAY])] <- dis$BOTM[,,dis$NLAY][which(new_dis$BOTM[,,k] < dis$BOTM[,,dis$NLAY])]
  }

  return(new_dis)
}