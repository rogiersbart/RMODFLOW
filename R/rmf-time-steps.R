#' Calculate the internal time step sequence of a transient MODFLOW model
#' 
#' \code{rmf_time_steps} calculates the internal sequence of time steps of a transient MODFLOW model from either an \code{RMODFLOW} dis object or separate parameters
#' 
#' @param dis optional, an \code{RMODFLOW} dis object
#' @param perlen optional, only read when a \code{dis} object is not supplied; numeric vector of length \code{nper} specifying the stress period lengths
#' @param tsmult optional, only read when a \code{dis} object is not supplied; numeric vector of length \code{nper} specifying the time step multipliers
#' @param nstp optional, only read when a \code{dis} object is not supplied; numeric vector of length \code{nper} specifying the number of time steps in each stress period
#' @param incl_ss logical, only read when a \code{dis} object is supplied; should the lengths of steady-state stress periods in the \code{dis} object be incorporated in the calculation; defaults to TRUE 
#' 
#' @return a list holding the numeric vectors of the computed sequence of time step lengths and its cumulative sum
#' @export
#' @seealso \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?dis.htm}

rmf_time_steps = function(dis = NULL,
                          perlen = NULL,
                          tsmult = NULL,
                          nstp = NULL,
                          incl_ss = T){
  

  if(!is.null(dis)){
    if(incl_ss){
      perlen = dis$perlen
      tsmult = dis$tsmult
      nstp = dis$nstp
      itmuni = dis$itmuni
    } else {
      perlen = dis$perlen[which(dis$sstr == 'TR')]
      tsmult = dis$tsmult[which(dis$sstr == 'TR')]
      nstp = dis$nstp[which(dis$sstr == 'TR')]
      itmuni = dis$itmuni[which(dis$sstr == 'TR')]
    }
    
  } 
  
  its=list()
  for(i in 1:length(perlen)){
    if(tsmult[i]==1) t1 = perlen[i]/nstp[i] else t1 = perlen[i]*((tsmult[i]-1)/((tsmult[i]^nstp[i])-1))   
   
     its[[i]] = t1
    
    if(nstp[i] > 1){
      for(j in 2:nstp[i]){
        its[[i]] = append(its[[i]], its[[i]][j-1]*tsmult[i])
      }
    }
  }
 
  its = list(tsl = unlist(its), cumsum = cumsum(unlist(its)))
  
  return(its)

}