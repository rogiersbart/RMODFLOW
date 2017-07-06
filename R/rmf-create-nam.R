#' Create an \code{RMODFLOW} nam object
#' 
#' \code{rmf_create_nam} creates an \code{RMODFLOW} nam object.
#' 
#' @param ... RMODFLOW objects to be included in the nam file
#' @return Object of class nam
#' @export
#' @seealso \code{\link{read_nam}}, \code{\link{write_nam}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?name_file.htm}
rmf_create_nam <- function(...) {
  
  fobjects <- list(...)
  nam <- data.frame(ftype = c('LIST',rep(NA, length(fobjects))), nunit = c(700, 700 + seq_along(fobjects)), fname = c('output.lst',rep(NA, length(fobjects))), stringsAsFactors = FALSE)
  
  # data set 1
  
    # add all input objects
      for(i in seq_along(fobjects)) {
        nam$fname[i+1] <- paste0('input.',class(fobjects[[i]])[1])
        nam$ftype[i+1] <- c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF')[class(fobjects[[i]])[1] == c('hob','pvl','dis','zon','mlt','bas','huf','oc','wel','ghb','pcg','kdep','lpf')]
      }
  
    # check for additional output files
      # check if hed, ccf, hpr output files are required or not
      if('HOB' %in% nam$ftype) {
        hob <- fobjects[which(nam$ftype=='HOB')-1]
        if(hob$iuhobsv != 0) {
          nam <- rbind(nam, data.frame(ftype = 'DATA', nunit = hob$iuhobsv, fname = 'output.hpr'))  
        }
      }
      if('OC' %in% nam$ftype) {
        
      }
  
  class(nam) <- c('nam','rmf_package','data.frame')
  return(nam)
}

#' @describeIn rmf_create_nam Deprecated function name
create_nam <- function(...) {
  .Deprecated(new = "rmf_create_nam", old = "create_nam")
  rmf_create_nam(...)
}
