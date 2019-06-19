#' Write a MODFLOW horizontal flow barrier file
#'
#' \code{rmf_write_hfb} writes a MODFLOW horizontal flow barrier file based on an \code{RMODFLOW} hfb object
#' 
#' @param hfb an \code{RMODFLOW} hfb object
#' @param file filename to write to; typically '*.hfb'
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}


rmf_write_hfb = function(hfb, file = {cat('Please select hfb file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Horizontal Flow Barrier Package created by RMODFLOW, version',v,'\n'), file = file)
  cat(paste('#', comment(hfb)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(hfb$nphfb, hfb$mxfb, hfb$nhfbnp, ifelse(!is.null(hfb$option), hfb$option, ''), file=file)
  
  # data set 2
  # parameters
  if(hfb$nphfb > 0){
    for (i in 1:hfb$nphfb){
      # data set 2
      rmfi_write_variables(hfb$parnam[i], hfb$partyp[i], hfb$parval[i], hfb$nlst[i], file=file)
      
        # data set 3
        for (j in 1:hfb$nclu[i]){
          rmfi_write_variables(hfb$layer_parm[[i]][j], hfb$irow1_parm[[i]][j], hfb$icol1_parm[[i]][j], hfb$irow2_parm[[i]][j], hfb$icol2_parm[[i]][j], hfb$factor_parm[[i]][j], file=file)
          
        }
    }
  }
  
  # data set 4
  if(hfb$nhfbnp > 0){
    for(i in 1:hfb$nhfbnp){
      rmfi_write_variables(hfb$layer_noparm[i], hfb$irow1_noparm[i], hfb$icol1_noparm[i], hfb$irow2_noparm[i], hfb$icol2_noparm[i], hfb$hydchr_noparm[i], file=file)
    }
  }
  
  # data set 5
  rmfi_write_variables(hfb$nacthfb, file=file)
  
  # data set 6
  if(hfb$nacthfb > 0){
    for(i in 1:hfb$nacthfb){
      rmfi_write_variables(hfb$pname[i], file=file)
    }
  }
}