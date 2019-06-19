#' Write a MODFLOW horizontal flow barrier file
#'
#' \code{rmf_write_hfb} writes a MODFLOW horizontal flow barrier file based on an \code{RMODFLOW} hfb object
#' 
#' @param hfb an \code{RMODFLOW} hfb object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.hfb'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file.
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}


rmf_write_hfb<-  function(hfb, dis = rmf_read_dis(), file={cat('Please choose hfb file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('irow2', 'icol2', 'hydchr')
  header <-  'Horizontal Flow Barrier Package'
  package <- 'hfb'
  partyp <- 'HFB'
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW', header, 'created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(hfb)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(hfb$dimensions$np, hfb$dimensions$mxl, hfb$dimensions$nnp, ifelse(hfb$option['noprint'], 'NOPRINT', ''), file=file)
  
  # parameters
  if(hfb$dimensions$np > 0){
    parm_names <- names(hfb$parameter_values)

    for (i in 1:hfb$dimensions$np){
      p_name <- parm_names[i]
      df <- subset(hfb$data, name == p_name)
      nlst <- nrow(df)
      
      # data set 2
      rmfi_write_variables(p_name, toupper(partyp), hfb$parameter_values[i], nlst, file=file)
      # data set 3
      for (k in 1:nlst){
        rmfi_write_variables(df$k[k], df$i[k], df$j[k], df[k, vars], file=file)
      }
      rm(df)
    }
  }
  
  # data set 4
  df <- subset(hfb$data, parameter == FALSE)
  if(nrow(df) > 0) {
    for(j in 1:nrow(df)){
      rmfi_write_variables(df$k[j], df$i[j], df$j[j], df[j, vars], file=file)
    }
    rm(df)
  }

  # data set 5
  rmfi_write_variables(hfb$dimensions$nacthfb, file = file)
  
  # data set 6
  if(hfb$dimensions$nacthfb > 0){
    for(j in 1:hfb$dimensions$nacthfb){
      rmfi_write_variables(hfb$dimensions$acthfb[j], file=file)
    }
  }
  
  
}
