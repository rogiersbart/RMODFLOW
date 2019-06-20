
#' Write MODFLOW array parameters
#'
#' @param obj \code{RMODFLOW} object to write
#' @param arrays list of arrays to write
#' @param file filename to write to
#' @param partyp character denoting the parameter type. Only used when \code{type = 'bc'} otherwise it is derived from the arrays in \code{arrays}
#' @param type character; type of array parameter. Allowed values are \code{'bc'} for boundary-condition arrays and \code{'flow'} for flow package arrays
#' @param ... additional arguments passed to \code{rmfi_write_array} 
#'
#' @return NULL
#' @keywords internal
#' @seealso \code{\link{rmfi_parse_array_parameters}}
#' 
rmfi_write_array_parameters <- function(obj, arrays, file, partyp, type, ...) {
  
  if(type == 'bc') {
    parm_names <- names(obj$parameter_values)
    tv_parm <- structure(rep(F,obj$dimensions$np), names = parm_names)
    
    for (i in 1:obj$dimensions$np){
      
      p_name <- parm_names[i]
      arr <- arrays[[p_name]]
      
      tv_parm[i] <- (!is.null(obj$dimensions$instances) && obj$dimensions$instances[p_name] != 0)
      nclu <- ifelse(tv_parm[i], length(attr(arr[[1]], 'mlt')), length(attr(arr, 'mlt')))
      
      # headers
      rmfi_write_variables(p_name, toupper(partyp), obj$parameter_values[i], nclu, ifelse(tv_parm[i], 'INSTANCES', ''), ifelse(tv_parm[i],  obj$dimensions$instances[p_name], ''), file=file)
      
      # time-varying
      if(tv_parm[i]){
        instances <- names(arr)
        for (jj in 1:length(instances)){
          
          arr2 <- arr[[instances[jj]]]
          
          # instnam
          rmfi_write_variables(instances[jj], file=file)
          
          # clusters
          for (k in 1:nclu){
            rmfi_write_variables(toupper(attr(arr2, 'mlt')[k]), toupper(attr(arr2, 'zon')[k]), ifelse(toupper(attr(arr2, 'zon')[k]) == "ALL", '', as.numeric(attr(arr2, 'iz')[[k]])), file=file)
          }
          rm(arr2)
        }
      } else { # non-time-varying
        for (k in 1:nclu){
          rmfi_write_variables(toupper(attr(arr, 'mlt')[k]), toupper(attr(arr, 'zon')[k]), ifelse(toupper(attr(arr, 'zon')[k]) == "ALL", '', as.numeric(attr(arr, 'iz')[[k]])), file=file)
        }  
        rm(arr)
      }
    }
  } else if(type == 'flow') {
    
  }
 
}


  