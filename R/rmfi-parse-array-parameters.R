
#' Read MODFLOW array parameters
#'
#' @param lines lines to read the parameter arrays from
#' @param dis \code{RMODFLOW} dis object
#' @param np numeric; number of parameters to read
#' @param type character; type of array parameter. Allowed values are \code{'bc'} for boundary-condition arrays and \code{'flow'} for flow package arrays
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#'
#' @return A list containing the parameter arrays and the remaining text of the MODFLOW input file
#' @keywords internal
#' @seealso \code{\link{rmfi_write_array_parameters}}
#' 
rmfi_parse_array_parameters <- function(lines, dis, np, type, mlt = NULL, zon = NULL) {
  
  parm_list <- list()
  
  if(type == 'bc') {
    i <- 1
    while(i <= np){
      
      # data set 3
      data_set_3 <- rmfi_parse_variables(lines)
      parnam <-   as.character(data_set_3$variables[1])
      parval <-  as.numeric(data_set_3$variables[3])
      nclu <- as.numeric(data_set_3$variables[4])
      p_tv <- NULL
      if(length(data_set_3$variables) > 4){
        p_tv <- TRUE
        numinst = as.numeric(data_set_3$variables[6])
        arr <- list()
      } 
      lines <- data_set_3$remaining_lines
      rm(data_set_3)
      
      mltarr <- zonarr <- vector(mode = 'character', length = nclu)
      iz <- as.list(rep(0, nclu))
      
      # time-varying parameters
      if(!is.null(p_tv) && p_tv){
        
        # loop over instances
        for(j in 1:numinst){
          
          # data set 4a
          data_set_4a <- rmfi_parse_variables(lines)
          instnam <- as.character(data_set_4a$variables)
          lines <-  data_set_4a$remaining_lines
          rm(data_set_4a)
          
          # loop over clusters
          for(k in 1:nclu) {
            # data set 4b
            data_set_4b <- rmfi_parse_variables(lines)
            mltarr[k] <- toupper(data_set_4b$variables[1])
            zonarr[k] <- toupper(data_set_4b$variables[2])
            
            if(toupper(data_set_4b$variables[1]) != 'NONE') {
              if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
              
            }
            if(toupper(data_set_4b$variables[2]) != 'ALL') {
              if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
              iz[[k]] <- as.numeric(data_set_4b$variables[3:length(data_set_4b$variables)])
            }
            lines <- data_set_4b$remaining_lines
            rm(data_set_4b)
          }
        } 
        
      } else {
        # non time-varying
        # loop over clusters
        for(k in 1:nclu) {
          # data set 4b
          data_set_4b <- rmfi_parse_variables(lines)
          mltarr[k] <- toupper(data_set_4b$variables[1])
          zonarr[k] <- toupper(data_set_4b$variables[2])
          
          if(toupper(data_set_4b$variables[1]) != 'NONE') {
            if(is.null(mlt)) stop('Please provide a mlt object', call. = FALSE)
            
          }
          if(toupper(data_set_4b$variables[2]) != 'ALL') {
            if(is.null(zon)) stop('Please provide a zon object', call. = FALSE)
            iz[[k]] <- as.numeric(data_set_4b$variables[3:length(data_set_4b$variables)])
          }
          lines <- data_set_4b$remaining_lines
          rm(data_set_4b)
        }
      }
      
      parm_list[[length(parm_list)+1]] <- rmf_create_parameter(dis = dis, mlt = mlt, mltnam = mltarr, zon = zon, zonnam = zonarr, iz = iz, instnam = rmfi_ifelse0(!is.null(p_tv) && p_tv, instnam, NULL),
                                                               parval = parval, parnam = parnam, kper = 1:dis$nper)
      names(parm_list)[length(parm_list)] <- parnam
      i <- i+1
    }
  } else if(type == 'flow') {
    
  }
 
  return(list(parameters = parm_list, remaining_lines = lines))
}
