#'
#' Read input for a MODFLOW boundary condition package which uses list-directed input
#'
#' @param lines lines as returned from readr::read_lines
#' @param dis an \code{RMODFLOW} dis object
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk)
#' @param option optional named logical vector with the names of the options besides aux
#' @param scalevar integer, indicating which column is (possibly) scaled by SFAC
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.
#' @return list with (optional) comments, icb, option, aux and rmf_lists
#' @keywords internal
#' @seealso \code{\link{rmfi_create_bc_list}}, \code{\link{rmfi_write_bc_list}}


rmfi_read_bc_list <- function(lines, dis, varnames, option, scalevar, ...) {
  
  rmf_lists <- list()
  
  # data set 0
  data_set_0 <-  rmfi_parse_comments(lines)
  comments <-  data_set_0$comments
  lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <-  rmfi_parse_variables(lines)
  
  if('PARAMETER' %in% data_set_1$variables) {
    np_def <-  as.numeric(data_set_1$variables[2])
    lines <-  data_set_1$remaining_lines
  }  else {
    np_def <- 0
  }
  rm(data_set_1)
  
  # data set 2
  data_set_2 <-  rmfi_parse_variables(lines, n=2, ...)
  icb <-  as.numeric(data_set_2$variables[2])
  option[] <- FALSE
  if(length(data_set_2$variables) > 2) {
    if(any(c(names(option), "AUX", "AUXILIARY") %in% data_set_2$variables[3:length(data_set_2$variables)])) {
      option <- vapply(names(option), function(i) i %in% data_set_2$variables, TRUE)
      aux <- as.character(data_set_2$variables[pmatch('AUX', data_set_2$variables)+1])
    }
  }
  lines <-  data_set_2$remaining_lines
  rm(data_set_2)
  
  # parameters
  if(np_def > 0){
    
    i <- 1
    while(i <= np_def){
      # data set 3
      data_set_3 <-  rmfi_parse_variables(lines)
      p_name <-   as.character(data_set_3$variables[1])
      p_val <-  as.numeric(data_set_3$variables[3])
      p_nlst <- as.numeric(data_set_3$variables[4])
      p_tv <- NULL
      if(length(data_set_3$variables) > 4){
        p_tv <- TRUE
        p_numinst = as.numeric(data_set_3$variables[6])
      } 
      lines <- data_set_3$remaining_lines
      rm(data_set_3)
      
      
      # time-varying parameters
      if(!is.null(p_tv) && p_tv){
        
        j=1
        while(j <= p_numinst){
          # data set 4a
          data_set_4a <- rmfi_parse_variables(lines)
          instnam <- as.character(data_set_4a$variables)
          lines <-  data_set_4a$remaining_lines
          rm(data_set_4a)
          
          # data set 4b
          data_set_4b <- rmfi_parse_list(lines, nlst = p_nlst, varnames = rmfi_ifelse0(is.null(aux), varnames, c(varnames, aux)), scalevar = scalevar, file = file, ...)
          rmf_lists[[length(rmf_lists)+1]] <- rmf_create_list_parameter(data_set_4b$list, name = p_name, value = p_val, instnam = instnam)
          
          lines <- data_set_4b$remaining_lines
          rm(data_set_4b)
          
          j <-  j+1
        } 
        
      } else {
        # non time-varying
        # data set 4b
        data_set_4b <- rmfi_parse_list(lines, nlst = p_nlst, varnames = rmfi_ifelse0(is.null(aux), varnames, c(varnames, aux)), scalevar = scalevar, file = file, ...)
        rmf_lists[[length(rmf_lists)+1]] <- rmf_create_list_parameter(data_set_4b$list, name = p_name, value = p_val)
        
        lines <- data_set_4b$remaining_lines
        rm(data_set_4b)
      }
      
      i <-  i+1
    }
  }
  
  # stress periods
  
  # function for setting kper attribute for parameters
  set_kper <- function(k, kper, p_name, i_name) {
    if(!is.null(attr(k, 'name')) && attr(k, 'name') == p_name) {
      if(!is.null(i_name)) {
        if(attr(k, "instnam") == i_name) attr(k, 'kper') <- c(attr(k, 'kper'), kper)
      } else {
        attr(k, 'kper') <- c(attr(k, 'kper'), kper)
      }
    }
    return(k)
  }
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 <-  rmfi_parse_variables(lines, n=2, ...)
    itmp <- as.numeric(data_set_5$variables[1])
    np <- as.numeric(data_set_5$variables[2])
    lines <- data_set_5$remaining_lines
    rm(data_set_5)
    
    if(itmp > 0){
      data_set_6 <- rmfi_parse_list(lines, nlst = itmp, varnames = rmfi_ifelse0(is.null(aux), varnames, c(varnames, aux)), scalevar = scalevar, file = file, ...)
      rmf_lists[[length(rmf_lists)+1]] <- structure(data_set_6$list, kper = i)
      # to do : see if list already exists; then just add kper to attribute
      lines <-  data_set_6$remaining_lines
      rm(data_set_6)
    } else if(i > 1 && itmp < 0) {
      attr(rmf_lists[[length(rmf_lists)]], 'kper') <- c(attr(rmf_lists[[length(rmf_lists)]], 'kper'), i)
    }
    
    if(np > 0){
      for(j in 1:np){
        # data set 7
        data_set_7 <-  rmfi_parse_variables(lines)
        p_name <-  as.character(data_set_7$variables[1])
        i_name <- rmfi_ifelse0(length(data_set_7$variables) > 1, as.character(data_set_7$variables[2]), NULL)
        
        rmf_lists <- lapply(rmf_lists, set_kper, p_name = p_name, i_name = i_name, kper = i)
        
        lines <- data_set_7$remaining_lines
        rm(data_set_7)
      }
    }
  }
  
  return(list(comments = comments, icb = icb, option = option, aux = aux, rmf_lists = rmf_lists))
}