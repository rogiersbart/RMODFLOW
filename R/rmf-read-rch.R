#' Read a MODFLOW recharge file
#' 
#' \code{rmf_read_rch} reads in a MODFLOW recharge file and returns it as an \code{RMODFLOW} rch object.
#'
#' @param file filename; typically '*.rch'
#' @param dis an \code{RMODFLOW} dis object
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_write_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_read_rch <-  function(file = {cat('Please select rch file ...\n'); file.choose()},
                        dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                        mlt = NULL,
                        zon = NULL,
                        ... ){

    lines <- readr::read_lines(file)
    rmf_arrays <- list()
    
    # data set 0
    data_set_0 <-  rmfi_parse_comments(lines)
    comments <-  data_set_0$comments
    lines <-  data_set_0$remaining_lines
    rm(data_set_0)
    
    # data set 1
    data_set_1 <-  rmfi_parse_variables(lines)
    
    if('PARAMETER' %in% data_set_1$variables) {
      np <-  as.numeric(data_set_1$variables[2])
      lines <-  data_set_1$remaining_lines
    }  else {
      np <- 0
    }
    rm(data_set_1)
    
    # data set 2
    data_set_2 <-  rmfi_parse_variables(lines, n=2, ...)
    nrchop <- as.numeric(data_set_2$variables[1])
    irchcb <- as.numeric(data_set_2$variables[2])
    lines <-  data_set_2$remaining_lines
    rm(data_set_2)
    irch <- rmfi_ifelse0(nrchop == 2, list(), NULL)
    
    # parameters: data set 3 & 4
    if(np > 0) {
      data_set_3 <- rmfi_parse_array_parameters(lines, dis = dis, np = np, type = 'bc', mlt = mlt, zon = zon)
      rmf_arrays <- data_set_3$parameters
      lines <- data_set_3$remaining_lines
      rm(data_set_3)
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
      inrech <- as.numeric(data_set_5$variables[1])
      inirch <- as.numeric(data_set_5$variables[2])
      lines <- data_set_5$remaining_lines
      rm(data_set_5)
      
      # data set 6-8
      if(np == 0) {
        
        if(inrech >= 0) {
          data_set_6 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, ...)
          rmf_arrays[[length(rmf_arrays) + 1]] <- structure(data_set_6$array, kper = i)
          lines <- data_set_6$remaining_lines
          rm(data_set_6)
        } else if(inrech < 0 && i > 1) {
          attr(rmf_arrays[[length(rmf_arrays)]], 'kper') <- c(attr(rmf_arrays[[length(rmf_arrays)]], 'kper'), i)
        }
        
      } else {
        for(j in 1:np){
          # data set 7
          data_set_7 <-  rmfi_parse_variables(lines)
          p_name <-  as.character(data_set_7$variables[1])
          irchpf <- NULL
          if(!is.null(attr(rmf_arrays[[p_name]], 'instnam'))) {
            i_name <- data_set_7$variables[2]
            if(length(data_set_7$variables) > 2) irchpf[i] <- as.numeric(data_set_7$variables[3])
          } else {
            i_name <- NULL
            if(length(data_set_7$variables) > 1) irchpf[i] <- as.numeric(data_set_7$variables[2])
          }
          
          rmf_arrays <- lapply(rmf_arrays, set_kper, p_name = p_name, i_name = i_name, kper = i)
          
          lines <- data_set_7$remaining_lines
          rm(data_set_7)
          
        }
      }
      
      # data set 8
      if(nrchop == 2) {
        if(inirch >= 0) {
          data_set_8 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, ...)
          irch[[length(irch) + 1]] <- structure(data_set_8$array, kper = i)
          lines <- data_set_8$remaining_lines
          rm(data_set_8)
        } else if(inirch < 0 && i > 1) {
          attr(irch[[length(irch)]], 'kper') <- c(attr(irch[[length(irch)]], 'kper'), i)
        }
      }
    }
    
    rch <- rmf_create_rch(rmf_arrays, dis = dis, nrchop = nrchop, irchcb = irchcb, irch = irch, irchpf = rmfi_ifelse0(is.null(irchpf), -1, irchpf))
    comment(rch) <- comments
    return(rch)
}
