#' Read a MODFLOW horizontal flow barrier file
#' 
#' \code{rmf_read_hfb} reads in a MODFLOW horizontal flow barrier file and returns it as an \code{RMODFLOW} hfb object.
#'
#' @param file filename; typically '*.hfb'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_variables} and \code{rmfi_parse_list}.
#'  
#' @return \code{RMODFLOW} hfb object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}

rmf_read_hfb <-  function(file = {cat('Please select horizontal flow barrier file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('irow2', 'icol2', 'hydchr')
  option <- c('NOPRINT' = FALSE)
  lines <-  read_lines(file)
  scalevar <- 6
  
  rmf_lists <- list()
  
  # data set 0
  data_set_0 <-  rmfi_parse_comments(lines)
  comments <-  data_set_0$comments
  lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <-  rmfi_parse_variables(lines)
  np <- as.numeric(data_set_1$variables[1])
  nnp <- as.numeric(data_set_1$variables[3])
  if('NOPRINT' %in% toupper(as.character(data_set_1$variables))) option['NOPRINT'] <- TRUE
  lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2 & 3
  if(np > 0) {
    
    for(i in 1:np) {
      data_set_2 <- rmfi_parse_variables(lines)
      parnam <- as.character(data_set_2$variables[1])
      parval <- as.numeric(data_set_2$variables[3])
      nlst <- as.numeric(data_set_2$variables[4])
      lines <- data_set_2$remaining_lines
      rm(data_set_2)
      
      data_set_3 <- rmfi_parse_list(lines, nlst = nlst, varnames = vars, scalevar = scalevar, file = file, ...)
      rmf_lists[[length(rmf_lists)+1]] <- rmf_create_list_parameter(data_set_3$list, parnam = parnam, parval = parval)
      lines <- data_set_3$remaining_lines
      rm(data_set_3)

    }
  }
  
  # data set 4
  data_set_4 <- rmfi_parse_list(lines, nlst = nnp, varnames = vars, scalevar = scalevar, file = file, ...)
  rmf_lists[[length(rmf_lists)+1]] <- structure(data_set_4$list, kper = 1:dis$nper)
  lines <- data_set_4$remaining_lines
  rm(data_set_4)
 
  # data set 5
  data_set_5 <- rmfi_parse_variables(lines)
  nacthfb <- as.numeric(data_set_5$variables[1])
  lines <- data_set_5$remaining_lines
  rm(data_set_5)
    
  # data set 6
  acthfb <- vector(mode = 'character', length = nacthfb)
  for(i in 1:nacthfb) {
    data_set_6 <- rmfi_parse_variables(lines)
    acthfb[i] <- data_set_6$variables[1]
    lines <- data_set_6$remaining_lines
    rm(data_set_6)
  }
  
  # set kper for parameters
  rmf_lists <- lapply(rmf_lists, function(i) rmfi_ifelse0(inherits(i, 'rmf_parm') && (attr(i, 'parnam') %in% acthfb), structure(i, kper = 1:dis$nper), i))
  
  # create hfb
  obj <- rmf_create_hfb(rmf_lists, dis = dis, noprint = unname(option['NOPRINT']))
  comment(obj) <- comments
  return(obj)
}

