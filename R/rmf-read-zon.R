#' Read a MODFLOW zone file
#' 
#' \code{rmf_read_zon} reads in a MODFLOW zone file and returns it as an \code{RMODFLOW} zon object
#' 
#' @param file filename; typically '*.zon'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} zon object
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_zon}}, \code{\link{rmf_create_zon}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?zone.htm}

rmf_read_zon <-  function(file = {cat('Please select zon file ...\n'); file.choose()},
                        dis = {cat('Please select dis file ...\n'); rmf_read_dis(file.choose())},
                        ...){
  
  zon <-  list()
  zon_lines <-  read_lines(file)
  
  # data set 0
  data_set_0 <-  rmfi_parse_comments(zon_lines)
  comment(zon) <-  data_set_0$comments
  zon_lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(zon_lines)
  zon$nzn <- data_set_1$variables[1]
  zon_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2 + 3
  zon$izon <- list()
  for(i in 1:zon$nzn){
    
    # data set 2
    data_set_2 <- rmfi_parse_variables(zon_lines)
    zon$zonnam[i] <- as.character(data_set_2$variables[1])
    zon_lines <- data_set_2$remaining_lines
    rm(data_set_2)
    
    # data set 3
    data_set_3 <- rmfi_parse_array(zon_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = 1, file = file, integer = TRUE, ...)
    zon$izon[[i]] <- apply(data_set_3$array, 1:length(dim(data_set_3$array)), function(i) as.integer(i))
    zon_lines <- data_set_3$remaining_lines
    rm(data_set_3)
  }
  zon$izon <- lapply(zon$izon, rmf_create_array)
  class(zon) = c('zon', 'rmf_package')
  return(zon)
  
}