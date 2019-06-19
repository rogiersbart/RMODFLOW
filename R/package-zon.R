#' Create an \code{RMODFLOW} zon object
#' 
#' \code{rmf_create_zon} creates an \code{RMODFLOW} zon object
#' 
#' @param nzn number of zone arrays to be defined; defaults to 1
#' @param zonnam character vector of length \code{nzn} specifying the names of zone arrays; defaults to 'ZONE'
#' @param izon list with \code{nzn} elements where each element is a \code{rmf_2d_array} specifying a zone array; defaults to a \code{rmf_2d_array} with 1 for all cells
#'
#' @return an \code{RMODFLOW} zon object
#' @export
#' @seealso \code{\link{rmf_read_zon}}, \code{\link{rmf_write_zon}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?zone.htm}

rmf_create_zon <-  function(nzn = 1,
                      zonnam = 'ZONE',
                      izon = list(rmf_create_array(1L, dim=c(10, 10)))
                      ){
  
  zon <- list()
  
  # data set 
  # to provide comments, use ?comment on resulting zon object
  
  # data set 1
  zon$nzn <-  nzn
  
  # data set 2
  zon$zonnam <-  zonnam
  
  # data set 3
  zon$izon <-  lapply(izon, function(i) apply(i, MARGIN = 1:length(dim(i)), function(x) as.integer(x)))
  
  class(zon) <-  c('zon', 'modflow_package')
  return(zon)
  
}

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

#' Write a MODFLOW zone file
#' 
#' \code{rmf_write_zon} writes a MODFLOW zone file based on a \code{RMODFLOW} zon object
#' 
#' @param zon an \code{RMODFLOW} zon object
#' @param file filename to write to; typically '*.zon'
#' @param ... arguments passed to \code{rmfi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_zon}}, \code{\link{rmf_create_zon}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?zone.htm}

rmf_write_zon <-  function(zon, file = {cat('Please choose zon file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Zone File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(zon)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(zon$nzn, file=file)
  
  for (i in 1:zon$nzn){
    
    # data set 2
    rmfi_write_variables(zon$zonnam[i], file=file)
    
    # data set 3
    rmfi_write_array(zon$izon[[i]], file=file, ...)
    
  }
}