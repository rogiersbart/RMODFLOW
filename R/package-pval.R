#' Create an \code{RMODFLOW} pvl object
#' 
#' \code{rmf_create_pvl} creates an \code{RMODFLOW} pvl object
#' 
#' @param np number of parameters; defaults to NULL
#' @param parnam character vector of length \code{np} specifying the parameter names; defaults to NULL
#' @param parval numeric vector of length \code{np} specifying the parameter values; defaults to NULL+
#' 
#' @return an \code{RMODFLOW} pvl object
#' @export
#' @seealso \code{\link{rmf_read_pvl}}, \code{\link{rmf_write_pvl}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?pvl.htm}

rmf_create_pvl = function(np = NULL,
                          parnam = NULL, 
                          parval = NULL){
  
  pvl = list()
  
  # data set 0
  # to provide comments, use ?comment on resulting pvl object
  
  # data set 1
  pvl$np = np
  
  # data set 2
  pvl$parnam = parnam
  pvl$parval = parval
  
  class(pvl) = c('pvl', 'rmf_package')
  return(pvl)
  
  
}

#' Read a MODFLOW parameter value file
#' 
#' \code{read_pvl} reads in a MODFLOW parameter value file and returns it as an \code{\link{RMODFLOW}} pvl object.
#' 
#' @param file filename; typically '*.pvl'
#' @param read_all logical, indicating if \code{np} parameters should be read, or the full parameter table (only relevant if external codes use the pvl file for storing additional parameters).
#' @return object of class pvl
#' @importFrom readr read_lines
#' @export
rmf_read_pvl <- function(file = {cat('Please select pvl file ...\n'); file.choose()},
                         read_all=F) {
  
  pvl_lines <- read_lines(file)
  pvl <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(pvl_lines)
  comment(pvl) <- data_set_0$comments
  pvl_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  ifelse(read_all, pvl$np <- length(pvl_lines)-1, pvl$np <- as.numeric(pvl_lines[1]))
  pvl_lines <- pvl_lines[-1]
  
  # data set 2
  for(i in 1:pvl$np) {
    pvl$parnam[i] <- as.character(strsplit(pvl_lines[1],' ')[[1]][1])
    pvl$parval[i] <- as.numeric(rmfi_remove_empty_strings(strsplit(pvl_lines[1],' ')[[1]])[2])
    pvl_lines <- pvl_lines[-1]
  }
  
  class(pvl) <- c('pvl','rmf_package')
  return(pvl)
}

#' @describeIn rmf_read_pvl Deprecated function name
#' @export
read_pvl <- function(...) {
  .Deprecated(new = "rmf_read_pvl", old = "read_pvl")
  rmf_read_pvl(...)
}

#' Write a MODFLOW parameter value file
#' 
#' @param pvl an \code{\link{RMODFLOW}} pvl object
#' @param file filename to write to; typically '*.pvl'
#' @return \code{NULL}
#' @export
rmf_write_pvl <- function(pvl,
                          file = {cat('Please select pvl file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Parameter Value File created by RMODFLOW, version',v,'at',date(),'\n'), file=file)
  cat(paste('#', comment(pvl)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(pvl$np, file=file)
  
  # data set 2
  for(i in 1:pvl$np) {
    rmfi_write_variables(pvl$parnam[i], pvl$parval[i], file=file)
  }  
}

#' @describeIn rmf_write_pvl Deprecated function name
#' @export
write_pvl <- function(...) {
  .Deprecated(new = "rmf_write_pvl", old = "write_pvl")
  rmf_write_pvl(...)
}
