#' Create an \code{RMODFLOW} pval object
#' 
#' \code{rmf_create_pval} creates an \code{RMODFLOW} pval object
#' 
#' @param parnam character vector specifying the parameter names; defaults to NULL
#' @param parval numeric vector specifying the parameter values; defaults to NULL
#' @param np number of MODFLOW-supported parameters; defaults to `length(parnam)`
#' @details parnam & parval should be of the same length. Extra parameters that
#'   are not supported by MODFLOW can be introduced, but np should be adjusted
#'   accordingly (*i.e.* not accounting for the extra parameters). For working
#'   with these extra parameters, see the `preprocess` argument to
#'   `rmf_execute()`.
#' @return an \code{RMODFLOW} pval object
#' @export
#' @seealso \code{\link{rmf_read_pval}}, \code{\link{rmf_write_pval}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?parameter_value_file.htm}

rmf_create_pval = function(parnam, 
                           parval,
                           np = length(parnam)){
  
  pval <-  list()
  
  # data set 0
  # to provide comments, use ?comment on resulting pval object
  
  # data set 1
  pval$np <- np
  
  # data set 2
  pval$parnam <-  parnam
  pval$parval <-  parval
  
  class(pval) = c('pval', 'rmf_package')
  return(pval)
  
}

#' Read a MODFLOW parameter value file
#' 
#' \code{rmf_read_pval} reads in a MODFLOW parameter value file and returns it as an \code{\link{RMODFLOW}} pval object.
#' 
#' @param path Path to the PVAL file. Typically with extension `.pval`.
#' @return object of class pval
#' @export
#' @seealso \code{\link{rmf_create_pval}}, \code{\link{rmf_write_pval}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?pval.htm}
rmf_read_pval <- function(path) {
  
  pval_lines <- readr::read_lines(path)
  pval <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(pval_lines)
  comment(pval) <- data_set_0$comments
  pval_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  pval$np <- as.numeric(pval_lines[1])
  pval_lines <- pval_lines[-1]
  
  # data set 2
  for(i in 1:length(pval_lines)) {
    pval$parnam[i] <- as.character(strsplit(pval_lines[1],' ')[[1]][1])
    pval$parval[i] <- as.numeric(rmfi_remove_empty_strings(strsplit(pval_lines[1],' ')[[1]])[2])
    pval_lines <- pval_lines[-1]
  }
  
  class(pval) <- c('pval','rmf_package')
  return(pval)
}

#' Write a MODFLOW parameter value file
#' 
#' @param pval an \code{\link{RMODFLOW}} pval object
#' @param path Path to write the PVAL file. Typically with extension `.pval`.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_create_pval}}, \code{\link{rmf_read_pval}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?pval.htm}
rmf_write_pval <- function(pval,
                           path) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Parameter Value File created by RMODFLOW, version',v,'at',date(),'\n'), file = path)
  cat(paste('#', comment(pval)), sep='\n', file = path, append=TRUE)
  
  # data set 1
  rmfi_write_variables(pval$np, file = path, integer = TRUE)
  
  # data set 2
  for(i in 1:length(pval$parnam)) {
    rmfi_write_variables(pval$parnam[i], pval$parval[i], file = path)
  }  
}
