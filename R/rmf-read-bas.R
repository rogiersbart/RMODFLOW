#' Read a MODFLOW basic file
#' 
#' \code{read_bas} reads in a MODFLOW basic file and returns it as an \code{\link{RMODFLOW}} bas object.
#' 
#' @param file filename; typically '*.bas'
#' @param dis discretization file object; defaults to that with the same filename but with extension '.dis'
#' @return object of class bas
#' @importFrom readr read_lines
#' @export
rmf_read_bas <- function(file = {cat('Please select bas file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}) {
  
  bas <- list()
  bas_lines <- read_lines(file)
  
  # data set 0
    data_set_0 <- rmfi_parse_comments(bas_lines)
    comment(bas) <- data_set_0$comments
    bas_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    data_set_1 <- rmfi_parse_variables(bas_lines)
    bas$xsection <- 'XSECTION' %in% data_set_1$variables
    bas$chtoch <- 'CHTOCH' %in% data_set_1$variables
    bas$free <- 'FREE' %in% data_set_1$variables
    bas$printtime <- 'PRINTTIME' %in% data_set_1$variables
    bas$showprogress <- 'SHOWPROGRESS' %in% data_set_1$variables
    bas$stoperror <- 'STOPERROR' %in% data_set_1$variables
    if(bas$stoperror) bas$stoper <- as.numeric(data_set_1$variables[match('stoperror',data_set_1$variables)+1]) else bas$stoper <- as.numeric(NA)
    bas_lines <- data_set_1$remaining_lines
    rm(data_set_1)
    
  # data set 2
    data_set_2 <- rmfi_parse_array(bas_lines,ifelse(bas$xsection,dis$nlay,dis$nrow),dis$ncol,ifelse(bas$xsection,1,dis$nlay))
    bas$ibound <- data_set_2$array
    bas_lines <- data_set_2$remaining_lines
    rm(data_set_2)
  
  # data set 3
    data_set_3 <- rmfi_parse_variables(bas_lines)
    bas$hnoflo <- data_set_3$variables
    bas_lines <- data_set_3$remaining_lines
    rm(data_set_3)
    
  # data set 4
    data_set_4 <- rmfi_parse_array(bas_lines,ifelse(bas$xsection,dis$nlay,dis$nrow),dis$ncol,ifelse(bas$xsection,1,dis$nlay))
    bas$strt <- data_set_4$array
    bas_lines <- data_set_4$remaining_lines
    rm(data_set_4)
  
  class(bas) <- c('bas','rmf_package')
  return(bas)
}

#' @describeIn rmf_read_bas Deprecated function name
#' @export
read_bas <- function(...) {
  .Deprecated(new = "rmf_read_bas", old = "read_bas")
  rmf_read_bas(...)
}
