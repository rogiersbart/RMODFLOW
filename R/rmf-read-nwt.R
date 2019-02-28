#' Read a MODFLOW Newton solver package file
#' 
#' \code{rmf_read_nwt} reads in a MODFLOW Newton solver package file and returns it as an \code{\link{RMODFLOW}} nwt object.
#' 
#' @param file filename; typically '*.nwt'
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return object of class nwt
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_nwt}}, \code{\link{rmf_create_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_read_nwt <- function(file = {cat('Please select nwt file ...\n'); file.choose()}, ...) {
  
  nwt_lines <- read_lines(file)
  nwt <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(nwt_lines)
  comment(nwt) <- data_set_0$comments
  nwt_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(nwt_lines, ...)
  nwt$headtol <- rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  nwt$fluxtol <- rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  nwt$maxiterout <- rmfi_ifelse0(is.na(data_set_1$variables[3]), 0, as.numeric(data_set_1$variables[3]))
  nwt$thickfact <- rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4]))
  nwt$linmeth <- rmfi_ifelse0(is.na(data_set_1$variables[5]), 0, as.numeric(data_set_1$variables[5]))
  nwt$iprnwt <- rmfi_ifelse0(is.na(data_set_1$variables[6]), 0, as.numeric(data_set_1$variables[6]))
  nwt$ibotav <- rmfi_ifelse0(is.na(data_set_1$variables[7]), 0, as.numeric(data_set_1$variables[7]))
  nwt$options <- toupper(as.character(data_set_1$variables[8]))
  
  if(nwt$options == "SPECIFIED") {
    nwt$dbdtheta <- rmfi_ifelse0(is.na(data_set_1$variables[9]), 0, as.numeric(data_set_1$variables[9]))
    nwt$dbdkappa <- rmfi_ifelse0(is.na(data_set_1$variables[10]), 0, as.numeric(data_set_1$variables[10]))
    nwt$dbdgamma <- rmfi_ifelse0(is.na(data_set_1$variables[11]), 0, as.numeric(data_set_1$variables[11]))
    nwt$momfact <- rmfi_ifelse0(is.na(data_set_1$variables[12]), 0, as.numeric(data_set_1$variables[12]))
    nwt$backflag <- rmfi_ifelse0(is.na(data_set_1$variables[13]), 0, as.numeric(data_set_1$variables[13]))
    nwt$maxbackiter <- rmfi_ifelse0(is.na(data_set_1$variables[14]), 0, as.numeric(data_set_1$variables[14]))
    nwt$backtol <- rmfi_ifelse0(is.na(data_set_1$variables[15]), 0, as.numeric(data_set_1$variables[15]))
    nwt$backreduce <- rmfi_ifelse0(is.na(data_set_1$variables[16]), 0, as.numeric(data_set_1$variables[16]))
    nwt_lines <- data_set_1$remaining_lines
    rm(data_set_1)
    
    data_set_2 <- rmfi_parse_variables(nwt_lines, ...)
    
    # data set 2a
    if(nwt$linmeth == 1) {
      nwt$maxitinner <- rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
      nwt$ilumethod <- rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
      nwt$levfill <- rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
      nwt$stoptol <- rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
      nwt$msdr <- rmfi_ifelse0(is.na(data_set_2$variables[5]), 0, as.numeric(data_set_2$variables[5]))
      
    # data set 2b
    } else if(nwt$linmeth == 2) {
      nwt$iacl <- rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
      nwt$norder <- rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
      nwt$level <- rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
      nwt$north <- rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
      nwt$iredsys <- rmfi_ifelse0(is.na(data_set_2$variables[5]), 0, as.numeric(data_set_2$variables[5]))
      nwt$rrctols <- rmfi_ifelse0(is.na(data_set_2$variables[6]), 0, as.numeric(data_set_2$variables[6]))
      nwt$idroptol <- rmfi_ifelse0(is.na(data_set_2$variables[7]), 0, as.numeric(data_set_2$variables[7]))
      nwt$epsrn <- rmfi_ifelse0(is.na(data_set_2$variables[8]), 0, as.numeric(data_set_2$variables[8]))
      nwt$hclosexmd <- rmfi_ifelse0(is.na(data_set_2$variables[9]), 0, as.numeric(data_set_2$variables[9]))
      nwt$mxiterxmd <- rmfi_ifelse0(is.na(data_set_2$variables[10]), 0, as.numeric(data_set_2$variables[10]))
      
    }
    rm(data_set_2)
    
  }

  class(nwt) <- c('nwt','rmf_package')
  return(nwt)
}

