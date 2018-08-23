#' Read a MODFLOW Newton solver package file
#' 
#' \code{rmf_read_nwt} reads in a MODFLOW Newton solver package file and returns it as an \code{\link{RMODFLOW}} nwt object.
#' 
#' @param file filename; typically '*.nwt'
#' @return object of class nwt
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_nwt}}, \code{\link{rmf_create_nwt}} and \url{https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/}
rmf_read_nwt <- function(file = {cat('Please select nwt file ...\n'); file.choose()}) {
  
  nwt_lines <- read_lines(file)
  nwt <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(nwt_lines)
  comment(nwt) <- data_set_0$comments
  nwt_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(nwt_lines)
  nwt$headtol <- as.numeric(data_set_1$variables[1])
  nwt$fluxtol <- as.numeric(data_set_1$variables[2])
  nwt$maxiterout <- as.numeric(data_set_1$variables[3])
  nwt$thickfact <- as.numeric(data_set_1$variables[4])
  nwt$linmeth <- as.numeric(data_set_1$variables[5])
  nwt$iprnwt <- as.numeric(data_set_1$variables[6])
  nwt$ibotav <- as.numeric(data_set_1$variables[7])
  nwt$options <- toupper(as.character(data_set_1$variables[8]))
  
  if(nwt$options == "SPECIFIED") {
    nwt$dbdtheta <- as.numeric(data_set_1$variables[9])
    nwt$dbdkappa <- as.numeric(data_set_1$variables[10])
    nwt$dbdgamma <- as.numeric(data_set_1$variables[11])
    nwt$momfact <- as.numeric(data_set_1$variables[12])
    nwt$backflag <- as.numeric(data_set_1$variables[13])
    nwt$maxbackiter <- as.numeric(data_set_1$variables[14])
    nwt$backtol <- as.numeric(data_set_1$variables[15])
    nwt$backreduce <- as.numeric(data_set_1$variables[16])
    nwt_lines <- data_set_1$remaining_lines
    rm(data_set_1)
    
    data_set_2 <- rmfi_parse_variables(nwt_lines)
    
    # data set 2a
    if(nwt$linmeth == 1) {
      nwt$maxitinner <- as.numeric(data_set_2$variables[1])
      nwt$ilumethod <- as.numeric(data_set_2$variables[2])
      nwt$levfill <- as.numeric(data_set_2$variables[3])
      nwt$stoptol <- as.numeric(data_set_2$variables[4])
      nwt$msdr <- as.numeric(data_set_2$variables[5])
      
    # data set 2b
    } else if(nwt$linmeth == 2) {
      nwt$iacl <- as.numeric(data_set_2$variables[1])
      nwt$norder <- as.numeric(data_set_2$variables[2])
      nwt$level <- as.numeric(data_set_2$variables[3])
      nwt$north <- as.numeric(data_set_2$variables[4])
      nwt$iredsys <- as.numeric(data_set_2$variables[5])
      nwt$rrctols <- as.numeric(data_set_2$variables[6])
      nwt$idroptol <- as.numeric(data_set_2$variables[7])
      nwt$epsrn <- as.numeric(data_set_2$variables[8])
      nwt$hclosexmd <- as.numeric(data_set_2$variables[9])
      nwt$mxiterxmd <- as.numeric(data_set_2$variables[10])
      
    }
    rm(data_set_2)
    
  }

  class(nwt) <- c('nwt','rmf_package')
  return(nwt)
}

