
#' Create a \code{RMODFLOW} lmt object
#'
#' @param fname character; name of the flow-transport link file produced. Defaults to \code{'output.ftl'}
#' @param inftl integer; unit number on which the flow-transport link file will be saved; defaults to 333
#' @param extended logical; should the extended header be used (instead of the standard header); defaults to TRUE
#' @param formatted logical; should the file be formatted. Under the default (FALSE), an unformatted flow-transport link file will be created by lmt 
#' @param package_flows optional; additional flows to be saved in the flow-transport link file. Possible values are \code{'ALL'} or any combination of \code{'UZF', 'SFR' or 'LAK'}. Defaults to not including any of the above.
#'
#' @return object of class lmt
#' @export
#'
#' @seealso \code{\link{rmf_read_lmt}}, \code{\link{rmf_write_lmt}} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?lmt6.htm}
#' @examples 
#' rmf_create_lmt(fname = file.path('..', 'transport', 'output.ftl'), formatted = TRUE, package_flows = c('UZF', 'LAK'))
#' 
rmf_create_lmt <- function(fname = 'output.ftl',
                           inftl = 333,
                           extended = TRUE,
                           formatted = FALSE,
                           package_flows = NULL) {
  
  lmt <- list()
  
  lmt$fname <- fname
  lmt$inftl <- inftl
  lmt$extended <- extended
  lmt$formatted <- formatted
  lmt$package_flows <- package_flows
  
  class(lmt) <- c('lmt', 'rmf_package')
  return(lmt)
}

#' Read a MODFLOW Link-MT3DMS Package file
#'
#' @param file filename; typically '*.lmt'
#' @param ... ignored
#'
#' @return object of class lmt
#' @export
#'
#' @seealso \code{\link{rmf_create_lmt}}, \code{\link{rmf_write_lmt}} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?lmt6.htm}
#' @examples 
#' file <- rmf_example_file('rocky-mountain-arsenal.lmt')
#' rmf_read_lmt(file)
#' 
rmf_read_lmt <- function(file={cat('Please select lmt file ...\n'); file.choose()}, ...) {
  
  lmt_lines <- readr::read_lines(file, lazy = FALSE)
  lmt <- list()
  
  # data set 0
  data_set_0 <- rmfi_parse_comments(lmt_lines)
  comment(lmt) <- data_set_0$comments
  lmt_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  lmt$fname <- grep('OUTPUT_FILE_NAME', toupper(lmt_lines))
  lmt$inftl <- grep('OUTPUT_FILE_UNIT', toupper(lmt_lines))
  lmt$extended <- grep('OUTPUT_FILE_HEADER', toupper(lmt_lines))
  lmt$formatted <- grep('OUTPUT_FILE_FORMAT', toupper(lmt_lines))
  lmt$package_flows <- grep('PACKAGE_FLOWS', toupper(lmt_lines))
  
  lmt <- lapply(lmt, function(i)  
    if(length(i) > 0) {
      i <- rmfi_parse_variables(lmt_lines[i])$variables[2]
    } else {
      i <- NA
    })
  
  if(is.na(lmt$fname)) lmt$fname <- paste(sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(file)), 'ftl', sep = '.')
  if(is.na(lmt$inftl)) lmt$inftl <- 333
  if(is.na(lmt$extended)) {
    lmt$extended <- FALSE
  } else if(is.character(lmt$extended)) {
    lmt$extended <- as.logical(match(toupper(lmt$extended), c('STANDARD', 'EXTENDED')) - 1)
  }
  if(is.na(lmt$formatted)) {
    lmt$formatted <- FALSE
  } else if(is.character(lmt$formatted)) {
    lmt$formatted <- as.logical(match(toupper(lmt$formatted), c('UNFORMATTED', 'FORMATTED')) - 1)
  }
  if(is.na(lmt$package_flows)) lmt$package_flows <- NULL
  
  lmt$inftl <- as.numeric(lmt$inftl)
  
  class(lmt) <- c('lmt', 'rmf_package')
  return(lmt)
}

#' Write a MODFLOW Link-MT3DMS Package file
#'
#' @param lmt \code{RMODFLOW} lmt object
#' @param file filename to write to; typically '*.lmt'
#' @param ... ignored
#'
#' @return \code{NULL}
#' @export
#'
#' @seealso \code{\link{rmf_create_lmt}}, \code{\link{rmf_read_lmt}} and \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?lmt6.htm}
#' @examples 
#' lmt <- rmf_create_lmt()
#' rmf_write_lmt(lmt, file = file.path(tempdir(), 'input.lmt'))
#' 
rmf_write_lmt <- function(lmt,
                          file={cat('Please choose lmt file to overwrite or provide new filename ...\n'); file.choose()},
                          ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Link-MT3DMS Package File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(lmt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables('OUTPUT_FILE_NAME', lmt$fname, file = file)
  rmfi_write_variables('OUTPUT_FILE_UNIT', lmt$inftl, file = file)
  rmfi_write_variables('OUTPUT_FILE_HEADER', ifelse(lmt$extended, 'extended', 'standard'), file = file)
  rmfi_write_variables('OUTPUT_FILE_FORMAT', ifelse(lmt$formatted, 'formatted', 'unformatted'), file = file)
  if(!is.null(lmt$package_flows)) rmfi_write_variables('PACKAGE_FLOWS', lmt$package_flows, file = file)
  
}
