#' Read a MODFLOW output control option file
#' 
#' \code{read_oc} reads in a MODFLOW output control option file and returns it as an \code{\link{RMODFLOW}} oc object.
#' 
#' @param file filename; typically '*.oc'
#' @return object of class oc
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{write_oc}}, \code{\link{create_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
rmf_read_oc <- function(file = {cat('Please select oc file ...\n'); file.choose()}) {

  oc_lines <- read_lines(file)
  oc <- list()
  
  # data set 0
    data_set_0 <- rmfi_parse_comments(oc_lines)
    comment(oc) <- data_set_0$comments
    oc_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # data set 1
    oc$ihedfm <- oc$chedfm <- oc$ihedun <- oc$iddnfm <- oc$cddnfm <- oc$iddnun <- oc$cboufm <- oc$ibouun <- NA
    oc$compact_budget <- oc$aux <- oc$head_label <- oc$drawdown_label <- oc$ibound_label <- FALSE
    while(rmfi_parse_variables(oc_lines[1])$variables[1] != 'PERIOD') {
      data_set_1 <- rmfi_parse_variables(oc_lines[1])$variables
      if(grepl('HEAD PRINT FORMAT',oc_lines[1])) {
        oc$ihedfm <- data_set_1[4]
      } else if(grepl('HEAD SAVE FORMAT',oc_lines[1])) {
        oc$chedfm <- data_set_1[4]
        if(grepl('LABEL',oc_lines[1])) oc$head_label <- TRUE
      } else if(grepl('HEAD SAVE UNIT',oc_lines[1])) {
        oc$ihedun <- as.numeric(data_set_1[4])
      } else if(grepl('DRAWDOWN PRINT FORMAT',oc_lines[1])) {
        oc$iddnfm <- data_set_1[4]
      } else if(grepl('DRAWDOWN SAVE FORMAT',oc_lines[1])) {
        oc$cddnfm <- data_set_1[4]
        if(grepl('LABEL',oc_lines[1])) oc$drawdown_label <- TRUE
      } else if(grepl('DRAWDOWN SAVE UNIT',oc_lines[1])) {
        oc$iddnun <- as.numeric(data_set_1[4])
      } else if(grepl('IBOUND SAVE FORMAT',oc_lines[1])) {
        oc$cboufm <- data_set_1[4]
        if(grepl('LABEL',oc_lines[1])) oc$ibound_label <- TRUE
      } else if(grepl('IBOUND SAVE UNIT',oc_lines[1])) {
        oc$ibouun <- as.numeric(data_set_1[4])
      } else if(grepl('COMPACT BUDGET',oc_lines[1])) {
        oc$compact_budget <- TRUE
        if(grepl('AUX',oc_lines[1])) oc$aux <- TRUE
      }
      oc_lines <- oc_lines[-1]
    }
    
  # data set 2 & 3
    oc$iperoc <- NULL
    oc$itsoc <- NULL
    oc$print_head <- oc$print_budget <- oc$print_drawdown <- oc$save_head <- oc$save_ibound <- oc$save_drawdown <- oc$save_budget <- NULL
    while(length(oc_lines) != 0) {
      data_set_2 <- rmfi_parse_variables(oc_lines[1])$variables
      oc_lines <- oc_lines[-1]
      oc$iperoc <- append(oc$iperoc, as.numeric(data_set_2[2]))
      oc$itsoc <- append(oc$itsoc, as.numeric(data_set_2[4]))
      while(rmfi_parse_variables(oc_lines[1])$variables[1] != 'PERIOD' & length(oc_lines) != 0) {
        if(grepl('PRINT HEAD', oc_lines[1])) {
          oc$print_head[length(oc$iperoc)] <- TRUE
        } else if(grepl('PRINT DRAWDOWN', oc_lines[1])) {
          oc$print_drawdown[length(oc$iperoc)] <- TRUE
        } else if(grepl('PRINT BUDGET', oc_lines[1])) {
          oc$print_budget[length(oc$iperoc)] <- TRUE
        } else if(grepl('SAVE HEAD', oc_lines[1])) {
          oc$save_head[length(oc$iperoc)] <- TRUE
        } else if(grepl('SAVE DRAWDOWN', oc_lines[1])) {
          oc$save_drawdown[length(oc$iperoc)] <- TRUE
        } else if(grepl('SAVE IBOUND', oc_lines[1])) {
          oc$save_ibound[length(oc$iperoc)] <- TRUE
        } else if(grepl('SAVE BUDGET', oc_lines[1])) {
          oc$save_budget[length(oc$iperoc)] <- TRUE
        }
        oc_lines <- oc_lines[-1]
      }
      if(length(oc$print_head) != length(oc$iperoc)) oc$print_head[length(oc$iperoc)] <- FALSE
      if(length(oc$print_drawdown) != length(oc$iperoc)) oc$print_drawdown[length(oc$iperoc)] <- FALSE
      if(length(oc$print_budget) != length(oc$iperoc)) oc$print_budget[length(oc$iperoc)] <- FALSE
      if(length(oc$save_head) != length(oc$iperoc)) oc$save_head[length(oc$iperoc)] <- FALSE
      if(length(oc$save_drawdown) != length(oc$iperoc)) oc$save_drawdown[length(oc$iperoc)] <- FALSE
      if(length(oc$save_ibound) != length(oc$iperoc)) oc$save_ibound[length(oc$iperoc)] <- FALSE
      if(length(oc$save_budget) != length(oc$iperoc)) oc$save_budget[length(oc$iperoc)] <- FALSE
    }
  
  class(oc) <- c('oc')
  return(oc)
}

#' @describeIn rmf_read_oc Deprecated function name
#' @export
read_oc <- function(...) {
  .Deprecated(new = "rmf_read_oc", old = "read_oc")
  rmf_read_oc(...)
}
