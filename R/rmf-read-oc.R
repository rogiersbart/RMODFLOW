#' Read a MODFLOW output control option file
#' 
#' \code{read_oc} reads in a MODFLOW output control option file and returns it as an \code{\link{RMODFLOW}} oc object.
#' 
#' @param file filename; typically '*.oc'
#' @param dis an \code{RMODFLOW} dis object. Used when reading OC specified using numeric codes.
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return object of class oc
#' @importFrom readr read_lines
#' @export
#' @seealso \code{\link{rmf_write_oc}}, \code{\link{rmf_create_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
rmf_read_oc <- function(file = {cat('Please select oc file ...\n'); file.choose()}, dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...) {

  oc_lines <- read_lines(file)
  oc <- list()
  
  # data set 0
    data_set_0 <- rmfi_parse_comments(oc_lines)
    comment(oc) <- data_set_0$comments
    oc_lines <- data_set_0$remaining_lines
    rm(data_set_0)
  
  # OC using words
  if(toupper(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[1]) %in% c('HEAD','DRAWDOWN','IBOUND','COMPACT','PERIOD')) {

  # data set 1
    oc$ihedfm <- oc$chedfm <- oc$ihedun <- oc$iddnfm <- oc$cddnfm <- oc$iddnun <- oc$cboufm <- oc$ibouun <- NA
    oc$compact_budget <- oc$aux <- oc$head_label <- oc$drawdown_label <- oc$ibound_label <- FALSE
    while(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[1] != 'PERIOD') {
      data_set_1 <- rmfi_parse_variables(oc_lines[1], format = 'free')$variables
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
    oc$print_drawdown <-  oc$print_head <- matrix(nrow = dis$nlay, ncol = 0)
    oc$print_budget <- NULL
    oc$save_ibound <- oc$save_drawdown <-  oc$save_head <- matrix(nrow = dis$nlay, ncol = 0)
    oc$save_budget <- NULL
    
    while(length(oc_lines) != 0) {
      data_set_2 <- rmfi_parse_variables(oc_lines[1], format = 'free')$variables
      oc_lines <- oc_lines[-1]
      oc$iperoc <- append(oc$iperoc, as.numeric(data_set_2[2]))
      oc$itsoc <- append(oc$itsoc, as.numeric(data_set_2[4]))
      
      while(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[1] != 'PERIOD' & length(oc_lines) != 0) {
        if(grepl('PRINT HEAD', oc_lines[1])) {
          if(length(strsplit(trimws(oc_lines[1]), ' ')[[1]]) > 2) {
            layers <- as.numeric(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[-c(1:2)])
            oc$print_head <- cbind(oc$print_head, 1:dis$nlay %in% layers)
          } else {
            oc$print_head <- cbind(oc$print_head, rep(T,dis$nlay))
          }
        } 
        
        if(grepl('PRINT DRAWDOWN', oc_lines[1])) {
          if(length(strsplit(trimws(oc_lines[1]), ' ')[[1]]) > 2) {
            layers <- as.numeric(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[-c(1:2)])
            oc$print_drawdown <- cbind(oc$print_drawdown, 1:dis$nlay %in% layers)
          } else {
            oc$print_drawdown <- cbind(oc$print_drawdown, rep(T,dis$nlay))
          }
        } 
        
        if(grepl('PRINT BUDGET', oc_lines[1])) {
          oc$print_budget <- append(oc$print_budget, T)
        } 
        
        if(grepl('SAVE HEAD', oc_lines[1])) {
          if(length(strsplit(trimws(oc_lines[1]), ' ')[[1]]) > 2) {
            layers <- as.numeric(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[-c(1:2)])
            oc$save_head <- cbind(oc$save_head, 1:dis$nlay %in% layers)
          } else {
            oc$save_head <- cbind(oc$save_head, rep(T,dis$nlay))
          }
        } 
        
        if(grepl('SAVE DRAWDOWN', oc_lines[1])) {
          if(length(strsplit(trimws(oc_lines[1]), ' ')[[1]]) > 2) {
            layers <- as.numeric(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[-c(1:2)])
            oc$save_drawdown <- cbind(oc$save_drawdown, 1:dis$nlay %in% layers)
          } else {
            oc$save_drawdown <- cbind(oc$save_drawdown, rep(T,dis$nlay))
          }
        } 
        
        if(grepl('SAVE IBOUND', oc_lines[1])) {
          if(length(strsplit(trimws(oc_lines[1]), ' ')[[1]]) > 2) {
            layers <- as.numeric(rmfi_parse_variables(oc_lines[1], format = 'free')$variables[-c(1:2)])
            oc$save_ibound <- cbind(oc$save_ibound, 1:dis$nlay %in% layers)
          } else {
            oc$save_ibound <- cbind(oc$save_ibound, rep(T,dis$nlay))
          }
        } 
        
        if(grepl('SAVE BUDGET', oc_lines[1])) {
          oc$save_budget <- append(oc$save_budget, T)
        } 
      
        oc_lines <- oc_lines[-1]
      }
      if(ncol(oc$print_head) != length(oc$iperoc)) oc$print_head <- cbind(oc$print_head, rep(FALSE, dis$nlay))
      if(ncol(oc$print_drawdown) != length(oc$iperoc)) oc$print_drawdown <- cbind(oc$print_drawdown, rep(FALSE, dis$nlay))
      if(length(oc$print_budget) != length(oc$iperoc)) oc$print_budget[length(oc$iperoc)] <- FALSE
      if(ncol(oc$save_head) != length(oc$iperoc)) oc$save_head <- cbind(oc$save_head, rep(FALSE, dis$nlay))
      if(ncol(oc$save_drawdown) != length(oc$iperoc)) oc$save_drawdown <- cbind(oc$save_drawdown, rep(FALSE, dis$nlay))
      if(ncol(oc$save_ibound) != length(oc$iperoc)) oc$save_ibound <- cbind(oc$save_ibound, rep(FALSE, dis$nlay))
      if(length(oc$save_budget) != length(oc$iperoc)) oc$save_budget[length(oc$iperoc)] <- FALSE
      
    }
    
    
    
    # collapse if values for all layers are always the same
    if(all(apply(oc$print_head, 2, function(i) length(unique(i)) == 1) == TRUE)) {
      oc$print_head <- oc$print_head[1,]
    }
    if(all(apply(oc$print_drawdown, 2, function(i) length(unique(i)) == 1) == TRUE)) {
      oc$print_drawdown <- oc$print_drawdown[1,]
    }
    if(all(apply(oc$save_head, 2, function(i) length(unique(i)) == 1) == TRUE)) {
      oc$save_head <- oc$save_head[1,]
    }
    if(all(apply(oc$save_drawdown, 2, function(i) length(unique(i)) == 1) == TRUE)) {
      oc$save_drawdown <- oc$save_drawdown[1,]
    }
    if(all(apply(oc$save_ibound, 2, function(i) length(unique(i)) == 1) == TRUE)) {
      oc$save_ibound <- oc$save_ibound[1,]
    }
    
  } else { # OC using numeric codes

  # data set 1
  data_set_1 = rmfi_parse_variables(oc_lines, ...)
  oc$ihedfm = rmfi_ifelse0(is.na(data_set_1$variables[1]), 0, as.numeric(data_set_1$variables[1]))
  oc$iddnfm = rmfi_ifelse0(is.na(data_set_1$variables[2]), 0, as.numeric(data_set_1$variables[2]))
  oc$ihedun = rmfi_ifelse0(is.na(data_set_1$variables[3]), 0, as.numeric(data_set_1$variables[3]))
  oc$iddnun = rmfi_ifelse0(is.na(data_set_1$variables[4]), 0, as.numeric(data_set_1$variables[4]))
  oc_lines = data_set_1$remaining_lines
  rm(data_set_1)

  # data set 2 & 3
  oc$incode = oc$ihddfl = oc$ibudfl = oc$icbcfl = NULL
  oc$hdpr = oc$ddpr = oc$hdsv = oc$ddsv = matrix(NA, nrow = sum(dis$nstp), ncol = dis$nlay)
  for (i in 1:sum(dis$nstp)) {
    data_set_2 = rmfi_parse_variables(oc_lines, ...)
    oc$incode[i] = rmfi_ifelse0(is.na(data_set_2$variables[1]), 0, as.numeric(data_set_2$variables[1]))
    oc$ihddfl[i] = rmfi_ifelse0(is.na(data_set_2$variables[2]), 0, as.numeric(data_set_2$variables[2]))
    oc$ibudfl[i] = rmfi_ifelse0(is.na(data_set_2$variables[3]), 0, as.numeric(data_set_2$variables[3]))
    oc$icbcfl[i] = rmfi_ifelse0(is.na(data_set_2$variables[4]), 0, as.numeric(data_set_2$variables[4]))
    oc_lines = data_set_2$remaining_lines
    rm(data_set_2)

    if(oc$incode[i] < 0) {
      if(i == 1) {
        oc$hdpr[i,] = NA
        oc$ddpr[i,] = NA
        oc$hdsv[i,] = NA
        oc$ddsv[i,] = NA
      } else {
        oc$hdpr[i,] = oc$hdpr[i-1,]
        oc$ddpr[i,] = oc$ddpr[i-1,]
        oc$hdsv[i,] = oc$hdsv[i-1,]
        oc$ddsv[i,] = oc$ddsv[i-1,]
      }

    } else if(oc$incode[i] == 0) {
      data_set_3 = rmfi_parse_variables(oc_lines, ...)
      oc$hdpr[i,] = rmfi_ifelse0(is.na(data_set_3$variables[1]), 0, as.numeric(data_set_3$variables[1]))
      oc$ddpr[i,] = rmfi_ifelse0(is.na(data_set_3$variables[2]), 0, as.numeric(data_set_3$variables[2]))
      oc$hdsv[i,] = rmfi_ifelse0(is.na(data_set_3$variables[3]), 0, as.numeric(data_set_3$variables[3]))
      oc$ddsv[i,] = rmfi_ifelse0(is.na(data_set_3$variables[4]), 0, as.numeric(data_set_3$variables[4]))  
      oc_lines = data_set_3$remaining_lines
      rm(data_set_3)
    } else if(oc$incode[i] > 0) {
      for(k in 1:dis$nlay) {
        data_set_3 = rmfi_parse_variables(oc_lines, ...)
        oc$hdpr[i,k] = rmfi_ifelse0(is.na(data_set_3$variables[1]), 0, as.numeric(data_set_3$variables[1]))
        oc$ddpr[i,k] = rmfi_ifelse0(is.na(data_set_3$variables[2]), 0, as.numeric(data_set_3$variables[2]))
        oc$hdsv[i,k] = rmfi_ifelse0(is.na(data_set_3$variables[3]), 0, as.numeric(data_set_3$variables[3]))
        oc$ddsv[i,k] = rmfi_ifelse0(is.na(data_set_3$variables[4]), 0, as.numeric(data_set_3$variables[4]))  
        oc_lines = data_set_3$remaining_lines
        rm(data_set_3)
      }
    }
  }

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
