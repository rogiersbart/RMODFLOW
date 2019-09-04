#' Create an \code{RMODFLOW} oc object
#' 
#' \code{rmf_create_oc} creates an \code{RMODFLOW} oc object.
#' 
#' @param dis \code{RMODFLOW} dis object
#' @param ihedfm code for the format in which heads will be printed; defaults to NA (heads are not printed)
#' @param chedfm code for the format in which heads will be saved; defaults to NA (heads are saved in binary file)
#' @param ihedun unit number on which heads will be saved; defaults to 666
#' @param iddnfm code for the format in which drawdowns will be printed; defaults to NA (drawdowns are not printed)
#' @param cddnfm code for the format in which drawdowns will be saved; defaults to NA (drawdowns are saved in binary file)
#' @param iddnun unit number on which drawdowns will be saved; defaults to NA (drawdowns are not saved)
#' @param cboufm code for the format in which ibound will be saved; defaults to NA (ibound is saved in binary file)
#' @param ibouun unit number on which ibound will be saved; defaults to NA (ibound is not saved)
#' @param compact_budget logical; should the COMPACT BUDGET option be used
#' @param aux logical; should the auxiliary data be included in the budget file
#' @param head_label logical; should labels be included in the unformatted head file?
#' @param drawdown_label logical; should labels be included in the unformatted drawdown file?
#' @param ibound_label logical; should labels be included in the unformatted ibound file?
#' @param iperoc vector of stress period numbers
#' @param itsoc vector of time step numbers (within the corresponding stress period). A negative number indicates the last time step within the corresponding stress period
#' @param print_head logical vector or logical matrix of dimensions NLAY x length(iperoc); should heads be printed for the time step corresponding to iperoc and itsoc? If not a matrix, all layers are treated the same.
#' @param print_drawdown logical vector or logical matrix of dimensions NLAY x length(iperoc); should drawdowns be printed for the time step corresponding to iperoc and itsoc? If not a matrix, all layers are treated the same.
#' @param print_budget logical vector; should budget be printed for the time step corresponding to iperoc and itsoc?
#' @param save_head logical vector or logical matrix of dimensions NLAY x length(iperoc); should heads be saved for the time step corresponding to iperoc and itsoc? If not a matrix, all layers are treated the same.
#' @param save_drawdown logical vector or logical matrix of dimensions NLAY x length(iperoc); should drawdowns be saved for the time step corresponding to iperoc and itsoc? If not a matrix, all layers are treated the same.
#' @param save_ibound logical vector or logical matrix of dimensions NLAY x length(iperoc); should ibound be saved for the time step corresponding to iperoc and itsoc? If not a matrix, all layers are treated the same.
#' @param save_budget logical vector; should budget be saved for the time step corresponding to iperoc and itsoc?
#' @param incode vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ihddfl vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ibudfl vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param icbcfl vector of length \code{sum(dis$nstp)}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param hdpr matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ddpr matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param hdsv matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#' @param ddsv matrix of dimensions \code{sum(dis$nstp), dis$nlay}; used when OC is specified with numeric codes; defaults to NULL (OC is specified using words)
#'
#' @return Object of class oc
#' @export
#' @seealso \code{\link{rmf_read_oc}}, \code{\link{rmf_write_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
rmf_create_oc <- function(dis,
                          ihedfm = NA,
                          chedfm = NA,
                          ihedun = 666,
                          iddnfm = NA,
                          cddnfm = NA,
                          iddnun = NA,
                          cboufm = NA,
                          ibouun = NA,
                          compact_budget = TRUE,
                          aux = FALSE,
                          head_label = TRUE,
                          drawdown_label = TRUE,
                          ibound_label = TRUE,
                          iperoc = rep(1:dis$nper,dis$nstp),
                          itsoc = unlist(apply(data.frame(1,dis$nstp),1,FUN=function(x) x[1]:x[2])),
                          print_head = FALSE,
                          print_drawdown = FALSE,
                          print_budget = TRUE,
                          save_head = TRUE,
                          save_drawdown = FALSE,
                          save_ibound = FALSE,
                          save_budget = TRUE,
                          incode = NULL,
                          ihddfl = NULL,
                          ibudfl = NULL,
                          icbcfl = NULL,
                          hdpr = NULL, 
                          ddpr = NULL,
                          hdsv = NULL,
                          ddsv = NULL
                          ) {
      
  oc <- NULL
  
  # data set 0
    # to provide comments, use ?comment on the resulting oc object
  
    if(is.null(incode)) { # words
      # data set 1
      oc$ihedfm <- ihedfm
      oc$chedfm <- chedfm
      oc$ihedun <- ihedun
      oc$iddnfm <- iddnfm
      oc$cddnfm <- cddnfm
      oc$iddnun <- iddnun
      oc$cboufm <- cboufm
      oc$ibouun <- ibouun
      oc$compact_budget <- compact_budget
      oc$aux <- aux
      oc$head_label <- head_label
      oc$drawdown_label <- drawdown_label
      oc$ibound_label <- ibound_label
      
      # data set 2
      oc$iperoc <- iperoc
      oc$itsoc <- replace(itsoc, which(itsoc < 0), dis$nstp[oc$iperoc[which(itsoc < 0)]])
      
      # data set 3
      oc$print_head <- rmfi_ifelse0(length(print_head) == 1, rep(print_head, length(iperoc)), print_head)
      oc$print_drawdown <- rmfi_ifelse0(length(print_drawdown) == 1, rep(print_drawdown, length(iperoc)), print_drawdown)
      oc$print_budget <- rmfi_ifelse0(length(print_budget) == 1, rep(print_budget, length(iperoc)), print_budget)
      oc$save_head <- rmfi_ifelse0(length(save_head) == 1, rep(save_head, length(iperoc)), save_head)
      oc$save_drawdown <- rmfi_ifelse0(length(save_drawdown) == 1, rep(save_drawdown, length(iperoc)), save_drawdown)
      oc$save_ibound <- rmfi_ifelse0(length(save_ibound) == 1, rep(save_ibound, length(iperoc)), save_ibound)
      oc$save_budget <- rmfi_ifelse0(length(save_budget) == 1, rep(save_budget, length(iperoc)), save_budget)
      

    } else { # numeric codes
      # data set 1
      oc$ihedfm <- ihedfm
      oc$iddnfm <- iddnfm
      oc$ihedun <- ihedun
      oc$iddnun <- iddnun
      
      # data set 2
      oc$incode <- incode
      oc$ihddfl <- ihddfl
      oc$ibudfl <- ibudfl
      oc$icbcfl <- icbcfl
      
      # data set 3
      oc$hdpr <- hdpr
      oc$ddpr <- ddpr
      oc$hdsv <- hdsv
      oc$ddsv <- ddsv
    }
  class(oc) <- c('oc', 'rmf_package')
  return(oc)
}

#' @describeIn rmf_create_oc Deprecated function name
#' @export
create_oc <- function(...) {
  .Deprecated(new = "rmf_create_oc", old = "create_oc")
  rmf_create_oc(...)
}

#' Read a MODFLOW output control option file
#' 
#' \code{read_oc} reads in a MODFLOW output control option file and returns it as an \code{\link{RMODFLOW}} oc object.
#' 
#' @param file filename; typically '*.oc'
#' @param dis an \code{RMODFLOW} dis object. Used when reading OC specified using numeric codes.
#' @param ... arguments passed to \code{rmfi_parse_variables}. Can be ignored when input is 'free' format.
#' @return object of class oc
#' @export
#' @seealso \code{\link{rmf_write_oc}}, \code{\link{rmf_create_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
rmf_read_oc <- function(file = {cat('Please select oc file ...\n'); file.choose()}, dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...) {
  
  oc_lines <- readr::read_lines(file)
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
    data_set_1 = rmfi_parse_variables(oc_lines, n = 4, ...)
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
      data_set_2 = rmfi_parse_variables(oc_lines, n = 4, ...)
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
        data_set_3 = rmfi_parse_variables(oc_lines, n = 4, ...)
        oc$hdpr[i,] = rmfi_ifelse0(is.na(data_set_3$variables[1]), 0, as.numeric(data_set_3$variables[1]))
        oc$ddpr[i,] = rmfi_ifelse0(is.na(data_set_3$variables[2]), 0, as.numeric(data_set_3$variables[2]))
        oc$hdsv[i,] = rmfi_ifelse0(is.na(data_set_3$variables[3]), 0, as.numeric(data_set_3$variables[3]))
        oc$ddsv[i,] = rmfi_ifelse0(is.na(data_set_3$variables[4]), 0, as.numeric(data_set_3$variables[4]))  
        oc_lines = data_set_3$remaining_lines
        rm(data_set_3)
      } else if(oc$incode[i] > 0) {
        for(k in 1:dis$nlay) {
          data_set_3 = rmfi_parse_variables(oc_lines, n = 4, ...)
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
  
  class(oc) <- c('oc', 'rmf_package')
  return(oc)
}

#' @describeIn rmf_read_oc Deprecated function name
#' @export
read_oc <- function(...) {
  .Deprecated(new = "rmf_read_oc", old = "read_oc")
  rmf_read_oc(...)
}

#' Write a MODFLOW output control option file
#' 
#' \code{rmf_write_oc} writes a MODFLOW output control option file based on an \code{\link{RMODFLOW}} oc object.
#' 
#' @param oc an \code{\link{RMODFLOW}} oc object
#' @param file filename to write to; typically '*.oc'
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format OC file using numeric codes.
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_oc}}, \code{\link{rmf_create_oc}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?oc.htm}
rmf_write_oc <- function(oc,
                         file = {cat('Please select oc file to overwrite or provide new filename ...\n'); file.choose()}, ...) {
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste('# MODFLOW Output Control Option File created by RMODFLOW, version',v,'\n'), file=file)
  cat(paste('#', comment(oc)), sep='\n', file=file, append=TRUE)
  
  if(is.null(oc$incode)) { # words
    
    # data set 1
    if(!is.na(oc$ihedfm)) cat(paste('HEAD PRINT FORMAT', oc$ihedfm, '\n'), file=file, append=TRUE)
    if(!is.na(oc$chedfm)) cat(paste('HEAD SAVE FORMAT', oc$chedfm, ifelse(oc$head_label,'LABEL',''), '\n'), file=file, append=TRUE)
    if(!is.na(oc$ihedun)) cat(paste('HEAD SAVE UNIT', oc$ihedun, '\n'), file=file, append=TRUE)
    if(!is.na(oc$iddnfm)) cat(paste('DRAWDOWN PRINT FORMAT', oc$iddnfm, '\n'), file=file, append=TRUE)
    if(!is.na(oc$cddnfm)) cat(paste('DRAWDOWN SAVE FORMAT', oc$cddnfm, ifelse(oc$drawdown_label,'LABEL',''), '\n'), file=file, append=TRUE)
    if(!is.na(oc$iddnun)) cat(paste('DRAWDOWN SAVE UNIT', oc$iddnun, '\n'), file=file, append=TRUE)
    if(!is.na(oc$cboufm)) cat(paste('IBOUND SAVE FORMAT', oc$cboufm, ifelse(oc$ibound_label,'LABEL',''), '\n'), file=file, append=TRUE)
    if(!is.na(oc$ibouun)) cat(paste('IBOUND SAVE UNIT', oc$ibouun, '\n'), file=file, append=TRUE)
    if(oc$compact_budget) cat(paste('COMPACT BUDGET',ifelse(oc$aux,'AUX',''), '\n'), file=file, append=TRUE)
    
    # data set 2
    for(i in 1:length(oc$iperoc)) {
      rmfi_write_variables(paste('PERIOD',oc$iperoc[i],'STEP',oc$itsoc[i]),file = file)
      if(is.matrix(oc$print_head)) {
        if(all(oc$print_head[,i])) {
          rmfi_write_variables('PRINT HEAD', file = file)
        } else if(any(oc$print_head[,i])) {
          rmfi_write_variables('PRINT HEAD', which(oc$print_head[,i]),file = file)
        }
      } else {
        if(oc$print_head[i]) rmfi_write_variables('PRINT HEAD', file = file)
      }
      
      if(is.matrix(oc$print_drawdown)) {
        if(all(oc$print_drawdown[,i])) {
          rmfi_write_variables('PRINT DRAWDOWN', file = file)
        } else if(any(oc$print_drawdown[,i])) {
          rmfi_write_variables('PRINT DRAWDOWN', which(oc$print_drawdown[,i]),file = file)
        }
      } else {
        if(oc$print_drawdown[i]) rmfi_write_variables('PRINT DRAWDOWN', file = file)
      }
      
      if(oc$print_budget[i]) rmfi_write_variables('PRINT BUDGET', file = file)
      
      if(is.matrix(oc$save_head)) {
        if(all(oc$save_head[,i])) {
          rmfi_write_variables('SAVE HEAD', file = file)
        } else if(any(oc$save_head[,i])) {
          rmfi_write_variables('SAVE HEAD', which(oc$save_head[,i]),file = file)
        }
      } else {
        if(oc$save_head[i]) rmfi_write_variables('SAVE HEAD', file = file)
      }
      
      if(is.matrix(oc$save_drawdown)) {
        if(all(oc$save_drawdown[,i])) {
          rmfi_write_variables('SAVE DRAWDOWN', file = file)
        } else if(any(oc$save_drawdown[,i])) {
          rmfi_write_variables('SAVE DRAWDOWN', which(oc$save_drawdown[,i]),file = file)
        }
      } else {
        if(oc$save_drawdown[i]) rmfi_write_variables('SAVE DRAWDOWN', file = file)
      }
      
      if(is.matrix(oc$save_ibound)) {
        if(all(oc$save_ibound[,i])) {
          rmfi_write_variables('SAVE IBOUND', file = file)
        } else if(any(oc$save_ibound[,i])) {
          rmfi_write_variables('SAVE IBOUND', which(oc$save_ibound[,i]),file = file)
        }
      } else {
        if(oc$save_ibound[i]) rmfi_write_variables('SAVE IBOUND', file = file)
      }
      
      if(oc$save_budget[i]) rmfi_write_variables('SAVE BUDGET', file = file)
    }
    
  } else { # numeric codes
    # data set 1
    rmfi_write_variables(oc$ihedfm, oc$iddnfm, oc$ihedun, oc$iddnun, file = file, ...)
    
    for(i in 1:length(oc$incode)) {
      # data set 2
      rmfi_write_variables(oc$incode[i], oc$ihddfl[i], oc$ibudfl[i], oc$icbcfl[i], file = file, ...)
      
      # data set 3
      if(oc$incode[i] == 0) {
        rmfi_write_variables(oc$hdpr[i,1], oc$ddpr[i,1], oc$hdsv[i,1], oc$ddsv[i,1], file = file, ...)
      } else if(oc$incode[i] > 0) {
        for(k in 1:ncol(oc$hdpr)) {
          rmfi_write_variables(oc$hdpr[i,k], oc$ddpr[i,k], oc$hdsv[i,k], oc$ddsv[i,k], file = file, ...)
        }
      }
    }
  }
}

#' @describeIn rmf_write_oc Deprecated function name
#' @export
write_oc <- function(...) {
  .Deprecated(new = "rmf_write_oc", old = "write_oc")
  rmf_write_oc(...)
}
