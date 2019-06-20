#' Create an \code{RMODFLOW} oc object
#' 
#' \code{rmf_create_oc} creates an \code{RMODFLOW} oc object.
#' 
#' @param dis dis object
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
#' @param itsoc vector of time step numbers (within the corresponding stress period)
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
rmf_create_oc <- function(dis = create_dis(),
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
                          print_head = rep(FALSE, length(iperoc)),
                          print_drawdown = rep(FALSE, length(iperoc)),
                          print_budget = rep(FALSE, length(iperoc)),
                          save_head = rep(TRUE, length(iperoc)),
                          save_drawdown = rep(FALSE, length(iperoc)),
                          save_ibound = rep(FALSE, length(iperoc)),
                          save_budget = rep(TRUE, length(iperoc)),
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
      oc$itsoc <- itsoc
      
      # data set 3
      oc$print_head <- print_head
      oc$print_drawdown <- print_drawdown
      oc$print_budget <- print_budget
      oc$save_head <- save_head
      oc$save_drawdown <- save_drawdown
      oc$save_ibound <- save_ibound
      oc$save_budget <- save_budget
      

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
  class(oc) <- c('oc')
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
  
  class(oc) <- c('oc')
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

#' Read a MODFLOW cell-by-cell budget file
#' 
#' \code{rmf_read_cbc} reads in a MODFLOW cell-by-cell budget file
#' 
#' @param file filename; typically '*.cbc'
#' @param dis dis object.
#' @param huf huf object; optional. Provide only if huf heads are being read and \code{dis} is not NULL. See details.
#' @param oc oc object; optional. See details.
#' @param fluxes character; denotes which fluxes to read. Defaults to reading all fluxes. See details.
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @return object of class \code{cbc} which is a list consisting of named rmf_arrays and/or data.frames. The names of the elements correspond to the fluxes.
#'
#' @details 
#' Fluxes include \code{'constant_head'}, \code{'storage'}, \code{'flow_right_face'}, \code{'flow_front_face'}, \code{'flow_lower_face'}, \code{'wells'},
#' \code{'river_leakage'}, \code{'recharge'}, \code{'drains'}, \code{'head_dep_bounds'} or any other description as written by MODFLOW.
#'  
#' If a \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY is read and binded at each time step for which output is written. 
#' The resulting dimensions of the final array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#'
#' If flows are interpolated to huf units, a \code{huf} object is to be supplied as well to dimension the array. This will only affect the constant-head and cell flow terms.
#' The final array will have NHUF layers instead of NLAY.
#'
#' @export
rmf_read_cbc <- function(file = {cat('Please select cell-by-cell budget file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         huf = NULL,
                         oc = NULL,
                         precision = 'single',
                         fluxes = 'all') {
  
  # headers <- c('   CONSTANT HEAD',
  #              '         STORAGE',
  #              'FLOW RIGHT FACE ',
  #              'FLOW FRONT FACE ',
  #              'FLOW LOWER FACE ',
  #              '           WELLS',
  #              '   RIVER LEAKAGE',
  #              '        RECHARGE',
  #              '          DRAINS',
  #              ' HEAD DEP BOUNDS')
  
  nbytes <- ifelse(precision == 'single', 4, 8) 
  binary <-  TRUE # MODFLOW cbc budget file is always binary
  if(binary) {
    con <- file(file,open='rb')
    cbc <- list()
    
    try({
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      desc <- readChar(con,nchars=16)
      trial <- 1
      fail = c(FALSE,FALSE)
      
      # nsteps to dimension array
      if(is.null(oc)) {
        nsteps <- sum(dis$nstp)
      } else {
        if(is.null(oc$save_budget)) {
          nsteps <- length(which(oc$icbcfl == T))
        } else {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[oc$save_budget,]
          nsteps <- apply(m_oc, 1, function(i) rmfi_ifelse0(i[1] == 1, ifelse(i[2] > dis$nstp[i[1]], NA, i[2]), cumsum(dis$nstp)[i[1]-1]+i[2]))
          # check before going into nested for-loop
          if(any(is.na(nsteps)) || !all(diff(nsteps) >= 0)) {
            nsteps <- 0
            i <- 0
            for(k in 1:dis$nper) {
              for(l in 1:dis$nstp[k]) {
                if(m_oc[i+1,1] < k || (m_oc[i+1,1]==k && m_oc[i+1,2] < l)) {
                  i <- i + 1
                  m_oc[i,1] <- k
                  m_oc[i,2] <- l
                  nsteps <- nsteps + 1
                  
                } else if(m_oc[i+1,1]==k && m_oc[i+1,2]==l){
                  nsteps <- nsteps + 1
                  i <- i + 1
                }
              }
            }
          } else {
            nsteps <- min(sum(dis$nstp), length(which(oc$save_budget == TRUE)))
          }
        }
      }
      
      # loop
      while(length(desc!=0)) {
        
        name <- gsub(' ', '_', tolower(trimws(desc)))
        if(trial == 1) {
          if(!(name %in% c("storage","constant_head",'flow_right_face','flow_front_face'))) {
            fail[1] = TRUE
          }
        } else if(trial == 2) {
          if(!(name %in% c('constant_head','flow_right_face','flow_front_face','flow_lower_face'))) {
            fail[2] = TRUE
          }
        }
        if(any(fail)) {
          stop(paste('Header descriptions do not match. Are you sure the file is', precision,'precision?'))
        }
        
        read <- ifelse((fluxes != 'all' && !(name %in% fluxes)), FALSE, TRUE) 
        ncol <- readBin(con,what='integer',n=1)
        nrow <- readBin(con,what='integer',n=1)
        nlay <- readBin(con,what='integer',n=1)
        
        if(!read) {
          if(nlay > 0) {
            invisible(readBin(con,what='numeric',n=nrow*ncol*nlay,size = nbytes))
          } else {
            itype <- readBin(con,what='integer',n=1)
            invisible(readBin(con,what='numeric',n=3,size = nbytes))
            
            if(itype==5) {
              nval <- readBin(con,what='integer',n=1)
            } else {
              nval <- 1
            }
            if(nval > 1) invisible(readChar(con,nchars=(nval-1)*16))
            
            if(itype %in% c(2,5)) { 
              nlist <- readBin(con,what='integer',n=1)
              if(nlist > 0) {
                for(nr in 1:nlist) {
                  invisible(readBin(con,what='integer',n=1))
                  invisible(readBin(con,what='numeric',n=nval,size = nbytes))
                }
              }
            }
            
            if(itype %in% c(0,1)) invisible(readBin(con,what='numeric',n=ncol*nrow*abs(nlay),size = nbytes))
            
            if(itype ==3) {
              invisible(readBin(con,what='integer',n=ncol*nrow))
              invisible(readBin(con,what='numeric',n=ncol*nrow,size = nbytes))  
            }
            
            if(itype ==4) invisible(readBin(con,what='numeric',n=ncol*nrow,size = nbytes))
          }
          
          # read
        } else {
          
          # create arrays
          nnlay <- dis$nlay
          if(!is.null(huf) && !is.null(huf$iohufflows) && huf$iohufflows > 0 && name %in% c('constant_head','flow_right_face','flow_front_face','flow_lower_face')) nnlay <- huf$nhuf
          if(is.null(cbc[[name]])) {
            cbc[[name]] <- rmf_create_array(NA, dim = c(dis$nrow, dis$ncol, nnlay, nsteps))
            attr(cbc[[name]], 'kstp') <- attr(cbc[[name]], 'kper') <- attr(cbc[[name]], 'pertim') <- attr(cbc[[name]], 'totim') <- attr(cbc[[name]], 'delt') <- rep(NA, nsteps)
          }
          # set step number
          if(is.null(oc)) {
            stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
          } else {
            if(is.null(oc$save_budget)) {
              stp_nr <- length(which(oc$icbcfl[1:ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)] == T))
            } else {
              stp_nr <- length(which(oc$save_budget[1:which(m_oc[,1] == kper)[m_oc[,2][m_oc[,1] == kper] == kstp]] == T))
            }
          }
          
          # not compact
          if(nlay > 0) {
            cbc[[name]][,,,stp_nr] <- aperm(array(readBin(con,what='numeric',n=dis$nrow*dis$ncol*nnlay,size = nbytes),dim=c(dis$ncol,dis$nrow,nnlay)),c(2,1,3))
            
            # compact
          } else {
            itype <- readBin(con,what='integer',n=1)
            delt <- readBin(con,what='numeric',n=1,size = nbytes)
            pertim <- readBin(con,what='numeric',n=1,size = nbytes)
            totim <- readBin(con,what='numeric',n=1,size = nbytes)
            
            if(itype==5) {
              nval <- readBin(con,what='integer',n=1)
            } else {
              nval <- 1
            }
            if(nval > 1) {
              if(is.null(attr(cbc[[name]], 'ctmp'))) attr(cbc[[name]], 'ctmp') <- as.list(rep(NA,nsteps))
              ctmp <- rep(NA, (nval-1))
              for(nr in 1:(nval-1)) {
                ctmp[nr] <- trimws(readChar(con,nchars=16))
              }
            }
            
            # return a data.frame --> might change to list of multiple rmf_list for more consistency with e.g. plotting
            if(itype %in% c(2,5)) { 
              nlist <- readBin(con,what='integer',n=1)
              if(nlist > 0) {
                df <- matrix(NA,nrow=nlist,ncol=nval+1)
                for(nr in 1:nlist) {
                  df[nr,] <- c(readBin(con,what='integer',n=1),readBin(con,what='numeric',n=nval,size = nbytes))
                }
                ijk <- rmf_convert_id_to_ijk(df[,1], dis = list(nrow=nrow,ncol=ncol,nlay=abs(nlay)),type='modflow')
                if(is.null(cbc[[name]]) || is.array(cbc[[name]])) {
                  nstp <- ifelse(is.null(dis), 1, stp_nr)
                  cbc[[name]] <- as.data.frame(cbind(ijk$k,ijk$i,ijk$j,as.data.frame(df)[,-1], nstp, kper, kstp))
                  names(cbc[[name]]) <- c('k','i','j', 'flow', if(nval > 1) {ctmp},'nstp', 'kper','kstp')
                  rm(df)
                } else {
                  nstp <- ifelse(is.null(dis), cbc[[name]][nrow(cbc[[name]]),nstp]+1, stp_nr)
                  df <- as.data.frame(cbind(ijk$k,ijk$i,ijk$j, as.data.frame(df)[,-1], nstp, kper, kstp))
                  names(df) <- c('k','i','j', 'flow', if(nval > 1) {ctmp},'nstp', 'kper', 'kstp')
                  
                  cbc[[name]] <- rbind(cbc[[name]], df)
                }
              } else {
                if(is.array(cbc[[name]])) cbc[[name]] = NULL
              }
            }
            
            if(itype %in% c(0,1)) {
              cbc[[name]][,,,stp_nr] <- aperm(array(readBin(con,what='numeric',n=dis$ncol*dis$nrow*nnlay,size = nbytes),dim=c(dis$ncol,dis$nrow,nnlay)),c(2,1,3))
            }
            if(itype == 3) {
              layer <- matrix(readBin(con,what='integer',n=ncol*nrow),ncol=ncol,nrow=nrow,byrow=TRUE)
              data <- matrix(readBin(con,what='numeric',n=ncol*nrow,size = nbytes),ncol=ncol,nrow=nrow,byrow=TRUE)
              
              cbc[[name]][,,,stp_nr] <- 0
              cbc[[name]][,,c(layer),stp_nr] <- c(data)
              
              rm(layer, data)
            }
            if(itype ==4) {
              cbc[[name]][,,1,stp_nr] <- matrix(readBin(con,what='numeric',n=dis$ncol*dis$nrow,size = nbytes),ncol=dis$ncol,nrow=dis$nrow,byrow=TRUE)
            }
          }
          
          # set  attributes
          if(!is.null(cbc[[name]])) {
            if(nlay > 0 || itype %in% c(2,5)) {
              # cbc[[name]] <- rmf_create_list(cbc[[name]], kper =  attr(cbc[[name]], 'kper'))
            } else {
              cbc[[name]] <- rmf_create_array(cbc[[name]], kper = attr(cbc[[name]], 'kper'))
            }
            
            attr(cbc[[name]], 'kstp')[stp_nr] <- kstp
            attr(cbc[[name]], 'kper')[stp_nr] <- kper
            if(nlay < 0) {
              attr(cbc[[name]], 'pertim')[stp_nr] <- pertim
              attr(cbc[[name]], 'totim')[stp_nr] <- totim
              attr(cbc[[name]], 'delt')[stp_nr] <- delt
              if(nval > 1) attr(cbc[[name]], 'ctmp')[[stp_nr]] <- ctmp
            } else {
              stp <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper-1]+kstp)
              attr(cbc[[name]], 'pertim')[stp_nr] <- rmf_time_steps(perlen = dis$perlen[kper], tsmult = dis$tsmult[kper], nstp = dis$nstp[kper])[[2]][kstp]
              attr(cbc[[name]], 'totim')[stp_nr] <- rmf_time_steps(dis=dis)[[2]][stp]
              attr(cbc[[name]], 'delt')[stp_nr] <- rmf_time_steps(dis=dis)[[1]][stp]
            }
          }
        }
        
        kstp <- readBin(con,what='integer',n=1)
        kper <- readBin(con,what='integer',n=1)
        desc <- readChar(con,nchars=16)
        trial <- ifelse(trial == 1, 2, 0)
      }
    })
    
    close(con)
    class(cbc) <- c('cbc','rmf_package')
    return(cbc)
    
  } else {
    stop('Code not up to date')
    #     # update this to match the above structure!
    #     cbc <- list()
    #     cbc.lines <- readr::read_lines(file)
    #     while(length(cbc.lines)!=0) {
    #       name <- substr(cbc.lines[1],25,40)
    #       cat('Processing',name,'...\n')    
    #       cbc[[name]] <- list()
    #       cbc[[name]]$sp <- as.numeric(substr(cbc.lines[1],1,12))
    #       cbc[[name]]$ts <- as.numeric(substr(cbc.lines[1],13,24))
    #       cbc[[name]]$ncols <- as.numeric(substr(cbc.lines[1],41,52))
    #       cbc[[name]]$nrows <- as.numeric(substr(cbc.lines[1],53,64))
    #       cbc[[name]]$nlays <- as.numeric(substr(cbc.lines[1],65,76))
    #       cbc.lines <- cbc.lines[-1]
    #       
    #       cbc[[name]]$code <- as.numeric(substr(cbc.lines[1],1,12))
    #       cbc[[name]]$delt <- as.numeric(substr(cbc.lines[1],13,27))
    #       cbc[[name]]$pertim <- as.numeric(substr(cbc.lines[1],28,42))
    #       cbc[[name]]$totim <- as.numeric(substr(cbc.lines[1],43,57))
    #       cbc.lines <- cbc.lines[-1]
    #       
    #       if(cbc[[name]]$code==1) {
    #         nrecords <- cbc[[name]]$ncols * cbc[[name]]$nrows * cbc[[name]]$nlays
    #         nlines <- ceiling(nrecords/5)
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- NULL
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nlines],collapse=' ')))
    #         cbc[[name]]$data <- array(dataVector,dim=c(cbc[[name]]$ncols,cbc[[name]]$nrows,cbc[[name]]$nlays))
    #         cbc[[name]]$data <- aperm(cbc[[name]]$data,c(2,1,3))
    #         names(cbc[[name]]$data) <- c('ID','FLUX')
    #         cbc.lines <- cbc.lines[-c(1:nlines)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #       if(cbc[[name]]$code==2) {
    #         nrecords <- as.numeric(rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]]))
    #         cbc.lines <- cbc.lines[-1]
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nrecords],collapse=' ')))
    #         cbc[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=2,byrow=T))
    #         names(cbc[[name]]$data) <- c('ID','FLUX')
    #         cbc.lines <- cbc.lines[-c(1:nrecords)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #       if(cbc[[name]]$code==3 | cbc[[name]]$code==4) {
    #         nrecords <- cbc[[name]]$ncols * cbc[[name]]$nrows
    #         nlines <- ceiling(nrecords/5)
    #         dataVector <- NULL
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nlines],collapse=' ')))
    #         cbc[[name]]$data <- matrix(dataVector,ncol=cbc[[name]]$ncols,nrow=cbc[[name]]$nrows,byrow=T)
    #         cbc.lines <- cbc.lines[-c(1:nlines)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #       if(cbc[[name]]$code==5) {
    #         nvalues <- as.numeric(rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]]))
    #         cbc.lines <- cbc.lines[-1]
    #         if(nvalues > 1) {
    #           additionalColumns <- rep(NA,nvalues-1)
    #           for(i in 1:(nvalues-1)) {
    #             additionalColumns[i] <- rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]])
    #             cbc.lines <- cbc.lines[-1]
    #           }
    #           #cbc.lines <- cbc.lines[-1] #IFACE
    #           #cbc.lines <- cbc.lines[-1] #CONDFACT
    #           #cbc.lines <- cbc.lines[-1] #CELLGRP
    #         }
    #         nrecords <- as.numeric(rmfi_remove_empty_strings(strsplit(cbc.lines[1],' ')[[1]]))
    #         cbc.lines <- cbc.lines[-1]
    #         # use rmfi_parse_array or rmfi_parse_variables instead!!
    #         dataVector <- as.numeric(split_line_numbers(paste(cbc.lines[1:nrecords],collapse=' ')))
    #         cbc[[name]]$data <- as.data.frame(matrix(dataVector,nrow=nrecords,ncol=nvalues+1,byrow=T))
    #         if(nvalues > 1) names(cbc[[name]]$data) <- c('ID','FLUX',additionalColumns)
    #         if(nvalues == 1)names(cbc[[name]]$data) <- c('ID','FLUX')
    #         cbc.lines <- cbc.lines[-c(1:nrecords)]
    #         cbc.lines <- cbc.lines[-1]
    #       }
    #     }
    #     class(cbc) <- c('cbc','rmf_package')
    #     return(cbc)
  }
}

#' Read a MODFLOW head file
#' 
#' \code{rmf_read_hed} reads in a MODFLOW head file and returns it as an \code{RMODFLOW} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis dis object
#' @param huf huf object; optional. Provide only if huf heads are being read. See details.
#' @param oc oc object; optional. See details.
#' @param bas bas object; optional. If supplied, is used to set the hnoflo values to NA.
#' @param binary logical; is the file binary?
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @return object of class hed and rmf_4d_array. See details.
#' @details 
#' When huf heads are to be read, a \code{huf} object should also be supplied. The final array will have NHUF layers instead of NLAY.
#' 
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, the dimensions of the returned array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' If the array is in ASCII format and no headers are present, a \code{OC} object must be supplied. In that case, it is assumed that the file being read only contains head values and the keyword XSECTION in the bas file is not present.
#' 
#' The only use of the bas argument is to replace the hnoflo values in the final array with NA's. 
#'
#' @export
rmf_read_hed <- function(file = {cat('Please select head file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         huf = NULL,
                         oc = NULL,
                         bas = NULL,
                         binary = TRUE,
                         precision = 'single') {
  
  headers <- c('HEAD',
               'DRAWDOWN',
               'SUBSIDENCE',
               'COMPACTION',
               'CRITICAL HEAD', # spaces! fix!
               'HEAD IN HGU',
               'NDSYS COMPACTION',
               'Z DISPLACEMENT',
               'D CRITICAL HEAD',
               'LAYER COMPACTION',
               'DSYS COMPACTION',
               'ND CRITICAL HEAD',
               'LAYER COMPACTION',
               'SYSTM COMPACTION',
               'PRECONSOL STRESS',
               'CHANGE IN PCSTRS',
               'EFFECTIVE STRESS',
               'CHANGE IN EFF-ST',
               'VOID RATIO',
               'THICKNESS',
               'CENTER ELEVATION',
               'GEOSTATIC STRESS',
               'CHANGE IN G-STRS')
  other_desc <- NULL
  
  if(binary) { # Binary
    
    real_number_bytes <- ifelse(precision == 'single', 4, 8)
    con <- file(file,open='rb')
    
    try({   
      
      if(!is.null(huf) && huf$iohufheads > 0) {
        dis$nlay <- huf$nhuf
      }
      
      # check time steps if oc is specified
      if(!is.null(oc)) {
        # oc using words
        if(!is.null(oc$save_head)) {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[rmfi_ifelse0(is.matrix(oc$save_head), apply(oc$save_head, 2, any), oc$save_head),] 
          nsteps <- apply(m_oc, 1, function(i) rmfi_ifelse0(i[1] == 1, ifelse(i[2] > dis$nstp[i[1]], NA, i[2]), cumsum(dis$nstp)[i[1]-1]+i[2]))
          # check before going into nested for-loop
          if(any(is.na(nsteps)) || !all(diff(nsteps) >= 0)) {
            nsteps <- 0
            i <- 0
            for(k in 1:dis$nper) {
              for(l in 1:dis$nstp[k]) {
                if(m_oc[i+1,1] < k || (m_oc[i+1,1]==k && m_oc[i+1,2] < l)) {
                  i <- i + 1
                  m_oc[i,1] <- k
                  m_oc[i,2] <- l
                  nsteps <- nsteps + 1
                  
                } else if(m_oc[i+1,1]==k && m_oc[i+1,2]==l){
                  nsteps <- nsteps + 1
                  i <- i + 1
                }
              }
            }
          } else {
            nsteps <- min(sum(dis$nstp), length(which(oc$save_head == TRUE)))
          }
          # oc using codes
        } else {
          nsteps <- length(which(apply(oc$hdsv,2,function(i) any(i==TRUE))==TRUE))
        }
      } else {
        nsteps <- sum(dis$nstp)
      }
      
      first <- TRUE  
      
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      desc <- trimws(readChar(con,nchars=16))
      if(! desc %in% headers) {
        stop('Array description not recognized. Is the file really binary? If so, you could try double precision. If not, set the binary argument to FALSE.')
      }
      while(length(desc != 0)) {
        
        name <- gsub(' ', '_', tolower(desc))
        
        # if IOHUFHEADS > 0, there will also be normal head per layer arrays. Do not return those.
        if(!is.null(huf) && huf$iohufheads > 0 && desc != 'HEAD IN HGU'){
          other_desc <- append(other_desc, desc)
          invisible(readBin(con, what='integer', n=3))
          invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
          invisible(readBin(con, what='integer', n=2))
          invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
          desc <-  trimws(readChar(con,nchars=16))
          
        } else if(is.null(huf) && name != 'head') {
          other_desc <- append(other_desc, name)
          invisible(readBin(con, what='integer', n=3))
          invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
          invisible(readBin(con, what='integer', n=2))
          invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
          desc <-  trimws(readChar(con,nchars=16))
          
        } else {
          
          ncol <- readBin(con, what = 'integer', n = 1)
          nrow <- readBin(con, what = 'integer', n = 1)
          ilay <- readBin(con, what = 'integer', n = 1)
          # xsection - ilay never negative with huf. Only do this once.
          if(ilay < 0 && first) {
            dis$nrow <- dis$nlay
            dis$nlay <- 1
          }
          ilay <- abs(ilay)
          
          if(first) {
            hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
            attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- rep(NA, nsteps)
          }
          if(is.null(oc)) {
            stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
          } else {
            if(first) {
              stp_nr <- 1
            } else {
              stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
            }
          }
          
          hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = real_number_bytes),dim=c(ncol, nrow)), c(2, 1))
          
          attr(hed,'kstp')[stp_nr] <- kstp
          attr(hed,'kper')[stp_nr] <- kper
          attr(hed,'pertim')[stp_nr] <- pertim
          attr(hed,'totim')[stp_nr] <- totim
          
          first <- FALSE
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- trimws(readChar(con,nchars=16))
        }
      }
    })
    close(con)
    
  } else { # ASCII
    hed.lines <- readr::read_lines(file)
    label <- TRUE
    variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
    desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
    if(! desc %in% headers) {
      if(variables[2] != dis$ncol) { # weak test to check if there's a label
        stop('Array description not recognized. Are you sure the file is not binary ?')
      }
      label <- FALSE
      if(is.null(oc)) {
        stop('No label line detected. Please specify an OC object.')
      } else {
        warning('Assuming file being read only contains head values.')
      }
    }
    
    
    # skip non-head arrays
    if(label) {
      if(!is.null(huf) && huf$iohufheads > 0){
        
        if(any(grepl('HEAD IN HGU', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD IN HGU', hed.lines)[1]:length(hed.lines)]
          dis$nlay <-  huf$nhuf
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
          hed.lines <-  NULL
        }
      } else {
        if(any(grepl('HEAD', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD', hed.lines)[1]:length(hed.lines)]
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
          hed.lines <-  NULL
        }
      }
    }
    
    # check time steps if oc is specified
    if(!is.null(oc)) {
      # oc using words
      if(!is.null(oc$save_head) ) {
        if(is.matrix(oc$save_head)) {
          nsteps <- length(which(apply(oc$save_head,2,function(i) any(i==TRUE))==TRUE))
        } else {
          nsteps <- length(which(oc$save_head == TRUE))
        }
        # oc using codes
      } else {
        nsteps <- length(which(apply(oc$hdsv,2,function(i) any(i==TRUE))==TRUE))
      }
    } else {
      nsteps <- sum(dis$nstp)
    }
    
    first <- TRUE
    while(length(hed.lines) != 0) {
      
      if(label) {
        variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        if(! desc %in% headers) {
          stop('Array description not recognized. Are you sure the file is not binary ?')
        }
        name <- gsub(' ', '_', tolower(trimws(desc)))
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        hed.lines <- hed.lines[-1]
        
        # xsection - ilay never negative with huf. Only do this once.
        if(ilay < 0 && !is.null(dis) && first) {
          dis$nrow <- dis$nlay
          dis$nlay <- 1
        }
        ilay <- abs(ilay)
        
        # no label
      } else {
        
        # oc words
        if(!is.null(oc$save_head)) {
          if(is.matrix(oc$save_head)) {
            ind <- ifelse(first, 1, ind + 1)
            read <- oc$save_head[ind]
            ilay <- arrayInd(ind, dim(oc$save_head))[1]
            kstp <- oc$itsoc[arrayInd(ind, dim(oc$save_head))[2]]
            kper <- oc$iperoc[arrayInd(ind, dim(oc$save_head))[2]]
            pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
            totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
          } else {
            ind <- ifelse(first, 1, ifelse(ilay == dis$nlay, ind, ind + 1))
            read <- oc$save_head[ind]
            if(read) {
              if(first) {
                ilay <- 1
              } else {
                ilay <- ifelse(ilay != dis$nlay, ilay + 1, 1)
              }
              kstp <- oc$itsoc[ind]
              kper <- oc$iperoc[ind]
              pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
              totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
            }
          }
          
          # oc codes
        } else if(!is.null(oc$hdsv)) {
          ind <- ifelse(first, 1, ind + 1)
          read <- oc$hdsv[ind]
          ilay <- arrayInd(ind, dim(oc$hdsv))[1]
          nstp <-  arrayInd(ind, dim(oc$hdsv))[2]
          kper <- findInterval(nstp, cumsum(dis$nstp), left.open = T) + 1
          kstp <- rmfi_ifelse0(kper == 1, nstp, nstp - cumsum(dis$nstp)[kper - 1])
          pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:nstp])
          totim <- sum(rmf_time_steps(dis=dis)$tsl[1:nstp])
        }
        
      }
      
      # read array
      data_set <- rmfi_parse_array(hed.lines,dis$nrow,dis$ncol,1, skip_header = TRUE)
      
      if(first) {
        hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
        attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- rep(NA, nsteps)
      }
      if(is.null(oc)) {
        stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      } else {
        if(first) {
          stp_nr <- 1
        } else {
          stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
        }       
      }
      hed[,,ilay,stp_nr] <- data_set$array
      
      attr(hed,'kstp')[stp_nr] <- kstp
      attr(hed,'kper')[stp_nr] <- kper
      attr(hed,'pertim')[stp_nr] <- pertim
      attr(hed,'totim')[stp_nr] <- totim
      
      first <- FALSE
      hed.lines <- data_set$remaining_lines
      
      # skip non-head arrays
      if(!is.null(huf) && huf$iohufheads > 0){
        
        if(any(grepl('HEAD IN HGU', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD IN HGU', hed.lines)[1]:length(hed.lines)]
          dis$nlay <-  huf$nhuf
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
          hed.lines <-  NULL
        }
      } else {
        if(any(grepl('HEAD', hed.lines))) {
          hed.lines <-  hed.lines[grep('HEAD', hed.lines)[1]:length(hed.lines)]
        } else {
          other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
          hed.lines <-  NULL
        }
      }
      
    }
  }
  
  if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
  
  if(!is.null(other_desc) && length(other_desc) != 0) {
    warning(paste('HEAD or HEAD IN HGU not found in file. Found ', length(other_desc), ' other descriptions'), call. = FALSE)
    warning(other_desc, call. = FALSE)
    warning('Returning NULL', call. = FALSE)
    return(NULL)
  } else {
    class(hed) <- c('hed','rmf_4d_array')
    return(hed)
  }
}

#' @describeIn rmf_read_hed 
#' @export
rmf_read_head <- function(...) {
  rmf_read_hed(...)
}

#' @describeIn rmf_read_hed Deprecated function name
#' @export
read_hed <- function(...) {
  .Deprecated(new = "rmf_read_hed", old = "read_hed")
  rmf_read_hed(...)
}

#' @describeIn rmf_read_hed Compatible with default ModelMuse file extension
#' @export
rmf_read_fhd <- function(...) {
  rmf_read_hed(..., binary = FALSE)
}

#' @describeIn rmf_read_hed Compatible with default ModelMuse file extension
#' @export
rmf_read_bhd <- function(...) {
  rmf_read_hed(..., binary = TRUE)
}

#' Read a MODFLOW drawdown file
#' 
#' \code{rmf_read_ddn} reads in a MODFLOW drawdown file and returns it as an \code{RMODFLOW} ddn object.
#' 
#' @param file filename; typically '*.ddn'
#' @param dis dis object
#' @param oc oc object; optional. See details.
#' @param bas bas object; optional. If supplied, is used to set the hnoflo values to NA.
#' @param binary logical; is the file binary?
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of the binary file.
#' @return object or list with objects of class ddn and rmf_4d_array. See details.
#' @details 
#' 
#' If no \code{oc} object is supplied, a rmf_array of dimensions NROW x NCOL x NLAY x sum(NSTP) is created and filled. Time steps for which no output is given are filled with \code{NA}.
#' If a \code{oc} object is supplied, the dimensions of the returned array are NROW x NCOL x NLAY x STPS where STPS are timesteps for which output is saved. 
#' If the array is in ASCII format and no headers are present, a \code{OC} object must be supplied. In that case, it is assumed that the file being read only contains head values and the keyword XSECTION in the bas file is not present.
#' 
#' The only use of the bas argument is to replace the hnoflo values in the final array with NA's. 
#'
#' @export
rmf_read_ddn <- function(file = {cat('Please select ddn file ...\n'); file.choose()},
                         dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                         oc = NULL,
                         bas = NULL,
                         binary = TRUE,
                         precision = 'single') {
  
  headers <- c('HEAD',
               'DRAWDOWN',
               'SUBSIDENCE',
               'COMPACTION',
               'CRITICAL HEAD', # spaces! fix!
               'HEAD IN HGU',
               'NDSYS COMPACTION',
               'Z DISPLACEMENT',
               'D CRITICAL HEAD',
               'LAYER COMPACTION',
               'DSYS COMPACTION',
               'ND CRITICAL HEAD',
               'LAYER COMPACTION',
               'SYSTM COMPACTION',
               'PRECONSOL STRESS',
               'CHANGE IN PCSTRS',
               'EFFECTIVE STRESS',
               'CHANGE IN EFF-ST',
               'VOID RATIO',
               'THICKNESS',
               'CENTER ELEVATION',
               'GEOSTATIC STRESS',
               'CHANGE IN G-STRS')
  other_desc <- NULL
  
  if(binary) { # Binary
    
    real_number_bytes <- ifelse(precision == 'single', 4, 8)
    con <- file(file,open='rb')
    
    try({   
      
      # if(!is.null(huf) && huf$iohufheads > 0) {
      #   dis$nlay <- huf$nhuf
      # }
      
      # check time steps if oc is specified
      if(!is.null(oc)) {
        # oc using words
        if(!is.null(oc$save_drawdown) ) {
          # problem: oc records might be in non-ascending order or have non-existing time steps but output is still writen for current timestep
          # e.g. UZFtest2
          m_oc<- cbind(oc$iperoc, oc$itsoc)[rmfi_ifelse0(is.matrix(oc$save_drawdown), apply(oc$save_drawdown, 2, any), oc$save_drawdown),]
          nsteps <- apply(m_oc, 1, function(i) rmfi_ifelse0(i[1] == 1, ifelse(i[2] > dis$nstp[i[1]], NA, i[2]), cumsum(dis$nstp)[i[1]-1]+i[2]))
          # check before going into nested for-loop
          if(any(is.na(nsteps)) || !all(diff(nsteps) >= 0)) {
            nsteps <- 0
            i <- 0
            for(k in 1:dis$nper) {
              for(l in 1:dis$nstp[k]) {
                if(m_oc[i+1,1] < k || (m_oc[i+1,1]==k && m_oc[i+1,2] < l)) {
                  i <- i + 1
                  m_oc[i,1] <- k
                  m_oc[i,2] <- l
                  nsteps <- nsteps + 1
                  
                } else if(m_oc[i+1,1]==k && m_oc[i+1,2]==l){
                  nsteps <- nsteps + 1
                  i <- i + 1
                }
              }
            }
          } else {
            nsteps <- min(sum(dis$nstp), length(which(oc$save_drawdown == TRUE)))
          }
          
          # oc using codes
        } else {
          nsteps <- length(which(apply(oc$ddsv,2,function(i) any(i==TRUE))==TRUE))
        }
      } else {
        nsteps <- sum(dis$nstp)
      }
      
      first <- TRUE  
      
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      desc <- trimws(readChar(con,nchars=16))
      if(! desc %in% headers) {
        stop('Array description not recognized. Is the file really binary? If so, you could try double precision. If not, set the binary argument to FALSE.')
      }
      while(length(desc != 0)) {
        
        name <- gsub(' ', '_', tolower(desc))
        
        # # if IOHUFHEADS > 0, there will also be normal head per layer arrays. Do not return those.
        # if(!is.null(huf) && huf$iohufheads > 0 && desc != '     HEAD IN HGU'){
        #    other_desc <- append(other_desc, desc)
        #    invisible(readBin(con, what='integer', n=3))
        #    invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
        #    invisible(readBin(con, what='integer', n=2))
        #    invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
        #    desc = readChar(con,nchars=16)
        #   
        # } else 
        if(name != 'drawdown') {
          other_desc <- append(other_desc, name)
          invisible(readBin(con, what='integer', n=3))
          invisible(readBin(con,what='numeric',n = dis$ncol * dis$nrow, size = real_number_bytes))
          invisible(readBin(con, what='integer', n=2))
          invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
          desc <-  trimws(readChar(con,nchars=16))
          
        } else {
          
          ncol <- readBin(con, what = 'integer', n = 1)
          nrow <- readBin(con, what = 'integer', n = 1)
          ilay <- readBin(con, what = 'integer', n = 1)
          # xsection - ilay never negative with huf. Only do this once.
          if(ilay < 0 && first) {
            dis$nrow <- dis$nlay
            dis$nlay <- 1
          }
          ilay <- abs(ilay)
          
          if(first) {
            hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
            attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- rep(NA, nsteps)
          }
          if(is.null(oc)) {
            stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
          } else {
            if(first) {
              stp_nr <- 1
            } else {
              stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
            }
          }
          
          hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = real_number_bytes),dim=c(ncol, nrow)), c(2, 1))
          
          attr(hed,'kstp')[stp_nr] <- kstp
          attr(hed,'kper')[stp_nr] <- kper
          attr(hed,'pertim')[stp_nr] <- pertim
          attr(hed,'totim')[stp_nr] <- totim
          
          first <- FALSE
          kstp <- readBin(con,what='integer',n=1)
          kper <- readBin(con,what='integer',n=1)
          pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
          desc <- trimws(readChar(con,nchars=16))
        }
      }
    })
    close(con)
    
  } else { # ASCII
    hed.lines <- readr::read_lines(file)
    label <- TRUE
    variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
    desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
    if(! desc %in% headers) {
      if(variables[2] != dis$ncol) { # weak test to check if there's a label
        stop('Array description not recognized. Are you sure the file is not binary ?')
      }
      label <- FALSE
      if(is.null(oc)) {
        stop('No label line detected. Please specify an OC object.')
      } else {
        warning('Assuming file being read only contains head values.')
      }
    }
    
    # skip non-drawdown arrays
    if(label) {
      # if(!is.null(huf) && huf$iohufheads > 0){
      #   if(!any(grepl('HEAD IN HGU', hed.lines))) other_desc <- headers[which(headers %in% hed.lines)]
      #   dis$nlay = huf$nhuf
      #   hed.lines = hed.lines[grep('HEAD IN HGU', hed.lines)[1]:length(hed.lines)]
      # } else {
      if(any(grepl('DRAWDOWN', hed.lines))) {
        hed.lines <-  hed.lines[grep('DRAWDOWN', hed.lines)[1]:length(hed.lines)]
      } else {
        other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
        hed.lines <-  NULL
      }
      #   }
    }
    
    # check time steps if oc is specified
    if(!is.null(oc)) {
      # oc using words
      if(!is.null(oc$save_drawdown) ) {
        if(is.matrix(oc$save_drawdown)) {
          nsteps <- length(which(apply(oc$save_drawdown,2,function(i) any(i==TRUE))==TRUE))
        } else {
          nsteps <- length(which(oc$save_drawdown == TRUE))
        }
        # oc using codes
      } else {
        nsteps <- length(which(apply(oc$ddsv,2,function(i) any(i==TRUE))==TRUE))
      }
    } else {
      nsteps <- sum(dis$nstp)
    }
    
    first <- TRUE
    while(length(hed.lines) != 0) {
      
      if(label) {
        variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
        kstp <- as.numeric(variables[1])
        kper <- as.numeric(variables[2])
        pertim <- as.numeric(variables[3])
        totim <- as.numeric(variables[4])
        desc <- paste(variables[5:(length(variables)-4)], collapse=' ')
        if(! desc %in% headers) {
          stop('Array description not recognized. Are you sure the file is not binary ?')
        }
        name <- gsub(' ', '_', tolower(trimws(desc)))
        
        ncol <- as.numeric(variables[length(variables)-3])
        nrow <- as.numeric(variables[length(variables)-2])
        ilay <- abs(as.numeric(variables[length(variables)-1]))
        hed.lines <- hed.lines[-1]
        
        # xsection - ilay never negative with huf. Only do this once.
        if(ilay < 0 && !is.null(dis) && first) {
          dis$nrow <- dis$nlay
          dis$nlay <- 1
        }
        ilay <- abs(ilay)
        
        # no label
      } else {
        
        # oc words
        if(!is.null(oc$save_drawdown)) {
          if(is.matrix(oc$save_drawdown)) {
            ind <- ifelse(first, 1, ind + 1)
            read <- oc$save_drawdown[ind]
            ilay <- arrayInd(ind, dim(oc$save_drawdown))[1]
            kstp <- oc$itsoc[arrayInd(ind, dim(oc$save_drawdown))[2]]
            kper <- oc$iperoc[arrayInd(ind, dim(oc$save_drawdown))[2]]
            pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
            totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
          } else {
            ind <- ifelse(first, 1, ifelse(ilay == dis$nlay, ind, ind + 1))
            read <- oc$save_drawdown[ind]
            if(read) {
              if(first) {
                ilay <- 1
              } else {
                ilay <- ifelse(ilay != dis$nlay, ilay + 1, 1)
              }
              kstp <- oc$itsoc[ind]
              kper <- oc$iperoc[ind]
              pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:(cumsum(dis$nstp)[kper]+kstp)])
              totim <- sum(rmf_time_steps(dis=dis)$tsl[1:(cumsum(dis$nstp)[kper]+kstp)])
            }
          }
          
          # oc codes
        } else if(!is.null(oc$ddsv)) {
          ind <- ifelse(first, 1, ind + 1)
          read <- oc$ddsv[ind]
          ilay <- arrayInd(ind, dim(oc$ddsv))[1]
          nstp <-  arrayInd(ind, dim(oc$ddsv))[2]
          kper <- findInterval(nstp, cumsum(dis$nstp), left.open = T) + 1
          kstp <- rmfi_ifelse0(kper == 1, nstp, nstp - cumsum(dis$nstp)[kper - 1])
          pertim <- sum(rmf_time_steps(dis=dis)$tsl[cumsum(dis$nstp)[kper - 1]:nstp])
          totim <- sum(rmf_time_steps(dis=dis)$tsl[1:nstp])
        }
        
      }
      
      # read array
      data_set <- rmfi_parse_array(hed.lines,dis$nrow,dis$ncol,1, skip_header = TRUE)
      
      if(first) {
        hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, nsteps))
        attr(hed, 'kstp') <- attr(hed, 'kper') <-  attr(hed, 'pertim') <-  attr(hed, 'totim') <- rep(NA, nsteps)
      }
      if(is.null(oc)) {
        stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      } else {
        if(first) {
          stp_nr <- 1
        } else {
          stp_nr <- ifelse(ilay == 1, stp_nr+1, stp_nr)
        }        
      }
      hed[,,ilay,stp_nr] <- data_set$array
      
      attr(hed,'kstp')[stp_nr] <- kstp
      attr(hed,'kper')[stp_nr] <- kper
      attr(hed,'pertim')[stp_nr] <- pertim
      attr(hed,'totim')[stp_nr] <- totim
      
      first <- FALSE
      hed.lines <- data_set$remaining_lines
      
      # skip non-drawdown arrays
      if(any(grepl('DRAWDOWN', hed.lines))) {
        hed.lines <-  hed.lines[grep('DRAWDOWN', hed.lines)[1]:length(hed.lines)]
      } else {
        other_desc <- headers[vapply(seq_along(headers), function(i) any(grepl(headers[i], hed.lines)), FUN.VALUE = F)]
        hed.lines <-  NULL
      }
      
    }
  }
  
  
  if(!is.null(other_desc) && length(other_desc) != 0) {
    warning(paste('DRAWDOWN not found in file. Found ', length(other_desc), ' other descriptions'), call. = FALSE)
    warning(other_desc, call. = FALSE)
    warning('Returning NULL', call. = FALSE)
    return(NULL)
  } else {
    if(!is.null(bas)) hed[which(hed == bas$hnoflo)] <-  NA
    class(hed) <- c('ddn','rmf_4d_array')
    return(hed)
  }
}

#' @describeIn rmf_read_ddn
#' @export
rmf_read_drawdown <- function(...) {
  rmf_read_ddn(...)
}

#' Reads the volumetric budget from a MODFLOW listing file
#'
#' \code{rmf_read_bud} reads a volumetric budget from a MODFLOW listing file and returns it as a list with data frame elements
#' 
#' @param file path to the listing file; typically '*.lst'
#'
#' @return an object of class bud which is a list with two data frames: one with cumulative fluxes and one with rates
#' @export

rmf_read_bud <-  function(file = {cat('Please select listing file ...\n'); file.choose()}){
  
  lst.lines <- readr::read_lines(file)
  headers <- grep("VOLUMETRIC BUDGET FOR ENTIRE MODEL", lst.lines)
  enders <- grep("TIME SUMMARY AT END OF TIME STEP", lst.lines)
  
  # if budget is printed
  if(length(headers) > 0) {
    
    # helper functions
    read_vars <- function(index, lines) rmfi_remove_empty_strings(strsplit(lines[index], ' ')[[1]])
    get_timing <- function(header_vector) {
      kstp <- as.numeric(strsplit(header_vector[11],',')[[1]])
      kper <- as.numeric(header_vector[length(header_vector)])
      # nstp <- ifelse(kper == 1, kstp, cumsum(dis$nstp)[kper - 1] + kstp)
      return(list(kstp, kper))
    }
    get_vars <- function(var_vector) {
      breaks <- which(var_vector == '=')
      #name <- paste(tolower(var_vector[1:(breaks[1] - 1)]), collapse = "_")
      cuml <-  as.numeric(var_vector[breaks[1] + 1])
      rate <-  as.numeric(var_vector[breaks[2] + 1])
      return(c(cuml,rate))
    }
    get_balance <- function(lines) {
      vars <- lapply(c((1:nr) + strt.in - 1, tot.in, (1:nr) + strt.out - 1, tot.out, inout, discrp),
                     function(i) read_vars(index = i, lines = lines))
      
      m <- do.call(cbind, c(get_timing(read_vars(index = 1, lines = lines)), 
                            lapply(vars, get_vars)))
      return(list(cuml = m[1,], rate = m[2,]))
    } 
    break_names <- function(var_vector) {
      breaks <- which(var_vector == '=')
      name <- paste(tolower(var_vector[1:(breaks[1] - 1)]), collapse = "_")
    }
    get_names <- function(lines) {
      vars <- lapply(c((1:nr) +strt.in - 1), function(i) read_vars(index = i, lines = lines))
      names <- unlist(lapply(vars, break_names))
      c('kstp', 'kper', paste(names, 'in', sep='_'), 'total_in',
        paste(names, 'out', sep='_'), 'total_out', 'difference', 'discrepancy')
    }
    
    # call  
    # set indices based on first budget
    lines <- lst.lines[headers[1]:enders[1]]
    strt.in <- 9
    tot.in <- grep('TOTAL IN', lines)
    end.in <- tot.in - 2
    strt.out <- tot.in + 4
    tot.out <- grep('TOTAL OUT', lines)
    end.out <- tot.out - 2
    inout <- tot.out + 2
    discrp <- inout + 2
    # number of variables; same in IN as in OUT
    nr <- (end.in - strt.in) + 1
    names <- get_names(lines)
    
    balance <- lapply(seq_along(headers), function(i) get_balance(lst.lines[headers[i]:enders[i]]))
    balance <-  list(cumulative = as.data.frame(do.call(rbind,lapply(balance, '[[', 'cuml'))), 
                     rates =     as.data.frame(do.call(rbind,lapply(balance, '[[', 'rate'))))
    colnames(balance$cumulative) <- colnames(balance$rates) <- names
    
    
    # no budget is printed
  } else {
    stop("No budget was printed to listing file. You can change this in the OC file.")
  }
  
  class(balance) = 'bud'
  return(balance)
  
}


#' @describeIn rmf_read_bud
#' @export
rmf_read_budget <- function(...) {
  rmf_read_bud(...)
}
