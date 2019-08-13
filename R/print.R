
#' @export
print.rmf_2d_array <- function(obj, ...) {
  
  cat(paste('RMODFLOW 2d array with', dim(obj)[1], ifelse(dim(obj)[1] > 1, 'rows', 'row'), 'and', 
            dim(obj)[2], ifelse(dim(obj)[2] > 1, 'columns,', 'column,'), 'representing the',
            paste(attr(obj, 'dimlabels'), collapse = ' & '), 'dimensions.', '\n'))
  if(is.null(attr(obj, 'kper'))) {
    cat('Not representing stress period data', '\n')
  } else {
    cat('Active during', ifelse(length(attr(obj, 'kper')) > 1, 'stress periods:', 'stress period:'), 
        attr(obj, 'kper'), '\n')
  }
  print(as.array(obj), ...)
  
}

#' @export
print.rmf_3d_array <- function(obj, ...) {
  
  cat(paste('RMODFLOW 3d array with', dim(obj)[1], ifelse(dim(obj)[1] > 1, 'rows,', 'row,'),
            dim(obj)[2], ifelse(dim(obj)[2] > 1, 'columns', 'column'), 'and', dim(obj)[3],
            ifelse(dim(obj)[3] > 1, 'layers,', 'layer,'), 'representing the',
            paste(attr(obj, 'dimlabels')[1:2], collapse = ', '), paste('&', attr(obj, 'dimlabels')[3]),
            'dimensions.', '\n'))
  if(is.null(attr(obj, 'kper'))) {
    cat('Not representing stress period data', '\n')
  } else {
    cat('Active during', ifelse(length(attr(obj, 'kper')) > 1, 'stress periods:', 'stress period:'), 
        attr(obj, 'kper'), '\n')
  }
  print(as.array(obj), ...)
  
}

#' @export
print.rmf_4d_array <- function(obj, ...) {
  
  cat(paste('RMODFLOW 4d array with', dim(obj)[1], ifelse(dim(obj)[1] > 1, 'rows,', 'row,'),
            dim(obj)[2], ifelse(dim(obj)[2] > 1, 'columns,', 'column'), dim(obj)[3],
            ifelse(dim(obj)[3] > 1, 'layers', 'layer'), 'and', dim(obj)[4], 
            ifelse(dim(obj)[4] > 1, 'timesteps,', 'timestep'), 'representing the', 
            paste(attr(obj, 'dimlabels')[1:3], collapse = ', '), paste('&', attr(obj, 'dimlabels')[4]),
            'dimensions.', '\n'))
  if(is.null(attr(obj, 'kper'))) {
    cat('Not representing stress period data', '\n')
  } else {
    cat('Active during', ifelse(length(attr(obj, 'kper')) > 1, 'stress periods:', 'stress period:'), 
        attr(obj, 'kper'), '\n')
  }
  print(as.array(obj), ...)
  
}

#' @export
print.rmf_list <- function(obj, ...) {
  
  cat('RMODFLOW list with', nrow(obj), 'features', 'and', ncol(obj) - 3,
      ifelse(ncol(obj) - 3 > 1, 'variables', 'variable'), '\n')
  if(is.null(attr(obj, 'kper'))) {
    cat('Not representing stress period data', '\n')
  } else {
    cat('Active during', ifelse(length(attr(obj, 'kper')) > 1, 'stress periods:', 'stress period:'), 
        attr(obj, 'kper'), '\n')
  }
  print(as.data.frame(obj), ...)
  
}

#' @export
print.rmf_package <- function(obj) cat('A undefined RMODFLOW package')

#' @export
print.nam <- function(nam) {
  cat('RMODFLOW Name File object with:\n')
  cat(nrow(nam), 'files with', length(which(!nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)'))), 'packages', '\n')
  print.data.frame(nam)
}

#' @export
print.dis <- function(dis) {
  
  # dimensions
  cat('RMODFLOW Discretization File object with:\n')
  cat(paste(dis$nrow, ifelse(dis$nrow > 1, 'rows,', 'row,'), dis$ncol, ifelse(dis$ncol > 1, 'columns', 'column'),
            'and', dis$nlay, ifelse(dis$nlay > 1, 'layers', 'layer'), 'totalling', dis$nrow*dis$ncol*dis$nlay, 'cells' ,'\n'))
  cat('\n')
  
  # units
  cat(paste('Time units:', c('undefined', 'seconds', 'minutes', 'hours', 'days', 'years')[dis$itmuni+1], '\n'))
  cat(paste('Length units:', c('undefined', 'feet', 'meters', 'centimeters')[dis$lenuni+1], '\n'))
  
  # cbd
  if(any(dis$laycbd != 0)) {
    cat('\n')
    cat('Confining beds present below layer(s)', which(dis$laycbd != 0), '\n')
   } # else {
  #   cat('No confining beds present\n')
  # }
  
  # delr & delc
  cat('\n')
  cat('Spacing along rows (DELR): ', if(dis$ncol > 7) c(dis$delr[1:7], '...') else dis$delr, '\n')
  cat('Spacing along columns (DELC): ', if(dis$nrow > 7) c(dis$delc[1:7], '...') else dis$delc, '\n')
  cat('\n')
  
  # top
  cat('Summary of top elevations: \n')
  c(dis$top) %>% as.data.frame() %>% setNames('Top') %>% summary() %>% print
  cat('\n')
  
  # botm
  if(dis$nlay > 5) {
    cat('Summary of bottom elevations (first 5 layers): \n')
    nlay <- 5
  } else {
    cat('Summary of bottom elevations: \n')
    nlay <- dis$nlay
  }
  if(any(dis$laycbd != 0)) {
    cbd <- rmfi_confining_beds(dis)
    nnlay <- dis$nlay + sum(cbd)
    names_botm <- vapply(1:nnlay,
                         function(i) rmfi_ifelse0(cbd[i], paste('Confining bed', cumsum(cbd)[i]),
                                                  paste('Layer', i - cumsum(cbd)[i])), 
                         'text')
  } else {
    names_botm <- paste('Layer', 1:dis$nlay)
  }
  apply(dis$botm, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(names_botm) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # stress periods
  sp_names <- setNames(c('Steady-state', 'Transient'), c('SS', 'TR'))
  sp <- data.frame(kper = 1:dis$nper, perlen = dis$perlen, nstp = dis$nstp, tsmult = dis$tsmult, sstr = sp_names[dis$sstr])
  names(sp) <- c('Period', 'Length', 'Timesteps', 'Multiplier', 'Type')
  if(dis$nper > 5) {
    cat('Information for', dis$nper, if(dis$nper > 1) 'stress periods' else 'stress period', '(first 5 shown):', '\n')
    nper <- 5
  } else {
    cat('Information for', dis$nper, if(dis$nper > 1) 'stress periods:' else 'stress period:', '\n')
    nper <- dis$nper
  }
  print(sp[1:nper,], row.names = FALSE)
  
}

#' @export
print.bas <- function(bas) {
  
  cat('RMODFLOW Basic Package object with:\n')
  cat(length(which(bas$ibound == -1)), 'constant head cells,', length(which(bas$ibound == 0)), 'inactive cells and',
      length(which(bas$ibound == 1)), 'active cells', '\n')
  
  cat('\n')
  if(bas$xsection) cat('Model is a 1-row cross-section; strt & ibound have dimensions ncol*nlay.', '\n')
  if(bas$chtoch) cat('Flow between adjacent constant head cells is calculated.', '\n')
  if(bas$free) cat('Free format is used for reading and writing input variables.', '\n')
  if(bas$printtime) cat('Start, end and elapsed execution time is written to the listing file.', '\n')
  if(bas$showprogress) cat('Stress period, time step and equation being solved are written to the monitor.', '\n')
  if(bas$stoperror) cat('When failing to converge, execution will continue as long as the budget discrepancy is smaller than', bas$stoper, '\n')
  cat('\n')
  
  cat('Inactive cells are assigned a head value of', bas$hnoflo, '\n')
  cat('\n')
  
  if(dim(bas$strt)[3] > 5) {
    cat('Summary of initial head values (first 5 layers): \n')
    nlay <- 5
  } else {
    cat('Summary of initial head values: \n')
    nlay <- dim(bas$strt)[3]
  }   
  apply(bas$strt, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(paste('Layer', 1:dim(bas$strt)[3])) %>% subset(select = 1:nlay) %>% print()
}

#' 
#' #' @export
#' print.hob
#' 
#' #' @export
#' print.pvl
#' 
#' #' @export
#' print.zon
#' 
#' #' @export
#' print.mlt
#' 
#' #' @export
#' print.huf
#' 
#' #' @export
#' print.oc
#' 
#' #' @export
#' print.wel
#' 
#' #' @export
#' print.ghb
#' 
#' #' @export
#' print.pcg
#' 
#' #' @export
#' print.kdep
#' 
#' #' @export
#' print.lpf
#' 
#' #' @export
#' print.rch
#' 
#' #' @export
#' print.chd
#' 
#' #' @export
#' print.bcf
#' 
#' #' @export
#' print.hfb
#' 
#' #' @export
#' print.riv
#' 
#' #' @export
#' print.drn
#' 
#' #' @export
#' print.evt
#' 
#' #' @export
#' print.sip
#' 
#' #' @export
#' print.de4
#' 
#' #' @export
#' print.nwt
#' 
#' #' @export
#' print.upw
#' 
#' #' @export
#' print.lvda

#' #' @export
#' print.hed
#' 
#' #' @export
#' print.ddn
#' 
#' #' @export
#' print.bud
#' 
#' #' @export
#' print.cbc
