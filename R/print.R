
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
  cat('RMODFLOW Name File object with:', '\n')
  cat(nrow(nam), 'files with', length(which(!nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)'))), 'packages', '\n')
  cat('\n')
  print.data.frame(nam)
}

#' @export
print.dis <- function(dis, n = 5) {
  
  # dimensions
  cat('RMODFLOW Discretization File object with:', '\n')
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
  #   cat('No confining beds present', '\n')
  # }
  
  # delr & delc
  cnst_delr <- isTRUE(do.call(all.equal, as.list(range(dis$delr) / mean(dis$delr))))
  cnst_delc <- isTRUE(do.call(all.equal, as.list(range(dis$delc) / mean(dis$delc))))
  
  if(cnst_delr) {
    delr <- c(dis$delr[1], '(constant)') 
  } else {
    if(dis$ncol > n) {
      delr <- c(dis$delr[1:n], '...')
    } else {
      delr <- dis$delr
    }
  }
  if(cnst_delc) {
    delc <- c(dis$delc[1], '(constant)')
  } else {
    if(dis$nrow > n) {
      delc <- c(dis$delc[1:n], '...')
    } else {
      delc <- dis$delc
    }
  }
  cat('\n')
  cat('Spacing along rows (DELR): ', delr, '\n')
  cat('Spacing along columns (DELC): ', delc, '\n')
  cat('\n')
  
  # top
  cat('Summary of top elevations:', '\n')
  c(dis$top) %>% as.data.frame() %>% setNames('Top') %>% summary() %>% print
  cat('\n')
  
  # botm
  if(dis$nlay > n) {
    cat('Summary of bottom elevations (first', n,'layers):', '\n')
    nlay <- n
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
  sp <- data.frame(kper = 1:dis$nper, perlen = dis$perlen, nstp = dis$nstp, tsmult = dis$tsmult, sstr = sp_names[toupper(dis$sstr)])
  names(sp) <- c('Period', 'Length', 'Timesteps', 'Multiplier', 'Type')
  if(dis$nper > n) {
    cat('Information for', dis$nper, if(dis$nper > 1) 'stress-periods' else 'stress-period', '(first', n, 'shown):', '\n')
    nper <- n
  } else {
    cat('Information for', dis$nper, if(dis$nper > 1) 'stress-periods:' else 'stress-period:', '\n')
    nper <- dis$nper
  }
  print(sp[1:nper,], row.names = FALSE)
  
}

#' @export
print.bas <- function(bas, n = 5) {
  
  cat('RMODFLOW Basic Package object with:', '\n')
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
  
  if(dim(bas$strt)[3] > n) {
    cat('Summary of initial head values (first', n,'layers):', '\n')
    nlay <- n
  } else {
    cat('Summary of initial head values:', '\n')
    nlay <- dim(bas$strt)[3]
  }   
  apply(bas$strt, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(paste('Layer', 1:dim(bas$strt)[3])) %>% subset(select = 1:nlay) %>% print()
}

#' @export
print.pvl <- function(pvl, n = 30) {
  cat('RMODFLOW Parameter Value File object with:', '\n')
  df <- data.frame(parnam = pvl$parnam, parval = pvl$parval)
  
  if(pvl$np > n) {
    cat(pvl$np, 'parameter values', '(first', n, 'shown):', '\n')
    nlay <- n
  } else {
    cat(pvl$np, 'parameter values:', '\n')
    nlay <- pvl$np
  }   
  
  cat('\n')
  print(df[1:nlay, ])
}

#' @export
print.zon <- function(zon, n = 5) {
  cat('RMODFLOW Zone File object with:', '\n')
  cat(zon$nzn, ifelse(zon$nzn > 1, 'zone arrays', 'zone array'), '\n')
  cat('\n')
  nlay <- ifelse(zon$nzn > n, n, zon$nzn)
  cat(rmfi_ifelse0(zon$nzn > n, c('Overview of zone arrays (first', n, 'arrays):'), 'Overview of zone arrays:'), '\n')
  obj <- abind::abind(zon$izon[1:nlay], along = 3) %>% 
    apply(3, table)
  for(i in 1:nlay) {
    w <- max(nchar(obj[[i]]), nchar(names(obj[[i]])))
    cat('\n')
    cat(zon$zonnam[i], '\n')
    cat('IZ:   ', format(as.character(names(obj[[i]])), width = w, justify = 'right'), '\n')
    cat('Freq: ', format(c(obj[[i]]), width = w, justify = 'right'), '\n')
  }
}

#' @export
print.mlt <- function(mlt, n = 5) {
  cat('RMODFLOW Multipler File object with:', '\n')
  cat(mlt$nml, ifelse(mlt$nml > 1, 'multiplier arrays', 'mutliplier array'), '\n')
  cat('\n')
  nlay <- ifelse(mlt$nml > n, n, mlt$nml)
  cat(rmfi_ifelse0(mlt$nml > n, c('Summary of multiplier arrays (first', n, 'arrays):'), 'Summary of multiplier arrays:'), '\n')
  abind::abind(mlt$rmlt, along = 3) %>% apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(mlt$mltnam) %>% subset(select = 1:nlay) %>% print()
}

#' @export
print.huf <- function(huf, n = 5) {
  
  cat('RMODFLOW Hydrogeologic-Unit Flow object with:', '\n')
  cat(huf$nhuf, ifelse(huf$nhuf > 1, 'hydrogeological units', 'hydrogeological unit'), 'and', huf$nphuf, ifelse(huf$nphuf > 1, 'flow parameters', 'flow parameter'), '\n')
  cat('\n')
  
  cat('Cell-by-cell flow terms are', ifelse(huf$ihufcb == 0, 'not written',
                                        ifelse(huf$ihufcb > 0, paste('written to file number', huf$ihufcb), 
                                               '(only flow between constant-head cells) printed to the listing file')), '\n')
  cat('Dry cells are assigned a head value of', huf$hdry, '\n')
  cat('\n')
  cat('Heads interpolated to hydrogeological units are', ifelse(huf$iohufheads == 0, 'not written', paste('written to file number', huf$iohufheads)), '\n')
  cat('Flow terms interpolated to hydrogeological units are', ifelse(huf$iohufflows == 0, 'not written', paste('written to file number', huf$iohufflows)), '\n')
  cat('\n')
  
  # Layer overview
  ll <- data.frame('Layer' = 1:length(huf$lthuf), 'Type' = 'Confined', 'Wetting' = 'Inactive', stringsAsFactors = FALSE)
  ll$Type[which(huf$lthuf != 0)] <- 'Convertible'
  ll$Wetting[which(huf$laywt != 0)] <- 'Active'
  if(length(huf$lthuf) > n) {
    cat('Layer overview (first', n, 'layers): ', '\n')
    nlay <- n
  } else {
    cat('Layer overview:', '\n')
    nlay <- length(huf$lthuf)
  }
  print(ll[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # Wetting
  if(any(huf$latwt != 0)) {
   cat('Wetting factor:', huf$wetfct, '\n')
   cat('Wetting is attempted every', ifelse(huf$iwetit == 1, 'interval', paste(huf$iwetit, 'intervals')), '\n')
   cat('Initial heads at cells that become wet are defined using equation', ifelse(huf$ihdwet == 0, '3a', '3b'), '(see MODFLOW manual)', '\n')
  
   # wetdry
   wetdry <- huf$wetdry[,,which(huf$laywt != 0)]
   if(length(dim(wetdry)) == 2) wetdry <- rmf_create_array(wetdry, dim = c(dim(wetdry), 1))
   if(dim(wetdry)[3] > n) {
     cat('Summary of wetdry values (first', n ,'layers):', '\n')
     nlay <- n
   } else {
     cat('Summary of wetdry values:', '\n')
     nlay <- dim(wetdry)[3]
   }
   names_wetdry <- paste('Layer', which(huf$laywt != 0))
   apply(wetdry, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
     setNames(names_wetdry) %>% subset(select = 1:nlay) %>% print()
   cat('\n')   
  }
  
  # HGU overview
  un <- data.frame('Index' = 1:huf$nhuf, 'Unit' = huf$hgunam, 'HANI' = 'Parameter', 'VANI' = 'VK', stringsAsFactors = FALSE)
  un$HANI <- replace(un$HANI, which(huf$hguhani != 0), huf$hguhani[which(huf$hguhani != 0)])
  un$VANI <- replace(un$VANI, which(huf$hguvani != 0), huf$hguvani[which(huf$hguvani != 0)])
  df <- cbind(hgu = vapply(huf$parameters, function(i) attr(i, 'hgunam'), 'txt'), 
              type = vapply(huf$parameters, function(i) attr(i, 'partyp'), 'txt')) %>%
    as.data.frame(stringsAsFactors = FALSE)
  if('VANI' %in% df$type) {
    df <- subset(df, type == 'VANI')
    df$unit <- vapply(df$hgu, function(i) which(huf$hgunam == i), 1)
    un$VANI <- replace(un$VANI, unique(df$unit), 'Parameter')
  }
  if(huf$nhuf > n) {
    cat('HGU overview (first', n, 'hgu\'s): ', '\n')
    nlay <- n
  } else {
    cat('HGU overview:', '\n')
    nlay <- huf$nhuf
  }
  print(un[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # Parameters
  pdf <- data.frame('Name' = vapply(huf$parameters, function(i) attr(i, 'parnam'), 'txt'),
                    'Type' = vapply(huf$parameters, function(i) attr(i, 'partyp'), 'txt'), 
                    'Unit' = vapply(huf$parameters, function(i) attr(i, 'hgunam'), 'txt'), 
                    'Value' = vapply(huf$parameters, function(i) attr(i, 'parval'), 1), 
                    stringsAsFactors = FALSE)
  if(nrow(pdf) > n) {
    cat('Parameter overview (first', n, 'parameters): ', '\n')
    nlay <- n
  } else {
    cat('Parameter overview:', '\n')
    nlay <- nrow(pdf)
  }
  print(pdf[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # top
  if(huf$nhuf > n) {
    cat('Summary of top elevations (first', n ,'units):', '\n')
    nlay <- n
  } else {
    cat('Summary of top elevations:', '\n')
    nlay <- huf$nhuf
  }
  apply(huf$top, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(huf$hgunam) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # thck
  if(huf$nhuf > n) {
    cat('Summary of thicknesses (first', n, 'units):', '\n')
    nlay <- n
  } else {
    cat('Summary of thicknesses:', '\n')
    nlay <- huf$nhuf
  }
  apply(huf$thck, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(huf$hgunam) %>% subset(select = 1:nlay) %>% print()

}

#' @export
print.oc <- function(oc, n = 500) {
  
  cat('RMODFLOW Output Control Option file using:', '\n')
  cat(ifelse(is.null(oc$incode), 'words', 'numeric codes'), 'to specify output', '\n')
  cat('\n')
  
  # words
  if(is.null(oc$incode)) {
    
    # save
    if(!is.na(oc$ihedun) && any(c(oc$save_head))) {

      if(is.matrix(oc$save_head)) {
        df <- cbind('Time step' = 1:nrow(oc$save_head), setNames(as.data.frame(oc$save_head), paste('Layer', 1:ncol(oc$save_head))))
        cat('Simulated heads are written to a', if(oc$head_label) {'labelled'}, ifelse(is.na(oc$chedfm), 'binary', 'formatted'), 'file on unit number', oc$ihedun, 'at following',
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$save_head)
        cat('Simulated heads are written to a', if(oc$head_label) {'labelled'}, ifelse(is.na(oc$chedfm), 'binary', 'formatted'), 'file on unit number', oc$ihedun, 'at following',
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    if(!is.na(oc$iddnun) && any(c(oc$save_drawdown))) {
      if(is.matrix(oc$save_drawdown)) {
        df <- cbind('Time step' = 1:nrow(oc$save_drawdown), setNames(as.data.frame(oc$save_drawdown), paste('Layer', 1:ncol(oc$save_drawdown))))
        cat('Simulated drawdowns are written to a', if(oc$drawdown_label) {'labelled'}, ifelse(is.na(oc$cddnfm), 'binary', 'formatted'), 'file on unit number', oc$iddnun, 'at following', 
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$save_drawdown)
        cat('Simulated drawdowns are written to a', if(oc$drawdown_label) {'labelled'}, ifelse(is.na(oc$cddnfm), 'binary', 'formatted'), 'file on unit number', oc$iddnun, 'at following', 
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    if(!is.na(oc$ibouun) && any(c(oc$save_ibound))) {
      if(is.matrix(oc$save_ibound)) {
        df <- cbind('Time step' = 1:nrow(oc$save_ibound), setNames(as.data.frame(oc$save_ibound), paste('Layer', 1:ncol(oc$save_ibound))))
        cat('The ibound array is written to a', if(oc$ibound_label) {'labelled'}, ifelse(is.na(oc$cddnfm), 'binary', 'formatted'), 'file on unit number', oc$iddnun, 'at following', 
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$save_ibound)
        cat('The ibound array is written to a', if(oc$ibound_label) {'labelled'}, ifelse(is.na(oc$cddnfm), 'binary', 'formatted'), 'file on unit number', oc$iddnun, 'at following', 
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    if(any(c(oc$save_budget))) {
      vc <- which(oc$save_budget)
      cat('The', if(oc$compact_budget) {'compacted'}, 'cell-by-cell flow budget', if(oc$aux){'including auxiliary data'}, 'is saved to the binary file(s) specified in the flow and/or stress-packages', 'at following', 
          ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
      cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      cat('\n')
    }
    
    # print
    if(!is.na(oc$ihedfm) && any(c(oc$print_head))) {
      if(is.matrix(oc$print_head)) {
        df <- cbind('Time step' = 1:nrow(oc$print_head), setNames(as.data.frame(oc$print_head), paste('Layer', 1:ncol(oc$print_head))))
        cat('Simulated heads are printed to the listing file', 'at following', 
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$print_head)
        cat('Simulated heads are printed to the listing file', 'at following', 
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    if(!is.na(oc$iddnfm) && any(c(oc$print_drawdown))) {
      if(is.matrix(oc$print_drawdown)) {
        df <- cbind('Time step' = 1:nrow(oc$print_drawdown), setNames(as.data.frame(oc$print_drawdown), paste('Layer', 1:ncol(oc$print_drawdown))))
        cat('Simulated drawdowns are printed to the listing file', 'at following', 
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$print_drawdown)
        cat('Simulated drawdowns are printed to the listing file', 'at following', 
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    if(any(c(oc$print_budget))) {
      vc <- which(oc$print_budget)
      cat('The volumetric budget is printed to the listing file', 'at following', 
          ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
      cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      cat('\n')
    }
    
    
  } else { # codes
    
    # save
    if(!is.na(oc$ihedun) && oc$ihedun > 0 && sum(oc$ihddfl) != 0 && any(c(oc$hdsv) != 0, na.rm = TRUE)) {
      if(sum(oc$incode) > 0) {
        df <- cbind('Time step' = 1:nrow(oc$hdsv), setNames(as.data.frame(oc$hdsv), paste('Layer', 1:ncol(oc$hdsv))))
        cat('Simulated heads are written to a binary file on unit number', oc$ihedun, 'at following',
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$hdsv[,1] != 0)
        cat('Simulated heads are written to a binary file on unit number', oc$ihedun, 'at following',
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    
    if(!is.na(oc$iddnun) && oc$iddnun > 0 && sum(oc$ihddfl) != 0 && any(c(oc$ddsv) != 0, na.rm = TRUE)) {
      if(sum(oc$incode) > 0) {
        df <- cbind('Time step' = 1:nrow(oc$ddsv), setNames(as.data.frame(oc$ddsv), paste('Layer', 1:ncol(oc$ddsv))))
        cat('Simulated heads are written to a binary file on unit number', oc$iddnun, 'at following',
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$ddsv[,1] != 0)
        cat('Simulated heads are written to a binary file on unit number', oc$iddnun, 'at following',
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    
    if(sum(oc$icbcfl) != 0) {
      vc <- which(oc$icbcfl != 0)
      cat('The cell-by-cell flow budget is saved to the binary file(s) specified in the flow and/or stress-packages', 'at following', 
          ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
      cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      cat('\n')
    }
    
    # print
    if(sum(oc$ihddfl, na.rm = TRUE) != 0 && any(c(oc$hdpr) != 0, na.rm = TRUE)) {
      if(sum(oc$incode) > 0) {
        df <- cbind('Time step' = 1:nrow(oc$hdpr), setNames(as.data.frame(oc$hdpr), paste('Layer', 1:ncol(oc$hdpr))))
        cat('Simulated heads are printed to the listing file', 'at following', 
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$hdpr[,1] != 0)
        cat('Simulated heads are printed to the listing file', 'at following', 
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    
    if(sum(oc$ihddfl) != 0 && any(c(oc$ddpr) != 0, na.rm = TRUE)) {
      if(sum(oc$incode) > 0) {
        df <- cbind('Time step' = 1:nrow(oc$ddpr), setNames(as.data.frame(oc$ddpr), paste('Layer', 1:ncol(oc$ddpr))))
        cat('Simulated drawdowns are printed to the listing file', 'at following', 
            ifelse(nrow(df) > n, paste('time steps (first', n, 'shown)'), 'time steps'), 'and',  ifelse(ncol(df) > n, paste('layers (first', n, 'shown):'), 'layers:'),  '\n')
        nr <- ifelse(nrow(df) > n, n, nrow(df))   
        nc <- ifelse(ncol(df) > n, n, ncol(df))
        print(df[1:nr, 1:nc], row.names = FALSE)
      } else {
        vc <- which(oc$ddpr[,1] != 0)
        cat('Simulated drawdowns are printed to the listing file', 'at following', 
            ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
        cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      }
      cat('\n')
    }
    
    if(sum(oc$ibudfl) != 0) {
      vc <- which(oc$ibudfl != 0)
      cat('The volumetric budget is printed to the listing file', 'at following', 
          ifelse(length(vc) > n, paste('time steps (first', n, 'shown):'), 'time steps:'), '\n')
      cat(' ', rmfi_ifelse0(length(vc) > n, c(vc[1:n], '...'), vc), '\n')
      cat('\n')
    }
    
  }
  
}

#' @export
print.wel <- function(wel, n = 15) {
  
  i_parm <- nrow(subset(wel$data, parameter == TRUE))
  i_noparm <- nrow(subset(wel$data, parameter == FALSE))
  
  cat('RMODFLOW Well object with:', '\n')
  if(wel$dimensions$np > 0) cat(wel$dimensions$np, if(!is.null(wel$dimensions$instances)) {'time-varying'}, 'parameters representing', i_parm, 'wells', '\n')
  cat(i_noparm, 'non-parameter wells', '\n')
  if(!is.null(wel$aux)) cat('Auxiliary variables defined:', wel$aux, '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(wel$iwelcb == 0, 'WEL fluxes are not saved to a cell-by-cell flow budget file', c('WELs fluxes are saved to the cell-by-cell flow budget file on unit number', wel$iwelcb)), '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(nrow(wel$data) > n, c('Summary of the first', n, 'defined wells:'), 'Summary of the defined wells:'), '\n')
  rmfi_ifelse0(nrow(wel$data) > n, print(as.data.frame(wel$data)[1:n, ]), print(as.data.frame(wel$data)))
  cat('\n')

  cat('Summary of the stress-period', rmfi_ifelse0(nrow(wel$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(wel$kper) > n, print(wel$kper[1:n, ]), print(wel$kper))
  
}
 
#' @export
print.ghb <- function(ghb, n = 15) {
  
  i_parm <- nrow(subset(ghb$data, parameter == TRUE))
  i_noparm <- nrow(subset(ghb$data, parameter == FALSE))
  
  cat('RMODFLOW General-Head Boundary object with:', '\n')
  if(ghb$dimensions$np > 0) cat(ghb$dimensions$np, if(!is.null(ghb$dimensions$instances)) {'time-varying'}, 'parameters representing', i_parm, 'general-head boundaries', '\n')
  cat(i_noparm, 'non-parameter general-head boundaries', '\n')
  if(!is.null(ghb$aux)) cat('Auxiliary variables defined:', ghb$aux, '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(ghb$ighbcb == 0, 'GHB fluxes are not saved to a cell-by-cell flow budget file', c('GHB fluxes are saved to the cell-by-cell flow budget file on unit number', ghb$ighbcb)), '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(nrow(ghb$data) > n, c('Summary of the first', n, 'defined general-head boundaries:'), 'Summary of the defined general-head boundaries:'), '\n')
  rmfi_ifelse0(nrow(ghb$data) > n, print(as.data.frame(ghb$data)[1:n, ]), print(as.data.frame(ghb$data)))
  cat('\n')
  
  cat('Summary of the stress-period', rmfi_ifelse0(nrow(ghb$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(ghb$kper) > n, print(ghb$kper[1:n, ]), print(ghb$kper))
  
}

#' @export
print.pcg <- function(pcg) {
  
  cat('RMODFLOW Preconditioned Conjugate-Gradient Package object with:', '\n')
  cat('A maximum of', pcg$mxiter, 'outer iterations with', pcg$iter1, 'inner iterations using the', '\n')
  cat(ifelse(pcg$npcond == 1, 'Modified Incomplete Cholesky', 'Polynomial'), 'matrix conditioning method.', '\n')
  cat('\n')
  cat('Head change criterion for convergence:', pcg$hclose, '\n')
  cat('Residual criterion for convergence:', pcg$rclose, '\n')
  cat('\n')
  if(pcg$npcond == 1) {
    cat('The relaxation parameter for the matrix conditioning is', pcg$relax, '\n')
  } else {
    cat('The upper bound on the maximum eigenvalue for the matrix conditioning', ifelse(pcg$nbpol == 2, 'is 2.0', 'will be calculated'), '\n')
  }
  cat('Maximum head change and residual change are printed to the listing file at every', ifelse(pcg$iprpcg == 0, '999', pcg$iprpcg), 'time steps', '\n')
  cat('The damping factor', ifelse(pcg$damppcg > 0, 'for steady-state & transient stress periods is', 'for steady-state stress periods is'), abs(pcg$damppcg), '\n')
  if(pcg$damppcg < 0 && !is.null(pcg$damppcgt)) cat('The damping factor for transient stress periods is', pcg$damppcgt, '\n')
  
}

#' @export
print.kdep <- function(kdep, n = 10) {
  
  cat('RMODFLOW Hydraulic-Conductivity Depth-Dependence Capability object with:', '\n')
  cat(kdep$npkdep, ifelse(kdep$npkdep > 1, 'parameters', 'parameter'), '\n')
  cat('Parameters represent depth-dependence coefficients used to modify horizontal hydraulic conductivity with depth for the hydrogeologic unit(s) specified in the HUF package', '\n')
  cat('The reference surface elevation is specified by', ifelse(kdep$ifkdep == 0, 'the TOP array in the dis object', 'the RS array specified below'), '\n')
  cat('\n')
  
  if(kdep$ifkdep > 0) {
    cat('Summary of the reference surface elevations:', '\n')
    c(kdep$rs) %>% as.data.frame() %>% setNames('RS') %>% summary() %>% print
    cat('\n')
  }
  
  # Parameters
  pdf <- data.frame('Name' = vapply(kdep$parameters, function(i) attr(i, 'parnam'), 'txt'),
                    'Unit' = vapply(kdep$parameters, function(i) attr(i, 'hgunam'), 'txt'), 
                    'Value' = vapply(kdep$parameters, function(i) attr(i, 'parval'), 1), 
                    stringsAsFactors = FALSE)
  if(nrow(pdf) > n) {
    cat('Parameter overview (first', n, 'parameters): ', '\n')
    nlay <- n
  } else {
    cat('Parameter overview:', '\n')
    nlay <- nrow(pdf)
  }
  print(pdf[1:nlay,], row.names = FALSE)
  cat('\n')
  
}

#' @export
print.lpf <- function(lpf, n = 5) {
  
  cat('RMODFLOW Layer-Property Flow Package object with:', '\n')
  if(lpf$nplpf > 0) cat(lpf$nplpf, ifelse(lpf$nplpf > 1 , 'flow parameters', 'flow parameter'), '\n')
  cat('Cell-by-cell flow terms', ifelse(lpf$ilpfcb == 0, 'not written',
                                        ifelse(lpf$ilpfcb > 0, paste('written to file number', lpf$ilpfcb), 
                                               '(only flow between constant-head cells) printed to the listing file')), '\n')
  cat('Dry cells are assigned a head value of', lpf$hdry, '\n')
  cat('\n')
  
  # options
  if(lpf$storagecoefficient) cat('Ss values are read as storage coefficients rather than specific storage', '\n')
  if(lpf$constantcv) cat('Vertical conductance for an unconfined cell is computed from the cell thickness rather than the saturated thickness', '\n')
  if(lpf$thickstrt) cat('Layers with a negative LAYTYP are confined and have their cell thickness for conductance calculations computed from STRT-BOTM rather than TOP-BOTM', '\n')
  if(lpf$nocvcorrection | lpf$constantcv) cat('Vertical conductance is not corrected when the vertical flow correction is applied', '\n')
  if(lpf$novfc) cat('The vertical flow correction under dewatered conditions is turned off', '\n')
  if(lpf$noparcheck) cat('There is no check to see if a variable is defined for all cells when parameters are used', '\n')
  if(lpf$storagecoefficient + lpf$constantcv + lpf$thickstrt + lpf$nocvcorrection + lpf$novfc + lpf$noparcheck > 0) cat('\n')
 
  # Layer overview
  ll <- data.frame('Layer' = 1:length(lpf$laytyp), 'Type' = 'Confined', 'Averaging' = 'Harmonic',
                   'CHANI' = 'HANI', 'VKA' = 'VK',  'Wetting' = 'Inactive', stringsAsFactors = FALSE)
  ll$Type[which(lpf$laytyp != 0)] <- 'Convertible'
  if(lpf$thickstrt) {
    ll$Type[which(lpf$laytyp < 0)] <- 'Confined (thickstrt)'
    lpf$laytyp[which(lpf$laytyp < 0)] <- 0
  }
  avg <- c('Harmonic', 'Logarithmic', 'Arithmetic THCK + Log K')
  ll$Averaging <- avg[lpf$layavg + 1]
  ll$CHANI <- replace(ll$CHANI, which(lpf$chani > 0), lpf$chani[which(lpf$chani > 0)])
  ll$VKA <- replace(ll$VKA, which(lpf$layvka != 0), 'VANI')
  ll$Wetting[which(lpf$laywet != 0)] <- 'Active'
  if(length(lpf$laytyp) > n) {
    cat('Layer overview (first', n, 'layers): ', '\n')
    nlay <- n
  } else {
    cat('Layer overview:', '\n')
    nlay <- length(lpf$laytyp)
  }
  print(ll[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # Wetting
  if(any(lpf$laywet != 0)) {
    cat('Wetting factor:', lpf$wetfct, '\n')
    cat('Wetting is attempted every', ifelse(lpf$iwetit == 1, 'interval', paste(lpf$iwetit, 'intervals')), '\n')
    cat('Initial heads at cells that become wet are defined using equation', ifelse(lpf$ihdwet == 0, '3a', '3b'), '(see MODFLOW manual)', '\n')
    cat('\n')
  }
 
  # Parameters
  if(lpf$nplpf > 0) {
    pdf <- data.frame('Name' = vapply(lpf$parameters, function(i) attr(i, 'parnam'), 'txt'),
                      'Type' = vapply(lpf$parameters, function(i) attr(i, 'partyp'), 'txt'), 
                      'Layer' = vapply(lpf$parameters, function(i) paste(attr(i, 'layer'), collapse = ' '), 'text'), 
                      'Value' = vapply(lpf$parameters, function(i) attr(i, 'parval'), 1), 
                      stringsAsFactors = FALSE)
    if(nrow(pdf) > n) {
      cat('Parameter overview (first', n, 'parameters): ', '\n')
      nlay <- n
    } else {
      cat('Parameter overview:', '\n')
      nlay <- nrow(pdf)
    }
    print(pdf[1:nlay,], row.names = FALSE)
    cat('\n')
  }
 
  # HK
  if(length(lpf$laytyp) > n) {
    cat('Summary of HK (first', n, 'layers):', '\n')
    nlay <- n
  } else {
    cat('Summary of HK:', '\n')
    nlay <- length(lpf$laytyp)
  }
  apply(lpf$hk, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(paste('Layer', 1:length(lpf$laytyp))) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # HANI
  if(!is.null(lpf$hani)) {
    if(length(lpf$laytyp) > n) {
      cat('Summary of HANI (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of HANI:', '\n')
      nlay <- length(lpf$laytyp)
    }
    apply(lpf$hani, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(lpf$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # VKA
  if(!is.null(lpf$vka)) {
    if(length(lpf$laytyp) > n) {
      cat('Summary of VKA (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of VKA:', '\n')
      nlay <- length(lpf$laytyp)
    }
    apply(lpf$vka, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(lpf$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # SS
  if(!is.null(lpf$ss)) {
    if(length(lpf$laytyp) > n) {
      cat('Summary of SS (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of SS:', '\n')
      nlay <- length(lpf$laytyp)
    }
    apply(lpf$ss, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(lpf$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # SY
  if(!is.null(lpf$sy)) {
    sy <- lpf$sy[,,which(lpf$laytyp != 0)]
    if(length(dim(sy)) == 2) sy <- rmf_create_array(sy, dim = c(dim(sy), 1))
    if(dim(sy)[3] > n) {
      cat('Summary of SY (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of SY:', '\n')
      nlay <- dim(sy)[3]
    }
    names_sy <- paste('Layer', which(lpf$laytyp != 0))
    apply(sy, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names_sy) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # VKCB
  if(!is.null(lpf$vkcb)) {
    if(length(lpf$laytyp) > n) {
      cat('Summary of VKCB (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of VKCB:', '\n')
      nlay <- length(lpf$laytyp)
    }
    apply(lpf$vkcb, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(lpf$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # WETDRY
  if(!is.null(lpf$wetdry)) {
    wetdry <- lpf$wetdry[,,which(lpf$laywet != 0)]
    if(length(dim(wetdry)) == 2) wetdry <- rmf_create_array(wetdry, dim = c(dim(wetdry), 1))
    if(dim(wetdry)[3] > n) {
      cat('Summary of WETDRY (first', n ,'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of WETDRY:', '\n')
      nlay <- dim(wetdry)[3]
    }
    names_wetdry <- paste('Layer', which(lpf$laywet != 0))
    apply(wetdry, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names_wetdry) %>% subset(select = 1:nlay) %>% print()
    cat('\n')  
  }
  
}

#' @export
print.rch <- function(rch, n = 5) {
  
  cat('RMODFLOW Recharge Package object with:', '\n')
  cat(length(rch$recharge), 'recharge', ifelse(length(rch$recharge) > 1, 'arrays', 'array'), '\n')
  if(rch$dimensions$np > 0) cat('inlcuding', rch$dimensions$np, if(!is.null(rch$dimensions$instances)) {'time-varying'}, 'parameters', '\n')
  if(rch$nrchop == 1) {
    nrchop <- 'the top grid layer'
  } else if(rch$nrchop == 2) {
    nrchop <- 'the cells defined by layer variable irch'
  } else if(rch$nrchop == 3) {
    nrchop <- 'the highest active cell in each vertical column'
  }
  cat('Recharge applied to', nrchop, '\n')
  cat('\n')
  cat(rmfi_ifelse0(rch$irchcb == 0, 'RCH fluxes are not saved to a cell-by-cell flow budget file', c('RCH fluxes are saved to the cell-by-cell flow budget file on unit number', rch$irchcb)), '\n')
  cat('\n')
  
  # for time-varing parameters
  list_arrays <- function(i) {
    if(is.list(i) && !is.null(attr(i[[1]], 'instnam'))) {
      return(i)
    } else {
      return(list(i))
    }
  }
  rmf_arrays <- lapply(rch$recharge, list_arrays)
  rmf_arrays <- do.call(c, rmf_arrays)
  
  # recharge
  if(length(rmf_arrays) > n) {
    cat('Summary of recharge (first', n, 'arrays):', '\n')
    nlay <- n
  } else {
    cat('Summary of recharge arrays:', '\n')
    nlay <- length(rmf_arrays)
  }
  
  abind::abind(rmf_arrays, along = 3) %>%
  apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(names(rmf_arrays)) %>% subset(select = 1:nlay) %>% print()
  
  # irch
  if(rch$nrchop == 2) {
    cat('\n')
    if(length(rch$irch) > n) {
      cat('Summary of irch (first', n, 'arrays):', '\n')
      nlay <- n
    } else {
      cat('Summary of irch arrays:', '\n')
      nlay <- length(rch$irch)
    }
    
    abind::abind(rch$irch, along = 3) %>%
      apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names(rch$irch)) %>% subset(select = 1:nlay) %>% print()
  }
  
  cat('\n')
  cat('Summary of the stress-period', rmfi_ifelse0(nrow(rch$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(rch$kper) > n, print(rch$kper[1:n, ]), print(rch$kper))
    
}

#' @export
print.chd <- function(chd, n = 15) {
  
  i_parm <- nrow(subset(chd$data, parameter == TRUE))
  i_noparm <- nrow(subset(chd$data, parameter == FALSE))
  
  cat('RMODFLOW Time-Varying Specified Head object with:', '\n')
  if(chd$dimensions$np > 0) cat(chd$dimensions$np, if(!is.null(chd$dimensions$instances)) {'time-varying'}, 'parameters representing', i_parm, 'specified-heads', '\n')
  cat(i_noparm, 'non-parameter specified-heads', '\n')
  if(!is.null(chd$aux)) cat('Auxiliary variables defined:', chd$aux, '\n')
  cat('\n')
  
  # cat(rmfi_ifelse0(chd$ichdcb == 0, 'CHD fluxes are not saved to a cell-by-cell flow budget file', c('CHD fluxes are saved to the cell-by-cell flow budget file on unit number', chd$ichdcb)), '\n')
  # cat('\n')
  
  cat(rmfi_ifelse0(nrow(chd$data) > n, c('Summary of the first', n, 'defined specified-heads:'), 'Summary of the defined specified-heads:'), '\n')
  rmfi_ifelse0(nrow(chd$data) > n, print(as.data.frame(chd$data)[1:n, ]), print(as.data.frame(chd$data)))
  cat('\n')
  
  cat('Summary of the stress-period', rmfi_ifelse0(nrow(chd$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(chd$kper) > n, print(chd$kper[1:n, ]), print(chd$kper))
  
}
 
#' @export
print.bcf <- function(bcf, n = 5) {
  
  cat('RMODFLOW Block-Centered Flow Package object with:', '\n')
  cat('Cell-by-cell flow terms', ifelse(bcf$ibcfcb == 0, 'not written',
                                        ifelse(bcf$ibcfcb > 0, paste('written to file number', bcf$ibcfcb), 
                                               '(only flow between constant-head cells) printed to the listing file')), '\n')
  cat('Dry cells are assigned a head value of', bcf$hdry, '\n')
  cat('Wetting is', ifelse(bcf$iwdflg == 0, 'inactive', 'active'), '\n')
  if(bcf$iwdflg != 0) {
    cat('Wetting factor:', bcf$wetfct, '\n')
    cat('Wetting is attempted every', ifelse(bcf$iwetit == 1, 'interval', paste(bcf$iwetit, 'intervals')), '\n')
    cat('Initial heads at cells that become wet are defined using equation', ifelse(bcf$ihdwet == 0, '3a', '3b'), '(see MODFLOW manual)', '\n')
  }
  cat('\n')
  
  # Layer overview
  ll <- data.frame('Layer' = 1:length(bcf$layavg), 'Type' = 'Confined', 'Averaging' = 'Harmonic', 'TRPY' = bcf$trpy, stringsAsFactors = FALSE)
  
  type <- c('Confined', 'Unconfined', 'Confined/unconfined (constant T)', 'Confined/unconfined (T varies)')
  ll$Type <- type[bcf$laycon + 1]
  avg <- c('Harmonic', 'Arithmetic', 'Logarithmic', 'Arithmetic THCK + Log K')
  ll$Averaging <- avg[bcf$layavg + 1]
  
  if(length(bcf$layavg) > n) {
    cat('Layer overview (first', n, 'layers): ', '\n')
    nlay <- n
  } else {
    cat('Layer overview:', '\n')
    nlay <- length(bcf$layavg)
  }
  print(ll[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # HY
  if(!is.null(bcf$hy)) {
    hy <- bcf$hy[,,which(bcf$laycon %in% c(1,3))]
    if(length(dim(hy)) == 2) hy <- rmf_create_array(hy, dim = c(dim(hy), 1))
    if(dim(hy)[3] > n) {
      cat('Summary of HY (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of HY:', '\n')
      nlay <- dim(hy)[3]
    }
    names <- paste('Layer', which(bcf$laycon %in% c(1,3)))
    apply(hy, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
  }
  
  # TRAN
  if(!is.null(bcf$tran)) {
    tran <- bcf$tran[,,which(bcf$laycon %in% c(0,2))]
    if(length(dim(tran)) == 2) tran <- rmf_create_array(tran, dim = c(dim(tran), 1))
    if(dim(tran)[3] > n) {
      cat('Summary of TRAN (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of TRAN:', '\n')
      nlay <- dim(tran)[3]
    }
    names <- paste('Layer', which(bcf$laycon %in% c(0,2)))
    apply(tran, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
  }
  
  # VCONT
  if(!is.null(bcf$vcont) && length(bcf$layavg) > 1) {
    vcont <- bcf$vcont[,,1:(length(bcf$layavg)-1)]
    if(length(dim(vcont)) == 2) vcont <- rmf_create_array(vcont, dim = c(dim(vcont), 1))
    if(dim(vcont)[3] > n) {
      cat('Summary of VCONT (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of VCONT:', '\n')
      nlay <- dim(vcont)[3]
    }
    names <- paste('Layer', 1:(length(bcf$layavg)-1))
    apply(vcont, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
  }
  
  # SF1
  if(!is.null(bcf$sf1)) {
    sf1 <- bcf$sf1
    if(dim(sf1)[3] > n) {
      cat('Summary of SF1 (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of SF1:', '\n')
      nlay <- dim(sf1)[3]
    }
    names <- paste('Layer', 1:length(bcf$layavg))
    apply(sf1, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
  }
  
  # SF2
  if(!is.null(bcf$sf2)) {
    sf2 <- bcf$sf2[,,which(bcf$laycon %in% c(2,3))]
    if(length(dim(sf2)) == 2) sf2 <- rmf_create_array(sf2, dim = c(dim(sf2), 1))
    if(dim(sf2)[3] > n) {
      cat('Summary of SF2 (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of SF2:', '\n')
      nlay <- dim(sf2)[3]
    }
    names <- paste('Layer', which(bcf$laycon %in% c(2,3)))
    apply(sf2, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
  }
  
  # WETDRY
  if(!is.null(bcf$wetdry) && bcf$iwdflg != 0) {
    wetdry <- bcf$wetdry[,,which(bcf$laycon %in% c(1,3))]
    if(length(dim(wetdry)) == 2) wetdry <- rmf_create_array(wetdry, dim = c(dim(wetdry), 1))
    if(dim(wetdry)[3] > n) {
      cat('Summary of WETDRY (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of WETDRY:', '\n')
      nlay <- dim(wetdry)[3]
    }
    names <- paste('Layer', which(bcf$laycon %in% c(1,3)))
    apply(wetdry, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
  }
}

#' @export
print.hfb <- function(hfb, n = 15) {

  i_parm <- nrow(subset(hfb$data, parameter == TRUE))
  i_noparm <- nrow(subset(hfb$data, parameter == FALSE))
  i_parm_active <- nrow(subset(hfb$data, parameter == TRUE & active == TRUE))
  
  cat('RMODFLOW Horizontal-Flow Barrier object with:', '\n')
  if(hfb$dimensions$np > 0) cat(hfb$dimensions$np, 'parameters representing', i_parm, 'horizontal-flow barriers', '(', i_parm_active, 'are active)', '\n')
  cat(i_noparm, 'non-parameter horizontal-flow barriers', '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(nrow(hfb$data) > n, c('Summary of the first', n, 'defined horizontal-flow barriers:'), 'Summary of the defined horizontal-flow barriers:'), '\n')
  rmfi_ifelse0(nrow(hfb$data) > n, print(as.data.frame(hfb$data)[1:n, ]), print(as.data.frame(hfb$data)))
  cat('\n')
  
}
 
#' @export
print.riv <- function(riv, n = 15) {
  
  i_parm <- nrow(subset(riv$data, parameter == TRUE))
  i_noparm <- nrow(subset(riv$data, parameter == FALSE))
  
  cat('RMODFLOW River Package object with:', '\n')
  if(riv$dimensions$np > 0) cat(riv$dimensions$np, if(!is.null(riv$dimensions$instances)) {'time-varying'}, 'parameters representing', i_parm, 'river cells', '\n')
  cat(i_noparm, 'non-parameter river cells', '\n')
  if(!is.null(riv$aux)) cat('Auxiliary variables defined:', riv$aux, '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(riv$irivcb == 0, 'RIV fluxes are not saved to a cell-by-cell flow budget file', c('RIV fluxes are saved to the cell-by-cell flow budget file on unit number', riv$irivcb)), '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(nrow(riv$data) > n, c('Summary of the first', n, 'defined river cells:'), 'Summary of the defined river cells:'), '\n')
  rmfi_ifelse0(nrow(riv$data) > n, print(as.data.frame(riv$data)[1:n, ]), print(as.data.frame(riv$data)))
  cat('\n')
  
  cat('Summary of the stress-period', rmfi_ifelse0(nrow(riv$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(riv$kper) > n, print(riv$kper[1:n, ]), print(riv$kper))
  
}
 
#' @export
print.drn <- function(drn, n = 15) {
  
  i_parm <- nrow(subset(drn$data, parameter == TRUE))
  i_noparm <- nrow(subset(drn$data, parameter == FALSE))
  
  cat('RMODFLOW Drain Package object with:', '\n')
  if(drn$dimensions$np > 0) cat(drn$dimensions$np, if(!is.null(drn$dimensions$instances)) {'time-varying'}, 'parameters representing', i_parm, 'drain cells', '\n')
  cat(i_noparm, 'non-parameter drain cells', '\n')
  if(!is.null(drn$aux)) cat('Auxiliary variables defined:', drn$aux, '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(drn$idrncb == 0, 'DRN fluxes are not saved to a cell-by-cell flow budget file', c('DRN fluxes are saved to the cell-by-cell flow budget file on unit number', drn$idrncb)), '\n')
  cat('\n')
  
  cat(rmfi_ifelse0(nrow(drn$data) > n, c('Summary of the first', n, 'defined drain cells:'), 'Summary of the defined drain cells:'), '\n')
  rmfi_ifelse0(nrow(drn$data) > n, print(as.data.frame(drn$data)[1:n, ]), print(as.data.frame(drn$data)))
  cat('\n')
  
  cat('Summary of the stress-period', rmfi_ifelse0(nrow(drn$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(drn$kper) > n, print(drn$kper[1:n, ]), print(drn$kper))
  
}

#' @export
print.evt <- function(evt, n = 5) {
  
  cat('RMODFLOW Evapotranspiration Package object with:', '\n')
  cat(length(evt$evt), 'evapotranspiration', ifelse(length(evt$evt) > 1, 'arrays', 'array'), '\n')
  if(evt$dimensions$np > 0) cat('including', evt$dimensions$np, if(!is.null(evt$dimensions$instances)) {'time-varying'}, 'parameters', '\n')
  if(evt$nevtop == 1) {
    nevtop <- 'the top grid layer'
  } else if(evt$nevtop == 2) {
    nevtop <- 'cells defined by layer variable ievt'
  } 
  cat('Evapotranspiration calculated for', nevtop, '\n')
  cat('\n')
  cat(rmfi_ifelse0(evt$ievtcb == 0, 'EVT fluxes are not saved to a cell-by-cell flow budget file', c('EVT fluxes are saved to the cell-by-cell flow budget file on unit number', evt$ievtcb)), '\n')
  cat('\n')
  
  # for time-varing parameters
  list_arrays <- function(i) {
    if(is.list(i) && !is.null(attr(i[[1]], 'instnam'))) {
      return(i)
    } else {
      return(list(i))
    }
  }
  rmf_arrays <- lapply(evt$evt, list_arrays)
  rmf_arrays <- do.call(c, rmf_arrays)
  
  # evt
  if(length(rmf_arrays) > n) {
    cat('Summary of evapotranspiration (first', n, 'arrays):', '\n')
    nlay <- n
  } else {
    cat('Summary of evapotranspiration arrays:', '\n')
    nlay <- length(rmf_arrays)
  }
  
  abind::abind(rmf_arrays, along = 3) %>%
    apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(names(rmf_arrays)) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # surf
  if(length(evt$surf) > n) {
    cat('Summary of ET surface (first', n, 'arrays):', '\n')
    nlay <- n
  } else {
    cat('Summary of ET surface arrays:', '\n')
    nlay <- length(evt$surf)
  }
  
  abind::abind(evt$surf, along = 3) %>%
    apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(names(evt$surf)) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # exdp
  if(length(evt$exdp) > n) {
    cat('Summary of ET extinction depth (first', n, 'arrays):', '\n')
    nlay <- n
  } else {
    cat('Summary of ET extinction depth arrays:', '\n')
    nlay <- length(evt$exdp)
  }
  
  abind::abind(evt$exdp, along = 3) %>%
    apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(names(evt$exdp)) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # ievt
  if(evt$nevtop == 2) {
    cat('\n')
    if(length(evt$ievt) > n) {
      cat('Summary of ievt (first', n, 'arrays):', '\n')
      nlay <- n
    } else {
      cat('Summary of ievt arrays:', '\n')
      nlay <- length(evt$ievt)
    }
    
    abind::abind(evt$ievt, along = 3) %>%
      apply(3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names(evt$ievt)) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  cat('Summary of the stress-period', rmfi_ifelse0(nrow(evt$kper) > n, c('information (first', n, 'stress-periods shown):'), 'information:'), '\n')
  rmfi_ifelse0(nrow(evt$kper) > n, print(evt$kper[1:n, ]), print(evt$kper))
  
}
 
#' @export
print.sip <- function(sip) {
  
  cat('RMODFLOW Strongly Implicit Procedure Package object with:', '\n')
  cat('A maximum of', sip$mxiter, 'outer iterations', '\n')
  cat(sip$nparm, 'iteration variables', '\n')
  cat('\n')
  cat('An acceleration variable of', sip$accl, '\n')
  cat('Head change criterion for convergence:', sip$hclose, '\n')
  cat('The seed for calculating iteration variables', ifelse(sip$ipcalc == 0, paste('is', sip$wseed), 'will be calculated from problem variables'), '\n')
  cat('Maximum head change for each iteration is printed to the listing file every', ifelse(sip$iprsip == 0, 999, sip$iprsip), '\n')

}

#' @export
print.de4 <- function(de4) {
  
  cat('RMODFLOW Direct Solver Package object with:', '\n')
  cat('A maximum of', de4$itmx, 'iterations per time step', '\n')
  cat('A maximum of', ifelse(de4$mxup == 0, 'total cells/2', de4$mxup), 'equations being solved in the upper part of the equations', '\n')
  cat('A maximum of', ifelse(de4$mxlow == 0, 'total cells/2', de4$mxlow), 'equations being solved in the lower part of the equations', '\n')
  cat('A maximum band width of the [AL] matrix', ifelse(de4$mxbw == 0, 'equal to the product of the two smallest grid dimensions + 1', paste('of', de4$mxbw, '+ 1')), '\n')
  cat('\n')
  if(de4$ifreq == 1) {
    ifreq <- 'remain constant for all stress periods'
  } else if(de4$ifreq == 2) {
    ifreq <- 'depending on stress terms change at the start of each stress period'
  } else if(de4$ifreq == 3) {
    ifreq <- 'are nonlinear'
  }
  cat('Coefficients in the [A] matrix', ifreq, '\n')
  if(de4$mutd4 == 0) {
    mutd4 <- 'The number of iterations in each time step and the maximum head change each iteration are'
  } else if(de4$mutd4 == 1) {
    mutd4 <- 'The number of iterations each time step is'
  } else if(de4$mutd4 == 2) {
    mutd4 <- 'No convergence information is'
  }
  cat(mutd4, 'printed to the listing file every', de4$iprd4, 'time steps', '\n')
  cat('The multiplier for computed head change each iteration is', de4$accl, '\n')
  cat('Head change criterion for convergence:', de4$hclose, '\n')

}

#' @export
print.nwt <- function(nwt) {
  
  cat('RMODFLOW Newton Solver object:', '\n')
  cat('Maximum head change for closure of the outer iteration loop:', nwt$headtol, '\n')
  cat('Maximum root-mean squared flux difference for closure of the outer iteration loop:', nwt$fluxtol, '\n')
  cat('Maximum allowed number of outer iterations:', nwt$maxiterout, '\n')
  cat('THICKFACT (portion of cell thickness used for smoothly adjusting S & C coefficients to zero):', nwt$thickfact, '\n')
  cat('\n')
  cat(ifelse(nwt$linmeth == 1, 'The GMRES', 'The xMD'), 'matrix solver will be used', '\n')
  cat('Information about solver convergence will', ifelse(nwt$iprnwt == 0, 'not be printed', 'be printed'), 'to the listing file', '\n')
  cat('Corrections to heads in cells surrounded by dewatered cells will', ifelse(nwt$ibotav == 0, 'not be made', 'be made'), '\n')
  cat('\n')
  
  if(toupper(nwt$options) == 'SPECIFIED') {
    cat('The following solver options are user-specified:', '\n')
    cat('\n')
    cat('If the model fails to converge during a time step,', ifelse(nwt$continue, 'it will continue to solve the next time step', 'model execution will stop'), '\n')
    cat('DBDTHETA:', nwt$dbdtheta, '\n')
    cat('DBDKAPPA:', nwt$dbdkappa, '\n')
    cat('DBDGAMMA:', nwt$dbdgamma, '\n')
    cat('MOMFACT:', nwt$momfact, '\n')
    cat('Residual control is', ifelse(nwt$backflag == 0, 'inactive:', 'active:'), '\n')
    if(nwt$backflag > 0) {
      cat('MAXBACKITER:', nwt$maxbackiter, '\n')
      cat('BACKTOL:', nwt$backtol, '\n')
      cat('BACKREDUCE:', nwt$backreduce, '\n')
    }
    cat('\n')
    
    cat('Variables for the linear solution using the', ifelse(nwt$linmeth == 1, 'GMRES', 'xMD'), 'solver:', '\n')
    if(nwt$linmeth == 1) {
      cat('Maximum number of iterations for the linear solution:', nwt$itinner, '\n')
      cat('The', ifelse(nwt$ilumethod == 1, 'ILU method with drop tolerance and fill limit', 'ILU(k) method'), 'is used as preconditioner', '\n')
      cat(ifelse(nwt$ilumethod == 1, 'Fill limit:', 'Level of fill:'), nwt$levfill, '\n')
      cat('Tolerance for convergence of the linear solver:', nwt$stoptol, '\n')
      cat('Number of iterations between restarts of the GMRES solver:', nwt$msdr, '\n')
    } else if(nwt$linmeth == 2) {
      acc <- c('conjugate gradient', 'ORTHOMIN', 'Bi-CGSTAB')
      norder <- c('original', 'RCM', 'Minimum Degree')
      cat('The', acc[nwt$iacl + 1], 'acceleration method is used', '\n')
      cat('The', norder[nwt$norder + 1], 'ordering method is used', '\n')
      cat('Level of fill for the incomplete LU factorization:', nwt$level, '\n')
      cat('Number of orthogonalization if ORTHOMIN acceleration is used:', nwt$north, '\n')
      cat('Reduced system preconditioning is', ifelse(nwt$iredsys == 0, 'not applied', 'applied'), '\n')
      cat('Residual reduction-convergence criteria (if used):', nwt$rrctols, '\n')
      cat('Drop tolerance in the preconditioning is', ifelse(nwt$idroptol == 0, 'not used', 'used'), '\n')
      cat('Drop tolerance for preconditioning (if used):', nwt$epsrn, '\n')
      cat('Head closure criteria for inner iterations:', nwt$hclosexmd, '\n')
      cat('Maximum number of inner iterations:', nwt$mxiterxmd, '\n')
    }

  } else {
    lvl <- c('SIMPLE' = 'linear', 'MODERATE' = 'moderately nonlinear', 'COMPLEX' = 'highly nonlinear')
    cat('Default', toupper(nwt$options), 'solver options are used corresponding to', lvl[toupper(nwt$options)], 'models (see MODFLOW-NWT manual for detailed values)', '\n')
  }
    
}

#' @export
print.upw <- function(upw, n = 5) {
  
  cat('RMODFLOW Upstream Weighting Package object with:', '\n')
  if(upw$npupw > 0) cat(upw$npupw, ifelse(upw$npupw > 1 , 'flow parameters', 'flow parameter'), '\n')
  cat('Cell-by-cell flow terms', ifelse(upw$iupwcb == 0, 'not written',
                                        ifelse(upw$iupwcb > 0, paste('written to file number', upw$iupwcb), 
                                               '(only flow between constant-head cells) printed to the listing file')), '\n')
  cat('Dry cells are assigned a head value of', upw$hdry, '\n')
  if(upw$ihdry > 0) cat('When heads are less than 1e-4 above the bottom of a cell, they will be set to', upw$hdry, '\n')
  cat('\n')
  
  # options
  if(upw$noparcheck) {
    cat('There is no check to see if a variable is defined for all cells when parameters are used', '\n')
    cat('\n')
  }

  # Layer overview
  ll <- data.frame('Layer' = 1:length(upw$laytyp), 'Type' = 'Confined', 'Averaging' = 'Harmonic',
                   'CHANI' = 'HANI', 'VKA' = 'VK', stringsAsFactors = FALSE)
  ll$Type[which(upw$laytyp != 0)] <- 'Convertible'
  avg <- c('Harmonic', 'Logarithmic', 'Arithmetic THCK + Log K')
  ll$Averaging <- avg[upw$layavg + 1]
  ll$CHANI <- replace(ll$CHANI, which(upw$chani > 0), upw$chani[which(upw$chani > 0)])
  ll$VKA <- replace(ll$VKA, which(upw$layvka != 0), 'VANI')
  if(length(upw$laytyp) > n) {
    cat('Layer overview (first', n, 'layers): ', '\n')
    nlay <- n
  } else {
    cat('Layer overview:', '\n')
    nlay <- length(upw$laytyp)
  }
  print(ll[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # Parameters
  if(upw$npupw > 0) {
    pdf <- data.frame('Name' = vapply(upw$parameters, function(i) attr(i, 'parnam'), 'txt'),
                      'Type' = vapply(upw$parameters, function(i) attr(i, 'partyp'), 'txt'), 
                      'Layer' = vapply(upw$parameters, function(i) paste(attr(i, 'layer'), collapse = ' '), 'text'), 
                      'Value' = vapply(upw$parameters, function(i) attr(i, 'parval'), 1), 
                      stringsAsFactors = FALSE)
    if(nrow(pdf) > n) {
      cat('Parameter overview (first', n, 'parameters): ', '\n')
      nlay <- n
    } else {
      cat('Parameter overview:', '\n')
      nlay <- nrow(pdf)
    }
    print(pdf[1:nlay,], row.names = FALSE)
    cat('\n')
  }
  
  # HK
  if(length(upw$laytyp) > n) {
    cat('Summary of HK (first', n, 'layers):', '\n')
    nlay <- n
  } else {
    cat('Summary of HK:', '\n')
    nlay <- length(upw$laytyp)
  }
  apply(upw$hk, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(paste('Layer', 1:length(upw$laytyp))) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
  # HANI
  if(!is.null(upw$hani)) {
    if(length(upw$laytyp) > n) {
      cat('Summary of HANI (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of HANI:', '\n')
      nlay <- length(upw$laytyp)
    }
    apply(upw$hani, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(upw$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # VKA
  if(!is.null(upw$vka)) {
    if(length(upw$laytyp) > n) {
      cat('Summary of VKA (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of VKA:', '\n')
      nlay <- length(upw$laytyp)
    }
    apply(upw$vka, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(upw$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # SS
  if(!is.null(upw$ss)) {
    if(length(upw$laytyp) > n) {
      cat('Summary of SS (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of SS:', '\n')
      nlay <- length(upw$laytyp)
    }
    apply(upw$ss, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(upw$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # SY
  if(!is.null(upw$sy)) {
    sy <- upw$sy[,,which(upw$laytyp != 0)]
    if(length(dim(sy)) == 2) sy <- rmf_create_array(sy, dim = c(dim(sy), 1))
    if(dim(sy)[3] > n) {
      cat('Summary of SY (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of SY:', '\n')
      nlay <- dim(sy)[3]
    }
    names_sy <- paste('Layer', which(upw$laytyp != 0))
    apply(sy, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(names_sy) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
  
  # VKCB
  if(!is.null(upw$vkcb)) {
    if(length(upw$laytyp) > n) {
      cat('Summary of VKCB (first', n, 'layers):', '\n')
      nlay <- n
    } else {
      cat('Summary of VKCB:', '\n')
      nlay <- length(upw$laytyp)
    }
    apply(upw$vkcb, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:length(upw$laytyp))) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
  }
} 

#' @export
print.lvda <- function(lvda, n = 10) {
  
  cat('RMODFLOW Model-Layer Variable-Direction Horizontal Anisotropy Capability object with:', '\n')
  cat(lvda$nplvda, ifelse(lvda$nplvda > 1, 'parameters', 'parameter'), '\n')
  cat('Parameters represent the angle between the grid axis and the principal direction of horizontal hydraulic conductivity (defined in the HUF package)', '\n')
  cat('\n')
  
  # Parameters
  pdf <- data.frame('Name' = vapply(lvda$parameters, function(i) attr(i, 'parnam'), 'txt'),
                    'Layer' = vapply(lvda$parameters, function(i) paste(attr(i, 'layer'), collapse = ' '), 'text'), 
                    'Value' = vapply(lvda$parameters, function(i) attr(i, 'parval'), 1), 
                    stringsAsFactors = FALSE)
  if(nrow(pdf) > n) {
    cat('Parameter overview (first', n, 'parameters): ', '\n')
    nlay <- n
  } else {
    cat('Parameter overview:', '\n')
    nlay <- nrow(pdf)
  }
  print(pdf[1:nlay,], row.names = FALSE)
  cat('\n')
  
  # LVDA
  if(dim(lvda$lvda)[3] > n) {
    cat('Summary of LVDA (first', n, 'layers):', '\n')
    nlay <- n
  } else {
    cat('Summary of LVDA:', '\n')
    nlay <- dim(lvda$lvda)[3]
  }
  apply(lvda$lvda, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
    setNames(paste('Layer', 1:dim(lvda$lvda)[3])) %>% subset(select = 1:nlay) %>% print()
  cat('\n')
  
}

#' @export
print.hob <- function(hob, n = 15) {
  
  cat('RMODFLOW Head-Observation Package object with:', '\n')
  cat(hob$dimensions$nh, 'head observations of which', hob$dimensions$mobs, 'are multilayer observations', '\n')
  if(hob$dimensions$mobs > 0) cat('Maximum number of layers used for multilayer observations is', hob$dimensions$maxm, '\n')
  cat('\n')
  cat('Observed values and simulated equivalents are', ifelse(hob$iuhobsv == 0, 'not written to a head-prediction file', paste('written to the head-prediction file on unit number', hob$iuhobsv)), '\n')
  if(hob$iuhobsv > 0) cat('Simulated equivalents of dry cells are assigned a value of', hob$hobdry, 'in the head-prediction file', '\n')
  cat('Input and output data are', ifelse(hob$noprint, 'printed', 'not printed'), 'to the listing file', '\n')
  cat('Time-offset multiplier:', hob$tomulth, '\n')
  cat('\n')
  
  # Data
  if(nrow(hob$data) > n) {
    cat('Observations overview (first', n, 'records): ', '\n')
    nlay <- n
  } else {
    cat('Observations overview:', '\n')
    nlay <- nrow(hob$data)
  }
  print(hob$data[1:nlay,], row.names = TRUE)
  # cat('\n')
  
}

#' @export
print.hed <- function(obj, ...) {
  
  cat(paste('RMODFLOW 4d array representing hydraulic head output with', dim(obj)[1], ifelse(dim(obj)[1] > 1, 'rows,', 'row,'),
            dim(obj)[2], ifelse(dim(obj)[2] > 1, 'columns,', 'column'), dim(obj)[3],
            ifelse(dim(obj)[3] > 1, 'layers', 'layer'), 'and', dim(obj)[4], 
            ifelse(dim(obj)[4] > 1, 'timesteps,', 'timestep'), 'representing the', 
            paste(attr(obj, 'dimlabels')[1:3], collapse = ', '), paste('&', attr(obj, 'dimlabels')[4]),
            'dimensions.', '\n'))

  print(as.array(obj), ...)
  
}

#' @export
print.ddn <- function(obj, ...) {
    
  cat(paste('RMODFLOW 4d array representing drawdown output with', dim(obj)[1], ifelse(dim(obj)[1] > 1, 'rows,', 'row,'),
            dim(obj)[2], ifelse(dim(obj)[2] > 1, 'columns,', 'column'), dim(obj)[3],
            ifelse(dim(obj)[3] > 1, 'layers', 'layer'), 'and', dim(obj)[4], 
            ifelse(dim(obj)[4] > 1, 'timesteps,', 'timestep'), 'representing the', 
            paste(attr(obj, 'dimlabels')[1:3], collapse = ', '), paste('&', attr(obj, 'dimlabels')[4]),
            'dimensions.', '\n'))

  print(as.array(obj), ...)
  
}

#' @export
print.bud <- function(bud, n = 10) {
  
  cat('RMODFLOW volumetric budget output object with:', '\n')
  kper <- length(unique(bud$rates$kper))
  cat('A total of', nrow(bud$rates), ifelse(nrow(bud$rates) > 1, 'time steps', 'time step'), 'for',
      kper, ifelse(kper > 1, 'stress periods', 'stress period'), '\n')
  cat((ncol(bud$rates) - 6)/2, 'flow terms:', '\n')
  
  fluxes <- names(bud$rates) %>%
             setdiff(c('kstp', 'kper', 'total_in', 'total_out', 'difference', 'discrepancy')) %>%
             strsplit("\\_in|\\_out") %>%
             unlist
  fluxes <- fluxes[1:(length(fluxes)/2)]
  cat(' ', fluxes, '\n')
  cat('\n')
  
  if(nrow(bud$rates) > n) {
    cat('Overview of volumetric rates (first', n, 'time steps):', '\n')
    nlay <- n
  } else {
    cat('Overview of volumetric rates:', '\n')
    nlay <- nrow(bud$rates)
  }
  print(bud$rates[1:nlay,], row.names = FALSE)
  cat('\n')
  
  if(nrow(bud$cumulative) > n) {
    cat('Overview of cumulative volumes (first', n, 'time steps):', '\n')
    nlay <- n
  } else {
    cat('Overview of cumulative volumes:', '\n')
    nlay <- nrow(bud$cumulative)
  }
  print(bud$cumulative[1:nlay,], row.names = FALSE)
  cat('\n')
  
  cat('Final cumulative difference:', bud$cumulative$difference[nrow(bud$cumulative)], '\n')
  cat('Final cumulative percent discrepancy:', bud$cumulative$discrepancy[nrow(bud$cumulative)], '\n')
  
}

#' @export
print.cbc <- function(cbc, n = 5, l = -1) {
  
  cat('RMODFLOW cell-by-cell flow budget output object with:', '\n')
  cat(length(cbc), 'flow terms:', '\n')
  cat(' ', names(cbc), '\n')
  nstp <- length(attr(cbc[[1]], 'kstp'))
  kper <- length(unique(attr(cbc[[1]], 'kper')))
  cat('Representing', nstp, ifelse(nstp > 1, 'time steps', 'time step'), 'in',
      kper, ifelse(kper > 1, 'stress periods', 'stress period'), '\n')
  cat('\n')
  
  for(i in 1:length(cbc)) {

    if(inherits(cbc[[i]], 'rmf_4d_array')) {
      ll <- ifelse(l < 0, tail(attr(cbc[[i]], 'kstp'), 1), l)
      if(dim(cbc[[i]])[3] > n) {
        cat('Summary of', names(cbc)[i] ,'(first', n, 'layers) for time step', paste0(ll, ':'), '\n')
        nlay <- n
      } else {
        cat('Summary of', names(cbc)[i], 'for time step', paste0(ll, ':'), '\n')
        nlay <- dim(cbc[[i]])[3]
      }
     apply(cbc[[i]][,,,ll], 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
      setNames(paste('Layer', 1:dim(cbc[[i]])[3])) %>% subset(select = 1:nlay) %>% print()
    cat('\n')
    
    } else if(inherits(cbc[[i]], 'data.frame')) {
      ll <- ifelse(l < 0, tail(attr(cbc[[i]], 'kstp'), 1), l)
      df <- subset(cbc[[i]], kstp == ll)
      if(nrow(df) > n) {
        cat('Overview of', names(cbc)[i], '(first', n, 'records) for time step', paste0(ll, ':'), '\n')
        nlay <- n
      } else {
        cat('Overview of', names(cbc)[i], 'for time step', paste0(ll, ':'), '\n')
        nlay <- nrow(df)
      }
      print(as.data.frame(df[1:nlay,]), row.names = FALSE)
      cat('\n')
    }
  }
  
}

#' @export
print.hpr <- function(hpr, n = 20) {
 
  cat('RMODFLOW head predictions output object with:', '\n')
  cat(nrow(hpr), 'records', '\n')
  cat('\n')

  if(nrow(hpr) > n) {
    cat('Overview of head predictions', '(first', n, 'records):', '\n')
    nlay <- n
  } else {
    cat('Overview of head predictions:', '\n')
    nlay <- nrow(hpr)
  }
  print.data.frame(hpr[1:nlay,], row.names = TRUE)
  cat('\n')
  
  cat('Goodness-of-fit metrics:', '\n')
  metrics <- suppressWarnings(rmf_performance(hpr, measures = c('rmse', 'pbias', 'r2', 'kge', 'ssq')))
  # pretty
  metrics <- lapply(metrics, function(i) ifelse(i > 1e5, as.numeric(formatC(i, format = 'e', digits = 2)), round(i, 2)))
  print(data.frame(metrics), row.names = FALSE)
   
}

#' @export
print.modflow <- function(modflow, n = 5) {
  
  packages <- c(rmfi_list_packages()$rmf, 'nam')
  input <- intersect(names(modflow), packages)
  output <- setdiff(names(modflow), packages)
  ftype <- modflow$nam$ftype[-which(modflow$nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)'))]  
  not_supported <- ftype[-which(ftype %in% rmfi_list_packages()$ftype)]

  # TODO add other versions
  v <- ifelse('upw' %in% input && 'nwt' %in% input, 'MODFLOW-NWT', 'MODFLOW-2005')
  
  cat('RMODFLOW', v,'model object with:', '\n')
  cat(length(input), 'input objects:', '\n')
  cat(' ', input, '\n')
  cat('\n')
  cat(length(output), ifelse(length(output) > 0, 'output objects:', 'output objects'), '\n')
  if(length(output) > 0) {
    cat(' ', output, '\n')
    cat('\n')
  } else {
    cat('\n')
  }
  if(length(not_supported) > 0) {
    cat(length(not_supported), ifelse(length(not_supported) > 1, 'packages', 'package') ,'not yet supported:', '\n')
    cat(' ', not_supported, '\n')
    cat('\n')
  }
  
  ibound <- table(modflow$bas$ibound)
  cat(paste(modflow$dis$nrow, ifelse(modflow$dis$nrow > 1, 'rows,', 'row,'), modflow$dis$ncol, ifelse(modflow$dis$ncol > 1, 'columns', 'column'),
            'and', modflow$dis$nlay, ifelse(modflow$dis$nlay > 1, 'layers', 'layer'), 'totalling', modflow$dis$nrow*modflow$dis$ncol*modflow$dis$nlay, 'cells',
            paste0('(', ifelse('0' %in% names(ibound), ibound['0'], '0'), ' inactive & ', ifelse('-1' %in% names(ibound), ibound['-1'], '0'), ' constant head',')'),'\n'))
  cat('\n')
  
  # TODO add other observations
  if('hob' %in% input) {
    cat(modflow$hob$dimensions$nh, ifelse(modflow$hob$dimensions$nh > 1, 'head observations', 'head observation'), '\n')
    cat('\n')
  }
  
  # Parameters
  parm <- vapply(modflow[input], function(i) !is.null(i[['parameter_values']]), TRUE)
  if(any(parm)) {
    cat('Parameters defined in following packages:', '\n')
    cat(' ', input[parm], '\n')
    cat('\n')
  }

  # stress periods
  sp_names <- setNames(c('Steady-state', 'Transient'), c('SS', 'TR'))
  sp <- data.frame(kper = 1:modflow$dis$nper, perlen = modflow$dis$perlen, nstp = modflow$dis$nstp, tsmult = modflow$dis$tsmult, sstr = sp_names[modflow$dis$sstr])
  names(sp) <- c('Period', 'Length', 'Timesteps', 'Multiplier', 'Type')
  if(modflow$dis$nper > n) {
    cat(modflow$dis$nper, if(modflow$dis$nper > 1) 'stress-periods' else 'stress-period', '(first', n, 'shown):', '\n')
    nper <- n
  } else {
    cat(modflow$dis$nper, if(modflow$dis$nper > 1) 'stress-periods:' else 'stress-period:', '\n')
    nper <- modflow$dis$nper
  }
  print(sp[1:nper,], row.names = FALSE)
  cat('\n')
  
  # output
  if('bud' %in% output) cat('Final percent discrepancy:', tail(modflow$bud$cumulative, 1)$discrepancy, '\n')
  if('hpr' %in% output) {
    cat('Goodness-of-fit metrics (head observations):', '\n')
    metrics <- suppressWarnings(rmf_performance(modflow$hpr, measures = c('rmse', 'pbias', 'r2', 'kge', 'ssq')))
    # pretty
    metrics <- lapply(metrics, function(i) ifelse(i > 1e5, as.numeric(formatC(i, format = 'e', digits = 2)), round(i, 2)))
    print(data.frame(metrics), row.names = FALSE)
  }
  
}

#' @export
print.gmg <- function(gmg) {
  
  cat('RMODFLOW Geometric Multigrid Solver object with:', '\n')
  cat('A maximum of', gmg$iiter, 'inner iterations with a residual convergence criterion of', gmg$rclose, '\n')
  cat('A maximum of', gmg$mxiter, 'outer iterations with a head change convergence criterion of', gmg$hclose, '\n')
  cat('\n')
  if(gmg$iadamp == 0) {
    cat('A constant damping parameter:', gmg$damp, '\n')
  } else if(gmg$iadamp == 1) {
    cat('Adaptive-damping using Cooley\'s method with initial damping value:', gmg$damp, '\n')
  } else if(gmg$iadmap == 2) {
    cat('Relative reduced residual damping with:', '\n')
    cat('  Damping value:', gmg$damp, '\n')
    cat('  Maximum damping value applied when solver is not oscillating:', gmg$dup, '\n')
    cat('  Minimum damping value to be generated by the adaptive-damping:', gmg$dlow, '\n')
    cat('  Maximum allowed head change between outer iterations:', gmg$chglimit, '\n')
  }
  cat('\n')
  if(gmg$ioutgmg == 0) {
    cat('Solver input is printed to the listing file', '\n')
  } else if(gmg$ioutgmg == 1) {
    cat('Detailed solver output is printed to the listing file for each linear solve', '\n')
  } else if(gmg$ioutgmg == 2) {
    cat('Basic solver solver output is printed to the listing file', '\n')
  } else if(gmg$ioutgmg == 3) {
    cat('Detailed solver output is printed to the terminal for each linear solve', '\n')
  } else if(gmg$ioutgmg == 4) {
    cat('Basic solver solver output is printed to the terminal', '\n')
  }
  if(gmg$iunitmhc > 0) cat('Maximum head change values are written to the file on unit number', gmg$iunitmhc, '\n')
  cat('\n')
  
  if(gmg$ism == 0) {
    cat('ILU(0) smoothing is implemented in the multigrid preconditioner', '\n')
  } else if(gmg$ism == 1) {
    cat('Symmetric Gauss-Seidel smoothing is implemented in the multigrid preconditioner', '\n')
  }
  
  if(gmg$isc == 0) {
    cat('Rows, columns and layers are coarsened in the multigrid preconditioner', '\n')
  } else if(gmg$isc == 1) {
    cat('Rows and columns (not layers) are coarsened in the multigrid preconditioner', '\n')
  } else if(gmg$isc == 2) {
    cat('Columns and layers (not rows) are coarsened in the multigrid preconditioner', '\n')
  } else if(gmg$isc == 3) {
    cat('Rows and layers (not columns) are coarsened in the multigrid preconditioner', '\n')
  } else if(gmg$isc == 4) {
    cat('There is no coarsening in the multigrid preconditioner', '\n')
    cat('Relaxation parameter for the ILU preconditioner:', gmg$relax, '\n')
  }

}

#' @export
print.lmt <- function(lmt) {
  cat('RMODFLOW Link-MT3DMS Package object:', '\n')
  cat('Flow-transport link file:', lmt$fname, '\n')
  cat('on unit number:', lmt$inftl, '\n')
  cat('as', ifelse(lmt$formatted, 'a formatted', 'an unformatted'), 'file', 'using the', ifelse(lmt$extended, 'extended', 'standard'), 'header', '\n')
  if(!is.null(lmt$package_flows)) cat('Package flows:', lmt$package_flows)
}
