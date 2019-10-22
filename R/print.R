
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
  if(sum(dis$delr)/dis$ncol == dis$delr[1]) {
    delr <- c(dis$delr[1], '(constant)') 
  } else {
    if(dis$ncol > n) {
      delr <- c(dis$delr[1:n], '...')
    } else {
      delr <- dis$delr
    }
  }
  if(sum(dis$delc)/dis$nrow == dis$delc[1]) {
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
  sp <- data.frame(kper = 1:dis$nper, perlen = dis$perlen, nstp = dis$nstp, tsmult = dis$tsmult, sstr = sp_names[dis$sstr])
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
print.pvl <- function(pvl) {
  cat('RMODFLOW Parameter Value File object with:', '\n')
  cat(pvl$np, 'parameter values', '\n')
  cat('\n')
  print(data.frame(parnam = pvl$parnam, parval = pvl$parval))
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
  cat(huf$nhuf, 'hydrogeological units and', huf$nphuf, 'flow parameters', '\n')
  cat('\n')
  
  cat('Cell-by-cell flow terms are', ifelse(huf$ihufcb == 0, 'not writen', paste('written to file number', huf$ihufcb)), '\n')
  cat('Dry cells are assigned a head value of', huf$hdry, '\n')
  cat('\n')
  cat('Heads interpolated to hydrogeological units are', ifelse(huf$iohufheads == 0, 'not written', paste('written to file number', huf$iohufheads)), '\n')
  cat('Flow interpolated to hydrogeological units are', ifelse(huf$iohufflows == 0, 'not written', paste('written to file number', huf$iohufflows)), '\n')
  cat('\n')
  
  # Layer overview
  ll <- data.frame('Layer' = 1:length(huf$lthuf), 'Type' = 'Confined', 'Wetting' = 'Inactive', stringsAsFactors = FALSE)
  ll$Type[which(huf$lthuf != 0)] <- 'Convertible'
  ll$Wetting[which(huf$laywt != 0)] <- 'Active'
  cat('Layer overview:', '\n')
  print(ll, row.names = FALSE)
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
   names_wetdry <- paste('Layer', 1:dim(wetdry)[3])
   apply(huf$wetdry, 3, function(i) summary(c(i))) %>% as.data.frame() %>% 
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
  cat('HGU overview:', '\n')
  print(un, row.names = FALSE)
  cat('\n')
  
  # Parameters
  pdf <- data.frame('Name' = vapply(huf$parameters, function(i) attr(i, 'parnam'), 'txt'),
                    'Type' = vapply(huf$parameters, function(i) attr(i, 'partyp'), 'txt'), 
                    'Unit' = vapply(huf$parameters, function(i) attr(i, 'hgunam'), 'txt'), 
                    'Value' = vapply(huf$parameters, function(i) attr(i, 'parval'), 1), 
                    stringsAsFactors = FALSE)
  
  cat('Parameter overview:', '\n')
  print(pdf, row.names = FALSE)
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
print.oc <- function(oc, n = 15) {
  
  cat('RMODFLOW Output Control Option file', '\n')
  cat('\n')
  
  # words
  if(is.null(oc$incode)) {
    
    # save
    if(!is.na(oc$ihedun) && any(c(oc$save_head))) {
      cat('Simulated heads are written to a', if(oc$head_label) {'labelled'}, ifelse(is.na(oc$chedfm), 'binary', 'formatted'), 'file on unit number', oc$ihedun, 'at following time steps:', '\n')
      vc <- which(oc$save_head)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    if(!is.na(oc$iddnun) && any(c(oc$save_drawdown))) {
      cat('Simulated drawdowns are written to a', if(oc$drawdown_label) {'labelled'}, ifelse(is.na(oc$cddnfm), 'binary', 'formatted'), 'file on unit number', oc$iddnun, 'at following time steps:', '\n')
      vc <- which(oc$save_drawdown)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    if(!is.na(oc$ibouun) && any(c(oc$save_ibound))) {
      cat('The ibound array is written to a', if(oc$ibound_label) {'labelled'}, ifelse(is.na(oc$cddnfm), 'binary', 'formatted'), 'file on unit number', oc$iddnun, 'at following time steps:', '\n')
      vc <- which(oc$save_ibound)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    if(any(c(oc$save_budget))) {
      cat('The', if(oc$compact_budget) {'compacted'}, 'cell-by-cell flow budget', if(oc$aux){'including auxiliary data'}, 'is saved to the binary files specified in the flow and/or stress-packages', 'at following time steps:', '\n')
      vc <- which(oc$save_budget)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    
    # print
    if(!is.na(oc$ihedfm) && any(c(oc$print_head))) {
      cat('Simulated heads are printed to the listing file', 'at following time steps:', '\n')
      vc <- which(oc$print_head)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    if(!is.na(oc$iddnfm) && any(c(oc$print_drawdown))) {
      cat('Simulated drawdowns are printed to the listing file', 'at following time steps:', '\n')
      vc <- which(oc$print_drawdown)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    if(any(c(oc$print_budget))) {
      cat('The volumetric budget is printed to the listing file', 'at following time steps:', '\n')
      vc <- which(oc$print_budget)
      cat(' ', rmfi_ifelse0(length(vc) > n, vc[1:n], vc), '\n')
      cat('\n')
    }
    
    
  } else { # codes
    
  }
  
}

#' @export
print.wel <- function(wel, n = 15) {
  
  i_parm <- nrow(subset(wel$data, parameter == TRUE))
  i_noparm <- nrow(subset(wel$data, parameter = FALSE))
  
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
  i_noparm <- nrow(subset(ghb$data, parameter = FALSE))
  
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

#' #' @export
#' print.pcg
#' 
#' #' @export
#' print.kdep
#' 
#' #' @export
#' print.lpf

#' @export
print.rch <- function(rch, n = 5) {
  
  cat('RMODFLOW Recharge Package object with:', '\n')
  if(rch$dimensions$np > 0) cat(rch$dimensions$np, if(!is.null(rch$dimensions$instances)) {'time-varying'}, 'parameters', '\n')
  cat(i_noparm, 'non-parameter specified-heads', '\n')
  if(!is.null(rch$aux)) cat('Auxiliary variables defined:', rch$aux, '\n')
  cat('\n')
  
}

#' @export
print.chd <- function(chd, n = 15) {
  
  i_parm <- nrow(subset(chd$data, parameter == TRUE))
  i_noparm <- nrow(subset(chd$data, parameter = FALSE))
  
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
 
#' #' @export
#' print.bcf

#' @export
print.hfb <- function(hfb, n = 15) {

  i_parm <- nrow(subset(hfb$data, parameter == TRUE))
  i_noparm <- nrow(subset(hfb$data, parameter = FALSE))
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
  i_noparm <- nrow(subset(riv$data, parameter = FALSE))
  
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
  i_noparm <- nrow(subset(drn$data, parameter = FALSE))
  
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
#'
#' #' @export
#' print.hob
#' 
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
#' 
#' #' @export
#' print.hpr

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
    cat(length(not_supported), 'not-supported', ifelse(length(not_supported) > 1, 'packages', 'package') ,'in nam object:', '\n')
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
    cat(modflow$hob$nh, ifelse(modflow$hob$nh > 1, 'head observations', 'head observation'), '\n')
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
  if('bud' %in% output) cat('Final percent discrepancy:', tail(modflow$bud$rates, 1)$discrepancy, '\n')
  if('hpr' %in% output) {
    cat('Goodness-of-fit metrics (head observations):', '\n')
    metrics <- suppressWarnings(rmf_performance(modflow$hpr, measures = c('rmse', 'pbias', 'r2', 'kge', 'ssq')))
    print(round(unlist(metrics), 2))
  }
  
}
