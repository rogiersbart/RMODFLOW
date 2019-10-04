
#' Create an \code{RMODFLOW} modflow object
#'
#' \code{rmf_create} creates an \code{RMODFLOW} modflow object from \code{rmf_package} objects 
#'
#' @param ... (list of) \code{RMODFLOW} objects of class \code{rmf_package} to be included in the modflow object. If a nam object is not provided, it is added automatically.
#' @param cbc optional integer; sets the flag and unit number for writing cell-by-cell flow data. Overwrites the values set in the objects. Defaults to NULL.
#'
#' @return a \code{modflow} object which is a list containing all MODFLOW packages
#' @export
#' @seealso \code{\link{rmf_read}}, \code{\link{rmf_write}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html}
rmf_create <- function(..., cbc = NULL) {
  
  modflow <- list(...)
  if(length(modflow) == 1 && inherits(modflow[[1]], c('list', 'modflow')) && !('rmf_package' %in% class(modflow[[1]]))) modflow <- unclass(modflow[[1]])
  ftype <- vapply(modflow, function(i) class(i)[which(class(i) == 'rmf_package') - 1], 'text')
  names(modflow) <- ftype
  
  # reset cbc if necessary
  if(!is.null(cbc)) {
    # some packages have i*cb1, i*cb2. SWI has iswibd
    set_cbc <- function(package) {
      cbc_name <- paste0('i', class(package)[which(class(package) == 'rmf_package')-1], 'cb')
      if(!is.null(package[[cbc_name]])) package[[cbc_name]] <- cbc
      if(!is.null(package[[paste0(cbc_name, '1')]])) package[[paste0(cbc_name, '1')]] <- cbc
      if(!is.null(package[[paste0(cbc_name, '2')]])) package[[paste0(cbc_name, '2')]] <- cbc
      if(!is.null(package[[paste0('i', class(package)[which(class(package) == 'rmf_package')-1], 'bd')]])) package[[paste0('i', class(package)[which(class(package) == 'rmf_package')-1], 'bd')]] <- cbc
      
      return(package)
    }
    
    modflow <- lapply(modflow, set_cbc)
  }
  
  # find nam object; if not present, create one. If present, check if all packages are also in nam
  if(!('nam' %in% ftype)) {
    modflow$nam <- rmf_create_nam(modflow)
  } else {
    df <- rmfi_list_packages(type = 'all')
    mf_types <- df$ftype[which(df$rmf %in% ftype)]
    nam_types <- modflow$nam$ftype[which(!(modflow$nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)')))]
    if(!isTRUE(all.equal(sort(mf_types), sort(nam_types)))) stop('Please make sure all packages are listed in the nam file.', call. = FALSE)
  }
  
  # check for dis, bas, flow package and solver
  error <- rep(0, 4)
  flow_packages <- c('bcf', 'lpf', 'huf', 'upw')
  solvers <- c('de4', 'gmg', 'pcg', 'pcgn', 'sip', 'nwt')
  error[1] <- sum(ftype == 'dis')
  error[2] <- sum(ftype == 'bas')
  error[3] <- sum(flow_packages %in% ftype)
  error[4] <- sum(solvers %in% ftype)
  if(any(error != 1)) stop('Please specify at least a dis, bas, solver and flow package. Only specify one of each.', call. = FALSE)
  
  # error check for modflow-nwt
  if(('upw' %in% ftype && !('nwt' %in% ftype)) || ('nwt' %in% ftype && !('upw' %in% ftype))) {
    stop('The upw and nwt have to be used together.', call. = FALSE)
  }
  
  class(modflow) <- c('modflow')
  return(modflow)
  
}

#' @describeIn rmf_create Deprecated function name
#' @export
create_modflow <- function(...) {
  .Deprecated(new = "rmf_create", old = "create_modflow")
  rmf_create(...)
}


#' Read a MODFLOW model
#'
#' \code{rmf_read} reads in a MODFLOW model and returns it as a \code{modflow} object
#'
#' @param file NAME file; typically '*.nam'
#' @param output logical; should output also be read. Defaults to FALSE.
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of binary output files if output is read. Defaults to \code{'single'}.
#' @param verbose logical; should information on reading files be printed to the console ? Defaults to TRUE.
#'
#' @return a \code{modflow} object which is a list containing all MODFLOW packages and optionally, model output
#' @export
#' @seealso \code{\link{rmf_create}}, \code{\link{rmf_write}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html}

rmf_read <- function(file = {cat('Please select nam file ...\n'); file.choose()},
                     output = FALSE,
                     precision = 'single',
                     verbose = TRUE) {
  
  modflow <- list()
  print_reading <- function(package, file, output = FALSE) {
    cat(paste0("---------------------------------", '\n'))
    if(output) {
      cat(paste0(paste("  Reading", package, "output from file", file), '\n'))
    } else {
      cat(paste0(paste("  Reading", package, "package from file", file), '\n'))
    }
  }
  
  # basic
  # first read nam, then dis, then bas, then mlt & zon
  if(verbose) print_reading('NAM', file = file)
  modflow$nam <- rmf_read_nam(file = file)
  dir_nam <- dirname(file)
  fname <- paste(dir_nam, modflow$nam$fname, sep = '/')
  ftype <- modflow$nam$ftype
  
  if(verbose) print_reading('DIS', file = fname[which(modflow$nam$ftype == 'DIS')])
  modflow$dis <- rmf_read_dis(file = fname[which(modflow$nam$ftype == 'DIS')], nam = modflow$nam)
  ftype <- ftype[-which(ftype == 'DIS')]
  
  if(verbose) print_reading('BAS6', file = fname[which(modflow$nam$ftype == 'BAS6')])
  modflow$bas <- rmf_read_bas(file = fname[which(modflow$nam$ftype == 'BAS6')], dis = modflow$dis, nam = modflow$nam)
  format <- ifelse(modflow$bas$free, 'free', 'fixed')
  ftype <- ftype[-which(ftype == 'BAS6')]
  
  if('MULT' %in% modflow$nam$ftype) {
    mltfiles <- which(modflow$nam$ftype == 'MULT')
    if(length(mltfiles) > 1) {
      mlt <- list(mltnam = vector(mode = 'character'), functn = vector(mode = 'logical'), rmlt = list(), operators = list(), iprn = vector(mode = 'numeric'))
      for(i in length(mltfiles)) {
        if(verbose) print_reading('MULT', file = fnamefname[which(modflow$nam$ftype == 'MULT')][i])
        mult <- rmf_read_mlt(file = fname[which(modflow$nam$ftype == 'MULT')][i], dis = modflow$dis, nam = modflow$nam, format = format)
        mlt$mltnam <- c(mlt$mltnam, mult$mltnam)
        mlt$functn <- c(mlt$functn, rmfi_ifelse0(is.null(mult$functn), rep(FALSE, mult$nml), mult$functn))
        mlt$rmlt <- c(mlt$rmlt, mult$rmlt)
        mlt$operators <- c(mlt$operators, rmfi_ifelse0(is.null(mult$functn), rep(NULL, mult$nml), mult$operators))
        mlt$iprn <- c(mlt$iprn, rmfi_ifelse0(is.null(mult$functn), rep(0, mult$nml), mult$iprn))
      }
      # make single object
      mlt <- rmf_create_mlt(mltnam = mlt$mltnam, functn = mlt$functn, rmlt = mlt$rmlt, operators = mlt$operators, iprn = mlt$iprn)
      
    } else {
      if(verbose) print_reading('MULT', file = fname[which(modflow$nam$ftype == 'MULT')])
      mlt <- rmf_read_mlt(file = fname[which(modflow$nam$ftype == 'MULT')], dis = modflow$dis, nam = modflow$nam, format = format)
    }
    modflow$mlt <- mlt
    ftype <- ftype[-which(ftype == 'MULT')]
  }
  
  if('ZONE' %in% modflow$nam$ftype) {
    zonfiles <- which(modflow$nam$ftype == 'ZONE')
    if(length(zonfiles) > 1) {
      zon <- list(zonnam = vector(mode = 'character'), izon = list())
      for(i in length(zonfiles)) {
        if(verbose) print_reading('ZONE', file = fnamefname[which(modflow$nam$ftype == 'ZONE')][i])
        zones <- rmf_read_zon(file = fname[which(modflow$nam$ftype == 'ZONE')][i], dis = modflow$dis, nam = modflow$nam, format = format)
        zon$zonnam <- c(zon$zonnam, zones$zonnam)
        zon$izon <- c(zon$izon, zones$izon)
      }
      # make single object
      zon <- rmf_create_zon(zonnam = zon$zonnam, izon = zon$izon)
    } else {
      if(verbose) print_reading('ZONE', file = fname[which(modflow$nam$ftype == 'ZONE')])
      zon <- rmf_read_zon(file = fname[which(modflow$nam$ftype == 'ZONE')], dis = modflow$dis, nam = modflow$nam, format = format)
    }
    modflow$zon <- zon
    ftype <- ftype[-which(ftype == 'ZONE')]
  }
  
  # pval
  if('PVAL' %in% modflow$nam$ftype) {
    if(verbose) print_reading('PVAL', file = fname[which(modflow$nam$ftype == 'PVAL')])
    modflow$pvl <- rmf_read_pvl(file = fname[which(modflow$nam$ftype == 'PVAL')])
    ftype <- ftype[-which(ftype == 'PVAL')]
  }
  
  # lgr
  # TODO update when lgr is supported
  # if('LGR' %in% modflow$nam$ftype) {
  #   if(verbose) print_reading('LGR', file = fname[which(modflow$nam$ftype == 'LGR')])
  #   modflow$lgr <- rmf_read_lgr(file = fname[which(modflow$nam$ftype == 'LGR')], dis = modflow$dis)
  #   ftype <- ftype[-which(ftype == 'LGR')]
  # }
  
  # flow packages
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'flow')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, nam = modflow$nam, mlt = modflow$mlt, zon = modflow$zon, format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }

  # boundary conditions 
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'boundary')

    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, format = format, nam = modflow$nam, mlt = modflow$mlt, zon = modflow$zon))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # solvers
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'solver')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # output control
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'oc')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # subsidence
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'sub')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, format = format, nam = modflow$nam, mlt = modflow$mlt, zon = modflow$zon))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # observation
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'obs')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # Surface-water routing
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'swr')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # Conduit flow process
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'cfp')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }
  
  # Farm process
  if(length(ftype) > 0) {
    df <- rmfi_list_packages(type = 'farm')
    
    i <- 1
    while(i <= length(df$rmf) && length(ftype) > 0) {
      if(df$ftype[i] %in% ftype) {
        if(verbose) print_reading(df$ftype[i], file = fname[which(modflow$nam$ftype == df$ftype[i])])
        funct <- paste0('rmf_read_', df$rmf[i])
        modflow[[df$rmf[i]]] <- do.call(funct, list(file = fname[which(modflow$nam$ftype == df$ftype[i])], dis = modflow$dis, format = format))
        ftype <- ftype[-which(ftype == df$ftype[i])]
      }
      i <- i+1
    }
  }

  
  # output
  if(output) {

    # hpr
    if('HOB' %in% modflow$nam$ftype && modflow$hob$iuhobsv != 0) {
      if(verbose) print_reading('HPR', file = fname[which(modflow$nam$nunit == modflow$hob$iuhobsv)], output = TRUE)
      modflow$hpr <- rmf_read_hpr(file = fname[which(modflow$nam$nunit == modflow$hob$iuhobsv)])
    }

    if('OC' %in% modflow$nam$ftype) {
      
      # heads
      if((!is.null(modflow$oc$save_head) && any(modflow$oc$save_head)) || (!is.null(modflow$oc$hdsv) && any(modflow$oc$hdsv != 0))) {
        if(verbose) print_reading('Head', file = fname[which(modflow$nam$nunit == modflow$oc$ihedun)], output = TRUE)
        
        # huf heads
        if(!is.null(modflow$huf) && modflow$huf$iohufheads != 0) {
          modflow$head <- rmf_read_head(file = fname[which(modflow$nam$nunit == modflow$oc$ihedun)], dis = modflow$dis, bas = modflow$bas, huf = modflow$huf, oc = modflow$oc,
                                        binary = modflow$nam$ftype[which(modflow$nam$nunit == modflow$oc$ihedun)] %in% c('DATA(BINARY)', 'DATAGLO(BINARY)'), precision = precision)
        } else {
          modflow$head <- rmf_read_head(file = fname[which(modflow$nam$nunit == modflow$oc$ihedun)], dis = modflow$dis, bas = modflow$bas, oc = modflow$oc,
                                        binary = modflow$nam$ftype[which(modflow$nam$nunit == modflow$oc$ihedun)] %in% c('DATA(BINARY)', 'DATAGLO(BINARY)'), precision = precision)
        }
      }
      
      # drawdown
      if((!is.null(modflow$oc$save_drawdown) && any(modflow$oc$save_drawdown)) || (!is.null(modflow$oc$ddsv) && any(modflow$oc$ddsv != 0))) {
        if(verbose) print_reading('Drawdown', file = fname[which(modflow$nam$nunit == modflow$oc$iddnun)], output = TRUE)
        modflow$drawdown <- rmf_read_drawdown(file = fname[which(modflow$nam$nunit == modflow$oc$iddnun)], dis = modflow$dis, bas = modflow$bas, oc = modflow$oc,
                                              binary = modflow$nam$ftype[which(modflow$nam$nunit == modflow$oc$iddnun)] %in% c('DATA(BINARY)', 'DATAGLO(BINARY)'), precision = precision)
      }
      
      # cbc
      if((!is.null(modflow$oc$save_budget) && any(modflow$oc$save_budget)) || (!is.null(modflow$oc$icbcfl) && any(modflow$oc$icbcfl != 0))) {
        cbc_packages <- rmfi_list_packages(type = 'cbc')
        cbcnum <-  vector(mode = "integer")
        for(i in 1:length(cbc_packages$ftype)) {
          if(cbc_packages$ftype[i] %in% modflow$nam$ftype) {
            obj <- modflow[[cbc_packages$rmf[i]]]
            # some packages have i*cb1, i*cb2. SWI has iswibd
            cbc_base <- paste0('i',cbc_packages$rmf[i],'cb')
            cbcnum <-  c(cbcnum, c(obj[[cbc_base]], obj[[paste0(cbc_base, '1')]], obj[[paste0(cbc_base, '2')]], obj[[paste0('i',cbc_packages$rmf[i],'bd')]]))
          }
        }
        cbcnum <-  unique(cbcnum[cbcnum > 0])
        if(length(cbcnum) == 1) {
          if(verbose) print_reading('Cell-by-cell flow', file = fname[which(modflow$nam$nunit == cbcnum)], output = TRUE)
          
          if(!is.null(modflow$huf) && modflow$huf$iohufflows != 0) {
            modflow$cbc <- rmf_read_cbc(file = fname[which(modflow$nam$nunit == cbcnum)], dis = modflow$dis, huf = modflow$huf, oc = modflow$oc, precision = precision)
          } else {
            modflow$cbc <- rmf_read_cbc(file = fname[which(modflow$nam$nunit == cbcnum)], dis = modflow$dis, oc = modflow$oc, precision = precision)
          }      
          
        } else if(length(cbcnum) > 1){
          if(!is.null(modflow$huf) && modflow$huf$iohufflows != 0) huf_cbc <- modflow$huf$iohufflows       
          for(i in 1:length(cbcnum)) {
            if(verbose) print_reading('Cell-by-cell flow', file = fname[which(modflow$nam$nunit == cbcnum[i])], output = TRUE)
            
            if(!is.null(modflow$huf) && modflow$huf$iohufflows != 0 && cbcnum[i] == huf_cbc) {
              modflow[[paste0('cbc_',i)]] <- rmf_read_cbc(file = fname[which(modflow$nam$nunit == cbcnum[i])], dis = modflow$dis, huf = modflow$huf, oc = modflow$oc, precision = precision)
            } else {
              modflow[[paste0('cbc_',i)]] <- rmf_read_cbc(file = fname[which(modflow$nam$nunit == cbcnum[i])], dis = modflow$dis, oc = modflow$oc, precision = precision)
            }     
          }
        }
      }
      
    }

    # budget
    if(verbose) print_reading('Volumetric budget', file = fname[which(modflow$nam$ftype == "LIST")], output = TRUE)
    modflow$bud <- rmf_read_bud(file = fname[which(modflow$nam$ftype == "LIST")])

   } 
    # remove output from ftype
    ftype <- ftype[-which(ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)'))]  
   
  
  # warning for not-supported packages
  if(length(ftype) > 0) warning(paste0('Following packages are not supported yet: \n ', paste(ftype, collapse = '\n ')), call. = FALSE)
  
  class(modflow) <- c('modflow')
  return(modflow)
}



#' Write a MODFLOW model
#'
#' \code{rmf_write} writes all input packages in a \code{RMODFLOW} modflow object to a directory
#'
#' @param modflow \code{RMODFLOW} modflow object
#' @param file filename of the name file to write
#' @param exclude character vector with packages names to exclude from the simulation. Defaults to NULL
#' @param suppress logical; remove non-supported (and thus not written) packages in the NAME file ? Defaults to FALSE
#' @param verbose logical; should information on writing files be printed to the console ? Defaults to TRUE.
#' @return \code{NULL}
#' @export
#' @details All arrays use free-format headers INTERNAL or CONSTANT
#'          All packages will be written according to the filenames (fname) defined in the nam object.
#'          To prevent any files being overwritten, it is best to write to an empty directory.
#' @seealso \code{\link{rmf_create}}, \code{\link{rmf_write}} and \url{http://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html}
rmf_write <- function(modflow, 
                      file = {cat('Please select nam file to overwrite or provide new filename ...\n'); file.choose()},
                      exclude = NULL,
                      suppress = FALSE,
                      verbose = TRUE) {
  
  print_writing <- function(package, file) {
    cat(paste0("---------------------------------", '\n'))
    cat(paste0(paste("  Writing", package, "package to file", file), '\n'))
  }
  
  dir_name <- dirname(file)
  packages <- rmfi_list_packages(type = 'all')
  
  # exclude packages
  if(!is.null(exclude)) {
    ftype <- packages$ftype[which(packages$rmf %in% exclude)]
    modflow$nam <- modflow$nam[-which(modflow$nam$ftype %in% ftype), ]
  }
  
  # all packages in a RMODFLOW modflow object are supported by RMODFLOW
  ftype <- modflow$nam$ftype
  ftype <- packages$rmf[packages$ftype %in% ftype]
  
  # not supported
  not_supported <- modflow$nam$ftype[-which(modflow$nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)'))]
  not_supported <- not_supported[-which(not_supported %in% packages$ftype)]

  if(length(not_supported) > 0) {
    warning(paste0('Packages in the NAME file not written:  \n ', paste(not_supported, collapse = '\n ')), call. = FALSE)
    if(suppress) {
      warning('Removing non-supported packages in the NAME file', call. = FALSE)
      modflow$nam <- modflow$nam[-which(modflow$nam$ftype %in% not_supported),]
    }
  } 
    
  fmt <- ifelse(modflow$bas$free, 'free', 'fixed')
  
  # first write nam, dis & bas, then everything else.
  # can't use loop because some write functions need different input arguments
  
  # nam
  if(verbose) print_writing('NAME', file = file)
  rmf_write_nam(nam = modflow$nam, file = file)
  
  # dis
  if(verbose) print_writing('DIS', file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'DIS')], sep = '/'))
  rmf_write_dis(dis = modflow$dis, file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'DIS')], sep = '/'))
  
  # bas
  if(verbose) print_writing('BAS6', file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'BAS6')], sep = '/'))
  rmf_write_bas(bas = modflow$bas, dis = modflow$dis, file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'BAS6')], sep = '/'))
  
  # mlt & zon
  if('mlt' %in% ftype) {
    if(verbose) print_writing('MULT', file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'MULT')], sep = '/'))
    rmf_write_mlt(mlt = modflow$mlt, file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'MULT')], sep = '/'))
  }
  if('zon' %in% ftype) {
    if(verbose) print_writing('ZONE', file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'ZONE')], sep = '/'))
    rmf_write_zon(zon = modflow$zon, file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'ZONE')], sep = '/'))
  } 
  
  # pvl
  if('pvl' %in% ftype) {
    if(verbose) print_writing('PVAL', file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'PVAL')], sep = '/'))
    rmf_write_pvl(pvl = modflow$pvl, file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'PVAL')], sep = '/'))
  }
    
  # lgr
  # if('lgr' %in% ftype) {
  #   if(verbose) print_writing('LGR', file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'LGR')], sep = '/'))
  #   rmf_write_lgr(lgr = modflow$lgr, file = paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == 'LGR')], sep = '/'))
  # }
    
  # flow packages
  df <- rmfi_list_packages(type = 'flow')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }

  # boundary conditions
  df <- rmfi_list_packages(type = 'boundary')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }
  
  # solver
  df <- rmfi_list_packages(type = 'solver')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], file = file, format = fmt))
      }
    }
  }
  
  # oc
  df <- rmfi_list_packages(type = 'oc')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }

  # sub
  df <- rmfi_list_packages(type = 'sub')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }

  # obs
  df <- rmfi_list_packages(type = 'obs')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], file = file, format = fmt))
      }
    }
  }

  # swr
  df <- rmfi_list_packages(type = 'swr')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }

  # cfp
  df <- rmfi_list_packages(type = 'cfp')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }

  # farm
  df <- rmfi_list_packages(type = 'farm')
  if(nrow(df) > 0) {
    for(i in 1:length(df$rmf)) {
      if(df$rmf[i] %in% ftype) {
        fnctn <- paste0('rmf_write_', df$rmf[i])
        file <- paste(dir_name, modflow$nam$fname[which(modflow$nam$ftype == df$ftype[i])], sep = '/')
        if(verbose) print_writing(df$ftype[i], file = file)
        do.call(fnctn, list(modflow[[df$rmf[i]]], dis = modflow$dis, file = file, format = fmt))
      }
    }
  }

}
