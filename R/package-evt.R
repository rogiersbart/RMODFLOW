#' Create an \code{RMODFLOW} evt object.
#' 
#' \code{rmf_create_evt} creates an \code{RMODFLOW} evt object
#' 
#' @param ... \code{rmf_2d_arrays} (possibly of class \code{rmf_parameter}) or a single \code{list} with \code{rmf_2d_arrays} (possibly of class \code{rmf_parameter}) elements; defines the maximum evapotranspiration fluxes. See details.
#' @param dis \code{RMODFLOW} dis object
#' @param nevtop evapotranspiration (ET) option code; defaults to 3 (ET is applied to the highest active cell in each vertical column)
#' @param ievtcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 
#' @param surf a single \code{rmf_2d_array} or a list of \code{rmf_2d_arrays} specifying the elevation of the ET surface. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. At least 1 surf array must be supplied.
#' @param exdp a single \code{rmf_2d_array} or a list of \code{rmf_2d_arrays} specifying the ET extinction depth as a distance from surf. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. At least 1 exdp array must be supplied.
#' @param ievt a single \code{rmf_2d_array} or a list of \code{rmf_2d_arrays} specifying the layer numbers defining in which layer ET is applied. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. Only used when \code{nevtop = 2}. Defaults to NULL
#' @param ievtpf numeric of length 1 or length \code{dis$nper}; optional format code for printing the \code{ET} variable it has been defined by parameters; defaults to -1 (no printing) for all stress periods
#' @details the \code{rmf_2d_arrays} should have \code{kper} attributes specifying the stress period in which they are active. This is also true for the surf, exdp and ievt arrays. There can be only one non-parameter array active per stress periods. Multiple parameters are however allowed per stress period.
#' @return \code{RMODFLOW} evt object
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_write_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}

rmf_create_evt <- function(...,
                           dis,
                           nevtop = 3,
                           ievtcb = 0,
                           surf,
                           exdp,
                           ievt = NULL,
                           ievtpf = -1
) {
  
  arg <- rmfi_create_bc_array(arg = list(...), dis = dis)
  
  # create evt object
  obj <- arg[c("np", "instances")]
  obj$nevtop <- nevtop
  obj$ievtcb <- ievtcb
  obj$evt <- arg$data
  if(arg$np > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  # surf
  if(!inherits(surf, 'list')) surf <- list(surf)
  obj$surf <- surf
  names(obj$surf) <- paste('surf', 1:length(surf), sep = '_')
  obj$kper$surf <- NA_character_
  for(i in 1:length(surf)) {
    obj$kper$surf[c(1:dis$nper) %in% attr(surf[[i]],'kper')] <- names(obj$surf)[i]
  }
  
  # check if multiple surf arrays are active
  surf_err <- unlist(lapply(surf, function(i) attr(i, 'kper')))
  if(any(duplicated(surf_err))) stop(paste('There can be only 1 active surf array per stress period. Stress period(s)', sort(unique(surf_err[duplicated(surf_err)])), 'have multiple active arrays.'), call. = FALSE)
  
  
  # exdp
  if(!inherits(exdp, 'list')) exdp <- list(exdp)
  obj$exdp <- exdp
  names(obj$exdp) <- paste('exdp', 1:length(exdp), sep = '_')
  obj$kper$exdp <- NA_character_
  for(i in 1:length(exdp)) {
    obj$kper$exdp[c(1:dis$nper) %in% attr(exdp[[i]],'kper')] <- names(obj$exdp)[i]
  }
  
  # check if multiple exdp arrays are active
  exdp_err <- unlist(lapply(exdp, function(i) attr(i, 'kper')))
  if(any(duplicated(exdp_err))) stop(paste('There can be only 1 active exdp array per stress period. Stress period(s)', sort(unique(exdp_err[duplicated(exdp_err)])), 'have multiple active arrays.'), call. = FALSE)
  
  
  # ievt
  if(nevtop == 2) {
    if(is.null(ievt)) stop('Please supply a ievt argument when nevtop = 2', call. = FALSE)
    if(!inherits(ievt, 'list')) ievt <- list(ievt)
    ievt <- lapply(ievt, function(i) {r <- apply(i, MARGIN = 1:length(dim(i)), function(x) as.integer(x)); attributes(r) <- attributes(i); r})
    obj$ievt <- ievt
    names(obj$ievt) <- paste('ievt', 1:length(ievt), sep = '_')
    obj$kper$ievt <- NA_character_
    for(i in 1:length(ievt)) {
      obj$kper$ievt[c(1:dis$nper) %in% attr(ievt[[i]],'kper')] <- names(obj$ievt)[i]
    }
    
    # check if multiple ievt arrays are active
    ievt_err <- unlist(lapply(ievt, function(i) attr(i, 'kper')))
    if(any(duplicated(ievt_err))) stop(paste('There can be only 1 active ievt array per stress period. Stress period(s)', sort(unique(ievt_err[duplicated(ievt_err)])), 'have multiple active arrays.'), call. = FALSE)
    
  } 
  obj$ievtpf <- ievtpf
  
  class(obj) <- c('evt', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW evapotranspiration file
#' 
#' \code{rmf_read_evt} reads in a MODFLOW evapotranspiration file and returns it as an \code{RMODFLOW} evt object.
#'
#' @param file filename; typically '*.evt'
#' @param dis an \code{RMODFLOW} dis object
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} evt object
#' @export
#' @seealso \code{\link{rmf_write_evt}}, \code{\link{rmf_create_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}

rmf_read_evt <-  function(file = {cat('Please select evt file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          mlt = NULL,
                          zon = NULL,
                          ... ){
  
  lines <- readr::read_lines(file, lazy = FALSE)
  rmf_arrays <- list()
  
  # data set 0
  data_set_0 <-  rmfi_parse_comments(lines)
  comments <-  data_set_0$comments
  lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <- rmfi_parse_variables(lines, character = TRUE)
  
  if('PARAMETER' %in% toupper(data_set_1$variables)) {
    np <-  as.numeric(data_set_1$variables[2])
    lines <-  data_set_1$remaining_lines
  }  else {
    np <- 0
  }
  rm(data_set_1)
  
  # data set 2
  data_set_2 <-  rmfi_parse_variables(lines, n=2, ...)
  nevtop <- as.numeric(data_set_2$variables[1])
  ievtcb <- as.numeric(data_set_2$variables[2])
  lines <-  data_set_2$remaining_lines
  rm(data_set_2)
  ievt <- rmfi_ifelse0(nevtop == 2, list(), NULL)
  surf <- list()
  exdp <- list()
  
  # parameters: data set 3 & 4
  if(np > 0) {
    data_set_3 <- rmfi_parse_array_parameters(lines, dis = dis, np = np, mlt = mlt, zon = zon)
    rmf_arrays <- data_set_3$parameters
    lines <- data_set_3$remaining_lines
    rm(data_set_3)
  }
  
  # stress periods
  # function for setting kper attribute for parameters
  set_kper <- function(k, kper, p_name, i_name) {
    if(!is.null(attr(k, 'parnam')) && toupper(attr(k, 'parnam')) == toupper(p_name)) {
      if(!is.null(i_name)) {
        if(toupper(attr(k, "instnam")) == toupper(i_name)) attr(k, 'kper') <- c(attr(k, 'kper'), kper)
      } else {
        attr(k, 'kper') <- c(attr(k, 'kper'), kper)
      }
    }
    return(k)
  }
  
  # function for setting kper attribute of parameter the same as previous kper
  previous_kper <- function(k, kper) {
    set_previous_kper <- function(kk, kper) {
      if(!is.null(attr(kk, 'kper')) && kper-1 %in% attr(kk, 'kper')) {
        attr(kk, 'kper') <- c(attr(kk, 'kper'), kper)
      }
      return(kk)
    }
    
    if(is.list(k) && !is.null(attr(k[[1]], 'instnam'))) {
      k <- lapply(k, set_previous_kper, kper = kper)
    } else {
      k <- set_previous_kper(k, kper)
    }
    
    return(k)
  }
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 <-  rmfi_parse_variables(lines, n=4, ...)
    insurf <- as.numeric(data_set_5$variables[1])
    inevtr <- as.numeric(data_set_5$variables[2])
    inexdp <- as.numeric(data_set_5$variables[3])
    if(nevtop == 2) inievt <- as.numeric(data_set_5$variables[4])
    lines <- data_set_5$remaining_lines
    rm(data_set_5)
    
    # data set 6
    if(insurf >= 0) {
      data_set_6 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, ndim = 2, file = file, ...)
      surf[[length(surf) + 1]] <- structure(data_set_6$array, kper = i)
      lines <- data_set_6$remaining_lines
      rm(data_set_6)
    } else if(insurf < 0 && i > 1) {
      attr(surf[[length(surf)]], 'kper') <- c(attr(surf[[length(surf)]], 'kper'), i)
    }

    # data set 7-8
    ievtpf <- NULL
    if(np == 0) {
      
      if(inevtr >= 0) {
        data_set_7 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, ndim = 2, file = file, ...)
        rmf_arrays[[length(rmf_arrays) + 1]] <- structure(data_set_7$array, kper = i)
        lines <- data_set_7$remaining_lines
        rm(data_set_7)
      } else if(inevtr < 0 && i > 1) {
        attr(rmf_arrays[[length(rmf_arrays)]], 'kper') <- c(attr(rmf_arrays[[length(rmf_arrays)]], 'kper'), i)
      }
      
    } else {
      if(inevtr > 0) {
        for(j in 1:inevtr){
          # data set 8
          data_set_8 <-  rmfi_parse_variables(lines, character = TRUE)
          p_name <-  as.character(data_set_8$variables[1])
          if(is.list(rmf_arrays[[p_name]]) && !is.null(attr(rmf_arrays[[p_name]][[1]], 'instnam'))) {
            i_name <- data_set_8$variables[2]
            if(length(data_set_8$variables) > 2 && !is.na(suppressWarnings(as.numeric(data_set_8$variables[3])))) {
              ievtpf[i] <- as.numeric(data_set_8$variables[3])
            }
            attr(rmf_arrays[[p_name]][[i_name]], 'kper') <- c(attr(rmf_arrays[[p_name]][[i_name]], 'kper'), i)
          } else {
            i_name <- NULL
            if(length(data_set_8$variables) > 1 && !is.na(suppressWarnings(as.numeric(data_set_8$variables[2])))) {
              ievtpf[i] <- as.numeric(data_set_8$variables[2])
            }
            attr(rmf_arrays[[p_name]], 'kper') <- c(attr(rmf_arrays[[p_name]], 'kper'), i)
          }
          
          # rmf_arrays <- lapply(rmf_arrays, set_kper, p_name = p_name, i_name = i_name, kper = i)
          
          lines <- data_set_8$remaining_lines
          rm(data_set_8)
          
        }
      } else if(inevtr < 0 && i > 1) {
        rmf_arrays <- lapply(rmf_arrays, previous_kper, kper = i)
      }
    
    }
    
    # data set 9
    if(inexdp >= 0) {
      data_set_9 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, ndim = 2, file = file, ...)
      exdp[[length(exdp) + 1]] <- structure(data_set_9$array, kper = i)
      lines <- data_set_9$remaining_lines
      rm(data_set_9)
    } else if(inexdp < 0 && i > 1) {
      attr(exdp[[length(exdp)]], 'kper') <- c(attr(exdp[[length(exdp)]], 'kper'), i)
    }
    
    # data set 10
    if(nevtop == 2) {
      if(inievt >= 0) {
        data_set_10 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, ndim = 2, file = file, integer = TRUE, ...)
        ievt[[length(ievt) + 1]] <- rmf_create_array(structure(apply(data_set_10$array, 1:length(dim(data_set_10$array)), function(i) as.integer(i)), kper = i))
        lines <- data_set_10$remaining_lines
        rm(data_set_10)
      } else if(inievt < 0 && i > 1) {
        attr(ievt[[length(ievt)]], 'kper') <- c(attr(ievt[[length(ievt)]], 'kper'), i)
      }
    }
  }
  
  list_arrays <- function(i) {
    if(is.list(i) && !is.null(attr(i[[1]], 'instnam'))) {
      return(i)
    } else {
      return(list(i))
    }
  }
  rmf_arrays <- lapply(rmf_arrays, list_arrays)
  rmf_arrays <- do.call(c, rmf_arrays)
  rmf_arrays <- lapply(rmf_arrays, function(i) rmfi_ifelse0(is.null(attr(i, 'kper')), structure(i, kper = 0), i))
  
  evt <- rmf_create_evt(rmf_arrays, dis = dis, nevtop = nevtop, ievtcb = ievtcb, surf = surf, exdp = exdp, ievt = ievt, ievtpf = rmfi_ifelse0(is.null(ievtpf), -1, ievtpf))
  comment(evt) <- comments
  return(evt)
}

#' Write a MODFLOW evapotranspiration file
#'
#' \code{rmf_write_evt} writes a MODFLOW evapotranspiration file based on an \code{RMODFLOW} evt object
#' 
#' @param evt an \code{RMODFLOW} evt object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.evt'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_variables} and \code{rmfi_write_array}
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_evt}}, \code{\link{rmf_create_evt}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?evt.htm}

rmf_write_evt <-  function(evt, 
                           dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                           file={cat('Please choose evt file to overwrite or provide new filename ...\n'); file.choose()}, 
                           iprn = -1,
                           ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW Evapotranspiration Package created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(evt)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(evt$np > 0) rmfi_write_variables('PARAMETER', as.integer(evt$np), file=file)
  
  # data set 2
  rmfi_write_variables(evt$nevtop, evt$ievtcb, file=file, integer = TRUE, ...)
  
  # parameters
  partyp <- 'EVT'
  if(evt$np > 0) {
    parm_names <- names(evt$parameter_values)
    tv_parm <- rep(FALSE, evt$np)
    if(!is.null(evt$instances)) tv_parm <- evt$instances > 0
    rmfi_write_array_parameters(obj = evt, arrays = evt$evt, file = file, partyp = 'evt', ...)
  }
  
  # stress periods
  for (i in 1:dis$nper){
    
    check_prev <- function(kper, i) {
      drop_names <- which(names(kper) %in% c('kper', 'surf', 'exdp', 'ievt'))
      df <- kper[c(i-1,i), -drop_names, drop = FALSE]
      identical(c(df[2,]), c(df[1,]))
    }
    
    # data set 5
    # insurf
    insurf_act <- evt$kper$surf[i]
    if(!is.na(insurf_act)) {
      if(i > 1 && identical(insurf_act, evt$kper$surf[i-1])) {
        insurf <- -1
      } else {
        insurf <- length(insurf_act)
      }
    } 
    
    # inevtr
    # drop_id <- which(colnames(evt$kper) %in% c('kper', 'surf', 'exdp', 'ievt'))
    # names_act <- colnames(evt$kper)[which(evt$kper[i,which(!is.na(evt$kper[i,]))] != FALSE)[-drop_id]]
    names_act <- colnames(evt$kper)[which(evt$kper[i,which(!is.na(evt$kper[i,]))] != FALSE)]
    names_act <- names_act[which(!(names_act %in% c('kper', 'surf', 'exdp', 'ievt')))]
    if(i > 1 && check_prev(evt$kper, i)) {
      inevtr <- -1
    } else {
      inevtr <- length(names_act)
    }
    
    # inexdp
    inexdp_act <- evt$kper$exdp[i]
    if(!is.na(inexdp_act)) {
      if(i > 1 && identical(inexdp_act, evt$kper$exdp[i-1])) {
        inexdp <- -1
      } else {
        inexdp <- length(inexdp_act)
      }
    } 
    
    # inievt
    inievt <- 0
    if(evt$nevtop == 2) {
      inievt_act <- evt$kper$ievt[i]
      if(!is.na(inievt_act)) {
        if(i > 1 && identical(inievt_act, evt$kper$ievt[i-1])) {
          inievt <- -1
        } else {
          inievt <- length(inievt_act)
        }
      } 
    }

    
    if(evt$np > 0) {
      parm_names_active <- parm_names[parm_names %in% names_act]
      np <- length(parm_names_active)
    } else {
      np <- 0
    }
    
    rmfi_write_variables(insurf, inevtr, inexdp, ifelse(evt$nevtop == 2, inievt, ''), file=file, integer = TRUE, ...)
    
    # data set 6
    if(insurf >= 0) rmfi_write_array(evt$surf[[insurf_act]], file = file, iprn = iprn, ...)
    
    # data set 7
    if(np == 0 && inevtr >= 0) rmfi_write_array(evt$evt[[names_act]], file = file, iprn = iprn, ...)
    
    # data set 8
    if(np > 0){
      for(j in 1:np){
        rmfi_write_variables(parm_names_active[j], ifelse(tv_parm[j], evt$kper[i,parm_names_active[j]], ''), as.integer(ifelse(length(evt$ievtpf) == 1, evt$ievtpf, evt$ievtpf[j])), file=file)
      }
    }
    
    # data set 9
    if(inexdp >= 0) rmfi_write_array(evt$exdp[[inexdp_act]], file = file, iprn = iprn, ...)
    
    # data set 10
    if(evt$nevtop == 2 && inievt >= 0) {
      rmfi_write_array(evt$ievt[[inievt_act]], file = file, iprn = iprn, ...)
    }
  }
  
}
