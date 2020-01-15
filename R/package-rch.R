#' Create an \code{RMODFLOW} rch object.
#' 
#' \code{rmf_create_rch} creates an \code{RMODFLOW} rch object
#' 
#' @param ... \code{rmf_2d_arrays} (possibly of class \code{rmf_parameter}) or a single \code{list} with \code{rmf_2d_arrays} (possibly of class \code{rmf_parameter}) elements; defines the recharge values. See details.
#' @param dis \code{RMODFLOW} dis object
#' @param nrchop recharge option code; defaults to 3 (recharge is applied to the highest active cell in each vertical column)
#' @param irchcb flag and unit number for writing cell-by-cell flow terms; defaults to 0 
#' @param irch a single \code{rmf_2d_array} or a list of \code{rmf_2d_arrays} specifying the layer numbers defining in which layer recharge is applied. The \code{'kper'} attribute of the arrays define the stress period in which the array is active, see details. Only used when \code{nrchop = 2}. Defaults to NULL
#' @param irchpf numeric of length 1 or length \code{dis$nper}; optional format code for printing the \code{RECH} variable it has been defined by parameters; defaults to -1 (no printing) for all stress periods
#' @details the \code{rmf_2d_arrays} should have \code{kper} attributes specifying the stress period in which they are active. This is also true for the irch arrays. There can be only one non-parameter array active per stress periods. Multiple parameters are however allowed per stress period.
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_write_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_create_rch <- function(...,
                           dis,
                           nrchop = 3,
                           irchcb = 0,
                           irch = NULL,
                           irchpf = -1
) {
  
  arg <- rmfi_create_bc_array(arg = list(...), dis = dis)
  
  # create rch object
  obj <- list()
  
  obj$dimensions <- arg$dimensions
  obj$nrchop <- nrchop
  obj$irchcb <- irchcb
  obj$recharge <- arg$data
  if(arg$dimensions['np'] > 0) obj$parameter_values <- arg$parameter_values
  obj$kper <- arg$kper
  
  # irch
  if(nrchop == 2) {
    if(is.null(irch)) stop('Please supply a irch argument when nrchop = 2', call. = FALSE)
    if(!inherits(irch, 'list')) irch <- list(irch)
    obj$irch <- irch
    names(obj$irch) <- paste('irch', length(irch), sep = '_')
    obj$kper$irch <- NA_character_
    for(i in 1:length(irch)) {
      obj$kper$irch[c(1:dis$nper) %in% attr(irch[[i]],'kper')] <- names(obj$irch)[i]
    }

    # check if multiple irch arrays are active
    irch_err <- unlist(lapply(irch, function(i) attr(i, 'kper')))
    if(any(duplicated(irch_err))) stop(paste('There can be only 1 active irch array per stress period. Stress period(s)', sort(unique(irch_err[duplicated(irch_err)])), 'have multiple active arrays.'), call. = FALSE)
  } 
  obj$irchpf <- irchpf
  
  class(obj) <- c('rch', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW recharge file
#' 
#' \code{rmf_read_rch} reads in a MODFLOW recharge file and returns it as an \code{RMODFLOW} rch object.
#'
#' @param file filename; typically '*.rch'
#' @param dis an \code{RMODFLOW} dis object
#' @param mlt a \code{RMODFLOW} mlt object. Only needed when reading parameter arrays defined by multiplier arrays
#' @param zon a \code{RMODFLOW} zon object. Only needed when reading parameter arrays defined by zone arrays
#' @param ... arguments passed to \code{rmfi_parse_array}. Can be ignored when input arrays are free-format and INTERNAL or CONSTANT.
#' @return \code{RMODFLOW} rch object
#' @export
#' @seealso \code{\link{rmf_write_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_read_rch <-  function(file = {cat('Please select rch file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                          mlt = NULL,
                          zon = NULL,
                          ... ){
  
  lines <- readr::read_lines(file)
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
  nrchop <- as.numeric(data_set_2$variables[1])
  irchcb <- as.numeric(data_set_2$variables[2])
  lines <-  data_set_2$remaining_lines
  rm(data_set_2)
  irch <- rmfi_ifelse0(nrchop == 2, list(), NULL)
  
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
    if(kper-1 %in% attr(k, 'kper')) {
      attr(k, 'kper') <- c(attr(k, 'kper'), kper)
    }
    return(k)
  }
  
  for(i in 1:dis$nper){
    # data set 5
    data_set_5 <-  rmfi_parse_variables(lines, n=2, ...)
    inrech <- as.numeric(data_set_5$variables[1])
    if(nrchop == 2) inirch <- as.numeric(data_set_5$variables[2])
    lines <- data_set_5$remaining_lines
    rm(data_set_5)
    
    # data set 6-7
    irchpf <- NULL
    
    if(np == 0) {
      
      if(inrech >= 0) {
        data_set_6 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, file = file, ...)
        rmf_arrays[[length(rmf_arrays) + 1]] <- structure(data_set_6$array, kper = i)
        lines <- data_set_6$remaining_lines
        rm(data_set_6)
      } else if(inrech < 0 && i > 1) {
        attr(rmf_arrays[[length(rmf_arrays)]], 'kper') <- c(attr(rmf_arrays[[length(rmf_arrays)]], 'kper'), i)
      }
      
    } else {
      # parameters
      if(inrech > 0) {
        for(j in 1:inrech){
          # data set 7
          data_set_7 <-  rmfi_parse_variables(lines, character = TRUE)
          p_name <-  toupper(as.character(data_set_7$variables[1]))
          if(!is.null(attr(rmf_arrays[[p_name]], 'instnam'))) {
            i_name <- data_set_7$variables[2]
            if(length(data_set_7$variables) > 2 && !is.na(suppressWarnings(as.numeric(data_set_7$variables[3])))) {
              irchpf[i] <- as.numeric(data_set_7$variables[3])
            }
          } else {
            i_name <- NULL
            if(length(data_set_7$variables) > 1 && !is.na(suppressWarnings(as.numeric(data_set_7$variables[2])))) {
              irchpf[i] <- as.numeric(data_set_7$variables[2])
            }
          }
          
          rmf_arrays <- lapply(rmf_arrays, set_kper, p_name = p_name, i_name = i_name, kper = i)
          
          lines <- data_set_7$remaining_lines
          rm(data_set_7)
          
        }
        
      } else if(inrech < 0 && i > 1) {
        rmf_arrays <- lapply(rmf_arrays, previous_kper, kper = i)
      }
      
    }
    
    # data set 8
    if(nrchop == 2) {
      if(inirch >= 0) {
        data_set_8 <- rmfi_parse_array(lines, dis$nrow, dis$ncol, 1, file = file, ...)
        irch[[length(irch) + 1]] <- structure(data_set_8$array, kper = i)
        lines <- data_set_8$remaining_lines
        rm(data_set_8)
      } else if(inirch < 0 && i > 1) {
        attr(irch[[length(irch)]], 'kper') <- c(attr(irch[[length(irch)]], 'kper'), i)
      }
    }
  }
  
  rch <- rmf_create_rch(rmf_arrays, dis = dis, nrchop = nrchop, irchcb = irchcb, irch = irch, irchpf = rmfi_ifelse0(is.null(irchpf), -1, irchpf))
  comment(rch) <- comments
  return(rch)
}

#' Write a MODFLOW recharge file
#'
#' \code{rmf_write_rch} writes a MODFLOW recharge file based on an \code{RMODFLOW} rch object
#' 
#' @param rch an \code{RMODFLOW} rch object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.rch'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmfi_write_variables} and \code{rmfi_write_array}
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_rch}}, \code{\link{rmf_create_rch}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?rch.htm}

rmf_write_rch <-  function(rch,
                           dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())},
                           file={cat('Please choose rch file to overwrite or provide new filename ...\n'); file.choose()},
                           iprn = -1,
                           ...){
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW Recharge Package created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(rch)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(rch$dimensions$np > 0) rmfi_write_variables('PARAMETER', rch$dimensions$np, file=file)
  
  # data set 2
  rmfi_write_variables(rch$nrchop, rch$irchcb, file=file, ...)
  
  # parameters
  partyp <- 'RCH'
  if(rch$dimensions$np > 0) {
    parm_names <- names(rch$parameter_values)
    tv_parm <- rep(FALSE, rch$dimensions$np)
    if(!is.null(rch$dimensions$instances)) tv_parm <- rch$dimensions$instances > 0
    rmfi_write_array_parameters(obj = rch, arrays = rch$recharge, file = file, partyp = 'RCH', ...)
  }
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    # inrech
    names_act <- colnames(rch$kper)[which(rch$kper[i,which(!is.na(rch$kper[i,]))] != FALSE)[-1]]
    if(i > 1 && identical(names_act, colnames(rch$kper)[which(rch$kper[i-1,which(!is.na(rch$kper[i-1,]))] != FALSE)[-1]])) {
      inrech <- -1
    } else {
      inrech <- length(names_act)
    }
    # inirch
    inirch <- 0
    if(rch$nrchop == 2) {
      irch_act <-  rch$kper$irch[i]
      if(!is.na(irch_act)) {
        if(i > 1 && identical(irch_act, rch$kper$irch[i-1])) {
          inirch <- -1
        } else {
          inirch <- length(irch_act)
        }
      } 
    }
    
    if(rch$dimensions$np > 0) {
      parm_names_active <- parm_names[parm_names %in% names_act]
      np <- length(parm_names_active)
    } else {
      np <- 0
    }
    
    rmfi_write_variables(inrech, ifelse(rch$nrchop == 2, inirch, ''), file=file, ...)
    
    # data set 6
    if(np == 0 && inrech >= 0) rmfi_write_array(rch$recharge[[names_act]], file = file, iprn = iprn, ...)
    
    # data set 7
    if(np > 0){
      for(j in 1:np){
        rmfi_write_variables(parm_names_active[j], ifelse(tv_parm[j], rch$kper[i,parm_names_active[j]], ''), ifelse(length(rch$irchpf) == 1, rch$irchpf, rch$irchpf[j]), file=file)
      }
    }
    
    # data set 8
    if(rch$nrchop == 2 && inirch >= 0) {
      rmfi_write_array(rch$irch[[irch_act]], file = file, iprn = iprn, ...)
    }
  }
  
}

