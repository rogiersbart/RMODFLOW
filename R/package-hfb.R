#' Create an \code{RMODFLOW} hfb object.
#' 
#' \code{rmf_create_hfb} creates an \code{RMODFLOW} hfb object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parameter}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parameter}) elements; defines the horizontal-flow barriers. 
#' @param dis dis object
#' @param noprint logical, should the printing of HFB cells to the listing file be suppressed ? Defaults to \code{FALSE}
#' 
#' @details As an alternative to specifying \code{irow2} and \code{icol2}, a \code{direction} column can be present in the rmf_lists objects to specify the direction of the horizontal flow barrier with respect to \code{i & j}.
#'          Allowed values for the \code{direction} column are \code{"right"}, \code{"back"}, \code{"left"} and \code{"front"}.
#' 
#' @return \code{RMODFLOW} hfb object
#' @export
#' @seealso \code{\link{rmf_read_hfb}}, \code{\link{rmf_write_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}

rmf_create_hfb <-  function(..., 
                            dis,
                            noprint = FALSE
) {
  vars <- c('irow2', 'icol2', 'hydchr')
  var_cols <- 4:(3+length(vars))
  
  # set kper and direction
  # find dis
  arg <- list(...)

  if(missing(dis)) {
    dis_present <- vapply(arg, function(i) 'dis' %in% class(i), TRUE)
    if(any(dis_present)) {
      dis <- arg[dis_present][[1]]
      arg <- arg[!dis_present]
    } else {
      stop('Please provide a dis argument', call. = FALSE)
    }
  }
  
  if(length(arg) == 1) {
    if(inherits(arg[[1]], 'list')) {
      arg <- arg[[1]]
    } else if(inherits(arg[[1]], 'data.frame') && !(inherits(arg[[1]], 'rmf_list'))) { # make rmf_list
      arg <- lapply(arg, function(i) rmf_create_list(i, kper = 1:dis$nper))
    }
  }
  
  # check if all varnames are present (partial string matching)
  nms_check <- lapply(arg, function(i) pmatch(colnames(i)[var_cols], vars))
  if(any(vapply(nms_check, function(i) any(is.na(i)), TRUE))) stop('Please make sure all rmf_list objects have columns k, i, j, ', paste(vars, collapse = ', '), call. = FALSE)
  arg <- lapply(seq_along(arg), function(i) setNames(arg[[i]], replace(colnames(arg[[i]]), var_cols, vars[nms_check[[i]]])))
  
  set_hfb <- function(rmf_list) {
    if(is.null(attr(rmf_list, 'kper'))) {
      warning('Missing kper argument for hfb input list. Assuming this list is active', call. = FALSE)
    } else if(!identical(as.numeric(attr(rmf_list, 'kper')), as.numeric(1:dis$nper))) {
      stop('Please make sure all hfb input lists have either a kper argument which is active for all stress periods or no kper argument at all.', call. = FALSE)
    }
    
    if(inherits(rmf_list, 'rmf_parameter') && !is.null(attr(rmf_list, 'instnam'))) {
      stop('Time-varying parameters are not supported for the hfb package.', call. = FALSE)
    }
    
    if('direction' %in% colnames(rmf_list)) {
      rl <- fb <-  rep(0, nrow(rmf_list))
      rl <- rl + as.numeric(rmf_list$direction == "right")
      rl <- rl - as.numeric(rmf_list$direction == "left")
      fb <- fb + as.numeric(rmf_list$direction == "back")
      fb <- fb - as.numeric(rmf_list$direction == "front")
      rmf_list$irow2 <- rmf_list$i + fb
      rmf_list$icol2 <- rmf_list$j + rl
    }
    return(rmf_list)
  }
  arg <- lapply(arg, set_hfb)
  
  # check for parameters and/or lists and name them
  parameters <- arg[vapply(arg, function(i) inherits(i, 'rmf_parameter'), TRUE)]
  if(length(parameters) > 0) names(parameters) <- vapply(parameters, function(i) attr(i, 'parnam'), 'text')
  lists <- arg[vapply(arg, function(i) !inherits(i, 'rmf_parameter'), TRUE)]
  if(length(lists) > 0) names(lists) <- paste('list', 1:length(lists), sep = '_')
  
  np <- 0
  mxl <- 0
  parameter_values <- NULL

  if(length(parameters) > 0) {
    np <- length(unique(names(parameters)))
    mxl <- sum(vapply(parameters[!duplicated(names(parameters))], nrow, 1))
  } 
  
  nacthfb <- 0
  acthfb <- NULL
  # parameters
  if(length(parameters) > 0) {
    parameter_values <- vapply(parameters, function(i) attr(i, 'parval'), 1.0)
    acthfb <- lapply(parameters, function(i) rmfi_ifelse0(!is.null(attr(i, 'kper')), attr(i, 'parnam'), NULL))
    nacthfb <- length(unlist(acthfb))
    
    # set parameter df
    parameters <- lapply(parameters, function(i) {i$parameter <-  TRUE;
                                                  i$name <-  attr(i, 'parnam');
                                                  i$active <- !is.null(attr(i,"kper"));
                                                  i <- i[c('k', 'i', 'j',vars,'parameter','name','active')];
                                                  colnames(i)[var_cols] <-  vars;
                                                  i})
    parameters <- do.call(rbind, unname(parameters))

  }
  
  # lists
  if(length(lists) > 0) {
    # set lists df
    lists <- lapply(lists, function(i) {i <- i[c('k', 'i', 'j',vars)];
                                        colnames(i)[var_cols] <-  vars;
                                        i$parameter <-  FALSE;
                                        i$active <- is.null(attr(i,"kper"));
                                        i})
    lists <- lapply(seq_along(lists), function(i) {lists[[i]]$name <- names(lists)[[i]]; lists[[i]]})
    lists <- do.call(rbind, unname(lists))
  }
  
  # combine
  data <- structure(rbind(parameters, lists), kper = NULL)

  # create hfb object
  obj <- list(np = np, mxl = mxl, nnp = nrow(data) - mxl, nacthfb = nacthfb, acthfb = acthfb)
  obj$ihfbcb <- NULL
  obj$option <- c('noprint' = noprint)
  obj$aux <- NULL
  obj$data <- data
  if(np > 0) obj$parameter_values <- parameter_values

  class(obj) <- c('hfb', 'rmf_package')
  return(obj)
  
}

#' Read a MODFLOW horizontal flow barrier file
#' 
#' \code{rmf_read_hfb} reads in a MODFLOW horizontal flow barrier file and returns it as an \code{RMODFLOW} hfb object.
#'
#' @param file filename; typically '*.hfb'
#' @param dis an \code{RMODFLOW} dis object
#' @param ... arguments passed to \code{rmfi_parse_list}.
#'  
#' @return \code{RMODFLOW} hfb object
#' @export
#' @seealso \code{\link{rmf_write_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}

rmf_read_hfb <-  function(file = {cat('Please select horizontal flow barrier file ...\n'); file.choose()},
                          dis = {cat('Please select corresponding dis file ...\n'); rmf_read_dis(file.choose())}, ...){
  
  vars <- c('irow2', 'icol2', 'hydchr')
  option <- c('NOPRINT' = FALSE)
  lines <-  readr::read_lines(file, lazy = FALSE)
  scalevar <- 6
  
  arg <- list(...)
  arg$format <- 'free'
  
  rmf_lists <- list()
  
  # data set 0
  data_set_0 <-  rmfi_parse_comments(lines)
  comments <-  data_set_0$comments
  lines <-  data_set_0$remaining_lines
  rm(data_set_0)
  
  # data set 1
  data_set_1 <-  rmfi_parse_variables(lines)
  np <- as.numeric(data_set_1$variables[1])
  nnp <- as.numeric(data_set_1$variables[3])
  if('NOPRINT' %in% toupper(as.character(data_set_1$variables))) option['NOPRINT'] <- TRUE
  lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2 & 3
  if(np > 0) {
    
    for(i in 1:np) {
      data_set_2 <- rmfi_parse_variables(lines)
      parnam <- as.character(data_set_2$variables[1])
      parval <- as.numeric(data_set_2$variables[3])
      nlst <- as.numeric(data_set_2$variables[4])
      lines <- data_set_2$remaining_lines
      rm(data_set_2)
      
      data_set_3 <- do.call(rmfi_parse_list, c(list(remaining_lines = lines, nlst = nlst, varnames = vars, scalevar = scalevar, naux = 0, file = file), arg))
      rmf_lists[[length(rmf_lists)+1]] <- rmf_create_parameter(data_set_3$list, parnam = parnam, parval = parval)
      lines <- data_set_3$remaining_lines
      rm(data_set_3)
      
    }
  }
  
  # data set 4
  if(nnp > 0) {
    data_set_4 <- do.call(rmfi_parse_list, c(list(remaining_lines = lines, nlst = nnp, varnames = vars, scalevar = scalevar, naux = 0, file = file), arg))
    rmf_lists[[length(rmf_lists)+1]] <- structure(data_set_4$list, kper = 1:dis$nper)
    lines <- data_set_4$remaining_lines
    rm(data_set_4)
  }
  
  # data set 5
  # data set 5 does not have to be present (not stated in manual/online-help)
  # source code states that data set 5 is only read when np (nphfb in src) > 0
  data_set_5 <- rmfi_parse_variables(lines)
  if(np > 0) {
    nacthfb <- as.numeric(data_set_5$variables[1])
    lines <- data_set_5$remaining_lines
    rm(data_set_5)
    
    # data set 6
    acthfb <- vector(mode = 'character', length = nacthfb)
    for(i in 1:nacthfb) {
      data_set_6 <- rmfi_parse_variables(lines)
      acthfb[i] <- toupper(data_set_6$variables[1])
      lines <- data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    # set kper for parameters
    rmf_lists <- lapply(rmf_lists, function(i) rmfi_ifelse0(inherits(i, 'rmf_parameter') && (toupper(attr(i, 'parnam')) %in% acthfb), structure(i, kper = 1:dis$nper), i))
  }

  # create hfb
  obj <- rmf_create_hfb(rmf_lists, dis = dis, noprint = unname(option['NOPRINT']))
  comment(obj) <- comments
  return(obj)
}

#' Write a MODFLOW horizontal flow barrier file
#'
#' \code{rmf_write_hfb} writes a MODFLOW horizontal flow barrier file based on an \code{RMODFLOW} hfb object
#' 
#' @param hfb an \code{RMODFLOW} hfb object
#' @param dis an \code{RMODFLOW} dis object
#' @param file filename to write to; typically '*.hfb'
#' @param ... ignored
#' 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmf_read_hfb}}, \code{\link{rmf_create_hfb}}, \url{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html?hfb6.htm}

rmf_write_hfb<-  function(hfb, dis = rmf_read_dis(), file={cat('Please choose hfb file to overwrite or provide new filename ...\n'); file.choose()}, ...){
  
  vars <- c('irow2', 'icol2', 'hydchr')
  header <-  'Horizontal Flow Barrier Package'
  package <- 'hfb'
  partyp <- 'HFB'
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW', header, 'created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(hfb)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  rmfi_write_variables(as.integer(hfb$np), as.integer(hfb$mxl), as.integer(hfb$nnp), ifelse(hfb$option['noprint'], 'NOPRINT', ''), file=file)
  
  # parameters
  if(hfb$np > 0){
    parm_names <- names(hfb$parameter_values)
    
    for (i in 1:hfb$np){
      p_name <- parm_names[i]
      df <- subset(hfb$data, name == p_name)
      df[[vars[1]]] <- as.integer(df[[vars[1]]])
      df[[vars[2]]] <- as.integer(df[[vars[2]]])
      nlst <- as.integer(nrow(df))
      
      # data set 2
      rmfi_write_variables(p_name, toupper(partyp), hfb$parameter_values[i], nlst, file=file)
      # data set 3
      for (k in 1:nlst){
        rmfi_write_variables(as.integer(df$k[k]), as.integer(df$i[k]), as.integer(df$j[k]), df[k, vars], file=file)
      }
      rm(df)
    }
  }
  
  # data set 4
  df <- subset(hfb$data, parameter == FALSE)
  df[[vars[1]]] <- as.integer(df[[vars[1]]])
  df[[vars[2]]] <- as.integer(df[[vars[2]]])
  
  if(nrow(df) > 0) {
    rmfi_write_list(df, file = file, varnames = vars)
    rm(df)
  }
  
  # data set 5
  rmfi_write_variables(as.integer(hfb$nacthfb), file = file)
  
  # data set 6
  if(hfb$nacthfb > 0){
    for(j in 1:hfb$nacthfb){
      rmfi_write_variables(hfb$acthfb[j], file=file)
    }
  }
  
  
}
