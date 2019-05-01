#' Create an \code{RMODFLOW} hfb object.
#' 
#' \code{rmf_create_hfb} creates an \code{RMODFLOW} hfb object
#' 
#' @param ... \code{rmf_list} (possibly of class \code{rmf_parm}) objects or a single \code{list} with \code{rmf_list} objects (possibly of class \code{rmf_parm}) elements; defines the horizontal-flow barriers. 
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
  
  # set kper and direction
  # find dis
  arg <- list(...)

  if(missing(dis)) {
    dis_present <- vapply(arg, function(i) 'dis' %in% class(i), TRUE)
    if(any(dis_present)) {
      dis <- arg[dis_present][[1]]
      arg <- arg[!dis_present]
    } else {
      stop('Please provide a dis argument')
    }
  }
  
  if(length(arg) == 1) {
    if(inherits(arg[[1]], 'list')) {
      arg <- arg[[1]]
    } else if(inherits(arg[[1]], 'data.frame') && !(inherits(arg[[1]], 'rmf_list'))) { # make rmf_list
      arg <- lapply(arg, function(i) rmf_create_list(i, kper = 1:dis$nper))
    }
  }
  
  set_hfb <- function(rmf_list) {
    if(is.null(attr(rmf_list, 'kper'))) {
      warning('Missing kper argument for hfb input list. Assuming this list is not active', call. = FALSE)
    } else if(!identical(as.numeric(attr(rmf_list, 'kper')), as.numeric(1:dis$nper))) {
      stop('Please make sure all hfb input lists have either a kper argument which is active for all stress periods or no kper argument at all.')
    }
    
    if(inherits(rmf_list, 'rmf_parm') && !is.null(attr(rmf_list, 'instnam'))) {
      stop('Time-varying parameters are not supported for the hfb package.')
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
  parameters <- arg[vapply(arg, function(i) inherits(i, 'rmf_parm'), TRUE)]
  if(length(parameters) > 0) names(parameters) <- vapply(parameters, function(i) attr(i, 'parnam'), 'text')
  lists <- arg[vapply(arg, function(i) !inherits(i, 'rmf_parm'), TRUE)]
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
    acthfb <- vapply(parameters, function(i) if(!is.null(attr(i, 'kper'))) attr(i, 'parnam'), 'text')
    nacthfb <- length(acthfb)
    
    # set parameter df
    parameters <- lapply(parameters, function(i) {i$parameter <-  TRUE;
                                                  i$name <-  attr(i, 'parnam');
                                                  i$active <- !is.null(attr(i,"kper"));
                                                  i <- i[c("i","j","k",vars,'parameter','name','active')];
                                                  colnames(i)[4:(3+length(vars))] <-  vars;
                                                  i})
    parameters <- do.call(rbind, unname(parameters))

  }
  
  # lists
  if(length(lists) > 0) {
    # set lists df
    lists <- lapply(lists, function(i) {i <- i[c("i","j","k",vars)];
                                        colnames(i)[4:(3+length(vars))] <-  vars;
                                        i$parameter <-  FALSE;
                                        i$active <- is.null(attr(i,"kper"));
                                        i})
    lists <- lapply(seq_along(lists), function(i) {lists[[i]]$name <- names(lists)[[i]]; lists[[i]]})
    lists <- do.call(rbind, unname(lists))
  }
  
  # combine
  data <- structure(rbind(parameters, lists), kper = NULL)

  # create hfb object
  obj <- list()
  obj$dimensions <- list(np = np, mxl = mxl, nnp = nrow(data) - mxl, nacthfb = nacthfb, acthfb = acthfb)
  obj$ihfbcb <- NULL
  obj$option <- c('noprint' = noprint)
  obj$aux <- NULL
  obj$data <- data
  if(np > 0) obj$parameter_values <- parameter_values

  class(obj) <- c('hfb', 'rmf_package')
  return(obj)
  
}