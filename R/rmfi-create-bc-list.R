
#' Set list input for a MODFLOW boundary condition package
#'
#' @param arg list of (1) rmf_list and/or rmf_parm list objects or (2) a single nested \code{list} with rmf_list and/or rmf_parm elements or (3) a single \code{data.frame} element that will be coerced to a rmf_list; defines the boundary condition input. 
#' @param dis dis object. If not explicitely suplied, the function will look in the arg argument for an object of class 'dis'.
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk)
#' @param aux optional character vector with the names of the auxiliary variables
#' @details typically, \code{arg} is \code{list(...)} where the ellipsis contains all the input \code{rmf_lists} for the \code{rmf_create_*} function.
#' @return list with the data, possible parameter values, dimensions and the kper data.frame
#' @keywords internal
#' @seealso \code{\link{rmfi_bc_array}}, \code{\link{rmfi_write_bc_list}}, \code{\link{rmfi_read_bc_list}}

rmfi_create_bc_list <- function(arg, dis, varnames, aux = NULL) {
  
  # find dis
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
      arg <-  arg[[1]]
      
    } else if(inherits(arg[[1]], 'data.frame') && !(inherits(arg[[1]], 'rmf_list'))) { # make rmf_list
      arg <-  lapply(arg, function(i) rmf_create_list(i, kper = 1:dis$nper))
    }
  }
  
  # check for parameters and/or lists and name them
  parameters <- arg[vapply(arg, function(i) inherits(i, 'rmf_parm'), TRUE)]
  if(length(parameters) > 0) names(parameters) <- vapply(parameters, function(i) attr(i, 'name'), 'text')
  lists <- arg[vapply(arg, function(i) !inherits(i, 'rmf_parm'), TRUE)]
  if(length(lists) > 0) names(lists) <- paste('list', 1:length(lists), sep = '_')
  if(any(vapply(c(parameters, lists), function(i) is.null(attr(i, 'kper')), TRUE))) {
    stop('Please make sure all rmf_list and rmf_parm objects have a kper attribute', call. = FALSE)
  }
  
  
  # stress period data frame
  kper <- cbind(data.frame(kper = 1:dis$nper),
                matrix(FALSE, dis$nper, length(unique(c(names(parameters), names(lists)))), dimnames = list(NULL, unique(c(names(parameters), names(lists))))))
  
  if(length(parameters) > 0) {
    for(i in 1:length(parameters)) {
      if(is.null(attr(parameters[[i]], 'instnam'))) {
        kper[attr(parameters[[i]], 'name')] <-  c(1:dis$nper) %in% attr(parameters[[i]],'kper')
      } else {
        kper[c(1:dis$nper) %in% attr(parameters[[i]],'kper'), attr(parameters[[i]], 'name')] <-  attr(parameters[[i]],'instnam')
      }
    }
  }
  
  if(length(lists) > 0) {
    for(i in 1:length(lists)) {
      kper[names(lists)[i]] <- c(1:dis$nper) %in% attr(lists[[i]],'kper')
    }
  }
  
  # dimensions
  np <- 0
  mxl <- 0
  itmp <- 0
  parameter_values <- NULL
  instances <- NULL
  
  if(length(parameters) > 0) {
    np <- length(unique(names(parameters)))
    mxl <- sum(vapply(parameters[!duplicated(names(parameters))], nrow, 1))
  } 
  
  find_mxact <- function(i) {
    kper_names <- names(kper)[which(kper[i,] == T)[-1]] 
    sum(unlist(lapply(parameters[kper_names], nrow))) +
      sum(unlist(lapply(lists[kper_names], nrow)))
  }
  mxact <- max(vapply(kper$kper, find_mxact, 1))

  
  # parameters
  if(length(parameters) > 0) {
    
    instances <- c(table(names(parameters)))
    instances[] <- vapply(seq_along(instances), function(i) rmfi_ifelse0(instances[i] < 2 && is.null(attr(parameters[[names(instances[i])]], 'instnam')), 0, instances[i]), 1)
    if(all(instances == 0)) instances <- NULL
    
    parameter_values <- vapply(parameters, function(i) attr(i, 'value'), 1.0)
    if(!is.null(instances)) parameter_values <- parameter_values[!duplicated(names(parameter_values))]
      
    #check aux
    if(!is.null(aux)) {
      all_aux <- all(vapply(lists, function(i) ncol(i) > 3+length(varnames), T))
      if(!all_aux) stop('Please make sure all AUX variables are defined in each rmf_list')
    }
    
    # set parameter df
    parameters <- lapply(parameters, function(i) {colnames(i)[4:(3+length(varnames))] <-  varnames;
                                                  if(!is.null(aux)) colnames(i)[(3+length(varnames)+1):(3+length(varnames)+length(aux))] <-  aux;
                                                  i$parameter <-  TRUE;
                                                  i$name <-  attr(i, "name");
                                                  i})
    
    # time-varying
    if(any(vapply(parameters, function(i) !is.null(attr(i, 'instnam')), T))) {
      parameters <- lapply(parameters, function(i) {rmfi_ifelse0(is.null(attr(i, 'instnam')), i$instance <-  NA, i$instance <-  attr(i, 'instnam')); i} )
    }
    
    parameters <- do.call(rbind, unname(parameters))
    
  }
  
  # lists
  if(length(lists) > 0) {
    
    #check aux
    if(!is.null(aux)) {
      all_aux <- all(vapply(lists, function(i) ncol(i) > 3+length(varnames), T))
      if(!all_aux) stop('Please make sure all AUX variables are defined in each rmf_list')
    }

    # itmp
    itmp <- structure(vapply(lists, nrow, 1), names = names(lists))
    
    # set lists df
    lists <- lapply(lists, function(i) {colnames(i)[4:(3+length(varnames))] <-  varnames;
                                        if(!is.null(aux)) colnames(i)[(3+length(varnames)+1):(3+length(varnames)+length(aux))] <-  aux;
                                        i$parameter <-  FALSE;
                                        i})
    lists <- lapply(seq_along(lists), function(i) {lists[[i]]$name <- names(lists)[[i]]; lists[[i]]})
    lists <- do.call(rbind, unname(lists))
    if(length(parameters) > 0 && 'instance' %in% colnames(parameters))  lists$instance <-  NA
      
  }
  
  # combine
  data <- structure(rbind(parameters, lists), kper = NULL)
  dimensions <- list(np = np, mxl = mxl, instances = instances, mxact = mxact, itmp = itmp)
  
  return(list(dimensions = dimensions, parameter_values = parameter_values, data = data, kper = kper))
  
}






