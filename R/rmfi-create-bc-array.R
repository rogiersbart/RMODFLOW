
#' Set array input for a MODFLOW boundary condition package
#'
#' @param arg list of (1) \code{rmf_2d_array's} and/or rmf_parameter array objects or (2) a single nested \code{list} with \code{rmf_2d_array's} and/or rmf_parameter elements or (3) a \code{matrix}; defines the boundary condition input. 
#' @param dis dis object. If not explicitely suplied, the function will look in the arg argument for an object of class 'dis'.
#' @details typically, \code{arg} is \code{list(...)} where the ellipsis contains all the input \code{rmf_arrays} for the \code{rmf_create_*} function. When matrix elements are present, they are coerced to rmf_2d_arrays which are active for all stress-periods with a warning.
#' @return list with the parameters, input arrays and the kper argument
#' @keywords internal
#' @seealso \code{\link{rmfi_bc_array}}

rmfi_create_bc_array <- function(arg, dis) {
  
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
  
  # if arg is nested list, unnest
  if(length(arg) == 1 && inherits(arg, 'list')) arg <- arg[[1]] 
  # if matrix or 2d-array, make rmf_2d_array which is always active
  arg <- lapply(arg, function(i) rmfi_ifelse0(inherits(i, 'matrix') && !(inherits(i, 'rmf_2d_array')), 
                                              {rmf_create_array(i, kper = 1:dis$nper); warning("Coercing matrix to rmf_2d_array; array active for all stress-periods.")},
                                              i) )
  
  
  # check for parameters and/or arrays and name them
  parameters <- arg[vapply(arg, function(i) inherits(i, 'rmf_parameter'), TRUE)]
  if(length(parameters) > 0) names(parameters) <- vapply(parameters, function(i) attr(i, 'parnam'), 'text')
  
  arrays <- arg[vapply(arg, function(i) !inherits(i, 'rmf_parameter'), TRUE)]
  if(length(arrays) > 0) {
    names(arrays) <- vapply(seq_along(arrays), function(i) rmfi_ifelse0(is.null(attr(arrays[[i]], 'parnam')), paste('array', i, sep = '_'), attr(arrays[[i]], 'parnam')), 'text')
  }

  if(any(vapply(c(parameters, arrays), function(i) is.null(attr(i, 'kper')), TRUE))) {
    stop('Please make sure all rmf_2d_array and rmf_parameter objects have a kper attribute', call. = FALSE)
  }
  
  # stress period data frame
  kper <- cbind(data.frame(kper = 1:dis$nper),
                matrix(FALSE, dis$nper, length(unique(c(names(parameters), names(arrays)))), dimnames = list(NULL, unique(c(names(parameters), names(arrays))))))

  if(length(parameters) > 0) {
    for(i in 1:length(parameters)) {
      if(is.null(attr(parameters[[i]], 'instnam'))) {
        kper[attr(parameters[[i]], 'parnam')] <-  c(1:dis$nper) %in% attr(parameters[[i]],'kper')
      } else {
        kper[c(1:dis$nper) %in% attr(parameters[[i]],'kper'), attr(parameters[[i]], 'parnam')] <-  attr(parameters[[i]],'instnam')
      }
    }
  }
  
  if(length(arrays) > 0) {
    for(i in 1:length(arrays)) {
      kper[names(arrays)[i]] <- c(1:dis$nper) %in% attr(arrays[[i]],'kper')
    }
  }
  
  # dimensions
  np <- 0
  parameter_values <- NULL
  instances <- NULL

  if(length(parameters) > 0) {
    np <- length(unique(names(parameters)))
  }

  # parameters
  if(length(parameters) > 0) {

    instances <- c(table(names(parameters)))
    instances[] <- vapply(seq_along(instances), function(i) rmfi_ifelse0(instances[i] < 2 && is.null(attr(parameters[[names(instances[i])]], 'instnam')), 0, instances[i]), 1)
    if(all(instances == 0)) instances <- NULL

    parameter_values <- vapply(parameters, function(i) attr(i, 'parval'), 1.0)
    if(!is.null(instances)) {
      parameter_values <- parameter_values[!duplicated(names(parameter_values))]
      
      # if parameter is time-varying, list all instances
      inst <- parameters[instances]
      p_names <- vapply(inst, function(i) attr(i, 'parnam'), 'text')
      unq_names <- unique(p_names)
      names(inst) <- vapply(inst, function(i) attr(i, 'instnam'), 'text')
      
      p_inst <- lapply(seq_along(unq_names), function(i) inst[which(p_names == unq_names[i])])
      names(p_inst) <- unq_names
      parameters <- c(parameters[!instances], p_inst)
    }
  }
  
  # check for wrong combinations of parameters and arrays in stress period
  # if parameters are defined: only parameters can be used for a stress period and there must be at least 1 parameter active
  #
  if(length(parameters) > 0) {
    if(length(arrays) > 0) {
      warning('Parameter and non-parameter recharge arrays present. Only parameter arrays will be used.', call. = FALSE)
      kper[,which(colnames(kper) %in% names(arrays))] <- FALSE
    }
    
    parm_df <- subset(kper, select = names(parameters))
    parm_err <- any(vapply(1:dis$nper, function(i) all(is.na(parm_df[i,]) | parm_df[i,] == FALSE), TRUE))
    if(parm_err) stop('If parameter arrays are provided, please make sure at least 1 parameter array is active during each stress period.')
  }
  # multiple non-parameter arrays can not be active for the same stress period
  if(length(arrays) > 0) {
    select <- rmfi_ifelse0(length(parameters > 0), names(kper) != names(parameters), names(kper))
    nparm_df <- subset(kper, select = select[-1])
    nparm_err <- vapply(1:dis$nper, function(i) sum(is.na(nparm_df[i,]) | nparm_df[i,] == TRUE) > 1, TRUE)
    if(any(nparm_err)) stop(paste('There can be only 1 active non-parameter array per stress period. Stress period(s)', which(nparm_err), 'have multiple active arrays.'))
   }

  
  # combine
  data <- c(parameters, arrays)
  dimensions <- list(np = np, instances = instances)
  return(list(dimensions = dimensions, parameter_values = parameter_values, data = data, kper = kper))
  
}
