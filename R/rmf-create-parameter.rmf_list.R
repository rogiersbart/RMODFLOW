

#' Create a List Data input parameter
#'
#' Create a parameter for List Data input used in MODFLOW boundary condition packages.
#'
#' @param rmf_list a rmf_list object
#' @param parnam character specifying the name of the parameter
#' @param parval numeric specifying the value of the parameter which is used to multiply the flux controlling variable in the data.frame. Defaults to 1.0
#' @param instnam optional character specying the instance name of the parameter is to be time-varying; defaults to NULL
#' @details the variable in the data.frame which is multiplied differs between boundary condition packages. 
#'          if the parameter is to be time-varying, a separate parameter should be created for each instance with a unique \code{instnam} but with the same \code{name} 
#' @return an object of class \code{rmf_parameter} and \code{rmf_list} 
#' @export
#' @seealso \code{\link{rmf_create_list}}
#' 

rmf_create_parameter.rmf_list <- function(rmf_list,
                                          parnam, 
                                          parval = 1.0,
                                          instnam = NULL, 
                                          kper = attr(rmf_list, 'kper')) {
  
  attr(rmf_list, 'kper') <- kper
  attr(rmf_list, 'parnam') <- parnam
  attr(rmf_list, 'parval') <- parval
  attr(rmf_list, 'instnam') <- instnam
  class(rmf_list) <- c('rmf_parameter', class(rmf_list))
  return(rmf_list)
}