#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#' @param i 
#' @param j 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_tibble.rmf_4d_array <- function(array,
                                       i = NULL,
                                       j = NULL,
                                       k = NULL,
                                       l = NULL,
                                       dis,
                                       mask = array * 0 + 1,
                                       prj = NULL,
                                       crs = NULL) {
  if(!is.null(l)) {
    rmf_as_tibble(rmf_create_array(array[,,,l]), i = i, j = j, k = k, dis = dis, mask = mask[,,,l], prj = prj, crs = crs)
  } else if(!is.null(i) & !is.null(j) & !is.null(k)) {
    tibble::tibble(value = array[i, j, k, ], time = attributes(array)$totim)
  } else {
    warning('Using final stress period results.', call. = FALSE)
    rmf_as_tibble(rmf_create_array(array[,,,dim(array)[4]]), i = i, j = j, k = k, dis = dis, mask = mask[,,,dim(array)[4]], prj = prj, crs = crs)
  }
}