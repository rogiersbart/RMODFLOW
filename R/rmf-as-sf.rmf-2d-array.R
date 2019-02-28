#' Title
#'
#' @param array 
#' @param dis 
#' @param mask 
#' @param prj 
#' @param crs 
#'
#' @return
#' @export
#'
#' @examples
rmf_as_sf.rmf_2d_array <- function(array,
                                   dis,
                                   mask = array * 0 + 1,
                                   prj = NULL,
                                   crs = NULL) {
  df <- rmf_as_tibble(array, dis, mask, prj, crs)
  df <- df %>%
    dplyr::group_by(id, value) %>% tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, function(df) sf::st_polygon(list(as.matrix(rbind(df, df[1,]))))))
  if (is.null(prj)) {
    df$data <- sf::st_sfc(df$data)
  } else {
    df$data <- sf::st_sfc(df$data, crs = prj$projection)
  }
  df <- sf::st_sf(df)
  return(df)
}