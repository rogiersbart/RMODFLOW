#' Convert id to id
#' 
#' @param id cell id, providing the place of the number in an input file 2d or 3d array
#' @param from 'r' or 'modflow'. The type of id to convert from. Defaults to 'modflow'
#' @param to 'r' or 'modflow'. The type of id to convert to. Defaults to 'r'
#' @param dis a discretisation file object
#' @details a modflow id provides the place of the number in an input file 3d array (not like the way R uses ids for arrays or matrices; rows and columns are switched)
#' @export
rmf_convert_id_to_id = function(id, dis, from = 'modflow', to = 'r') {
  
  ijk = rmf_convert_id_to_ijk(id, dis = dis, type = from)
  idn = rmf_convert_ijk_to_id(i = ijk$i, j = ijk$j, k = ijk$k, dis = dis, type = to)
  
  return(idn)
  
}
