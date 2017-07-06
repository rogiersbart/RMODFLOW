#' Generic function to export tables from RMODFLOW arrays
#' 
#' @rdname rmf_export_table
#' @export
rmf_export_table.rmf_4d_array <- function(array,
                                  k,
                                  l,
                                  dis,
                                  bas = NULL,
                                  mask = rmfi_ifelse0(is.null(bas),array*0+1,bas$ibound[,,1]),
                                  prj=NULL,
                                  crs=NULL,
                                  file='rmf_export.csv',
                                  type='csv') {
  if(type=='csv') {
    
    cell_coord <- cell_coordinates(dis)
    if(!is.null(prj)) {
      cell_coord <- convert_grid_to_xyz(x=c(cell_coord$x[,,k]),y=c(cell_coord$y[,,k]),prj=prj,dis=dis)
    } else {
      cell_coord <- data.frame(x = c(cell_coord$x[,,k]), y = c(cell_coord$y[,,k]))
    }
    write.csv(na.omit(data.frame(x = c(cell_coord$x), y = c(cell_coord$y), value = c(array[,,k,l]))), file = file, row.names = FALSE)
    
  } else {
    stop('Please provide valid type.')
  }
}
