

# TODO make contours work in ParaView
# has to do with the values at the vertices of the cells, which should vary between neighbouring cells
# based on the distance weighted average 
# this is not rmf_as_tibble behaviour though and should be added (eg a vertices = TRUE option to obtain interpolated values at vertices)

# TODO make ijk optional

# TODO upgrade to VTK 5.0 using CELL offset & connection

#' Write RMODFLOW objects to VTK files
#' 
#' @details \code{rmf_write_vtk} writes a Legacy VTK file.
#' @return \code{NULL}
#' @export
#'
#' @examples
rmf_write_vtk <- function(...) {
  UseMethod('rmf_write_vtk')
}


rmf_write_vtk.rmf_2d_array <- function(array,
                                       dis,
                                       file,
                                       mask = array * 0 + 1,
                                       title = 'array',
                                       as_points = FALSE,
                                       as_3d = FALSE,
                                       binary = FALSE) {
  
  # TODO maybe always add centroid points in data set (disregard as_points argument)
  
  vertices <- ifelse(as_points, 1, 4) # 2D grid
  
  df <- rmf_as_tibble(array, dis, mask = mask, as_points = as_points, id = 'r', name = 'value')
  df <- na.omit(df)
  values <- df$value[seq(1, nrow(df), vertices)]
  
  if(as_3d) {
    df$z <- df$value
  } else {
    df$z <- 0
  }
  
  pts <- df[,c('x', 'y', 'z', 'id')]
  
  # order such that x is fastest, then y, then z
  pts <- pts[order(pts$id, pts$z, pts$y, pts$x),]
  ijk <- rmf_convert_id_to_ijk(pts$id[seq(1, nrow(pts), vertices)], dis = dis, type = 'r')
  
  ncell <- length(values)
  cells <- matrix(order(pts$id), ncol = vertices, byrow = TRUE) - 1
  cells <- as.data.frame(cbind(numPoints = vertices, cells))
  
  if(binary) {
    stop('binary VTK files not yet supported', call. = FALSE)
  } else {
    
    # UNSTRUCTURED_GRID because RECTILINEAR_GRID does not support NaN values (in ASCII legacy vtk)
    
    cat('# vtk DataFile Version 2.0', '\n', append = FALSE, file = file)
    cat(paste('RMODFLOW', title, 'VTK object'), '\n', append = TRUE, file = file)
    cat('ASCII', '\n', append = TRUE, file = file)
    cat('DATASET', ifelse(as_points, 'POLYDATA', 'UNSTRUCTURED_GRID'), '\n', append = TRUE, file = file)
    cat('POINTS', nrow(pts), 'double', '\n', append = TRUE, file = file)
    readr::write_delim(pts[,c('x', 'y', 'z')], path = file, col_names = FALSE, append = TRUE)
    if(!as_points) {
      cat('\n', append = TRUE, file = file)
      cat('CELLS', ncell, (vertices + 1) * ncell, '\n', append = TRUE, file = file)
      readr::write_delim(cells, path = file, col_names = FALSE, append = TRUE)
      cat('\n', append = TRUE, file = file)
      cat('CELL_TYPES', ncell, '\n', append = TRUE, file = file)
      cat(paste0(rep(8, ncell), collapse = '\n'), '\n', append = TRUE, file = file) # 8 = PIXEL
    } 
    cat('\n', append = TRUE, file = file)
    
    # data
    cat(ifelse(as_points, 'POINT_DATA', 'CELL_DATA'), ncell, '\n', append = TRUE, file = file)
    cat('SCALARS', title, 'double', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    cat(paste0(values, collapse = '\n'), '\n', append = TRUE, file = file)
    cat('\n', append = TRUE, file = file)
    
    # i-j-k for ExplicitStructuredGrid
    cat('SCALARS', 'I', 'int', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    readr::write_lines(ijk$i, path = file, append = TRUE)
    cat('\n', append = TRUE, file = file)
    
    cat('SCALARS', 'J', 'int', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    readr::write_lines(ijk$j, path = file, append = TRUE)
    cat('\n', append = TRUE, file = file)
    
    cat('SCALARS', 'K', 'int', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    readr::write_lines(ijk$k, path = file, append = TRUE)
    cat('\n', append = TRUE, file = file)
    
    # vertex values of cells
    if(!as_points) {
      cat('POINT_DATA', ncell*vertices, '\n', append = TRUE, file = file)
      cat('SCALARS', paste('vertex', title, sep = '_'), 'double', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      cat(paste0(rep(values, each = vertices), collapse = '\n'), '\n', append = TRUE, file = file)
      cat('\n', append = TRUE, file = file)
      
      # i-j-k
      cat('SCALARS', paste('vertex', 'I', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(rep(ijk$i, each = vertices), path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', paste('vertex', 'J', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(rep(ijk$j, each = vertices), path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', paste('vertex', 'K', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(rep(ijk$k, each = vertices), path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
    }
    
    # if(as_3d) {
    
    # } else {
    # mask[which(mask == 0)] <- 0
    # values <- c(t(array * mask^2))
    # 
    # cat('# vtk DataFile Version 2.0', '\n', append = FALSE, file = file)
    # cat(paste('RMODFLOW', title, 'VTK object'), '\n', append = TRUE, file = file)
    # cat('ASCII', '\n', append = TRUE, file = file)
    # cat('DATASET RECTILINEAR_GRID', '\n', append = TRUE, file = file)
    # cat('DIMENSIONS', dis$ncol + 1, dis$nrow + 1, 1, '\n', append = TRUE, file = file)
    # cat('X_COORDINATES', dis$ncol + 1, 'double', '\n', append = TRUE, file = file)
    # cat(c(0, cumsum(dis$delr)), '\n', append = TRUE, file = file)
    # cat('Y_COORDINATES', dis$nrow + 1, 'double', '\n', append = TRUE, file = file)
    # cat(rev(c(0, cumsum(dis$delc))), '\n', append = TRUE, file = file)
    # cat('Z_COORDINATES', 1, 'double', '\n', append = TRUE, file = file)
    # cat(0, '\n', append = TRUE, file = file)
    # cat('CELL_DATA', prod(dis$nrow, dis$ncol), '\n', append = TRUE, file = file)
    # cat('SCALARS', title, 'double', 1, '\n', append = TRUE, file = file)
    # cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    # cat(paste0(values, collapse = '\n'), '\n', append = TRUE, file = file)
    # }
    
  }
}


rmf_write_vtk.rmf_3d_array <- function(array,
                                       dis,
                                       file,
                                       mask = array * 0 + 1,
                                       title = 'array',
                                       as_points = FALSE,
                                       binary = FALSE) {
  
  vertices <- ifelse(as_points, 1, 8) # 3D grid
  df <- rmf_as_tibble(array, dis, mask = mask, as_points = as_points, id = 'r', name = 'value')
  df <- na.omit(df)
  
  if(as_points) {
    values <- df$value
    pts <- df[,c('x', 'y', 'z', 'id')]
  } else {
    values <- df$value[seq(1, nrow(df), 4)] # check this
    pts_tops <- df[,c('x', 'y', 'top', 'id')]
    pts_botm <- df[,c('x', 'y', 'botm', 'id')]
    
    colnames(pts_tops) <- colnames(pts_botm) <- c('x', 'y', 'z', 'id')
    pts <- rbind(pts_tops, pts_botm)
  }
  
  
  # order such that x is fastest, then y, then z
  pts <- pts[order(pts$id, pts$z, pts$y, pts$x),]
  ijk <- rmf_convert_id_to_ijk(pts$id[seq(1, nrow(pts), vertices)], dis = dis, type = 'r')
  
  ncell <- length(values)
  cells <- matrix(order(pts$id), ncol = vertices, byrow = TRUE) - 1
  cells <- as.data.frame(cbind(numPoints = vertices, cells))
  
  if(binary) {
    stop('binary VTK files not yet supported', call. = FALSE)
  } else {
    
    cat('# vtk DataFile Version 2.0', '\n', append = FALSE, file = file)
    cat(paste('RMODFLOW', title, 'VTK object'), '\n', append = TRUE, file = file)
    cat('ASCII', '\n', append = TRUE, file = file)
    cat('DATASET', ifelse(as_points, 'POLYDATA', 'UNSTRUCTURED_GRID'), '\n', append = TRUE, file = file)
    cat('POINTS', nrow(pts), 'double', '\n', append = TRUE, file = file)
    readr::write_delim(pts[,c('x', 'y', 'z')], path = file, col_names = FALSE, append = TRUE)
    if(!as_points) {
      cat('\n', append = TRUE, file = file)
      cat('CELLS', ncell, (vertices + 1) * ncell, '\n', append = TRUE, file = file)
      readr::write_delim(cells, path = file, col_names = FALSE, append = TRUE)
      cat('\n', append = TRUE, file = file)
      cat('CELL_TYPES', ncell, '\n', append = TRUE, file = file)
      cat(paste0(rep(11, ncell), collapse = '\n'), '\n', append = TRUE, file = file) # 11 = VOXEL
    } 
    cat('\n', append = TRUE, file = file)
    
    # data
    cat(ifelse(as_points, 'POINT_DATA', 'CELL_DATA'), ncell, '\n', append = TRUE, file = file)
    cat('SCALARS', title, 'double', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    cat(paste0(values, collapse = '\n'), '\n', append = TRUE, file = file)
    cat('\n', append = TRUE, file = file)
    
    # i-j-k for ExplicitStructuredGrid
    cat('SCALARS', 'I', 'int', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    readr::write_lines(ijk$i, path = file, append = TRUE)
    cat('\n', append = TRUE, file = file)
    
    cat('SCALARS', 'J', 'int', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    readr::write_lines(ijk$j, path = file, append = TRUE)
    cat('\n', append = TRUE, file = file)
    
    cat('SCALARS', 'K', 'int', 1, '\n', append = TRUE, file = file)
    cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
    readr::write_lines(ijk$k, path = file, append = TRUE) 
    cat('\n', append = TRUE, file = file)
    
    # vertex values of cells
    if(!as_points) {
      cat('POINT_DATA', ncell*vertices, '\n', append = TRUE, file = file)
      cat('SCALARS', paste('vertex', title, sep = '_'), 'double', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      cat(paste0(rep(values, each = vertices), collapse = '\n'), '\n', append = TRUE, file = file)
      cat('\n', append = TRUE, file = file)
      
      # i-j-k
      cat('SCALARS', paste('vertex', 'I', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(rep(ijk$i, each = vertices), path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', paste('vertex', 'J', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(rep(ijk$j, each = vertices), path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', paste('vertex', 'K', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(rep(ijk$k, each = vertices), path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
    }
    
  }
}


rmf_write_vtk.rmf_4d_array <- function(array, 
                                       dis, 
                                       file,
                                       mask = array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                                       title = 'array',
                                       as_points = FALSE,
                                       binary = FALSE) {
  
  dir <- dirname(file)
  base <- basename(file)
  
  for(l in 1:dim(array)[4]) {
    
    f <- file.path(dir, paste(l, base, sep = '_'))
    rmf_write_vtk(array[,,,l], dis = dis, file = f, mask = mask, title = title, as_points = as_points, binary = binary)
  }
}

