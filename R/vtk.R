
# TODO binary

#' Write RMODFLOW objects to VTK files
#' 
#' @details \code{rmf_write_vtk} writes a Legacy VTK file.
#' @return \code{NULL}
#' @export
#'
#' @rdname rmf_write_vtk
rmf_write_vtk <- function(...) {
  UseMethod('rmf_write_vtk')
}


#'
#' @param array \code{rmf_2d/3d/4d_array}
#' @param dis \code{RMODFLOW} dis object
#' @param file file to write to
#' @param mask a 2d array when \code{array} is 2d or a 3d array when \code{array} is 3d or 4d that can be coerced to logical. Used to set inactive cells to NA. Defaults to all cells active.
#' @param title character; title of the dataset in the VTK file. Defaults to 'array'.
#' @param as_points logical; should cell-centered nodal values be written to the VTK file as points or rectilinear cells as voxels with a single value per cell (default).  
#' @param vertices logical; should values at the corner nodes of the cells be written to the VTK file as points? Defaults to FALSE. See details.
#' @param as_3d logical; should the 2d surface be represented as 3d? Defaults to FALSE. See details.
#' @param ijk logical; should the ijk indices of the cells be included in the dataset? Defaults to FALSE. See details.
#' @param na_value numeric value to which NA values should be set. Defaults to to NULL which omits cells/points with NA values. ASCII Legacy VTK files do not support Na or NaN values.
#' @param binary logical; should the VTK file be written in binary? Defaults to FALSE.
#' @param prj \code{RMODFLOW} prj object
#' @param endian See \code{\link{writeBin}}. Only applicable when \code{binary = TRUE}.
#'
#' @details A Legacy VTK file is written. If \code{as_points} is TRUE, the cell-centered nodal values are written as points (POLYDATA point structure in VTK). If FALSE, the cell geometry is written with a single array value per cell (UNSTRUCTURED_GRID in VTK using cell type 8 (Pixel) for 2d and 11 (Voxel) for 3d).
#'  If \code{vertices} is TRUE (only applicable when \code{as_points = FALSE}), the values at the cell corners are written as point values as well. Their values are determined using bi/trilinear interpolation and by applying a very small perturbation to the corner node coordinates in order to prevent out-of-bound errors.
#'  This option is useful if contours are to be displayed in post-processing, which is not feasible when only cells are written using this function. Note that the interpolation can be very slow, especially for 3d and 4d arrays.
#'  When \code{as_3d} is TRUE, the 2d array is written as a 3d cell/point type with the vertical coordinate set equal to the array value. This is useful if a topography needs to be displayed, e.g. a water-table.
#'  If \code{ijk} is TRUE, the ijk indices of the cells are also written to the data set as point values for the cell-centered nodes. This is useful for e.g. conversion to Explicit Structured Grids.
#'  \code{rmf_write_vtk} was tested using [Paraview](https://www.paraview.org/).
#' @rdname rmf_write_vtk
#' @export
#' @method rmf_write_vtk rmf_2d_array
#'
#' @examples
#' dis <- rmf_example_file('example-model.dis') %>% rmf_read_dis()
#' file <- tempfile()
#' dis <- rmf_set_prj(dis, NULL)
#' 
#' rmf_write_vtk(dis$top, dis, file = file, as_points = TRUE)
#' rmf_write_vtk(dis$top, dis, file = file, as_3d = TRUE)
#' rmf_write_vtk(dis$top, dis, file = file, as_points = FALSE, n.vertex = TRUE)
rmf_write_vtk.rmf_2d_array <- function(array,
                                       dis,
                                       file,
                                       mask = array * 0 + 1,
                                       title = 'array',
                                       as_points = FALSE,
                                       vertices = FALSE,
                                       as_3d = FALSE,
                                       ijk = FALSE,
                                       na_value = NULL,
                                       binary = FALSE,                                       
                                       prj = rmf_get_prj(dis),
                                       endian = .Platform$endian) {
  
  n.vertex <- ifelse(as_points, 1, 4) # 2D grid
  
  df <- rmf_as_tibble(array, dis, mask = mask, as_points = as_points, id = 'r', name = 'value', prj = prj)
  if(is.null(na_value)) df <- na.omit(df)
  values <- df$value[seq(1, nrow(df), n.vertex)]
  if(!is.null(na_value)) values <- replace(values, is.na(values), na_value)
  
  if(as_3d) {
    df$z <- df$value
  } else {
    df$z <- 0
  }
  
  pts <- df[,c('x', 'y', 'z', 'id')]
  
  # order such that x is fastest, then y, then z
  pts <- pts[order(pts$id, pts$z, pts$y, pts$x),]
  ijk.df <- rmf_convert_id_to_ijk(pts$id[seq(1, nrow(pts), n.vertex)], dis = dis, type = 'r')
  
  ncell <- length(values)
  cells <- matrix(order(pts$id), ncol = n.vertex, byrow = TRUE) - 1
  cells <- as.data.frame(cbind(numPoints = n.vertex, cells))
  
  if(binary) {
    stop('Writing binary VTK files not yet supported', call. = FALSE)
  } else {
    
    # UNSTRUCTURED_GRID because RECTILINEAR_GRID does not support NaN values (in ASCII legacy vtk)
    
    cat('# vtk DataFile Version 2.0', '\n', append = FALSE, file = file)
    cat(paste('RMODFLOW', title, 'VTK object'), '\n', append = TRUE, file = file)
    cat(ifelse(binary, 'BINARY', 'ASCII'), '\n', append = TRUE, file = file)
    cat('DATASET', ifelse(as_points, 'POLYDATA', 'UNSTRUCTURED_GRID'), '\n', append = TRUE, file = file)
    cat('POINTS', nrow(pts), 'double', '\n', append = TRUE, file = file)
    readr::write_delim(pts[,c('x', 'y', 'z')], path = file, col_names = FALSE, append = TRUE)
    if(!as_points) {
      cat('\n', append = TRUE, file = file)
      cat('CELLS', ncell, (n.vertex + 1) * ncell, '\n', append = TRUE, file = file)
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
    if(ijk) {
      cat('SCALARS', 'I', 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(ijk.df$i, path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', 'J', 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(ijk.df$j, path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', 'K', 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(ijk.df$k, path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
    }
    
    # vertex values of cells
    if(!as_points && vertices) {
      # apply 0.001 perturbation to prevent out-of-bound errors of corner node coordinates
      # keep perturbation inside grid by setting negative perturbation values for 1st row, last column and top layer
      perf <- 0.0001
      first_row_ids <- rmf_convert_ijk_to_id(i = 1, j = 1:dis$ncol, k = 1, dis = dis, type = 'r')
      last_col_ids <- rmf_convert_ijk_to_id(i = 1:dis$nrow, j = dis$ncol, k = 1, dis = dis, type = 'r')

      xout_pert <- pts$x + min(dis$delr)*ifelse(pts$id %in% last_col_ids, -perf, perf)
      yout_pert <- pts$y + min(dis$delc)*ifelse(pts$id %in% first_row_ids, -perf, perf)
      
      vertex_values <- rmf_interpolate(array, dis = dis, xout = xout_pert, yout = yout_pert, mask = mask, prj = prj, method = 'linear', outside = 'nearest')
      if(is.null(na_value)) {
        vertex_values <- vertex_values[which(!is.na(vertex_values))]
      } else {
        vertex_values <- replace(vertex_values, is.na(vertex_values), na_value)
      }
      cat('POINT_DATA', ncell*n.vertex, '\n', append = TRUE, file = file)
      cat('SCALARS', paste('vertex', title, sep = '_'), 'double', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      cat(paste0(vertex_values, collapse = '\n'), '\n', append = TRUE, file = file)
      cat('\n', append = TRUE, file = file)
      
      # not useful to write ijk for points
      # i-j-k
      # if(ijk) {
      #   cat('SCALARS', paste('vertex', 'I', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      #   cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      #   readr::write_lines(rep(ijk.df$i, each = n.vertex), path = file, append = TRUE)
      #   cat('\n', append = TRUE, file = file)
      #   
      #   cat('SCALARS', paste('vertex', 'J', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      #   cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      #   readr::write_lines(rep(ijk.df$j, each = n.vertex), path = file, append = TRUE)
      #   cat('\n', append = TRUE, file = file)
      #   
      #   cat('SCALARS', paste('vertex', 'K', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      #   cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      #   readr::write_lines(rep(ijk.df$k, each = n.vertex), path = file, append = TRUE)
      #   cat('\n', append = TRUE, file = file)
      # }
      
    }
  }
}

#'
#' @rdname rmf_write_vtk
#' @export
#' @method rmf_write_vtk rmf_3d_array
#'
#' @examples
#' rmf_write_vtk(dis$botm, dis, file = file, ijk = TRUE)
rmf_write_vtk.rmf_3d_array <- function(array,
                                       dis,
                                       file,
                                       mask = array * 0 + 1,
                                       title = 'array',
                                       as_points = FALSE,
                                       vertices = FALSE,
                                       ijk = FALSE,
                                       na_value = NULL,
                                       binary = FALSE,
                                       prj = rmf_get_prj(dis),
                                       endian = .Platform$endian) {
  
  n.vertex <- ifelse(as_points, 1, 8) # 3D grid
  df <- rmf_as_tibble(array, dis, mask = mask, as_points = as_points, id = 'r', name = 'value', prj = prj)
  if(is.null(na_value)) df <- na.omit(df)
  
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
  if(!is.null(na_value)) values <- replace(values, is.na(values), na_value)
  
  # order such that x is fastest, then y, then z
  pts <- pts[order(pts$id, pts$z, pts$y, pts$x),]
  ijk.df <- rmf_convert_id_to_ijk(pts$id[seq(1, nrow(pts), n.vertex)], dis = dis, type = 'r')
  
  ncell <- length(values)
  cells <- matrix(order(pts$id), ncol = n.vertex, byrow = TRUE) - 1
  cells <- as.data.frame(cbind(numPoints = n.vertex, cells))
  
  if(binary) {
    stop('Writing binary VTK files not yet supported', call. = FALSE)
  } else {
    
    cat('# vtk DataFile Version 2.0', '\n', append = FALSE, file = file)
    cat(paste('RMODFLOW', title, 'VTK object'), '\n', append = TRUE, file = file)
    cat('ASCII', '\n', append = TRUE, file = file)
    cat('DATASET', ifelse(as_points, 'POLYDATA', 'UNSTRUCTURED_GRID'), '\n', append = TRUE, file = file)
    cat('POINTS', nrow(pts), 'double', '\n', append = TRUE, file = file)
    readr::write_delim(pts[,c('x', 'y', 'z')], path = file, col_names = FALSE, append = TRUE)
    if(!as_points) {
      cat('\n', append = TRUE, file = file)
      cat('CELLS', ncell, (n.vertex + 1) * ncell, '\n', append = TRUE, file = file)
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
    if(ijk) {
      cat('SCALARS', 'I', 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(ijk.df$i, path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', 'J', 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(ijk.df$j, path = file, append = TRUE)
      cat('\n', append = TRUE, file = file)
      
      cat('SCALARS', 'K', 'int', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      readr::write_lines(ijk.df$k, path = file, append = TRUE) 
      cat('\n', append = TRUE, file = file)
    }
    
    # vertex values of cells
    if(!as_points && vertices) {
      stop('Writing vertices for 3D & 4D arrays not yet supported', call. = FALSE)
      # apply 0.001 perturbation to prevent out-of-bound errors of corner node coordinates
      # keep perturbation inside grid by setting negative perturbation values for 1st row, last column and top layer
      perf <- 0.0001
      first_row_ids <- rep(rmf_convert_ijk_to_id(i = 1, j = 1:dis$ncol, k = 1, dis = dis, type = 'r'), each = dis$nlay) + seq(0, dis$nlay - 1, by = 1)*prod(dis$nrow, dis$ncol)
      last_col_ids <- rep(rmf_convert_ijk_to_id(i = 1:dis$nrow, j = dis$ncol, k = 1, dis = dis, type = 'r'), each = dis$nlay) + seq(0, dis$nlay - 1, by = 1)*prod(dis$nrow, dis$ncol)
      top_layer_ids <- 1:prod(dis$nrow, dis$ncol)
      
      xout_pert <- pts$x + min(dis$delr)*ifelse(pts$id %in% last_col_ids, -perf, perf)
      yout_pert <- pts$y + min(dis$delc)*ifelse(pts$id %in% first_row_ids, -perf, perf)
      thck <- rmf_calculate_thickness(dis)
      zout_pert <- pts$z + min(thck, na.rm = TRUE)*ifelse(pts$id %in% top_layer_ids, -perf, perf)
      
      vertex_values <- rmf_interpolate(array, dis = dis, xout = xout_pert, yout = yout_pert, zout = zout_pert, mask = mask, prj = prj, method = 'linear', outside = 'nearest')
      if(is.null(na_value)) {
        vertex_values <- vertex_values[which(!is.na(vertex_values))]
      } else {
        vertex_values <- replace(vertex_values, is.na(vertex_values), na_value)
      }
      cat('POINT_DATA', ncell*n.vertex, '\n', append = TRUE, file = file)
      cat('SCALARS', paste('vertex', title, sep = '_'), 'double', 1, '\n', append = TRUE, file = file)
      cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      cat(paste0(vertex_values, collapse = '\n'), '\n', append = TRUE, file = file)
      cat('\n', append = TRUE, file = file)
      
      # not useful to write ijk for points
      # i-j-k
      # if(ijk) {
      #   cat('SCALARS', paste('vertex', 'I', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      #   cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      #   readr::write_lines(rep(ijk.df$i, each = n.vertex), path = file, append = TRUE)
      #   cat('\n', append = TRUE, file = file)
      #   
      #   cat('SCALARS', paste('vertex', 'J', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      #   cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      #   readr::write_lines(rep(ijk.df$j, each = n.vertex), path = file, append = TRUE)
      #   cat('\n', append = TRUE, file = file)
      #   
      #   cat('SCALARS', paste('vertex', 'K', sep = '_'), 'int', 1, '\n', append = TRUE, file = file)
      #   cat('LOOKUP_TABLE', 'default', '\n', append = TRUE, file = file)
      #   readr::write_lines(rep(ijk.df$k, each = n.vertex), path = file, append = TRUE)
      #   cat('\n', append = TRUE, file = file)
      # }
    }
  }
}


#'
#' @param ... arguments passed to \code{rmf_write_vtk.rmf_3d_array} or \code{rmf_as_array}
#' @details Writing a 4d array to a VTK file loops over all time steps in the array and writes 3d arrays for time step l to file \code{paste(l, basename(file), sep = '_')}. 
#' This format is recognized as a time-varying array by most software supporting VTK files.
#' @rdname rmf_write_vtk
#' @export
#' @method rmf_write_vtk rmf_4d_array
#'
rmf_write_vtk.rmf_4d_array <- function(array, 
                                       dis, 
                                       file,
                                       mask = array(1, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                                       ...) {
  
  dir <- dirname(file)
  base <- basename(file)
  
  for(l in 1:dim(array)[4]) {
    
    f <- file.path(dir, paste(l, base, sep = '_'))
    rmf_write_vtk(array[,,,l], dis = dis, file = f, mask = mask, ...)
  }
}

#'
#' @param obj \code{rmf_list} object 
#'
#' @rdname rmf_write_vtk
#' @export
#' @method rmf_write_vtk rmf_list
#'
rmf_write_vtk.rmf_list <- function(obj, dis, file, ...) {
  
  r <- rmf_as_array(obj, dis, ...)
  rmf_write_vtk(r, dis = dis, file = file, ...)
  
}

