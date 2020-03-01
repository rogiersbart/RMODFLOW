#' Generic function for static 2D plotting
#' 
#' @rdname rmf_plot
#' @export
rmf_plot <- function(...) {
  UseMethod('rmf_plot')
}


#' Plot a MODFLOW volumetric budget
#'
#' \code{rmf_plot.bud} plots a MODFLOW volumetric budget
#' 
#' @param bud \code{RMODFLOW} bud object
#' @param dis \code{RMODFLOW} dis object
#' @param what character; what to plot "rates", "cumulative", "total", "difference" or "discrepancy". Defaults to "rates".
#' @param fluxes character; either "all" or a character vector with the flux components to plot. Only used when \code{what} is "rates" or "cumulative"
#' @param net logical; if TRUE, it sums the inflows and outflows of the flux component to plot the net fluxes. If FALSE, it will plot both the inflows and outflows. Only used when \code{what} is "rates", "cumulative" or "total".
#' @param type character; plot type. Either "bar" or "area"
#' @param timesteps integer timesteps index to select from a transient budget. A single negative value will select the last available timestep. Defaults to NULL.
#' @details any flux components that are zero for the entire simulation are ommited from the plot. This might be the case with constant head cells, since these are always written to the budget even if no constant head cells are specified in the model.
#'          By default, geom_area is used for plotting (type = "area"). If there is only one stress period and this stress period is steady-state however, this will return unstacked bar plots. If type is "bar", geom_col is used and stacked bar plots are returned. 
#'          
#' @return ggplot2 object
#' @method rmf_plot bud
#' @export
rmf_plot.bud <-  function(bud, 
                          dis,
                          what = 'rates',
                          fluxes = 'all',
                          net = FALSE,
                          type = 'area', 
                          timesteps = NULL) {
  
  # nstp
  bud <- lapply(bud, function(i) cbind(i, nstp = c(0,cumsum(dis$nstp)[i$kper-1])+i$kstp))
  
  if(!is.null(timesteps)) {
    if(length(timesteps) == 1 && timesteps < 0) timesteps <- nrow(bud[[1]])
    bud <- lapply(bud, function(i) i[timesteps,])
  }
  
  # create df for plotting
  c_names <-  !(colnames(bud$rates) %in% c('kstp','kper','nstp'))
  tidy_df <- function(i) {
    i <- reshape(i, direction = 'long', varying = which(c_names), v.names = 'value', times = colnames(i)[c_names])
    i <- i[-6]
    colnames(i)[4] <- 'flux'
    i$io <- ifelse(endsWith(i$flux, '_in'), 'in', 'out')
    i$io <- factor(i$io)
    i$flux <- unlist(strsplit(i$flux, "\\_in|\\_out"))
    
    # remove all-zero fluxes
    l_zero <- aggregate(i$value, by=list(i$flux), function(i) sum(i) == 0)$x
    l_zero[which(levels(factor(i$flux)) %in% c('difference', 'discrepancy', 'total'))] <- FALSE
    i <- subset(i, i$flux %in% levels(factor(i$flux))[!l_zero])
    i$flux <- factor(i$flux)
    
    if(what %in% c('rates', 'cumulative', 'total')) i$value <- ifelse(i$io == 'in', i$value, -i$value)
    return(i)
  }
  
  df <- lapply(bud, tidy_df)
  
  # plot
  
  if(what %in% c('difference', 'discrepancy', 'total')) {
    df <- lapply(df, function(i) subset(i, flux == what))
    df$cumulative$volume <- 'cumulative'
    df$rates$volume <- 'rates'
    df <- rbind(df[[1]], df[[2]])
    df$volume <- factor(df$volume)
    
    x_label <- 'nstp'
    x <- ggplot2::sym('nstp')
    gm_line <- ggplot2::geom_path()
    
    # check if ss 
    if((dis$nper == 1 && dis$sstr == "SS") || !is.null(timesteps) && length(timesteps) == 1) {
      if(type == "area") {
        type <- 'bar'
        df$nstp <- factor(df$nstp)
        gm_line <- ggplot2::geom_col(ggplot2::aes(fill = volume))
      } 
    }
    
    if(what == 'total') {
      if(net) {
        df <- aggregate(list(value = df$value), by = list(nstp = df$nstp, volume = df$volume),  sum)
        p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, group=volume, colour = volume)) + gm_line +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::facet_wrap(~volume, ncol = 1, scales = 'free_y') + 
          ggplot2::labs(title = 'Net total volume [L**3]', y = 'Volume [L**3]', x = x_label)
      } else {
        if(type == 'bar') {
          p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, colour=io, fill=io)) +
            ggplot2::geom_col() + 
            ggplot2::geom_hline(yintercept = 0, colour = 'black') +
            ggplot2::facet_wrap(~volume, ncol = 1, scales = 'free_y') +
            ggplot2::labs(title = 'Gross total volume [L**3]', y = 'Volume [L**3]', x = x_label)
          
        } else if(type == 'area') {
          p <- ggplot2::ggplot() +
            ggplot2::geom_area(data=subset(df, io=='in'), ggplot2::aes(x=!!x ,y=value, colour = volume, fill = volume), alpha=0.7) +
            ggplot2::geom_area(data=subset(df, io=='out'), ggplot2::aes(x=!!x ,y=value, colour = volume, fill = volume), alpha=0.7) +
            ggplot2::geom_hline(yintercept = 0, colour = 'black') +
            ggplot2::facet_wrap(~volume, ncol = 1, scales = 'free_y')+ 
            ggplot2::labs(title = 'Gross total volume [L**3]', y = 'Volume [L**3]', x = x_label)
          
        }
      }
    } else { # difference/discrepancy
      # plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, group=io, colour = volume)) + gm_line + 
        ggplot2::geom_hline(yintercept = 0, colour = 'black') +
        ggplot2::facet_wrap(~volume, ncol = 1, scales = 'free_y') +
        ggplot2::labs(title = rmfi_ifelse0(what == 'difference', "Volume in - volume out", "Discrepancy"), y = rmfi_ifelse0(what == 'difference', "Volume [L**3]", "% discrepancy"), x = x_label)
    }
    
    # rates/cumulative
  } else {
    
    df <- subset(df[[what]], !(flux %in% c('difference', 'discrepancy', 'total')))
    # remove fluxes if necessary
    if(length(fluxes) > 1 || fluxes != 'all') df <- subset(df, flux %in% fluxes)
    
    x_label <- 'nstp'
    x <- ggplot2::sym('nstp')
    
    # check if ss 
    if((dis$nper == 1 && dis$sstr == "SS") || !is.null(timesteps) && length(timesteps) == 1) {
      if(type == "area") {
        type <- 'bar'
        x <- ggplot2::sym('flux')
        x_label <- 'flux'
      } else {
        df$nstp <- factor(df$nstp)
      }
    }
    
    # plot
    if(net) {
      df <- aggregate(list(value = df$value), by = list(nstp = df$nstp, flux = df$flux),  sum)
      if(type == 'bar') {
        p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, colour=flux, fill=flux)) +
          ggplot2::geom_col() +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = rmfi_ifelse0(what == 'rates', "Net volumetric rates", "Net cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
      } else if(type == 'area') {
        p <- ggplot2::ggplot(data=df, ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux)) +
          ggplot2::geom_area(alpha=0.7) +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = rmfi_ifelse0(what == 'rates', "Net volumetric rates", "Net cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
      }
    } else {
      if(type == 'bar') {
        p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, colour=flux, fill=flux)) +
          ggplot2::geom_col() +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = rmfi_ifelse0(what == 'rates', "Gross volumetric rates", "Gross cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
        
      } else if(type == 'area') {
        p <-  ggplot2::ggplot() +
          ggplot2::geom_area(data=subset(df, io=='in'), ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
          ggplot2::geom_area(data=subset(df, io=='out'), ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = rmfi_ifelse0(what == 'rates', "Gross volumetric rates", "Gross cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
      }
    }
  }
  
  return(p)
}

#' Plot a flux component of a cell-by-cell budget object
#'
#' @param cbc a \code{RMODFLOW} cell-by-cell budget object
#' @param dis a \code{RMODFLOW} dis object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot; defaults to plotting the final time step.
#' @param flux character denoting which flux to plot. See details.
#' @param type plot type: 'fill' (default), 'factor', 'grid', 'contour' or 'vector'
#' @param kper integer specifying the stress-period. Use in conjunction with kstp. See details.
#' @param kstp integer specifying the time step of kper. Use in conjunction with kper. See details.
#' @param active_only logical; indicating if only the active cells should be plotted for list-directed components of the cbc object. Non-active cells are set to NA. Defaults to FALSE.
#' @param hed optional hed object for only plotting the saturated part of the grid; possibly subsetted with time step number. Also used in \code{\link{rmf_convert_cbc_to_darcy}} when \code{flux = 'darcy'} and \code{type = 'vector'}. 
#' @param porosity optional 3d array with porosity values passed to \code{\link{rmf_convert_cbc_to_darcy}} when \code{flux = 'darcy'} and \code{type = 'vector'}.
#' @param ... additional parameters passed to \code{\link{rmf_plot.rmf_4d_array}} or \code{\link{rmf_plot.rmf_list}}
#' 
#' @details Flux can be \code{'constant_head'}, \code{'storage'}, \code{'flow_right_face'}, \code{'flow_front_face'}, \code{'flow_lower_face'}, \code{'wells'},
#' \code{'river_leakage'}, \code{'recharge'}, \code{'drains'}, \code{'head_dep_bounds'} or any other description as written by MODFLOW.
#'  Additionally, flux can be \code{'darcy'}, in which case Darcy fluxes are computed by \code{\link{rmf_convert_cbc_to_darcy}} and the magnitude is plotted. If
#'  type = 'vector', the x, y and z components are used to determine the direction of the arrows.
#'  
#'  There are two ways to specify which time step to plot. The \code{l} argument can be specified which represents the total time step number. 
#'  The other option is to specify both \code{kper} & \code{kstp} which specify the stress-period and corresponding time step in that stress-period.
#'  A negative \code{kstp} will plot the final time step of the stress-period.
#'  
#'  If no output is written for the specified time step, as controlled by the Output Control file in MODFLOW, an error is thrown.
#'
#' @return ggplot2 object or layer
#' @method rmf_plot cbc
#' @export
#'
rmf_plot.cbc <- function(cbc, 
                         dis,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         l = NULL,
                         flux = NULL,
                         type = 'fill',
                         kper = NULL,
                         kstp = NULL,
                         active_only = FALSE,
                         hed = NULL,
                         porosity = NULL,
                         ...) {
  
  if(is.null(flux)) stop('Please specify a flux to plot.', call. = FALSE)
  if(flux != 'darcy' && !flux %in% names(cbc)) stop('Specified flux component not present in cbc object.', call. = FALSE)
  if(flux == 'darcy') {
    darcy <- rmf_convert_cbc_to_darcy(cbc, dis = dis, hed = hed, porosity = porosity)
    cbc$darcy <- darcy$q
  } 
  obj <- cbc[[flux]]
  
  # skip if ijk are specified and a time series should be plotted
  if(!(!is.null(i) && !is.null(j) && !is.null(k))) {
    if(is.null(l) && (is.null(kper) && is.null(kstp))) {
      max_stp <- ifelse(inherits(obj, 'rmf_list'), length(unique(obj$nstp)), dim(obj)[4])
      if(max_stp > 1) warning('Plotting final time step results.', call. = FALSE)
      l <- max_stp
    }
    
    if(is.null(l)) {
      if(any(is.null(kper), is.null(kstp))) stop('Please specify either l or kstp & kper.', call. = FALSE)
      l <- ifelse(kper == 1, 0, cumsum(dis$nstp)[kper-1]) + ifelse(kstp < 0, dis$nstp[kper], kstp)
    }
    
    if(inherits(obj, 'rmf_list')) {
      if(!(l %in% obj$nstp)) stop('No output written for specified time step.', call. = FALSE)
      obj <- subset(obj, nstp == l)
      rmf_plot(obj, dis = dis, i=i, j=j, k=k, variable = 'value', active_only = active_only, hed = hed, type = type, ...)
    } else {
      if(!(l %in% attr(obj, 'nstp'))) stop('No output written for specified time step.', call. = FALSE)
      rmf_plot(obj, dis = dis, i=i, j=j, k=k, l=l, hed = hed, type = type, ...)
    }
    
  } else {
    if(inherits(obj, 'rmf_list')) {
      convert <- function(l) {
        subset(obj, nstp == l) %>%
          rmf_as_array(dis = dis, sparse = FALSE, na_value = ifelse(active_only, NA, 0), variable = which(colnames(obj) == 'value'))
      } 
      obj <- lapply(attr(obj, 'nstp'), convert) %>%
        abind::abind(along = 4) %>%
        structure(dimnames = NULL, totim = attr(obj, 'totim')) %>%
        rmf_create_array() 
      
      rmf_plot(obj, dis = dis, i=i, j=j, k=k, l=l, hed = hed, type = type, ...)
      
    } else {
      if(flux == 'darcy' && type == 'vector') {
        uvw <- list(u = darcy$qx, v = darcy$qy, w = darcy$qz)
        rmf_plot(obj, dis = dis, i=i, j=j, k=k, l=l, hed = hed, type = type, uvw=uvw, ...)
        
      } else {
        rmf_plot(obj, dis = dis, i=i, j=j, k=k, l=l, hed = hed, type = type, ...)
      }
    }
  }
}

#' Plot a RMODFLOW chd object
#' 
#' @param chd an \code{RMODFLOW} chd object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable single character or numeric indicating which column of \code{chd$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to mean for variables 'shead' & 'ehead'.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot chd

rmf_plot.chd <- function(chd,
                         dis,
                         kper = NULL,
                         variable = 'id',
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         active_only = TRUE,
                         fun = mean,
                         add = FALSE,
                         ...) {
  
  rmfi_plot_bc(obj = chd, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, add = add, ...)
  
}

#' Plot a MODFLOW drawdown file object
#'
#' @param ddn \code{RMODFLOW} ddn object
#' @param dis \code{RMODFLOW} dis object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot; defaults to plotting the final time step.
#' @param kper integer specifying the stress-period. Use in conjunction with kstp. See details.
#' @param kstp integer specifying the time step of kper. Use in conjunction with kper. See details.
#' @param ... additional parameters passed to \code{\link{rmf_plot.rmf_4d_array}}
#'
#' @details There are two ways to specify which time step to plot. The \code{l} argument can be specified which represents the total time step number. 
#'  The other option is to specify both \code{kper} & \code{kstp} which specify the stress-period and corresponding time step in that stress-period.
#'  A negative \code{kstp} will plot the final time step of the stress-period.
#'  
#'  
#'  If no output is written for the specified time step, as controlled by the Output Control file in MODFLOW, an error is thrown.
#'
#' @return ggplot2 object or layer
#' @method rmf_plot ddn
#' @export
#'
rmf_plot.ddn <- function(ddn, 
                         dis,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         l = NULL,
                         kper = NULL,
                         kstp = NULL,
                         ...) {
  
  if(inherits(ddn, 'rmf_4d_array')) {
    # skip if ijk are specified and a time series should be plotted
    if(!(!is.null(i) && !is.null(j) && !is.null(k))) {
      if(is.null(l) && (is.null(kper) && is.null(kstp))) {
        if(dis$nper > 1 || dis$nstp[1] > 1) warning('Plotting final time step results.', call. = FALSE)
        l <- dim(ddn)[4]
      }
      
      if(is.null(l)) {
        if(any(is.null(kper), is.null(kstp))) stop('Please specify either l or kstp & kper.', call. = FALSE)
        l <- ifelse(kper == 1, 0, cumsum(dis$nstp)[kper-1]) + ifelse(kstp < 0, dis$nstp[kper], kstp)
      }
      
      if(!(l %in% attr(ddn, 'nstp'))) stop('No output written for specified time step.', call. = FALSE)
      
    } 
    rmf_plot.rmf_4d_array(ddn, dis = dis, i=i, j=j, k=k, l=l, ...)
    
  } else if(inherits(ddn, 'rmf_3d_array')) {
    rmf_plot.rmf_3d_array(ddn, dis = dis, i=i, j=j, k=k, ...)
  } else if(inherits(ddn, 'rmf_2d_array')) {
    rmf_plot.rmf_2d_array(ddn, dis = dis, i=i, j=j, ...)
  } else {
    stop('Array is not of class rmf_2d_array, rmf_3d_array or rmf_4d_array. Is the array subsetted ?', call. = FALSE)
  }
}

#' Plot a RMODFLOW drn object
#' 
#' @param drn an \code{RMODFLOW} drn object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable single character or numeric indicating which column of \code{drn$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to mean for variable 'elevation' and sum for variable 'cond'.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot drn

rmf_plot.drn <- function(drn,
                         dis,
                         kper = NULL,
                         variable = 'id',
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         active_only = TRUE,
                         fun = ifelse(variable == 'elevation', mean, sum),
                         add = FALSE,
                         ...) {
  
  rmfi_plot_bc(obj = drn, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, add = add, ...)
  
}


#' Plot a RMODFLOW evt object
#' 
#' @param evt an \code{RMODFLOW} evt object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable character specifying which variable to plot. Possible values are 'evt' (default), 'surf', 'exdp' and 'ievt' if defined. 
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_2d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot evt

rmf_plot.evt <- function(evt,
                         dis,
                         kper = NULL,
                         variable = 'evt',
                         ...) {
  
  if(is.null(kper)) {
    if(dis$nper > 1) warning('Setting kper to last stress-period', call. = FALSE)
    kper <- dis$nper
  }
  
  if(variable == 'evt') {
    active_arrays <- colnames(evt$kper)[-1]
    active_arrays <- active_arrays[which(evt$kper[kper,-1] == TRUE)] 
    
    obj <- evt$evt[active_arrays]
    
    # sum if multiple arrays are active
    obj <- Reduce('+', obj)
    
  } else if(variable == 'surf') {
    obj <- evt$surf[[kper]]
    
  } else if(variable == 'exdp') {
    obj <- evt$exdp[[kper]]
    
  } else if(variable == 'irch') {
    if(evt$nevtop != 2) stop('No ievt arrays defined in evt object; nevtop does not equal 2', call. = FALSE)
    obj <- evt$ievt[[kper]]
    
  } 
  
  if(length(obj) == 0) {
    warning(paste0('No active ', variable, ' arrays in stress-period ', kper, '. Returning NULL.'), call. = FALSE)
    return(NULL)
  }
  
  rmf_plot(obj, dis = dis, ...)
  
}

#' Plot a RMODFLOW ghb object
#' 
#' @param ghb an \code{RMODFLOW} ghb object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable single character or numeric indicating which column of \code{ghb$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to mean for variable 'bhead' and sum for variable 'cond'.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot ghb

rmf_plot.ghb <- function(ghb,
                         dis,
                         kper = NULL,
                         variable = 'id',
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         active_only = TRUE,
                         fun = ifelse(variable == 'bhead', mean, sum),
                         add = FALSE,
                         ...) {
  
  rmfi_plot_bc(obj = ghb, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, add = add, ...)
  
}

#' Plot a MODFLOW head file object
#'
#' @param hed \code{RMODFLOW} hed object
#' @param dis \code{RMODFLOW} dis object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot; defaults to plotting the final time step.
#' @param kper integer specifying the stress-period. Use in conjunction with kstp. See details.
#' @param kstp integer specifying the time step of kper. Use in conjunction with kper. See details.
#' @param saturated logical indicating if the saturated grid should be used. Defaults to FALSE. See details.
#' @param ... additional parameters passed to \code{\link{rmf_plot.rmf_4d_array}}
#'
#' @details There are two ways to specify which time step to plot. The \code{l} argument can be specified which represents the total time step number. 
#'  The other option is to specify both \code{kper} & \code{kstp} which specify the stress-period and corresponding time step in that stress-period.
#'  A negative \code{kstp} will plot the final time step of the stress-period.
#'  
#'  If \code{saturated} is TRUE, the saturated grid is plotted as given by \code{\link{rmf_convert_dis_to_saturated_dis}}, which might be useful 
#'  for cross-sections with \code{grid = TRUE}.
#'  
#'  If no output is written for the specified time step, as controlled by the Output Control file in MODFLOW, an error is thrown.
#'
#' @return ggplot2 object or layer
#' @method rmf_plot hed
#' @export
#'
rmf_plot.hed <- function(hed, 
                         dis,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         l = NULL,
                         kper = NULL,
                         kstp = NULL,
                         saturated = FALSE,
                         type = 'fill',
                         gridlines = FALSE,
                         add = FALSE,
                         ...) {
  
  if(inherits(hed, 'rmf_4d_array')) {
    # skip if ijk are specified and a time series should be plotted
    if(!(!is.null(i) && !is.null(j) && !is.null(k))) {
      if(is.null(l) && (is.null(kper) && is.null(kstp))) {
        if(dim(hed)[4] > 1) warning('Plotting final time step results.', call. = FALSE)
        l <- dim(hed)[4]
      }
      
      if(is.null(l)) {
        if(any(is.null(kper), is.null(kstp))) stop('Please specify either l or kstp & kper.', call. = FALSE)
        l <- ifelse(kper == 1, 0, cumsum(dis$nstp)[kper-1]) + ifelse(kstp < 0, dis$nstp[kper], kstp)
      }
      
      if(!(l %in% attr(hed, 'nstp'))) stop('No output written for specified time step.', call. = FALSE)
      
      if(saturated) {  
        satdis <- rmf_convert_dis_to_saturated_dis(dis = dis, hed = hed, l = l)
        p <- rmf_plot(hed[,,,l], dis=satdis, i=i,j=j,k=k, gridlines = FALSE, type = type, add = add, ...)
        if(isTRUE(gridlines) || is.character(gridlines)) {
          p <- p + rmf_plot(hed[,,,l], dis = dis, i=i, j=j, k=k, type = 'grid', gridlines = gridlines, add = TRUE, ...)
        }
        return(p)
      } else {
        rmf_plot.rmf_4d_array(hed, dis = dis, i=i, j=j, k=k, l=l, gridlines = gridlines, type = type, add = add, ...)
      }
    } else {
      rmf_plot.rmf_4d_array(hed, dis = dis, i=i, j=j, k=k, l=l, gridlines = gridlines, type = type, add = add, ...)
    }
  } else if(inherits(hed, 'rmf_3d_array')) {
    rmf_plot.rmf_3d_array(hed, dis = dis, i=i, j=j, k=k, gridlines = gridlines, type = type, add = add, ...)
  } else if(inherits(hed, 'rmf_2d_array')) {
    rmf_plot.rmf_2d_array(hed, dis = dis, i=i, j=j, gridlines = gridlines, type = type, add = add, ...)
  } else {
    stop('Array is not of class rmf_2d_array, rmf_3d_array or rmf_4d_array. Is the array subsetted ?', call. = FALSE)
  }
}

#' Plot a RMODFLOW hfb object
#' 
#' @param hfb an \code{RMODFLOW} hfb object
#' @param dis a \code{RMODFLOW} dis object
#' @param variable single character or numeric indicating which column of \code{hfb$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param all_hfb logical; should all horizontal-flow barriers be plotted (TRUE) or only the ones that are active in the simulation (FALSE; default)
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum for variable 'hydchr'
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer
#' @export
#' @method rmf_plot hfb

rmf_plot.hfb <- function(hfb,
                         dis,
                         variable = 'id',
                         all_hfb = FALSE,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         type = 'fill',
                         active_only = TRUE,
                         fun = ifelse(variable == 'hydchr', sum, mean),
                         prj = NULL,
                         crs = NULL,
                         size = 1,
                         colour = 'black',
                         crop = FALSE,
                         add = FALSE,
                         ...) {
  
  if(is.null(i) & is.null(j) & is.null(k)) {
    stop('Please provide i, j or k.', call. = FALSE)
  }
  
  na_value <- ifelse(active_only, NA, 0)
  
  if(!all_hfb) hfb$data <- subset(hfb$data, active == TRUE)
  if(nrow(hfb$data) == 0) {
    if(add) {
      warning('No horizontal-flow barriers active. Returning NULL.', call. = FALSE)
      return(NULL)
    } else {
      stop('No horizontal-flow barriers active.', call. = FALSE)
    }
  }  
  
  if(!is.null(k)) {
    layer <- k
    data <- subset(hfb$data, k == layer)
    if(nrow(data) == 0) {
      if(add) {
        warning(paste0('No horizontal-flow barriers in layer ', k, '. Returning NULL.'), call. = FALSE)
        return(NULL)
      } else {
        stop(paste0('No horizontal-flow barriers in layer ', k, '.'), call. = FALSE)
      }
    }
    if(!is.character(variable)) variable <- colnames(data)[variable]
    data <- subset(data, select = c('i', 'j', 'k', 'irow2', 'icol2', if(variable != 'id') {variable}))
    data$id <- rmf_convert_ijk_to_id(i=data$i, j=data$j, k=1, dis = dis, type = 'modflow')
    
    # coordinate tibble
    
    get_face <- function(data) {
      i <- data$i
      j <- data$j
      irow2 <- data$irow2
      icol2 <- data$icol2
      df <- as.data.frame(df[which(as.character(df$id) == data$id),])
      
      if(i < irow2) {
        # back
        return(df[c(1,4),])
        
      } else if(i > irow2) {
        # front
        return(df[c(2,3),]) 
        
      } else if(j < icol2) {
        # right
        return(df[c(3,4),]) 
        
      } else if(j > icol2) {
        # left
        return(df[c(1,2),])
        
      }
    }
    
    df_mask <- rmf_as_array(data, dis = dis, sparse = TRUE, na_value = 0)
    df_mask[which(df_mask != 0)] <- 1
    
    df <- data %>%
      rmf_as_array(dis = dis, select = ifelse(variable == 'id', 1, which(colnames(data) == variable)), 
                   sparse = TRUE, na_value = na_value, fun = fun) %>%
      rmf_as_tibble(dis = dis, prj = prj, crs = crs, mask = df_mask)
    
    df <- lapply(1:nrow(data), function(i) get_face(data[i,]))
    df <- do.call(rbind, df)
    df$row <- rep(1:(nrow(df)/2), each = 2)
    
    # plot
    if(variable == 'id') {
      p <-  ggplot2::geom_path(data = df, ggplot2::aes(x=x, y=y, group = row), size = size, colour = colour)
    } else {
      p <-  ggplot2::geom_path(data = df, ggplot2::aes(x=x, y=y, group = row, colour = value), size = size)
    }
    
    if(add) {
      return(p)
    } else {
      if(!crop) {
        corners <- data.frame(x = rep(c(0, sum(dis$delr)), 2), y = rep(c(0, sum(dis$delc)), each = 2))
        xy <- rmf_convert_grid_to_xyz(x=corners$x,y=corners$y, dis = dis, prj=prj)
        p <- ggplot2::ggplot() + p + ggplot2::lims(x = c(min(xy$x), max(xy$x)), y = c(min(xy$y), max(xy$y)))
        return(p)
      } else {
        return(ggplot2::ggplot() + p)
      }
    }
    
  } else {
    if(!is.null(i)) {
      ind <- i
      data <- subset(hfb$data, i == ind)
      if(nrow(data) == 0) {
        if(add) {
          warning(paste0('No horizontal-flow barriers in row ', i, '. Returning NULL.'), call. = FALSE)
          return(NULL)
        } else {
          stop(paste0('No horizontal-flow barriers in row ', i, '.'), call. = FALSE)
        }
      }
    } else if(!is.null(j)) {
      ind <- j
      data <- subset(hfb$data, j == ind)
      if(nrow(data) == 0) {
        if(add) {
          warning(paste0('No horizontal-flow barriers in column ', j, '. Returning NULL.'), call. = FALSE)
          return(NULL)
        } else {
          stop(paste0('No horizontal-flow barriers in column ', j, '.'), call. = FALSE)
        }
      }
    }
    
    if(!is.character(variable)) variable <- colnames(data)[variable]
    data <- subset(data, select = c('i', 'j', 'k', 'irow2', 'icol2', if(variable != 'id') {variable}))
    
    df <- rmf_as_array(data, dis = dis, 
                       select = ifelse(variable == 'id', 1, which(colnames(data) == variable)), 
                       sparse = FALSE, na_value = na_value, fun = fun)
    if(variable == 'id') {
      indx <- rmfi_ifelse0(is.na(na_value), which(!is.na(df)), which(df != na_value))
      df[indx] <- 1
    }
    
    # plot
    rmf_plot(df, dis = dis, i=i, j=j, k=k, type = type, prj=prj, crs=crs, crop=crop, add=add, ...)
    
  }
}

#' Plot a RMODFLOW hob object
#'
#' @param hob \code{RMODFLOW} hob object
#' @param dis \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param exact logical specifying if the exact locations are to be plotted or the cells containing the observations (default)
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param geom either 'polygon' (default), 'line' or 'point'. Defines how the rmf_list features are plotted using \code{\link{rmf_plot.rmf_list}}
#' @param prj projection file object; defaults to NULL
#' @param crs coordinate reference system for the plot; defaults to NULL
#' @param gridlines logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param crop logical; should plot be cropped to the domain represented by the features; defaults to FALSE
#' @param add logical; if TRUE, provide ggplot2 layers instead of object; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_list}} or \code{ggplot2::geom_point} when \code{exact = TRUE}
#' 
#' @details specifying all of the \code{i, j & k} arguments will plot a time series at that cell location. The observations are grouped by name.
#' 
#' @return ggplot2 object or layer
#' @export
#' @method rmf_plot hob
#' 
rmf_plot.hob <- function(hob,
                         dis,
                         kper = NULL,
                         exact = FALSE,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         geom = 'polygon',
                         prj = NULL,
                         crs = NULL,
                         gridlines = FALSE,
                         crop = FALSE,
                         add = FALSE,
                         ...) {
  
  if(is.null(i) && is.null(j) && is.null(k)) stop('Please provide i, j or k.', call. = FALSE)
  
  if(!is.null(kper)) hob$data <- subset(hob$data, hob$data$irefsp == kper)
  
  locations <- rmf_convert_hob_to_locations(hob, dis, prj = prj)
  locations$hobs <- hob$data$hobs
  
  # plot time series
  if(!is.null(i) && !is.null(j) && !is.null(k)) {
    ts <- rmf_convert_hob_to_time_series(hob, dis)
    locations <- locations[which(locations$i == i & locations$j == j & locations$k == k),]
    ts <- ts[which(ts$name %in% locations$name),]
    mp <- any(table(ts$name) > 1)
    
    return(ggplot2::ggplot(ts, ggplot2::aes(x = time, y = head, group = name)) +
             rmfi_ifelse0(mp, ggplot2::geom_path(ggplot2::aes(colour = name)), ggplot2::geom_point(ggplot2::aes(colour = name))))
  } else {
    df <- rmf_create_list(locations)
    
    if(exact) {
      if(nrow(df) == 0) {
        if(add) {
          warning('No observations present in hob object. Returning NULL.', call. = FALSE)
          return(NULL)
        } else {
          stop('No observations present in hob object.', call. = FALSE)
        }
      }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        #warning('Transforming vertical coordinates', call. = FALSE)
        positions <- rmfi_convert_coordinates(df, from = sf::st_crs(prj$projection), to = sf::st_crs(crs))
        df$x <- positions$x
        df$y <- positions$x
      }
      
      if(!is.null(i)) {
        if(length(which(df$i == i)) == 0) {
          if(add) {
            warning(paste0('No observations in row ', i, '. Returning NULL.'), call. = FALSE)
            return(NULL)
          } else {
            stop(paste0('No observations in row ', i, '.'), call. = FALSE)
          }
        }
        df <- df[which(df$i == i),]
        colnames(df) <- replace(colnames(df), match(c('y', 'z'), colnames(df)), c('z', 'y'))
        lbl <- c('x', 'z')
        xlim <- rmf_convert_grid_to_xyz(x = c(0, sum(dis$delr)), y = rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)[i], dis = dis, prj = prj)
        ylim <- range(c(dis$top[i,], dis$botm[i,,])) + ifelse(is.null(prj), 0, ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          xlim <- rmfi_convert_coordinates(xlim, from = prj$projection, to = crs)
        }
        xlim <- xlim$x
        
      } else if(!is.null(j)) {
        if(length(which(df$j == j)) == 0) {
          if(add) {
            warning(paste0('No observations in column ', j, '. Returning NULL.'), call. = FALSE)
            return(NULL)
          } else {
            stop(paste0('No observations in column ', j, '.'), call. = FALSE)
          }
        }
        df <- df[which(df$j == j),]
        colnames(df) <- replace(colnames(df), match(c('x', 'y', 'z'), colnames(df)), c('z', 'x', 'y'))
        lbl <- c('y', 'z')
        xlim <- rmf_convert_grid_to_xyz(x = (cumsum(dis$delr) - dis$delr/2)[j], y = c(0, sum(dis$delc)), dis = dis, prj = prj)
        ylim <- range(c(dis$top[,j], dis$botm[,j,])) + ifelse(is.null(prj), 0, ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          xlim <- rmfi_convert_coordinates(xlim, from = prj$projection, to = crs)
        }
        xlim <- xlim$y
        
      } else if(!is.null(k)) {
        if(length(which(df$k == k)) == 0) {
          if(add) {
            warning(paste0('No observations in layer ', k, '. Returning NULL.'), call. = FALSE)
            return(NULL)
          } else {
            stop(paste0('No observations in layer ', k, '.'), call. = FALSE)
          }
        }
        df <- df[which(df$k == k),]
        lbl <- c('x', 'y')
        xlim <- rmf_convert_grid_to_xyz(x = c(0, sum(dis$delr)), y = 0, dis = dis, prj = prj)
        ylim <- rmf_convert_grid_to_xyz(x = 0, y = c(0, sum(dis$delc)), dis = dis, prj = prj)
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          xlim <- rmfi_convert_coordinates(xlim, from = prj$projection, to = crs)
          ylim <- rmfi_convert_coordinates(ylim, from = prj$projection, to = crs)
        }
        xlim <- xlim$x
        ylim <- ylim$y
      }
      
      lims <- rmfi_ifelse0(crop, NULL, ggplot2::lims(x = xlim, y = ylim))
      
      # grid
      if(gridlines || is.character(gridlines)) {
        p_grid <- rmf_plot(hob, dis = dis, kper = NULL, i = i, j = j, k = k, type = 'grid', gridlines = gridlines, add = TRUE, prj = prj, crs = crs, exact = FALSE, active_only = crop)
      } else {
        p_grid <- NULL
      }
      
      if(add) {
        return(list(p_grid, ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y), ...)))
      } else {
        return(ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
                 p_grid +
                 ggplot2::geom_point(...) +
                 ggplot2::xlab(lbl[1]) +
                 ggplot2::ylab(lbl[2]) +
                 lims)
      }
    } else {
      # rmf_plot.rmf_list
      df <- df[, -which(colnames(df) %in% c('x', 'y', 'z'))]
      rmf_plot(df, dis = dis, i = i, j = j, k = k, geom = geom, prj = prj, crs = crs, gridlines = gridlines, crop = crop, add = add, ...)
    }
  }
}

#' Plot a MODFLOW head predictions file
#' 
#' @param hpr head predictions file object
#' @param type plot type: 'scatter', 'residual' or 'histogram'
#' @param hobdry value used to flag dry cells; defaults to -888
#' @param bins number of bins to use in the histrogram plot; defaults to the Freedman-Diaconis rule
#' @method rmf_plot hpr
#' @export
rmf_plot.hpr <- function(hpr,type='scatter',hobdry = -888, bins = NULL) {
  dat <- data.frame(simulated=hpr$simulated, observed=hpr$observed,name=hpr$name)[which(hpr$simulated!=hobdry),]
  if(type=='scatter') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=observed,y=simulated))+
               ggplot2::geom_point(ggplot2::aes(colour=abs(observed-simulated)))+
               ggplot2::geom_abline(ggplot2::aes(intercept=0,slope=1),linetype='dashed')+
               ggplot2::scale_colour_gradientn('Misfit',colours=rmfi_rev_rainbow(7),trans='log10')+
               ggplot2::xlab('Observed value')+
               ggplot2::ylab('Simulated equivalent')
    )
  } else if(type=='residual') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=name,y=simulated-observed))+
               ggplot2::geom_bar(ggplot2::aes(fill=abs(observed-simulated)),stat='identity')+
               ggplot2::scale_fill_gradientn('Misfit',colours=rmfi_rev_rainbow(7),trans='log10')+
               ggplot2::xlab('Observation name')+
               ggplot2::ylab('Simulated equivalent - observed value')
    )
  } else if(type=='histogram') {
    if(is.null(bins)) bins <- nclass.FD(dat$simulated-dat$observed)
    
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=simulated-observed))+
               ggplot2::geom_histogram(ggplot2::aes(fill=..count..), bins=bins)+
               ggplot2::scale_fill_gradientn('Count',colours=rmfi_rev_rainbow(7))+
               ggplot2::xlab('Simulated equivalent - observed value')
    )
  }
}

#' Plot a 2D section through a MODFLOW 3D array
#' 
#' \code{rmf_plot.huf} plots a 2D section through a MODFLOW 3D array.
#' 
#' @param huf an object of class huf
#' @param dis discretization file object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param hgu character or integer of hgu to plot
#' @param bas basic file object; optional
#' @param mask a 3D array with 0 or F indicating inactive cells optional; 
#' @param colour_palette a colour palette for imaging continuous array values. 
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param levels labels that should be used on the factor legend; huf$hgunam is used by default
#' @param type plot type: 'fill', 'factor' (default) or 'grid'
#' @param gridlines logical; should grid lines be plotted? Alternatively, provide colour of the grid lines or set to 'huf' which plots the outline of the hgu's
#' @param ... parameters provided to plot.rmf_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot huf
#' @export
rmf_plot.huf <- function(huf,
                         dis,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         hgu = NULL,
                         bas = NULL,
                         mask = rmf_convert_huf_to_mask(huf = huf, dis = dis, bas = bas),
                         colour_palette = rmfi_rev_rainbow,
                         nlevels = 7,
                         levels = setNames(huf$hgunam, 1:huf$nhuf),
                         type='factor',
                         gridlines = FALSE,
                         ...) {
  if(is.null(i) && is.null(j) && is.null(k) && is.null(hgu)) stop('Please provide i, j, k or hgu.', call. = FALSE)
  
  hufdis <- rmf_convert_huf_to_dis(huf = huf, dis = dis)
  
  if(!is.null(hgu) && is.character(hgu)) hgu <- which(huf$hgunam == hgu)
  
  if(!is.null(k)) {
    huf_array <- rmf_create_array(dim=c(dis$nrow,dis$ncol))
    if(k == 1) {
      for(i in rev(1:dim(hufdis$botm)[3])) {
        huf_array[which(mask[,,i] == 1)] <- i
      }
    } else {
      for(i in rev(1:dim(hufdis$botm)[3])) {
        huf_array[which(dis$botm[,,k-1] >= hufdis$botm[,,i])] <- i
      }
    }
    mask <- rmfi_ifelse0(is.null(bas),huf_array*0+1,rmfi_ifelse0(bas$xsection, aperm(bas$ibound, c(3,2,1))[,,k], bas$ibound[,,k]))
    p <- rmf_plot(huf_array, dis = hufdis, mask=mask,colour_palette=colour_palette,nlevels=nlevels,type=type,levels=levels, gridlines = FALSE,...)
    
  } else {
    huf_array <- rmf_create_array(rep(1:huf$nhuf,each=dis$nrow*dis$ncol),dim=c(dis$nrow,dis$ncol,huf$nhuf))
    if(!is.null(hgu) && (!is.null(i) || !is.null(j))) {
      huf_array[which(huf_array != hgu)] <- NA
      hgu <- NULL
    }
    p <- rmf_plot(huf_array, dis = hufdis, mask=mask, i=i,j=j,k=hgu,colour_palette=colour_palette,nlevels=nlevels,type=type,levels=levels, gridlines = FALSE,...)
    
  }
  
  if(gridlines != FALSE) {
    if(gridlines == 'huf') {
      if(!is.null(k)) {
        return(p + rmf_plot(dis$botm, dis = dis, i=i,j=j,k=k,bas=bas,type='grid',add=TRUE))
      } else {
        return(p + rmf_plot(huf_array, mask=mask, dis = hufdis, i=i,j=j,k=hgu,type='grid',add=TRUE))
      }
    } else {
      return(p + rmf_plot(dis$botm, dis = dis, i=i,j=j,k=k,bas=bas,type='grid',gridlines = ifelse(is.character(gridlines), gridlines, 'black'),add=TRUE))
    }
  } else {
    return(p)
  }
}

#' Plot a RMODFLOW rch object
#' 
#' @param rch an \code{RMODFLOW} rch object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable character specifying which variable to plot. Possible values are 'recharge' (default) and 'irch' if defined. 
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_2d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot rch

rmf_plot.rch <- function(rch,
                         dis,
                         kper = NULL,
                         variable = 'recharge',
                         ...) {
  
  if(is.null(kper)) {
    if(dis$nper > 1) warning('Setting kper to last stress-period', call. = FALSE)
    kper <- dis$nper
  }
  
  if(variable == 'recharge') {
    active_arrays <- colnames(rch$kper)[-1]
    active_arrays <- active_arrays[which(rch$kper[kper,-1] == TRUE)] 
    
    obj <- rch$recharge[active_arrays]
    
    # sum if multiple arrays are active
    obj <- Reduce('+', obj)
    
  } else if(variable == 'irch') {
    if(rch$nrchop != 2) stop('No irch arrays defined in rch object; nrchop does not equal 2', call. = FALSE)
    obj <- rch$irch[[kper]]
    
  } 
  
  if(length(obj) == 0) {
    warning(paste0('No active ', variable, ' arrays in stress-period ', kper, '. Returning NULL.'), call. = FALSE)
    return(NULL)
  }
  
  rmf_plot(obj, dis = dis, ...)
  
}

#' Plot a RMODFLOW riv object
#' 
#' @param riv an \code{RMODFLOW} riv object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable single character or numeric indicating which column of \code{riv$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to mean for variables 'stage' & 'rbot' and sum for variable 'cond'.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot riv

rmf_plot.riv <- function(riv,
                         dis,
                         kper = NULL,
                         variable = 'id',
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         active_only = TRUE,
                         fun = ifelse(variable %in% c('stage', 'rbot'), mean, sum),
                         add = FALSE,
                         ...) {
  
  rmfi_plot_bc(obj = riv, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, add = add, ...)
  
}


#' Plot a MODFLOW 2D array
#' 
#' \code{rmf_plot.rmf_2d_array} plots a MODFLOW 2D array.
#' 
#' @param array an object of class rmf_2d_array
#' @param dis discretization file object
#' @param bas basic file object; optional
#' @param mask a 2D array with 0 or F indicating inactive cells; optional; defaults to having all cells active or, if bas is provided, the first layer of bas$ibound
#' @param colour_palette a colour palette for imaging continuous array values. If type = 'contour' or 'vector', a single character can also be used. 
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'factor', 'grid', 'contour' or 'vector'
#' @param levels (named) character vector with labels for the factor legend. If not named, factor levels are sorted before being labelled. If NULL, the array factor levels are used
#' @param gridlines logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param height_exaggeration height exaggeration for 3D plot; optional
#' @param binwidth binwidth for contour plot; defaults to 1/20 of zlim
#' @param label logical; should labels be added to contour plot
#' @param prj projection file object
#' @param crs coordinate reference system for the plot
#' @param alpha transparency value; defaults to 1
#' @param plot3d logical; should a 3D plot be made
#' @param height 2D array for specifying the 3D plot z coordinate
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask); defaults to FALSE
#' @param vecsize vector sizing if \code{type = 'vector'}. See \code{\link{ggquiver::geom_quiver}}. Defaults to NULL which automatically determines vector sizing.
#' @param uvw optional named list with u and v vectors or 2d arrays specifying the vector components in the x and y direction for every node if type = 'vector'. By default, these components are computed by \code{\link{rmf_gradient}}
#' @param legend either a logical indicating if the legend is shown or a character indicating the legend title
#' @param ... ignored
#' @details type = 'vector' assumes the array contains scalars and will calculate the gradient using \code{\link{rmf_gradient}} unless uvw is specified.
#'          For types 'fill' and 'factor', the fill aesthetic is used. For types 'contour' and 'vector', the colour aesthetic is used.
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_2d_array
#' @export
rmf_plot.rmf_2d_array <- function(array,
                                  dis,
                                  bas = NULL,
                                  mask = rmfi_ifelse0(is.null(bas), array*0+1, {if(dis$nlay > 1) warning('Using first ibound layer as mask.', call. = FALSE); rmfi_ifelse0(bas$xsection, aperm(bas$ibound, c(3,2,1))[,,1], bas$ibound[,,1])}),
                                  colour_palette = ifelse(type %in% c('contour', 'vector'), 'black', rmfi_rev_rainbow),
                                  zlim = range(array[as.logical(mask)], finite=TRUE),
                                  nlevels = 7,
                                  type = 'fill',
                                  levels = NULL,
                                  gridlines = FALSE,
                                  add = FALSE,
                                  height_exaggeration = 100,
                                  binwidth=pretty(diff(zlim)/20, 1)[1],
                                  label=TRUE,
                                  prj=NULL,
                                  crs=NULL,
                                  alpha=1,
                                  plot3d=FALSE,
                                  height=NULL,
                                  crop = FALSE,
                                  vecsize = NULL,
                                  uvw = NULL,
                                  legend = ifelse(type %in% c('fill', 'factor'), !add, FALSE),
                                  ...) {
  
  if(plot3d) {
    xyz <- rmf_cell_coordinates(dis)
    x <- xyz$x[,,1]
    y <- xyz$y[,,1]
    if(!is.null(prj)) {
      xyz <- rmf_convert_grid_to_xyz(x=c(x),y=c(y),prj=prj)
      x[,] <- xyz$x
      y[,] <- xyz$y
    }
    z <- t(height)*height_exaggeration
    if(!add) rgl::open3d()
    colorlut <- colorRampPalette(colour_palette(nlevels))(25) # height color lookup table
    col <- colorlut[ round(approx(seq(zlim[1],zlim[2],length=25+1),seq(0.5,25+0.5,length=25+1),xout=c(t(array)),rule=2)$y) ] # assign colors to heights for each point
    alpha <- rep(1,length(col))
    alpha[which(c(t(mask))==0)] <- 0
    if(type=='fill') rgl::surface3d(t(x),t(y),z,color=col,alpha=alpha,back='lines',smooth=FALSE) 
    if(type=='grid') rgl::surface3d(t(x),t(y),z,front='lines',alpha=alpha,back='lines',smooth=FALSE) 
  } else {
    
    # if array is already a cross-section, e.g. rmf_plot(array[,1,], dis = dis)
    # TODO: can not know what index was subsetted so assumes a subset on index 1; 
    # might remove later
    if(!all(attr(array, 'dimlabels') == c("i", "j"))) {
      stop('Array needs to represent dimensions i & j. Is the array transposed or subsetted ?', call. = FALSE)
      # if(attr(array, 'dimlabels')[1] == 'k') array <- t(array)
      # if("j" %in% attr(array, 'dimlabels')) {
      #   sub_array <- rmf_create_array(array, dim = c(1, dim(array)))
      #   if(!isTRUE(all.equal(attr(mask, 'dimlabels'), attr(array, 'dimlabels')))) {
      #     warning("Dimensions of mask do not match those of array. Skipping mask.", call. = FALSE)
      #     mask <- array*0 + 1
      #   }
      #   sub_mask <- rmf_create_array(mask, dim = c(1, dim(mask)))
      #   
      #   p <- rmf_plot(sub_array, dis = dis, i = 1, bas = bas, mask = sub_mask, zlim = zlim, colour_palette = colour_palette, nlevels = nlevels,
      #                 type = type, levels = levels, gridlines = gridlines, add = add, crop = crop, prj = prj, crs = crs,
      #                 height_exaggeration = height_exaggeration, binwidth = binwidth, label = label, alpha = alpha, plot3d = plot3d, height = height, 
      #                 vecsize = vecsize, uvw=uvw,legend=legend)
      #   return(p)
      #   
      # } else if("i" %in% attr(array, 'dimlabels')) {
      #   sub_array <- rmf_create_array(array, dim = c(dim(array)[1], 1, dim(array)[2]))
      #   if(!isTRUE(all.equal(attr(mask, 'dimlabels'), attr(array, 'dimlabels')))) {
      #     warning("Dimensions of mask do not match those of array. Skipping mask.", call. = FALSE)
      #     mask <- array*0 + 1
      #   }
      #   sub_mask <- rmf_create_array(mask, dim = c(dim(mask)[1], 1, dim(mask)[2]))
      #   
      #   p <- rmf_plot(sub_array, dis = dis, j = 1, bas = bas, mask = sub_mask, zlim = zlim, colour_palette = colour_palette, nlevels = nlevels,
      #                 type = type, levels = levels, gridlines = gridlines, add = add, crop = crop, prj = prj, crs = crs,
      #                 height_exaggeration = height_exaggeration, binwidth = binwidth, label = label, alpha = alpha, plot3d = plot3d, height = height,
      #                 vecsize = vecsize, uvw=uvw,legend=legend)
      #   return(p)
      # }
    }
    
    # datapoly
    xy <- expand.grid(cumsum(dis$delr)-dis$delr/2,sum(dis$delc)-(cumsum(dis$delc)-dis$delc/2))
    names(xy) <- c('x','y')
    mask[which(mask==0)] <- NA
    
    datapoly <- rmf_as_tibble(array, dis = dis, mask = mask, prj = prj, crs = crs, as_points = FALSE)
    datapoly$id <- factor(datapoly$id)
    if(crop) datapoly <- na.omit(datapoly)
    
    # legend
    if(is.logical(legend)) {
      name <- "value"
    } else {
      name <- legend
      legend <- TRUE
    }
    
    # plot
    if(type=='fill') {  
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly,show.legend=legend,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_gradientn(name, colours=colour_palette(nlevels),limits=zlim, na.value = NA))) 
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id),show.legend=legend, alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_gradientn(name, colours=colour_palette(nlevels),limits=zlim,  na.value = NA) +
                 ggplot2::coord_equal())
      }
    } else if(type=='factor') {  
      labels <- rmfi_ifelse0(is.null(names(levels)), levels, levels[as.character(sort(na.omit(unique(datapoly$value))))])
      datapoly$value <- rmfi_ifelse0(is.null(levels), factor(datapoly$value), factor(datapoly$value, labels = labels))
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly,show.legend=legend,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_discrete(name, breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA)))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id),show.legend=legend,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_discrete(name, breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA) +
                 ggplot2::coord_equal())
      }
    } else if(type=='grid') {  
      if(add) {
        return(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,group=id),data=datapoly,alpha=alpha,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(group=id),alpha=alpha,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA) +
                 ggplot2::coord_equal())
      }
    } else if(type=='contour') {
      if(!is.null(prj)) {
        new_xy <- rmf_convert_grid_to_xyz(x=xy$x,y=xy$y,prj=prj)
        xy$x <- new_xy$x
        xy$y <- new_xy$y
      }
      if(!is.null(crs)) {
        xy <- rmfi_convert_coordinates(xy,from=sf::st_crs(prj$projection),to=sf::st_crs(crs))
      }
      xy$z <- c(t(array*mask^2))
      xyBackup <- xy
      xy <- na.omit(as.data.frame(xy))
      
      # interpolate if grid is irregular for stat_contour
      # skip interpolation if grid is regular
      cnst_delr <- isTRUE(do.call(all.equal, as.list(range(dis$delr) / mean(dis$delr))))
      cnst_delc <- isTRUE(do.call(all.equal, as.list(range(dis$delc) / mean(dis$delc))))
      
      if(!(cnst_delr && cnst_delc)) {
        # TODO replace akima with interp (?)
        lx <- min(ceiling(sum(dis$delr)/min(dis$delr)), dis$ncol * 10)
        ly <- min(ceiling(sum(dis$delc)/min(dis$delc)), dis$nrow * 10)
        xy <- akima::interp(xy$x, xy$y, xy$z, xo = seq(min(xy$x), max(xy$x), length.out = lx), yo =  seq(min(xy$y), sum(max(xy$y)), length.out = ly))
        xy$x <- rep(xy$x, ly)
        xy$y <- rep(xy$y, each = lx)
        xy$z <- c(xy$z)
        xy <- as.data.frame(xy)
        xy <- xy[which(xy$z >= zlim[1] & xy$z <= zlim[2]),]
        closestGridPoints <- apply(xy[,c('x','y')],1,function(x) which.min((x[1]-xyBackup$x)^2 + (x[2]-xyBackup$y)^2))
        xy$z[which(is.na(xyBackup$z[closestGridPoints]))] <- NA
      }
      if(crop) {
        xlim <- c(min(xy$x, na.rm = TRUE), max(xy$x, na.rm = TRUE))
        ylim <- c(min(xy$y, na.rm = TRUE), max(xy$y, na.rm = TRUE))
      } else {
        xlim <- c(min(xyBackup$x, na.rm = TRUE), max(xyBackup$x, na.rm = TRUE))
        ylim <- c(min(xyBackup$y, na.rm = TRUE), max(xyBackup$y, na.rm = TRUE))
      }
      rm(xyBackup)
      if(add) {
        if(label) return(list(ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA),
                              ggplot2::stat_contour(ggplot2::aes(x=x,y=y,z=z,colour = ..level..),data=xy,show.legend=legend,binwidth=binwidth),directlabels::geom_dl(ggplot2::aes(x=x, y=y, z=z, label=..level.., colour=..level..),data=xy,method="top.pieces", stat="contour"),
                              ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA)))
        if(!label) return(list(ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA),
                               ggplot2::stat_contour(ggplot2::aes(x=x,y=y,z=z,colour = ..level..),data=xy,show.legend=legend,binwidth=binwidth),
                               ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA)))
      } else {
        if(label) {
          return(ggplot2::ggplot(xy, ggplot2::aes(x=x, y=y)) +
                   ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA) +
                   ggplot2::stat_contour(ggplot2::aes(z=z, colour = ..level..),show.legend=legend,binwidth=binwidth) +
                   ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA) +
                   directlabels::geom_dl(ggplot2::aes(z=z, label=..level.., colour=..level..),method="top.pieces", stat="contour") +
                   ggplot2::coord_equal(xlim = xlim, ylim = ylim))
        } else {
          return(ggplot2::ggplot(xy, ggplot2::aes(x=x, y=y)) +
                   ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA) +
                   ggplot2::stat_contour(ggplot2::aes(z=z,colour = ..level..),show.legend=legend,binwidth=binwidth) +
                   ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA) +
                   ggplot2::coord_equal(xlim = xlim, ylim = ylim))
        }
      }
    } else if(type == 'vector') {
      
      # x & y are center of cells
      vector_df <- xy
      if(!is.null(prj)) {
        new_positions <- rmf_convert_grid_to_xyz(x=vector_df$x,y=vector_df$y,prj=prj)
        vector_df$x <- new_positions$x
        vector_df$y <- new_positions$y
      }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        vector_df <- rmfi_convert_coordinates(vector_df,from=sf::st_crs(prj$projection),to=sf::st_crs(crs))
      }
      if(crop) vector_df <- na.omit(vector_df)
      # add gradient values; negative because want to show arrow from high to low
      if(is.null(uvw)) {
        grad <- rmf_gradient(array, dis = dis, mask = mask) 
      } else {
        grad <- list(x = rmf_create_array(uvw$u, dim = c(dis$nrow, dis$ncol)),
                     y = rmf_create_array(uvw$v, dim = c(dis$nrow, dis$ncol)))
      }
      vector_df$u <- -c(t(grad$x*mask^2))
      vector_df$v <- -c(t(grad$y*mask^2))

      if(add) {
        return(list(ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA),
                    ggquiver::geom_quiver(data = vector_df, ggplot2::aes(x=x, y=y, u=u, v=v, colour = sqrt(u^2 + v^2)),show.legend=legend, center = TRUE, vecsize=vecsize),
                    ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),  na.value = NA))) 
      } else {
        return(ggplot2::ggplot() +
                 ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA) +
                 ggquiver::geom_quiver(data=vector_df, ggplot2::aes(x=x, y=y, u=u, v=v, colour = sqrt(u^2 + v^2)),show.legend=legend, center = TRUE, vecsize = vecsize) +
                 ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),  na.value = NA) +
                 ggplot2::coord_equal())
      }
      
    } else {
      stop('Please provide valid plot type.', call. = FALSE)
    }
  }
}

#' Plot a 2D section through a MODFLOW 3D array
#' 
#' \code{rmf_plot.rmf_3d_array} plots a 2D section through a MODFLOW 3D array.
#' 
#' @param array an object of class rmf_3d_array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param dis discretization file object
#' @param bas basic file object; optional
#' @param mask a 3D array with 0 or F indicating inactive cells optional; defaults to having all cells active or, if bas is provided, bas$ibound
#' @param colour_palette a colour palette for imaging continuous array values. If type = 'contour' or 'vector', a single character can also be used. 
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'factor', 'grid', 'contour', or 'vector'
#' @param levels (named) character vector with labels for the factor legend. If not named, factor values are sorted before being labelled. If NULL, the array factor levels are used
#' @param gridlines logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask); defaults to FALSE
#' @param hed hed object for only plotting the saturated part of the grid; possibly subsetted with time step number; by default, last time step is used
#' @param l time step number for subsetting the hed object
#' @param binwidth binwidth for contour plot; defaults to 1/20 of zlim
#' @param label logical; should labels be added to contour plot
#' @param prj projection file object
#' @param crs coordinate reference system for the plot
#' @param vecsize vector sizing if \code{type = 'vector'}. See \code{\link{ggquiver::geom_quiver}}. Defaults to NULL which automatically determines vector sizing.
#' @param uvw optional named list with u, v and w vectors or 3d arrays specifying the vector components in the x, y and z direction for every node if type = 'vector'. By default, these components are computed by \code{\link{rmf_gradient}}
#' @param legend either a logical indicating if the legend is shown or a character indicating the legend title
#' @param ... parameters provided to plot.rmf_2d_array
#' @details type = 'vector' assumes the array contains scalars and will calculate the gradient using \code{\link{rmf_gradient}} unless uvw is specified.
#'          For types 'fill' and 'factor', the fill aesthetic is used. For types 'contour' and 'vector', the colour aesthetic is used.
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_3d_array
#' @export
rmf_plot.rmf_3d_array <- function(array,
                                  dis,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  bas = NULL,
                                  mask = rmfi_ifelse0(is.null(bas),array*0+1,rmfi_ifelse0(bas$xsection, aperm(bas$ibound, c(3,2,1)), bas$ibound)),
                                  zlim = range(array[rmfi_ifelse0(is.null(i),c(1:dim(array)[1]),i),rmfi_ifelse0(is.null(j),c(1:dim(array)[2]),j),rmfi_ifelse0(is.null(k),c(1:dim(array)[3]),k)][as.logical(mask[rmfi_ifelse0(is.null(i),c(1:dim(array)[1]),i),rmfi_ifelse0(is.null(j),c(1:dim(array)[2]),j),rmfi_ifelse0(is.null(k),c(1:dim(array)[3]),k)])], finite=TRUE),
                                  colour_palette = ifelse(type %in% c('contour', 'vector'), 'black', rmfi_rev_rainbow),
                                  nlevels = 7,
                                  type='fill',
                                  levels = NULL,
                                  gridlines = FALSE,
                                  add=FALSE,
                                  crop = FALSE,
                                  hed = NULL,
                                  l = NULL,
                                  binwidth = pretty(diff(zlim)/20, 1)[1],
                                  label = TRUE,
                                  prj = NULL,
                                  crs = NULL,
                                  vecsize = NULL,
                                  uvw = NULL,
                                  legend = ifelse(type %in% c('fill', 'factor'), !add, FALSE),
                                  ...) {
  
  if(is.null(i) & is.null(j) & is.null(k)) {
    stop('Please provide i, j or k.', call. = FALSE)
  }
  if(!all(attr(array, 'dimlabels') == c("i", "j", "k"))) {
    stop('Array needs to represent dimensions i, j & k. Is the array transposed or subsetted ?', call. = FALSE)
  }
  if(!is.null(hed)) {
    satdis <- rmf_convert_dis_to_saturated_dis(dis = dis, hed = hed, l = l)
    p <- rmf_plot(array, dis = satdis, i=i,j=j,k=k,bas=bas,mask=mask,zlim=zlim,colour_palette=colour_palette,nlevels=nlevels,type=type,add=add,
                  levels = levels, add=add, crop = crop, prj = prj, crs = crs, 
                  binwidth=binwidth, label=label, vecsize=vecsize, legend=legend, uvw = uvw, ...)
    if(isTRUE(gridlines) || is.character(gridlines)) {
      return(p + rmf_plot(array, dis = dis, i=i,j=j,k=k,bas=bas,mask=mask,type='grid',add=TRUE, gridlines=gridlines, crop=crop, prj=prj,crs=crs,...))
    } else {
      return(p)
    }
  }
  
  # layer is plotted
  if(!is.null(k)) {
    if(any(dis$laycbd != 0)) warning('Quasi-3D confining beds detected. Make sure k index is adjusted correctly if the array explicitly represents Quasi-3D confining beds.', 
                                     call. = FALSE)
    if(!is.null(uvw)) {
      if(!is.null(attr(uvw$u, 'dim'))) uvw$u <- uvw$u[,,k]
      if(!is.null(attr(uvw$v, 'dim'))) uvw$v <- uvw$v[,,k]
    }
    rmf_plot(array[,,k], dis=dis, mask=mask[,,k], zlim=zlim, colour_palette = colour_palette, nlevels = nlevels, type=type, levels = levels, gridlines = gridlines, add=add, crop = crop, prj = prj, crs = crs, 
             binwidth=binwidth, label=label, vecsize=vecsize, legend=legend, uvw = uvw, ...)
  } else {
    # cross-section
    # datapoly
    xy <- NULL
    xy$x <- cumsum(dis$delr)-dis$delr/2
    xy$y <- rev(cumsum(dis$delc)-dis$delc/2)
    mask[which(mask==0)] <- NA
    
    # warnings generated in rmf_as_tibble
    if(any(dis$laycbd != 0) && dim(array)[3] != dim(dis$botm)[3]) {
      # warning('Quasi-3D confining beds detected. Adding their thicknesses to the overlying numerical layers. Otherwise make sure the array explicitly contains Quasi-3D confining beds.')
      dis$thck <- rmf_calculate_thickness(dis, collapse_cbd = TRUE)
      botm <- rmfi_ifelse0(dis$nlay + sum(dis$laycbd != 0) > 1, dis$botm[,,cumsum((dis$laycbd != 0) +1)], dis$botm)
      nnlay <- dis$nlay
      dis$center <- botm
      for(a in 1:nnlay) dis$center[,,a] <- botm[,,a]+dis$thck[,,a]/2
    } else {
      # if(any(dis$laycbd != 0)) warning('Quasi-3D confining beds detected; explicitly representing them.')
      dis$thck <- rmf_calculate_thickness(dis)
      nnlay <- dis$nlay + sum(dis$laycbd != 0)
      dis$center <- dis$botm
      for(a in 1:nnlay) dis$center[,,a] <- dis$botm[,,a]+dis$thck[,,a]/2
    }    
    
    datapoly <- rmf_as_tibble(array, dis = dis, i = i, j = j, k = k, mask = mask, prj = prj, crs = crs, as_points = FALSE)
    datapoly$id <- factor(datapoly$id)
    if(crop) datapoly <- na.omit(datapoly)
    
    if(is.null(i) & !is.null(j)) {
      
      xlabel <- 'y'
      ylabel <- 'z'
      
      if(type == 'contour') {
        xy$x <- rep(xy$y, each = nnlay)
        xy$y <- c(t(dis$center[,j,]))
        if(!is.null(prj)) {
          new_xy <- rmf_convert_grid_to_xyz(x=rmf_convert_grid_to_xyz(i=1, j=j, dis=dis)[[1]], y=xy$x,z=xy$y,prj=prj)
          xy$x <- new_xy$y
          xy$y <- new_xy$z
        }
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          xy$x <- rmfi_convert_coordinates(xy,from=sf::st_crs(prj$projection),to=sf::st_crs(crs))$x
        }
        xy$z <- c(t(array[,j,]*mask[,j,]^2))
        xyBackup <- xy
        xy <- na.omit(as.data.frame(xy))
        
        thck <- na.omit(c(dis$thck[,j,]*mask[,j,]^2))
        lx <- min(ceiling(sum(dis$delc)/min(dis$delc)), dis$nrow * 10)
        ly <- min(ceiling(sum(thck)/min(thck)), dis$nlay * 10)
        xy <- akima::interp(xy$x, xy$y, xy$z, xo = seq(min(xy$x), max(xy$x), length.out = lx), yo = seq(min(xy$y), sum(max(xy$y)), length.out = ly))
        xy$x <- rep(xy$x, ly)
        xy$y <- rep(xy$y, each = lx)
        xy$z <- c(xy$z)
      }
    } else if(!is.null(i) & is.null(j)) {
      
      xlabel <- 'x'
      ylabel <- 'z'
      
      if(type == 'contour') {
        xy$x <- rep(xy$x, each = nnlay)
        xy$y <- c(t(dis$center[i,,]))
        if(!is.null(prj)) {
          new_xy <- rmf_convert_grid_to_xyz(x=xy$x, y=rmf_convert_grid_to_xyz(i=i, j=1, dis=dis)[[2]],z=xy$y,prj=prj)
          xy$x <- new_xy$x
          xy$y <- new_xy$z
        }
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          xy$x <- rmfi_convert_coordinates(xy,from=sf::st_crs(prj$projection),to=sf::st_crs(crs))$x
        }
        xy$z <- c(t(array[i,,]*mask[i,,]^2))
        xyBackup <- xy
        xy <- na.omit(as.data.frame(xy))
        
        thck <- na.omit(c(dis$thck[i,,]*mask[i,,]^2))
        lx <- min(ceiling(sum(dis$delr)/min(dis$delr)), dis$ncol * 10)
        ly <- min(ceiling(sum(thck)/min(thck)), dis$nlay * 10)
        xy <- akima::interp(xy$x, xy$y, xy$z, xo = seq(min(xy$x), max(xy$x), length.out = lx), yo = seq(min(xy$y), sum(max(xy$y)), length.out = ly))
        xy$x <- rep(xy$x, ly)
        xy$y <- rep(xy$y, each = lx)
        xy$z <- c(xy$z)
      } 
    }
    
    # legend
    if(is.logical(legend)) {
      name <- "value"
    } else {
      name <- legend
      legend <- TRUE
    }
    
    # plot
    if(type=='fill') {
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly,show.legend=legend, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_gradientn(name,colours=colour_palette(nlevels),limits=zlim, na.value = NA))) 
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id),show.legend=legend, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_gradientn(name,colours=colour_palette(nlevels),limits=zlim, na.value = NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel))
      }
    } else if(type=='factor') {
      labels <- rmfi_ifelse0(is.null(names(levels)), levels, levels[as.character(sort(na.omit(unique(datapoly$value))))])
      datapoly$value <- rmfi_ifelse0(is.null(levels), factor(datapoly$value), factor(datapoly$value, labels = labels))
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly,show.legend=legend, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_discrete(name, breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA)))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id),show.legend=legend,colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_discrete(name, breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel))
      }
    } else if(type=='grid') {
      if(add) {
        return(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,group=id),data=datapoly,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(group=id),colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel))
      }
    } else if(type == 'contour') {
      xy <- as.data.frame(xy)
      xy <- xy[which(xy$z >= zlim[1] & xy$z <= zlim[2]),]
      closestGridPoints <- apply(xy[,c('x','y')],1,function(x) which.min((x[1]-xyBackup$x)^2 + (x[2]-xyBackup$y)^2))
      xy$z[which(is.na(xyBackup$z[closestGridPoints]))] <- NA
      if(crop) {
        xlim <- c(min(xy$x, na.rm = TRUE), max(xy$x, na.rm = TRUE))
        ylim <- c(min(xy$y, na.rm = TRUE), max(xy$y, na.rm = TRUE))
      } else {
        xlim <- c(min(xyBackup$x, na.rm = TRUE), max(xyBackup$x, na.rm = TRUE))
        ylim <- c(min(xyBackup$y, na.rm = TRUE), max(xyBackup$y, na.rm = TRUE))
      }
      rm(xyBackup)
      if(add) {
        if(label) return(list(ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA),
                              ggplot2::stat_contour(ggplot2::aes(x=x,y=y,z=z,colour = ..level..),data=xy,show.legend=legend,binwidth=binwidth),directlabels::geom_dl(ggplot2::aes(x=x, y=y, z=z, label=..level.., colour=..level..),data=xy,method="top.pieces", stat="contour"),
                              ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA)))
        if(!label) return(list(ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA),
                               ggplot2::stat_contour(ggplot2::aes(x=x,y=y,z=z,colour = ..level..),data=xy,show.legend=legend,binwidth=binwidth),
                               ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA)))
      } else {
        if(label) {
          return(ggplot2::ggplot(xy, ggplot2::aes(x=x, y=y)) +
                   ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA) +
                   ggplot2::stat_contour(ggplot2::aes(z=z, colour = ..level..),show.legend=legend,binwidth=binwidth) +
                   ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA) +
                   directlabels::geom_dl(ggplot2::aes(z=z, label=..level.., colour=..level..),method="top.pieces", stat="contour") +
                   ggplot2::xlim(xlim)+
                   ggplot2::ylim(ylim) + 
                   ggplot2::xlab(xlabel) +
                   ggplot2::ylab(ylabel))
        } else {
          return(ggplot2::ggplot(xy, ggplot2::aes(x=x, y=y)) +
                   ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA) +
                   ggplot2::stat_contour(ggplot2::aes(z=z,colour = ..level..),show.legend=legend,binwidth=binwidth) +
                   ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),limits=zlim,  na.value = NA) +
                   ggplot2::xlim(xlim)+
                   ggplot2::ylim(ylim) + 
                   ggplot2::xlab(xlabel) +
                   ggplot2::ylab(ylabel))
        }
      }
    } else if(type == 'vector') {
      if(is.null(uvw)) {
        grad <- rmf_gradient(array, dis = dis, mask = mask) 
      } else {
        if(is.null(uvw$w)) stop('Please supply a w component in the uvm argument', call. = FALSE)
        grad <- list(x = rmf_create_array(uvw$u, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                     y = rmf_create_array(uvw$v, dim = c(dis$nrow, dis$ncol, dis$nlay)),
                     z = rmf_create_array(uvw$w, dim = c(dis$nrow, dis$ncol, dis$nlay)))
      }
      if(is.null(i) && !is.null(j)) {
        vector_df <- data.frame(x=xy$y,y=c(dis$center[,j,]))
        if(!is.null(prj)) {
          new_positions <- rmf_convert_grid_to_xyz(x=rmf_convert_grid_to_xyz(i=1, j=j, dis=dis)[[1]],y=vector_df$x,z=vector_df$y,prj=prj)
          vector_df$x <- new_positions$y
          vector_df$y <- new_positions$z
        }
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          vector_df$x <- rmfi_convert_coordinates(vector_df,from=sf::st_crs(prj$projection),to=sf::st_crs(crs))$x
        }
        # add gradient values; negative because want to show arrow from high to low
        vector_df$u <- -c(t(grad$y[,j,]*mask[,j,]^2))
        vector_df$v <- -c(grad$z[,j,]*mask[,j,]^2)
        
      } else if(!is.null(i) && is.null(j)) {
        vector_df <- data.frame(x=xy$x,y=c(dis$center[i,,]))
        if(!is.null(prj)) {
          new_positions <- rmf_convert_grid_to_xyz(x=vector_df$x,y=rmf_convert_grid_to_xyz(i=i,j=1,dis=dis)[[2]],z=vector_df$y,prj=prj)
          vector_df$x <- new_positions$x
          vector_df$y <- new_positions$z
        }
        if(!is.null(crs)) {
          if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
          vector_df$x <- rmfi_convert_coordinates(vector_df,from=sf::st_crs(prj$projection),to=sf::st_crs(crs))$x
        }
        # add gradient values; negative because want to show arrow from high to low
        vector_df$u <- -c(t(grad$x[i,,]*mask[i,,]^2))
        vector_df$v <- -c(grad$z[i,,]*mask[i,,]^2)
      }
      if(crop) vector_df <- na.omit(vector_df)

      if(add) {
        return(list(ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA),
                    ggquiver::geom_quiver(data = vector_df, ggplot2::aes(x=x, y=y, u=u, v=v, colour = sqrt(u^2 + v^2)),show.legend=legend, center = TRUE, vecsize=vecsize),
                    ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),  na.value = NA))) 
      } else {
        return(ggplot2::ggplot() +
                 ggplot2::geom_polygon(data=datapoly, ggplot2::aes(group=id,x=x,y=y),colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines)),fill=NA) +
                 ggquiver::geom_quiver(data=vector_df, ggplot2::aes(x=x,y=y,u=u,v=v, colour = sqrt(u^2 + v^2)),show.legend=legend, center = TRUE, vecsize = vecsize) +
                 ggplot2::scale_colour_gradientn(name, colours=rmfi_ifelse0(is.function(colour_palette), colour_palette(nlevels), colour_palette),  na.value = NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel))
      }
    } else {
      stop('Please provide valid plot type.', call. = FALSE)
    }
  }
}

#' Plot a 2D section through a MODFLOW 4D array
#' 
#' \code{rmf_plot.rmf_4d_array} plots a 2D section through a MODFLOW 4D array.
#' 
#' @param array an object of class rmf_3d_array
#' @param dis discretization file object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot
#' @param ... parameters provided to \code{\link{rmf_plot.rmf_3d_array}}
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @details specifying all of the \code{i, j & k} arguments will plot a time series at that cell location
#' @method rmf_plot rmf_4d_array
#' @export
rmf_plot.rmf_4d_array <- function(array,
                                  dis, 
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  l = NULL,
                                  ...) {
  if(!all(attr(array, 'dimlabels') == c("i", "j", "k", "l"))) {
    stop('Array needs to represent dimensions i, j, k & l. Is the array transposed or subsetted ?', call. = FALSE)
  }
  if(is.null(i) & is.null(j) & is.null(k) & is.null(l)) {
    stop('Please provide i, j, k and/or l.', call. = FALSE)
  }
  if(!is.null(l)) {
    rmf_plot(array[,,,l], dis=dis, i=i, j=j, k=k, ...)
  } else if(!is.null(i) & !is.null(j) & !is.null(k)) {
    time <- rmfi_ifelse0(is.null(attr(array, 'totim')), 1:dim(array)[4], attr(array, 'totim')[!is.na(attr(array, 'totim'))])
    tbl <- tibble::tibble(value = array[i, j, k, ], time = time)
    
    return(ggplot2::ggplot(na.omit(tbl), ggplot2::aes(x = time, y = value)) +
             rmfi_ifelse0(dim(array)[4] > 1, ggplot2::geom_path(), ggplot2::geom_point()))
  } else {
    if(dim(array)[4] > 1) warning('Using final time step results.', call. = FALSE)
    rmf_plot(array[,,,dim(array)[4]], dis=dis, i=i, j=j, k=k, ...)
  }
}

#' Plot a RMODFLOW list object
#'
#' @param obj a \code{RMODFLOW} object of class \code{rmf_list}
#' @param dis a \code{RMODFLOW} dis object
#' @param bas a \code{RMODFLOW} dis object; optional. If present, mask will be set to bas$ibound.
#' @param mask a 3D array with 0 or FALSE indicating inactive cells; optional; defaults to having all cells active or, if bas is provided, the first layer of bas$ibound
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param variable single character or numeric indicating which column in the \code{rmf_list} object to plot. Defaults to 'id', which plots the locations of the cells.
#' @param geom either 'polygon' (default), 'line' or 'point'. Defines how the rmf_list features are plotted. See details.
#' @param type plot type: 'fill' (default), 'factor', 'grid', 'contour' or 'vector'
#' @param levels (named) character vector with labels for the factor legend. If not named, factor levels are sorted before being labelled. If NULL, the array factor levels are used
#' @param group variable name or index in \code{obj} used to group the data when \code{geom = 'line'}. Passed to \code{ggplot2} aesthetics. Defaults to NULL.
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param prj projection file object
#' @param crs coordinate reference system for the plot
#' @param colour_palette a colour palette for imaging continuous array values. If type = 'contour' or 'vector', a single character can also be used. 
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param legend either a logical indicating if the legend is shown or a character indicating the legend title
#' @param crop logical; should plot be cropped to the domain represented by the features; defaults to FALSE
#' @param gridlines logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param ... additional arguments passed to either \code{\link{rmf_plot.rmf_3d_array}} if \code{geom = 'polygon'}, \code{ggplot2::geom_point} if \code{geom = 'point'} or \code{ggplot2::geom_path} if \code{geom = 'line'}
#' 
#' @return ggplot2 object or layer
#' @method rmf_plot rmf_list
#' 
#' @export
#' @details If \code{geom = 'polygon'}, the rmf_list is converted to a rmf_3d_array using \code{\link{rmf_as_array.rmf_list}}. The sparse argument is set to FALSE.
#'          If \code{geom = 'line'}, \code{ggplot2::geom_path} is used. If \code{geom = 'point'}, \code{ggplot2::geom_point} is used.
#'          \code{geom = 'line'} will only work optimally if the \code{group} argument is set. 
#'
rmf_plot.rmf_list <- function(obj, 
                              dis, 
                              bas = NULL,
                              mask = NULL,
                              i = NULL,
                              j = NULL,
                              k = NULL,
                              variable = 'id',
                              geom = 'polygon',
                              type = 'fill',
                              levels = NULL,
                              group = NULL,
                              active_only = FALSE,
                              fun = sum,
                              add = FALSE,
                              prj = NULL,
                              crs = NULL,
                              colour_palette = rmfi_rev_rainbow,
                              nlevels = 7,
                              legend = ifelse(variable == 'id', FALSE, !add), 
                              crop = FALSE,
                              gridlines = FALSE,
                              ...) {
  
  if(is.null(i) & is.null(j) & is.null(k)) {
    stop('Please provide i, j or k.', call. = FALSE)
  }
  if(nrow(obj) == 0) {
    if(add) {
      warning('No features present in rmf_list object. Returning NULL.', call. = FALSE)
      return(NULL)
    } else {
      stop('No features present in rmf_list object.', call. = FALSE)
    }
  }
  if(!is.null(k)) {
    if(length(which(obj$k == k)) == 0) {
      if(add) {
        warning(paste0('No rmf_list features in layer ', k, '. Returning NULL.'), call. = FALSE)
        return(NULL)
      } else {
        stop(paste0('No rmf_list features in layer ', k, '.'), call. = FALSE)
      }
    }
  }
  if(!is.null(i)) {
    if(length(which(obj$i == i)) == 0) {
      if(add) {
        warning(paste0('No rmf_list features in row ', i, '. Returning NULL.'), call. = FALSE)
        return(NULL)
      } else {
        stop(paste0('No rmf_list features in row ', i, '.'), call. = FALSE)
      }
    }
  }
  if(!is.null(j)) {
    if(length(which(obj$j == j)) == 0) {
      if(add) {
        warning(paste0('No rmf_list features in column ', j, '. Returning NULL.'), call. = FALSE)
        return(NULL)
      } else {
        stop(paste0('No rmf_list features in column ', j, '.'), call. = FALSE)
      }
    }
  }
  
  if(geom == 'polygon') {
    na_value <- ifelse(active_only, NA, 0)
    
    # TODO type = 'grid' is not properly plotted for active_only
    # actually ok since active_only removes the non-active cells from the plot
    if(variable == 'id') {
      arr <- rmf_as_array(obj, dis = dis, select = 3, sparse = FALSE, na_value = na_value, fun = fun)
      indx <- rmfi_ifelse0(is.na(na_value), which(!is.na(arr)), which(arr != na_value))
      arr[indx] <- 1
      rmf_plot(arr, dis = dis, type = ifelse(type == 'grid', type, 'factor'), add = add, i = i, j = j, k = k, prj = prj, crs = crs, crop = crop, gridlines = gridlines,
               levels = levels, colour_palette = colour_palette, nlevels = nlevels, legend = legend, ...)
      
    } else {
      arr <- rmf_as_array(obj, dis = dis, select = variable, sparse = FALSE, na_value = na_value, fun = fun)
      rmf_plot(arr, dis = dis, add = add, type = type, i = i, j = j, k = k, prj = prj, crs = crs, crop = crop, gridlines = gridlines, 
               levels = levels, colour_palette = colour_palette, nlevels = nlevels, legend = legend, ...)
    }
    
  } else {
    
    # geom point or lines
    
    # mask (possibly from bas)
    
    # subset based on ijk
    if(!is.null(i)) obj <- obj[which(obj$i == i), ]
    if(!is.null(j)) obj <- obj[which(obj$j == j), ]
    if(!is.null(k)) obj <- obj[which(obj$k == k), ]
    df <- rmf_as_tibble(obj, dis = dis, prj = prj, crs = crs, as_points = TRUE)
    
    # set axis labels 
    lbl <- c('x', 'y')
    if(is.null(k)) {
      df_names <- names(df)
      if(!is.null(i)) {
        new_names <- replace(df_names, match(c('x', 'y', 'z'), df_names), c('x', 'z', 'y'))
        lbl <- c('x', 'z')
      } else if(!is.null(j)) {
        new_names <- replace(df_names, match(c('x', 'y', 'z'), df_names), c('z', 'x', 'y'))
        lbl <- c('y', 'z')
      }
      df <- setNames(df, new_names)
    }
    
    # set limits
    # TODO clean up
    if(!is.null(k)) {
      # if(crop) {
      #   x <- cumsum(dis$delr)[range(obj$j)] - dis$delr[range(obj$j)]/2 + c(-dis$delr[min(obj$j)]/2, dis$delr[max(obj$j)]/2)
      #   y <- rev(cumsum(dis$delc))[range(obj$i)] - dis$delc[range(obj$i)]/2 + c(-dis$delc[min(obj$i)]/2, dis$delc[max(obj$i)]/2)
      #   xlim <- rmf_convert_grid_to_xyz(x = x, y = 0, dis = dis, prj = prj)
      #   ylim <- rmf_convert_grid_to_xyz(x = 0, y = y, dis = dis, prj = prj)
      # } else {
      xlim <- rmf_convert_grid_to_xyz(x = c(0, sum(dis$delr)), y = 0, dis = dis, prj = prj)
      ylim <- rmf_convert_grid_to_xyz(x = 0, y = c(0, sum(dis$delc)), dis = dis, prj = prj)
      # }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        xlim <- rmfi_convert_coordinates(xlim, from = prj$projection, to = crs)
        ylim <- rmfi_convert_coordinates(ylim, from = prj$projection, to = crs)
      }
      xlim <- xlim$x
      ylim <- ylim$y
    }
    if(!is.null(i)) {
      # if(crop) {
      #   x <- cumsum(dis$delr)[range(obj$j)] - dis$delr[range(obj$j)]/2 + c(-dis$delr[min(obj$j)]/2, dis$delr[max(obj$j)]/2)
      #   xlim <- rmf_convert_grid_to_xyz(x = x, y = 0, dis = dis, prj = prj)
      # 
      #   top <- dis$botm
      #   top[,,1] <- dis$top
      #   if(dis$nlay > 1) top[,,2:dis$nlay] <- dis$botm[,,1:(dis$nlay - 1)]
      #   ylim <- range(c(top[i,,range(obj$k)], dis$botm[i,,range(obj$k)])) + ifelse(is.null(prj), 0, ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
      # } else {
      xlim <- rmf_convert_grid_to_xyz(x = c(0, sum(dis$delr)), y = rev(cumsum(rev(dis$delc))-rev(dis$delc)/2)[i], dis = dis, prj = prj)
      ylim <- range(c(dis$top[i,], dis$botm[i,,])) + ifelse(is.null(prj), 0, ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
      # }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        xlim <- rmfi_convert_coordinates(xlim, from = prj$projection, to = crs)
      }
      xlim <- xlim$x
    }
    if(!is.null(j)) {
      # if(crop) {
      #   x <- rev(cumsum(dis$delc))[range(obj$i)] - dis$delc[range(obj$i)]/2 + c(-dis$delc[min(obj$i)]/2, dis$delc[max(obj$i)]/2)
      #   xlim <- rmf_convert_grid_to_xyz(x = x, y = 0, dis = dis, prj = prj)
      # 
      #   top <- dis$botm
      #   top[,,1] <- dis$top
      #   if(dis$nlay > 1) top[,,2:dis$nlay] <- dis$botm[,,1:(dis$nlay - 1)]
      #   ylim <- range(c(top[,j,range(obj$k)], dis$botm[,j,range(obj$k)])) + ifelse(is.null(prj), 0, ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
      # } else {
      xlim <- rmf_convert_grid_to_xyz(x = (cumsum(dis$delr) - dis$delr/2)[j], y = c(0, sum(dis$delc)), dis = dis, prj = prj)
      ylim <- range(c(dis$top[,j], dis$botm[,j,])) + ifelse(is.null(prj), 0, ifelse(is.na(prj$origin[3]), 0, prj$origin[3]))
      # }
      
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        xlim <- rmfi_convert_coordinates(xlim, from = prj$projection, to = crs)
      }
      xlim <- xlim$y
    }
    
    lims <- rmfi_ifelse0(crop, NULL ,ggplot2::lims(x = xlim, y = ylim))
    
    
    # set legend
    if(is.logical(legend)) {
      name <- variable
    } else {
      name <- legend
      legend <- TRUE
    }
    
    # set scales
    if(type == 'factor') {
      labels <- rmfi_ifelse0(is.null(names(levels)), levels, levels[as.character(sort(na.omit(unique(df[[variable]]))))])
      df[[variable]] <- rmfi_ifelse0(is.null(levels), factor(df[[variable]]), factor(df[[variable]], labels = labels))
      plot_scales <- ggplot2::scale_colour_discrete(name, breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(df[[variable]])), na.value = NA)
    } else if(type == 'fill') {
      plot_scales <- ggplot2::scale_colour_gradientn(name,colours=colour_palette(nlevels),limits=range(df[[variable]]), na.value = NA)
    }
    
    # plot
    # grid
    if(gridlines || is.character(gridlines)) {
      p_grid <- rmf_plot(obj, dis = dis, i = i, j = j, k = k, type = 'grid', gridlines = gridlines, add = TRUE, prj = prj, crs = crs)
    } else {
      p_grid <- NULL
    }
    
    if(add) {
      if(variable == 'id') {
        if(geom == 'point') return(list(p_grid, ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y), show.legend = legend, ...)))
        if(geom == 'line') return(list(p_grid, ggplot2::geom_path(data = df, ggplot2::aes(x = x, y = y, group = group), show.legend = legend, ...)))
      } else {
        if(geom == 'point') return(list(p_grid, ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y, colour = !!ggplot2::sym(variable)), show.legend = legend, ...),
                                        plot_scales))
        if(geom == 'line') return(list(p_grid, ggplot2::geom_path(data = df, ggplot2::aes(x = x, y = y, group = group, colour = !!ggplot2::sym(variable)), show.legend = legend, ...),
                                       plot_scales))
      }
    } else {
      if(variable == 'id') {
        if(geom == 'point') return(ggplot2::ggplot() +
                                     p_grid +
                                     ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y), show.legend = legend, ...) +
                                     ggplot2::xlab(lbl[1]) +
                                     ggplot2::ylab(lbl[2]) +
                                     lims)
        if(geom == 'line') return(ggplot2::ggplot() + 
                                    p_grid +
                                    ggplot2::geom_path(data = df, ggplot2::aes(x = x, y = y, group = group), show.legend = legend, ...) +
                                    ggplot2::xlab(lbl[1]) +
                                    ggplot2::ylab(lbl[2]) +
                                    lims)
      } else {
        if(geom == 'point') return(ggplot2::ggplot() +
                                     p_grid +
                                     ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = y, colour = !!ggplot2::sym(variable)), show.legend = legend, ...) +
                                     plot_scales +
                                     ggplot2::xlab(lbl[1]) +
                                     ggplot2::ylab(lbl[2]) +
                                     lims)
        if(geom == 'line') return(ggplot2::ggplot() + 
                                    p_grid +
                                    ggplot2::geom_path(data = df, ggplot2::aes(x = x, y = y, group = group, colour = !!ggplot2::sym(variable)), show.legend = legend, ...) +
                                    plot_scales +
                                    ggplot2::xlab(lbl[1]) +
                                    ggplot2::ylab(lbl[2]) +
                                    lims)
      }
    }
  }
  
}


#' Plot a MODFLOW sensitivity analysis object
#' 
#' @param sen sensitivity analysis object
#' @param plot type: 'css' or 'dss'
#' @method rmf_plot sen
#' @export
rmf_plot.sen <- function(sen,type='css')
{
  if(type=='css')
  {
    dat <- data.frame(parnam=sen$parnam,css=sen$css)
    dat$parnam <- factor(as.character(dat$parnam),levels=dat$parnam[order(dat$css,decreasing=TRUE)])
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=parnam,y=css))+
               ggplot2::geom_bar(stat='identity')
    )
  } else if(type=='dss')
  {
    stop('dss plotting not implemented yet', call. = FALSE)
  }
  
}

#' Plot a RMODFLOW wel object
#' 
#' @param wel an \code{RMODFLOW} wel object
#' @param dis a \code{RMODFLOW} dis object
#' @param kper integer specifying the stress-period to plot
#' @param variable single character or numeric indicating which column of \code{wel$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum for variable 'q'.
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot wel

rmf_plot.wel <- function(wel,
                         dis,
                         kper = NULL,
                         variable = 'id',
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         active_only = TRUE,
                         fun = sum,
                         add = FALSE,
                         ...) {
  
  rmfi_plot_bc(obj = wel, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, add = add, ...)
  
}
