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
#' 
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
                          type = 'area') {
  
  # nstp
  bud <- lapply(bud, function(i) cbind(i, nstp = c(0,cumsum(dis$nstp)[i$kper-1])+i$kstp))
  
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
    if(dis$nper == 1 && dis$sstr == "SS") {
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
    if(dis$nper == 1 && dis$sstr == "SS") {
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
                         ...) {
  
  rmfi_plot_bc(obj = chd, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
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
                         ...) {
  
  rmfi_plot_bc(obj = drn, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
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
    if(evt$nevtop != 2) stop('No ievt arrays defined in evt object; nevtop does not equal 2')
    obj <- evt$ievt[[kper]]
    
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
                         ...) {
  
  rmfi_plot_bc(obj = ghb, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
}

#' Plot a RMODFLOW hfb object
#' 
#' @param hfb an \code{RMODFLOW} hfb object
#' @param dis a \code{RMODFLOW} dis object
#' @param variable single character or numeric indicating which column of \code{hfb$data} to plot. Defaults to 'id', which plots the locations of the cells.
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to TRUE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum for variable 'hydchr'
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer; 
#' @export
#' @method rmf_plot hfb

rmf_plot.hfb <- function(hfb,
                         dis,
                         variable = 'id',
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
                         crop = TRUE,
                         add = FALSE,
                         ...) {
  
  if(is.null(i) & is.null(j) & is.null(k)) {
    stop('Please provide i, j or k.', call. = FALSE)
  }
  
  if(!is.null(k)) {
    layer <- k
    data <- subset(hfb$data, hfb$data$k == layer)
    if(!is.character(variable)) variable <- colnames(data)[variable]
    data <- subset(data, select = c('i', 'j', 'k', 'irow2', 'icol2', if(variable != 'id') {variable}))
    data$id <- rmf_convert_ijk_to_id(i=data$i, j=data$j, k=layer, dis = dis, type = 'modflow')
    
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
      rmf_as_array(dis = dis, select = ifelse(variable == 'id', 1, which(colnames(data) == variable)), sparse = TRUE) %>%
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
    stop('Not yet implemented')
  }
  
  # TODO: sum multiple instances (additive) at same cell as is done in rmf_plot.rmf_list through rmf_as_array
  
  # rmf_plot.rmf_list
  #rmf_plot(hfb$data, dis = dis, variable = variable, active_only = active_only, i=i, j=j, k=k, fun = fun, ...)
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
  dat <- data.frame(simulated_equivalent=hpr$simulated_equivalent, observed_value=hpr$observed_value,observation_name=hpr$observation_name)[which(hpr$simulated_equivalent!=hobdry),]
  if(type=='scatter') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=observed_value,y=simulated_equivalent))+
               ggplot2::geom_point(ggplot2::aes(colour=abs(observed_value-simulated_equivalent)))+
               ggplot2::geom_abline(ggplot2::aes(intercept=0,slope=1),linetype='dashed')+
               ggplot2::scale_colour_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               ggplot2::xlab('Observed value')+
               ggplot2::ylab('Simulated equivalent')
    )
  } else if(type=='residual') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=observation_name,y=simulated_equivalent-observed_value))+
               ggplot2::geom_bar(ggplot2::aes(fill=abs(observed_value-simulated_equivalent)),stat='identity')+
               ggplot2::scale_fill_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               ggplot2::xlab('Observation name')+
               ggplot2::ylab('Simulated equivalent - observed value')
    )
  } else if(type=='histogram') {
    if(is.null(bins)) bins <- nclass.FD(dat$simulated_equivalent-dat$observed_value)
    
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=simulated_equivalent-observed_value))+
               ggplot2::geom_histogram(ggplot2::aes(fill=..count..), bins=bins)+
               ggplot2::scale_fill_gradientn('Count',colours=rev(rainbow(7)))+
               ggplot2::xlab('Simulated equivalent - observed value')
    )
  }
}

#' @describeIn rmf_plot.hpr Deprecated function name
#' @export
plot.hpr <- function(...) {
  .Deprecated(new = "rmf_plot.hpr", old = "plot.hpr")
  rmf_plot.hpr(...)
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
#' @param colour_palette a colour palette for imaging the array values
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
                         levels = huf$hgunam,
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
        huf_array[which(hufmask[,,i] == 1)] <- i
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

#' @describeIn rmf_plot.huf Deprecated function name
#' @export
plot.huf <- function(...) {
  .Deprecated(new = "rmf_plot.huf", old = "plot.huf")
  rmf_plot.huf(...)
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
    if(rch$nrchop != 2) stop('No irch arrays defined in rch object; nrchop does not equal 2')
    obj <- rch$irch[[kper]]
    
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
                         ...) {
  
  rmfi_plot_bc(obj = riv, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
}


#' Plot a MODFLOW 2D array
#' 
#' \code{rmf_plot.rmf_2d_array} plots a MODFLOW 2D array.
#' 
#' @param array an object of class rmf_2d_array
#' @param dis discretization file object
#' @param bas basic file object; optional
#' @param mask a 2D array with 0 or F indicating inactive cells; optional; defaults to having all cells active or, if bas is provided, the first layer of bas$ibound
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'factor', 'grid' or 'contour'
#' @param levels labels that should be used on the factor legend; if NULL the array factor levels are used
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
#' @param title plot title
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask); defaults to TRUE
#' @param vecint positive integer specifying the interval to smooth the appearence of the plot if type = 'vector'; defaults to 1 i.e. no smoothing
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_2d_array
#' @export
rmf_plot.rmf_2d_array <- function(array,
                                  dis,
                                  bas = NULL,
                                  mask = rmfi_ifelse0(is.null(bas), array*0+1, {warning('Using first ibound layer as mask.', call. = FALSE); rmfi_ifelse0(bas$xsection, aperm(bas$ibound, c(3,2,1))[,,1], bas$ibound[,,1])}),
                                  colour_palette = rmfi_rev_rainbow,
                                  zlim = range(array[as.logical(mask)], finite=TRUE),
                                  nlevels = 7,
                                  type = 'fill',
                                  levels = NULL,
                                  gridlines = FALSE,
                                  add = FALSE,
                                  height_exaggeration = 100,
                                  binwidth=round(diff(zlim)/20),
                                  label=TRUE,
                                  prj=NULL,
                                  crs=NULL,
                                  alpha=1,
                                  plot3d=FALSE,
                                  height=NULL,
                                  title = NULL,
                                  crop = TRUE,
                                  vecint = 1) {
  
  
  
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
    if(!all(attr(array, 'dimlabels') == c("i", "j"))) {
      if(attr(array, 'dimlabels')[1] == 'k') array <- t(array)
      if("j" %in% attr(array, 'dimlabels')) {
        sub_array <- rmf_create_array(array, dim = c(1, dim(array)))
        if(!isTRUE(all.equal(attr(mask, 'dimlabels'), attr(array, 'dimlabels')))) {
          warning("Dimensions of mask do not match those of array. Skipping mask.")
          mask <- array*0 + 1
        }
        sub_mask <- rmf_create_array(mask, dim = c(1, dim(mask)))
        
        p <- rmf_plot(sub_array, dis = dis, i = 1, bas = bas, mask = sub_mask, zlim = zlim, colour_palette = colour_palette, nlevels = nlevels,
                      type = type, levels = levels, gridlines = gridlines, add = add, title = title, crop = crop, prj = prj, crs = crs,
                      height_exaggeration = height_exaggeration, binwidth = binwidth, label = label, alpha = alpha, plot3d = plot3d, height = height)
        return(p)
        
      } else if("i" %in% attr(array, 'dimlabels')) {
        sub_array <- rmf_create_array(array, dim = c(dim(array)[1], 1, dim(array)[2]))
        if(!isTRUE(all.equal(attr(mask, 'dimlabels'), attr(array, 'dimlabels')))) {
          warning("Dimensions of mask do not match those of array. Skipping mask.")
          mask <- array*0 + 1
        }
        sub_mask <- rmf_create_array(mask, dim = c(dim(mask)[1], 1, dim(mask)[2]))
        
        p <- rmf_plot(sub_array, dis = dis, j = 1, bas = bas, mask = sub_mask, zlim = zlim, colour_palette = colour_palette, nlevels = nlevels,
                      type = type, levels = levels, gridlines = gridlines, add = add, title = title, crop = crop, prj = prj, crs = crs,
                      height_exaggeration = height_exaggeration, binwidth = binwidth, label = label, alpha = alpha, plot3d = plot3d, height = height)
        return(p)
      }
    }
    
    xy <- expand.grid(cumsum(dis$delr)-dis$delr/2,sum(dis$delc)-(cumsum(dis$delc)-dis$delc/2))
    names(xy) <- c('x','y')
    mask[which(mask==0)] <- NA
    if(type %in% c('fill','factor','grid')) {
      ids <- factor(1:(dis$nrow*dis$ncol))
      xWidth <- rep(dis$delr,dis$nrow)
      yWidth <- rep(dis$delc,each=dis$ncol)
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c(t(array*mask^2)))
      if(!is.null(prj)) {
        new_positions <- rmf_convert_grid_to_xyz(x=positions$x,y=positions$y,prj=prj)
        positions$x <- new_positions$x
        positions$y <- new_positions$y
      }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        positions <- rmfi_convert_coordinates(positions,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))
      }
      datapoly <- merge(values, positions, by=c("id"))
      if(crop) datapoly <- na.omit(datapoly)
    }
    if(type=='fill') {  
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim, na.value = NA))) 
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id),alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim,  na.value = NA) +
                 ggplot2::coord_equal() +
                 ggplot2::ggtitle(title))
      }
    } else if(type=='factor') {  
      datapoly$value <- rmfi_ifelse0(is.null(levels), factor(datapoly$value), factor(datapoly$value, levels = seq_along(levels), labels = levels))
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_discrete('value', breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA)))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id),alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_discrete('value', breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA) +
                 ggplot2::coord_equal() +
                 ggplot2::ggtitle(title))
      }
    } else if(type=='grid') {  
      if(add) {
        return(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,group=id),data=datapoly,alpha=alpha,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(group=id),alpha=alpha,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA) +
                 ggplot2::coord_equal() +
                 ggplot2::ggtitle(title))
      }
    } else if(type=='contour') {
      if(!is.null(prj)) {
        new_xy <- rmf_convert_grid_to_xyz(x=xy$x,y=xy$y,prj=prj)
        xy$x <- new_xy$x
        xy$y <- new_xy$y
      }
      if(!is.null(crs)) {
        xy <- rmfi_convert_coordinates(xy,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))
      }
      xy$z <- c(t(array*mask^2))
      xyBackup <- xy
      xy <- na.omit(xy)
      xy <- akima::interp(xy$x,xy$y,xy$z,xo=seq(min(xy$x),max(xy$x),length=ceiling(sum(dis$delr)/min(dis$delr))),yo=seq(min(xy$y),sum(max(xy$y)),length=ceiling(sum(dis$delc)/min(dis$delc))))
      xy$x <- rep(xy$x,ceiling(sum(dis$delc)/min(dis$delc)))
      xy$y <- rep(xy$y,each=ceiling(sum(dis$delr)/min(dis$delr)))
      xy$z <- c(xy$z)
      xy <- as.data.frame(xy)
      xy <- xy[which(xy$z >= zlim[1] & xy$z <= zlim[2]),]
      closestGridPoints <- apply(xy[,c('x','y')],1,function(x) which.min((x[1]-xyBackup$x)^2 + (x[2]-xyBackup$y)^2))
      xy$z[which(is.na(xyBackup$z[closestGridPoints]))] <- NA
      if(crop) {
        xlim = c(min(xy$x, na.rm = T), max(xy$x, na.rm = T))
        ylim = c(min(xy$y, na.rm = T), max(xy$y, na.rm = T))
      } else {
        xlim = c(min(xyBackup$x, na.rm = T), max(xyBackup$x, na.rm = T))
        ylim = c(min(xyBackup$y, na.rm = T), max(xyBackup$y, na.rm = T))
      }
      rm(xyBackup)
      if(add) {
        if(label) return(list(ggplot2::stat_contour(ggplot2::aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth),directlabels::geom_dl(ggplot2::aes(x=x, y=y, z=z, label=..level.., colour=..level..),data=xy,method="top.pieces", stat="contour")))
        if(!label) return(ggplot2::stat_contour(ggplot2::aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth))
      } else {
        if(label) {
          return(ggplot2::ggplot(xy, ggplot2::aes(x=x, y=y)) +
                   ggplot2::stat_contour(ggplot2::aes(z=z, colour = ..level..),binwidth=binwidth) +
                   directlabels::geom_dl(ggplot2::aes(z=z, label=..level.., colour=..level..),method="top.pieces", stat="contour") +
                   ggplot2::coord_equal(xlim = xlim, ylim = ylim) +
                   ggplot2::theme(legend.position="none") +
                   ggplot2::ggtitle(title))
        } else {
          return(ggplot2::ggplot(xy, ggplot2::aes(x=x, y=y)) +
                   ggplot2::stat_contour(ggplot2::aes(z=z,colour = ..level..),binwidth=binwidth) +
                   ggplot2::coord_equal(xlim = xlim, ylim = ylim) +
                   ggplot2::ggtitle(title))
        }
      }
    } else if(type == 'vector') {
      # x & y are center of cells
      datapoly <- xy
      if(!is.null(prj)) {
        new_positions <- rmf_convert_grid_to_xyz(x=datapoly$x,y=datapoly$y,prj=prj)
        datapoly$x <- new_positions$x
        datapoly$y <- new_positions$y
      }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        datapoly <- rmfi_convert_coordinates(datapoly,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))
      }
      if(crop) datapoly <- na.omit(datapoly)
      
      # add gradient values
      grad <- rmf_gradient(array, dis = dis, mask = mask) 
      datapoly$u <- -c(t(grad$x))
      datapoly$v <- -c(t(grad$y))
      datapoly <- datapoly[seq(1,nrow(datapoly),vecint),]
      vecsize <- 0.75*vecint

      if(add) {
        return(ggquiver::geom_quiver(data = datapoly, ggplot2::aes(x=x, y=y, u=u, v=v), center = TRUE, vecsize=vecsize)) 
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y, u=u, v=v)) +
                 ggquiver::geom_quiver(center = TRUE, vecsize = vecsize) +
                 ggplot2::coord_equal() +
                 ggplot2::ggtitle(title))
      }
      
    } else {
      stop('Please provide valid plot type.', call. = FALSE)
    }
  }
}

#' @describeIn rmf_plot.rmf_2d_array Deprecated function name
#' @export
plot.rmf_2d_array <- function(...) {
  .Deprecated(new = "rmf_plot.rmf_2d_array", old = "plot.rmf_2d_array")
  rmf_plot.rmf_2d_array(...)
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
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'factor' or 'grid'
#' @param levels labels that should be used on the factor legend; if NULL the array factor levels are used
#' @param gridlines logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param title plot title
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask); defaults to TRUE
#' @param hed hed object for only plotting the saturated part of the grid; possibly subsetted with time step number; by default, last time step is used
#' @param l time step number for subsetting the hed object
#' @param prj projection file object
#' @param crs coordinate reference system for the plot
#' @param ... parameters provided to plot.rmf_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_3d_array
#' @export
rmf_plot.rmf_3d_array <- function(array,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  dis,
                                  bas = NULL,
                                  mask = rmfi_ifelse0(is.null(bas),array*0+1,rmfi_ifelse0(bas$xsection, aperm(bas$ibound, c(3,2,1)), bas$ibound)),
                                  zlim = range(array[rmfi_ifelse0(is.null(i),c(1:dim(array)[1]),i),rmfi_ifelse0(is.null(j),c(1:dim(array)[2]),j),rmfi_ifelse0(is.null(k),c(1:dim(array)[3]),k)][as.logical(mask[rmfi_ifelse0(is.null(i),c(1:dim(array)[1]),i),rmfi_ifelse0(is.null(j),c(1:dim(array)[2]),j),rmfi_ifelse0(is.null(k),c(1:dim(array)[3]),k)])], finite=TRUE),
                                  colour_palette = rmfi_rev_rainbow,
                                  nlevels = 7,
                                  type='fill',
                                  levels = NULL,
                                  gridlines = FALSE,
                                  add=FALSE,
                                  title = NULL,
                                  crop = TRUE,
                                  hed = NULL,
                                  l = NULL,
                                  prj = NULL,
                                  crs = NULL,
                                  ...) {
  
  if(is.null(i) & is.null(j) & is.null(k)) {
    stop('Please provide i, j or k.', call. = FALSE)
  }
  if(!is.null(hed)) {
    satdis <- rmf_convert_dis_to_saturated_dis(dis = dis, hed = hed, l = l)
    p <- rmf_plot(array, dis = satdis, i=i,j=j,k=k,bas=bas,mask=mask,zlim=zlim,colour_palette=colour_palette,nlevels=nlevels,type=type,add=add,title=title)
    if(gridlines) {
      return(p + rmf_plot(array, dis = dis, i=i,j=j,k=k,bas=bas,mask=mask,type='grid',add=TRUE))
    } else {
      return(p)
    }
  }
  
  if(!is.null(k)) {
    if(any(dis$laycbd != 0)) warning('Quasi-3D confining beds detected. Make sure k index is adjusted correctly if the array explicitly represents Quasi-3D confining beds.', 
                                     call. = FALSE)
    zlim <- zlim
    mask <- mask
    array <- array[,,k]
    class(array) <- 'rmf_2d_array'
    mask <- mask[,,k]
    class(mask) <- 'rmf_2d_array'
    rmf_plot(array, dis, mask=mask, zlim=zlim, colour_palette = colour_palette, nlevels = nlevels, type=type, levels = levels, gridlines = gridlines, add=add, title = title, crop = crop, prj = prj, crs = crs, ...)
  } else {
    xy <- NULL
    xy$x <- cumsum(dis$delr)-dis$delr/2
    xy$y <- rev(cumsum(dis$delc)-dis$delc/2)
    mask[which(mask==0)] <- NA
    dis$thck <- dis$botm
    dis$thck[,,1] <- dis$top-dis$botm[,,1]
    nnlay <- dis$nlay+length(which(dis$laycbd != 0))
    if(nnlay > 1) for(a in 2:nnlay) dis$thck[,,a] <- dis$botm[,,a-1]-dis$botm[,,a]
    dis$center <- dis$botm
    for(a in 1:nnlay) dis$center[,,a] <- dis$botm[,,a]+dis$thck[,,a]/2
    if(is.null(i) & !is.null(j)) {
      ids <- factor(1:(dis$nrow*nnlay))
      xWidth <- rep(rev(dis$delc),nnlay)
      yWidth <- dis$thck[,j,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$y,each=4),y=rep(dis$center[,j,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((array[,j,]*mask[,j,]^2)))
      if(!is.null(prj)) {
        new_positions <- rmf_convert_grid_to_xyz(x=rmf_convert_grid_to_xyz(i=1, j=j, dis=dis)[[1]],y=positions$x,z=positions$y,prj=prj)
        positions$x <- new_positions$y
        positions$y <- new_positions$z
      }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        #warning('Transforming vertical coordinates', call. = FALSE)
        positions$x <- rmfi_convert_coordinates(positions,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))$x
      }
      datapoly <- merge(values, positions, by=c("id"))
      if(crop) datapoly <- na.omit(datapoly)
      xlabel <- 'y'
      ylabel <- 'z'
    } else if(!is.null(i) & is.null(j)) {
      ids <- factor(1:(dis$ncol*nnlay))
      xWidth <- rep(dis$delr,nnlay)
      yWidth <- dis$thck[i,,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(dis$center[i,,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((array[i,,]*mask[i,,]^2)))
      if(!is.null(prj)) {
        new_positions <- rmf_convert_grid_to_xyz(x=positions$x,y=rmf_convert_grid_to_xyz(i=i,j=1,dis=dis)[[2]],z=positions$y,prj=prj)
        positions$x <- new_positions$x
        positions$y <- new_positions$z
      }
      if(!is.null(crs)) {
        if(is.null(prj)) stop('Please provide a prj file when transforming the crs', call. = FALSE)
        #warning('Transforming vertical coordinates', call. = FALSE)
        positions$x <- rmfi_convert_coordinates(positions,from=sf::st_crs(prj$crs),to=sf::st_crs(crs))$x
      }
      datapoly <- merge(values, positions, by=c("id"))
      if(crop) datapoly <- na.omit(datapoly)
      xlabel <- 'x'
      ylabel <- 'z'
    }
    if(type=='fill') {
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim, na.value = NA))) 
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id), colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim, na.value = NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel) +
                 ggplot2::ggtitle(title))
      }
    } else if(type=='factor') {
      datapoly$value <- rmfi_ifelse0(is.null(levels), factor(datapoly$value), factor(datapoly$value, levels = seq_along(levels), labels = levels))
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=value, group=id),data=datapoly, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_discrete('value', breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA)))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=value, group=id), colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_discrete('value', breaks = rmfi_ifelse0(is.null(levels), ggplot2::waiver(), levels(datapoly$value)), na.value = NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel) +
                 ggplot2::ggtitle(title))
      }
    } else if(type=='grid') {
      if(add) {
        return(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,group=id),data=datapoly,colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(group=id),colour=ifelse(is.logical(gridlines),'black',gridlines),fill=NA) +
                 ggplot2::xlab(xlabel) +
                 ggplot2::ylab(ylabel) +
                 ggplot2::ggtitle(title))
      }
    }
  }
}

#' @describeIn rmf_plot.rmf_3d_array Deprecated function name
#' @export
plot.rmf_3d_array <- function(...) {
  .Deprecated(new = "rmf_plot.rmf_3d_array", old = "plot.rmf_3d_array")
  rmf_plot.rmf_3d_array(...)
}

#' Plot a 2D section through a MODFLOW 4D array
#' 
#' \code{rmf_plot.rmf_4d_array} plots a 2D section through a MODFLOW 4D array.
#' 
#' @param array an object of class rmf_3d_array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot
#' @param ... parameters provided to plot.rmf_3d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot rmf_4d_array
#' @export
rmf_plot.rmf_4d_array <- function(array,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  l = NULL,
                                  ...) {
  if(!is.null(l)) {
    rmf_plot(rmf_create_array(array(array[,,,l],dim=dim(array)[1:3])), i=i, j=j, k=k, ...)
  } else if(!is.null(i) & !is.null(j) & !is.null(k)) {
    ggplot2::ggplot(na.omit(data.frame(value=c(array[i,j,k,]), time = attributes(array)$totim)),ggplot2::aes(x=time,y=value))+
      ggplot2::geom_path()
  } else {
    warning('Plotting final stress period results.', call. = FALSE)
    rmf_plot(rmf_create_array(array(array[,,,dim(array)[4]],dim=dim(array)[1:3])), i=i, j=j, k=k, ...)
  }
}

#' @describeIn rmf_plot.rmf_4d_array Deprecated function name
#' @export
plot.rmf_4d_array <- function(...) {
  .Deprecated(new = "rmf_plot.rmf_4d_array", old = "plot.rmf_4d_array")
  rmf_plot.rmf_4d_array(...)
}

#' Plot a RMODFLOW list object
#'
#' @param obj a \code{RMODFLOW} object of class \code{rmf_list}
#' @param dis a \code{RMODFLOW} dis object
#' @param variable single character or numeric indicating which column in the \code{rmf_list} object to plot. Defaults to 'id', which plots the locations of the cells.
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
#' @param fun function to compute values in the case multiple values are defined for the same MODFLOW cell. Typically either \code{mean} or \code{sum}. Defaults to sum.
#' @param ... additional arguments passed to \code{\link{rmf_plot.rmf_3d_array}}
#' 
#' @return ggplot2 object or layer
#' @method rmf_plot rmf_list
#' 
#' @export
#' @details the rmf_list is converted to a rmf_3d_array using \code{\link{rmf_as_array.rmf_list}}. The sparse argument is set to FALSE.
#'
rmf_plot.rmf_list <- function(obj, 
                              dis, 
                              variable = 'id',
                              active_only = FALSE,
                              fun = sum,
                              ...) {
  
  na_value <- ifelse(active_only, NA, 0)
  
  if(variable == 'id') {
    arr <- rmf_as_array(obj, dis = dis, select = 4, sparse = FALSE, na_value = na_value, fun = fun)
    indx <- rmfi_ifelse0(is.na(na_value), which(!is.na(arr)), which(arr != na_value))
    arr[indx] <- 1
    rmf_plot(arr, dis = dis, type = 'factor', ...)
    
  } else {
    arr <- rmf_as_array(obj, dis = dis, select = variable, sparse = FALSE, na_value = na_value, fun = fun)
    rmf_plot(arr, dis = dis, ...)
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
    stop('dss plotting not implemented yet')
  }
  
}

#' @describeIn rmf_plot.sen Deprecated function name
#' @export
plot.sen <- function(...) {
  .Deprecated(new = "rmf_plot.sen", old = "plot.sen")
  rmf_plot.sen(...)
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
                         ...) {
  
  rmfi_plot_bc(obj = wel, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
}
