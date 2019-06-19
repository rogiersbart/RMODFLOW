
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
    x <- sym('nstp')
    gm_line <- geom_path()
    
    # check if ss 
    if(dis$nper == 1 && dis$sstr == "SS") {
      if(type == "area") {
        type <- 'bar'
        df$nstp <- factor(df$nstp)
        gm_line <- geom_col(aes(fill = volume))
      } 
    }
    
    if(what == 'total') {
      if(net) {
        df <- aggregate(list(value = df$value), by = list(nstp = df$nstp, volume = df$volume),  sum)
        p <- ggplot(df, aes(x=!!x, y=value, group=volume, colour = volume)) + gm_line +
          geom_hline(yintercept = 0, colour = 'black') +
          facet_wrap(~volume, ncol = 1, scales = 'free_y') + 
          labs(title = 'Net total volume [L**3]', y = 'Volume [L**3]', x = x_label)
      } else {
        if(type == 'bar') {
          p <- ggplot(df, aes(x=!!x, y=value, colour=io, fill=io)) + geom_col() + 
            geom_hline(yintercept = 0, colour = 'black') +
            facet_wrap(~volume, ncol = 1, scales = 'free_y') +
            labs(title = 'Gross total volume [L**3]', y = 'Volume [L**3]', x = x_label)
          
        } else if(type == 'area') {
          p <- ggplot() +
            geom_area(data=subset(df, io=='in'), aes(x=!!x ,y=value, colour = volume, fill = volume), alpha=0.7) +
            geom_area(data=subset(df, io=='out'), aes(x=!!x ,y=value, colour = volume, fill = volume), alpha=0.7) +
            geom_hline(yintercept = 0, colour = 'black') +
            facet_wrap(~volume, ncol = 1, scales = 'free_y')+ 
            labs(title = 'Gross total volume [L**3]', y = 'Volume [L**3]', x = x_label)
          
        }
      }
    } else { # difference/discrepancy
      # plot
      p <- ggplot(df, aes(x=!!x, y=value, group=io, colour = volume)) + gm_line + 
        geom_hline(yintercept = 0, colour = 'black') +
        facet_wrap(~volume, ncol = 1, scales = 'free_y') +
        labs(title = rmfi_ifelse0(what == 'difference', "Volume in - volume out", "Discrepancy"), y = rmfi_ifelse0(what == 'difference', "Volume [L**3]", "% discrepancy"), x = x_label)
    }

    # rates/cumulative
  } else {
    
    df <- subset(df[[what]], !(flux %in% c('difference', 'discrepancy', 'total')))
    # remove fluxes if necessary
    if(length(fluxes) > 1 || fluxes != 'all') df <- subset(df, flux %in% fluxes)

    x_label <- 'nstp'
    x <- sym('nstp')
    
    # check if ss 
    if(dis$nper == 1 && dis$sstr == "SS") {
      if(type == "area") {
        type <- 'bar'
        x <- sym('flux')
        x_label <- 'flux'
      } else {
        df$nstp <- factor(df$nstp)
      }
    }
    
    # plot
    if(net) {
      df <- aggregate(list(value = df$value), by = list(nstp = df$nstp, flux = df$flux),  sum)
      if(type == 'bar') {
        p <- ggplot(df, aes(x=!!x, y=value, colour=flux, fill=flux)) + geom_col() +
          geom_hline(yintercept = 0, colour = 'black') +
          labs(title = rmfi_ifelse0(what == 'rates', "Net volumetric rates", "Net cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
      } else if(type == 'area') {
        p <- ggplot(data=df, aes(x=!!x ,y=value, colour = flux, fill = flux)) + geom_area(alpha=0.7) +
          geom_hline(yintercept = 0, colour = 'black') +
          labs(title = rmfi_ifelse0(what == 'rates', "Net volumetric rates", "Net cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
      }
    } else {
      if(type == 'bar') {
        p <- ggplot(df, aes(x=!!x, y=value, colour=flux, fill=flux)) + geom_col() +
          geom_hline(yintercept = 0, colour = 'black') +
          labs(title = rmfi_ifelse0(what == 'rates', "Gross volumetric rates", "Gross cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
        
      } else if(type == 'area') {
          p <-  ggplot() +
            geom_area(data=subset(df, io=='in'), aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
            geom_area(data=subset(df, io=='out'), aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
            geom_hline(yintercept = 0, colour = 'black') +
            labs(title = rmfi_ifelse0(what == 'rates', "Gross volumetric rates", "Gross cumulative volumes"), y = rmfi_ifelse0(what == 'rates', "Volumetric rate [L**3/T]", "Volume [L**3]"), x = x_label)
      }
    }
   }

  return(p)
}