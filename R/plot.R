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
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
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
                         active_only = FALSE,
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
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
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
                         active_only = FALSE,
                         fun = ifelse(variable == 'elevation', mean, sum),
                         ...) {
  
  rmfi_plot_bc(obj = drn, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
}

#' Plot a 2D section of an RMODFLOW evt object
#' 
#' \code{rmf_plot.evt} plots a 2D section of an \code{RMODFLOW} evt object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param evt an \code{RMODFLOW} evt object
#' @param dis an \code{RMODFLOW} dis object
#' @param mlt optional; an \code{RMODFLOW} mlt object; used for plotting parameter data
#' @param zon optional; an \code{RMODFLOW} zon object; used for plotting parameter data
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (plots the evapotranspiration cells' locations), "evt" (default) which defines the values of maximum ET, "surf" (for stress period data), "exdp" (for stress period data), "mltnam" (for parameter data), "zonnam" (for parameter data), "iz" (for parameter data), "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "evt"
#' @param instnum numeric vector of length \code{npevt} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot evt

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.evt = function(evt,
                        dis,
                        mlt = NULL,
                        zon = NULL,
                        all_parm = F, 
                        variable = 'evt',
                        instnum = NULL, 
                        l = NULL, 
                        sp = NULL, 
                        ... 
){
  
  # set stress period
  if(is.null(l) && is.null(sp) && !all_parm){
    warning('No stress period or time step defined; setting time step to last time step and setting stress period accordingly')
    l = tail(cumsum(dis$nstp), 1)
  } 
  if(is.null(sp)) sp = tail(which(cumsum(dis$nstp) <= l), 1)
  
  if(all_parm && (is.null(evt$npevt) || (!is.null(evt$npevt) && evt$npevt == 0))) stop('RMODFLOW evt object does not have parameters. Please specify parameters or set all_parm to FALSE')
  if(all_parm && variable %in% c('surf', 'exdp')) stop('Variables surf and exdp can only be plotted for stress period data. Please set all_parm to FALSE')
  
  ##### calculate new multiplier arrays if FUNCTION is specified in mlt ####
  if(!all(unlist(evt$mltarr) == "NONE") && (!is.null(mlt) && !is.null(mlt$functn) && any(mlt$functn) )  ){
    
    rmlts = lapply(seq_along(dim(mlt$rmlt)), function(x) mlt$rmlt[,,x])
    names(rmlts) = mlt$mltnam
    
    for(i in 1:mlt$nml){
      if(!is.null(mlt$functn) && mlt$functn[i]){
        funct = strsplit(mlt$operator[[i]], split=' ')[[1]]
        for(i in 1:((length(funct)-1)/2)){
          funct = append(append('(', funct), ')', after=i*1+3*i)
        }
        mlt$rmlt[,,i] = eval(parse(text=funct), envir = rmlts)
      }
    }
  }
  
  ##### create data frame  #####
  
  #### 0.1 check if variable can only be defined from stress period data ####
  if(!(variable %in% c('surf', 'exdp'))){
    
    #### 1. parameters defined  ####
    if(!is.null(evt$npevt) && evt$npevt > 0){   
      
      if(is.null(zon) && !all(unlist(evt$zonarr)=='ALL')) stop('Please specify an RMODFLOW zon object')
      if(is.null(mlt) && !all(unlist(evt$mltarr)=='NONE')) stop('Please specify an RMODFLOW mlt object')
      
      ### 1.1 for stress period data ###
      if(!all_parm && evt$npevt > 0){ 
        
        ## 1.1.1 stress data with time-varying parameters ##
        if(!is.null(evt$iname) && !(is.null(evt$iname[[sp]]) || all(is.na(unlist(evt$iname[[sp]]))))){ 
          
          sp_new = tail(subset(which(evt$inevtr > 0), which(evt$inevtr > 0) <= sp), 1)  # set stress period to last stress period with inevtr >= 0 before current stress period
          parnams = evt$pname[[sp_new]]
          instnum = unlist(lapply(1:evt$inevtr[sp_new], function(x) which(evt$instnam[[which(evt$parnam==parnams[x])]] == evt$iname[[sp_new]][x])))
          
          # lists with evt$npevt elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
          zones = lapply(1:evt$inevtr[sp_new], function(x) lapply(1:length(evt$zonarr[[which(evt$parnam == parnams[x])]][instnum[x],]), function(y)  if(evt$zonarr[[which(evt$parnam == parnams[x])]][instnum[x],y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == evt$zonarr[[which(evt$parnam == parnams[x])]][instnum[x],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
          multp = lapply(1:evt$inevtr[sp_new], function(x) lapply(1:length(evt$mltarr[[which(evt$parnam == parnams[x])]][instnum[x],]), function(y)  if(evt$mltarr[[which(evt$parnam == parnams[x])]][instnum[x],y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == evt$mltarr[[which(evt$parnam == parnams[x])]][instnum[x],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
          
          ## 1.1.2  stress data with non-time-varying parameters ##
        } else {  
          
          sp_new = tail(subset(which(evt$inevtr > 0), which(evt$inevtr > 0) <= sp), 1)  # set stress period to last stress period with inevtr >= 0 before current stress period
          parnams = evt$pname[[sp_new]]
          
          # lists with evt$npevt elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
          zones = lapply(1:evt$inevtr[sp_new], function(x) lapply(1:length(evt$zonarr[[which(evt$parnam == parnams[x])]][1,]), function(y)  if(evt$zonarr[[which(evt$parnam == parnams[x])]][1,y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == evt$zonarr[[which(evt$parnam == parnams[x])]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
          multp = lapply(1:evt$inevtr[sp_new], function(x) lapply(1:length(evt$mltarr[[which(evt$parnam == parnams[x])]][1,]), function(y)  if(evt$mltarr[[which(evt$parnam == parnams[x])]][1,y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == evt$mltarr[[which(evt$parnam == parnams[x])]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
          
        }
        
        npevt = evt$inevtr[sp_new]
        parnam = evt$pname[[sp_new]]
        parval = evt$parval[which(evt$parnam %in% evt$pname[[sp_new]])]
        
        ### 1.2  for parameter data ###
      } else if((all_parm && evt$npevt > 0) ){ 
        
        ## 1.2.1 not time-varying parameter data ##
        if(is.null(instnum)){      
          
          # lists with evt$npevt elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
          zones = lapply(1:evt$npevt, function(x) lapply(1:length(evt$zonarr[[x]][1,]), function(y)  if(evt$zonarr[[x]][1,y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == evt$zonarr[[x]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
          multp = lapply(1:evt$npevt, function(x) lapply(1:length(evt$mltarr[[x]][1,]), function(y)  if(evt$mltarr[[x]][1,y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == evt$mltarr[[x]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
          
          ## 1.2.2 time varying parameter data ##
        } else if(!is.null(instnum)){
          
          # lists with evt$npevt elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
          zones = lapply(1:evt$npevt, function(x) lapply(1:length(evt$zonarr[[x]][evt$instnam[[x]][instnum[x]],]), function(y)  if(evt$zonarr[[x]][evt$instnam[[x]][instnum[x]],y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == evt$zonarr[[x]][evt$instnam[[x]][instnum[x]],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
          multp = lapply(1:evt$npevt, function(x) lapply(1:length(evt$mltarr[[x]][evt$instnam[[x]][instnum[x]],]), function(y)  if(evt$mltarr[[x]][evt$instnam[[x]][instnum[x]],y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == evt$mltarr[[x]][evt$instnam[[x]][instnum[x]],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
          
        }
        npevt = evt$npevt
        parnam = evt$parnam
        parval = evt$parval
      }
      
      # if multiple clusters are used (rarely the case), than bind these clusters to 1 array [needs to be tested more]
      zones = lapply(zones, function(x) rmf_create_array(unlist(x), dim=c(dis$nrow, dis$ncol)) )
      multp = lapply(multp, function(x) rmf_create_array(unlist(x), dim=c(dis$nrow, dis$ncol)) )
      
      # for each parameter, create an array with final parameter values for the active cells defined by the corresponding zone array and multiply them with the corresponding multiplier array
      for(i in 1:npevt){
        
        if(!is.character(zones[[i]])){ # zone
          if(!is.character(multp[[i]])){ # zone + multiplier
            zones[[i]][which(!(zones[[i]] %in% as.numeric(strsplit(as.character(evt$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = NA
            zones[[i]][which((zones[[i]] %in% as.numeric(strsplit(as.character(evt$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = evt$parval[i]*multp[[i]][which(zones[[i]] %in% as.numeric(strsplit(as.character(evt$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]]))]
            
          } else { # zone no multiplier
            zones[[i]][which(!(zones[[i]] %in% as.numeric(strsplit(as.character(evt$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = NA
            zones[[i]][which((zones[[i]] %in% as.numeric(strsplit(as.character(evt$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = evt$parval[i]
          }
        } else { # no zone
          if(!is.character(multp[[i]])){ # no zone + multiplier
            zones[[i]] = evt$parval[i]*multp[[i]]
          } else { # no zone + no multiplier
            zones[[i]] = rmf_create_array(evt$parval[i], dim=c(dis$nrow, dis$ncol))
          }
        } 
        
      }
      
      # create data frame from npevt arrays
      evt_df = data.frame(id = rep((1:(dis$nrow*dis$ncol)), npevt), 
                          parnam = rep(parnam, each=dis$nrow*dis$ncol),
                          parval=rep(parval, each=dis$nrow*dis$ncol),
                          evt = unlist(zones))
      evt_df = cbind(evt_df, rmf_convert_id_to_ijk(evt_df$id, dis=dis, type='modflow'))
      
      # # add multiplier and zone names
      # if(variable %in% c('zonnam', 'mltnam')){
      #   if(any(evt$nclu > 1)){
      #     stop('Plotting of multiple zone/multiplier names for a single parameter not yet implemented. Please specify a different variable to plot.')
      #   } else {
      #     evt_df = cbind(evt_df, )
      #   }
      # } else if(variable == 'iz'){
      #   
      # } 
      
      # remove NA rows
      evt_df = na.omit(evt_df)
      
    } else { 
      #### 2. no parameters defined (so stress period data) ####
      
      if(variable %in% c('parnam', 'parval', 'instnam', 'mltnam', 'zonnam', 'iz')){
        stop('No parameter data specified for this stress period. Please choose a different variable to plot.')
      }
      sp_new = tail(subset(which(evt$inevtr >= 0), which(evt$inevtr >= 0) <= sp), 1)  # set stress period to last stress period with inevtr >= 0 before current stress period
      evtr = rmf_create_array(evt$evtr[,,sp_new], dim=c(dis$nrow, dis$ncol))
      evt_df = data.frame(id = 1:(dis$nrow*dis$ncol), 
                          evt = unlist(evtr))
      evt_df = cbind(evt_df, rmf_convert_id_to_ijk(evt_df$id, dis=dis, type='modflow'))
    } 
  } else {
    #### 0.2 variables can only be defined from stress period data ####
    
    if(variable == 'surf'){
      sp_new = tail(subset(which(evt$insurf >= 0), which(evt$insurf >= 0) <= sp), 1)  # set stress period to last stress period with insurf >= 0 before current stress period
      surf = rmf_create_array(evt$surf[,,sp_new])
      evt_df = data.frame(id = 1:(dis$nrow*dis$ncol),
                          surf = unlist(surf))
      evt_df = cbind(evt_df, rmf_convert_id_to_ijk(evt_df$id, dis=dis, type='modflow'))
    } else if(variable == 'exdp'){
      sp_new = tail(subset(which(evt$inexdp >= 0), which(evt$inexdp >= 0) <= sp), 1)  # set stress period to last stress period with inexdp >= 0 before current stress period
      exdp = rmf_create_array(evt$exdp[,,sp_new])
      evt_df = data.frame(id = 1:(dis$nrow*dis$ncol),
                          exdp = unlist(exdp))
      evt_df = cbind(evt_df, rmf_convert_id_to_ijk(evt_df$id, dis=dis, type='modflow'))
    }
  } 
  
  
  
  ##### transform data frame of parameter data into rmf_array #####
  
  # adjust id for nevtop != 1
  if(evt$nevtop == 2){
    sp_new = tail(subset(which(evt$inievt >= 0), which(evt$inievt >= 0) <= sp), 1)  # set stress period to last stress period with inievt >= 0 before current stress period
    ievt = evt$ievt[,,sp_new]
    evt_df$k = ievt[evt_df$id]
    
  } else if(evt$nevtop == 3 ){
    if(!('bas' %in% names(list(...))) ){
      warning('nevtop = 3; please specify an RMODFLOW bas object to correctly plot the highest active evapotranspiration cells')
    } else {
      
      bas = list(...)$bas
      
      # highest active cells
      high_act = data.frame(active = c(bas$ibound), id = 1:(dis$nrow*dis$ncol*dis$nlay))
      high_act = cbind(high_act, rmf_convert_id_to_ijk(id=high_act$id, dis=dis, type='modflow'))
      high_act$id2d = rep(1:(dis$nrow*dis$ncol), dis$nlay)
      high_act = subset(high_act, high_act$active != 0)
      high_act = do.call(rbind, lapply(split(high_act, high_act$id2d), function(x) x[which.min(x$k), c('id','i','j','k', 'id2d')]))
      high_act = high_act[order(high_act$id),]
      
      # create new id2d column in evt_df and merge with high_active$k on that column
      evt_df$id2d = rmf_convert_ijk_to_id(i=evt_df$i, j=evt_df$j, k=1, dis=dis, type='modflow')
      evt_df = evt_df[which(evt_df$id2d %in% high_act$id2d),] # only use cells that are defined in bas$ibound (via high_act)
      evt_df$k = high_act$k[match(evt_df$id2d, high_act$id2d)] # replace values
      
    }
  }
  
  id =  rmf_convert_ijk_to_id(i=evt_df$i, j=evt_df$j, k=evt_df$k, dis=dis, type='modflow')
  
  # additive parameters (can be a lot less verbose with dplyr)
  if(variable == 'evt' && any(duplicated(id))){
    
    evt_df$id = id
    id_dupl = id[duplicated(id)]
    evt_df_dupl = evt_df[id %in% id_dupl,]
    aggr = aggregate(evt_df_dupl[[variable]], by=list(evt_df_dupl$id), FUN=sum)
    
    for(i in 1:nrow(aggr)){
      evt_df[id==aggr[i, 1], variable] = aggr[i, 2]
    }
    
    evt_df = evt_df[!duplicated(id),]
    id =  rmf_convert_ijk_to_id(i=evt_df$i, j=evt_df$j, k=evt_df$k, dis=dis, type='modflow')
    
  }
  
  # create rmf_array
  rmf_array = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    rmf_array[id] = 1
  }  else if(variable %in% c('parnam', 'instnam', 'mltnam', 'zonnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
    names = factor(rep(seq_along(unique(evt_df[,variable])), as.vector(table(factor(evt_df[,variable], levels=as.character(unique(evt_df[,variable])))))), labels = unique(evt_df[,variable]))
    rmf_array[id] = names
  } else {
    rmf_array[id] = unlist(evt_df[variable])
  }
  
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam', 'mltnam', 'zonnam'))  rmf_plot(rmf_array, dis=dis, type='factor', levels=levels(names), ...) else rmf_plot(rmf_array, dis=dis, ...) 
  
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
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
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
                         active_only = FALSE,
                         fun = ifelse(variable == 'bhead', mean, sum),
                         ...) {
  
  rmfi_plot_bc(obj = ghb, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
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
#' @param levels labels that should be used on the factor legend; huf$hgunam is used by default
#' @param grid logical; should grid lines be plotted? alternatively, provide colour of the grid lines.
#' @param title plot title
#' @param hed hed object for only plotting the saturated part of the grid; possibly subsetted with time step number; by default, last time step is used
#' @param l time step number for subsetting the hed object
#' @param ... parameters provided to plot.rmf_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmf_plot huf
#' @export
rmf_plot.huf <- function(huf,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         dis,
                         bas = NULL,
                         #mask = rmfi_ifelse0(is.null(bas),array*0+1,bas$ibound),
                         #zlim = range(array[rmfi_ifelse0(is.null(i),c(1:dim(array)[1]),i),rmfi_ifelse0(is.null(j),c(1:dim(array)[2]),j),rmfi_ifelse0(is.null(k),c(1:dim(array)[3]),k)][as.logical(mask[rmfi_ifelse0(is.null(i),c(1:dim(array)[1]),i),rmfi_ifelse0(is.null(j),c(1:dim(array)[2]),j),rmfi_ifelse0(is.null(k),c(1:dim(array)[3]),k)])], finite=TRUE),
                         colour_palette = rmfi_rev_rainbow,
                         nlevels = 7,
                         levels = huf$hgunam,
                         type='fill',
                         grid = FALSE,
                         add=FALSE,
                         title = NULL,
                         hed = NULL,
                         l = NULL,
                         ...) {
  hufdis <- rmf_convert_huf_to_dis(huf = huf, dis = dis)
  huf_array <- rmf_create_array(rep(1:huf$nhuf,each=dis$nrow*dis$ncol),dim=c(dis$nrow,dis$ncol,huf$nhuf))
  p <- rmf_plot(huf_array, dis = hufdis, i=i,j=j,k=k,colour_palette=colour_palette,nlevels=nlevels,type='factor',add=add,title=title,levels=levels)
  if(grid == TRUE) {
    return(p + rmf_plot(dis$botm, dis = dis, i=i,j=j,k=k,bas=bas,type='grid',add=TRUE))
  } else if(grid == 'huf') {
    return(p + rmf_plot(hufdis$botm, dis = hufdis, i=i,j=j,k=k,type='grid',add=TRUE))
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

#' Plot a 2D section of an RMODFLOW rch object
#' 
#' \code{rmf_plot.rch} plots a 2D section of an \code{RMODFLOW} rch object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param rch an \code{RMODFLOW} rch object
#' @param dis an \code{RMODFLOW} dis object
#' @param mlt optional; an \code{RMODFLOW} mlt object; used for plotting parameter data
#' @param zon optional; an \code{RMODFLOW} zon object; used for plotting parameter data
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (plots the recharge cells' locations), "recharge" (default) "mltnam" (for parameter data), "zonnam" (for parameter data), "iz" (for parameter data), "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "recharge"
#' @param instnum numeric vector of length \code{nprch} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot rch

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.rch = function(rch,
                        dis,
                        mlt = NULL,
                        zon = NULL,
                        all_parm = F, 
                        variable = 'recharge',
                        instnum = NULL, 
                        l = NULL, 
                        sp = NULL, 
                        ... 
){
  
  # set stress period
  if(is.null(l) && is.null(sp) && !all_parm){
    warning('No stress period or time step defined; setting time step to last time step and setting stress period accordingly')
    l = tail(cumsum(dis$nstp), 1)
  } 
  if(is.null(sp)) sp = tail(which(cumsum(dis$nstp) <= l), 1)
  
  if(all_parm && (is.null(rch$nprch) || (!is.null(rch$nprch) && rch$nprch == 0))) stop('RMODFLOW rch object does not have parameters. Please specify parameters or set all_parm to FALSE')
  
  ##### calculate new multiplier arrays if FUNCTION is specified in mlt ####
  if(!all(unlist(rch$mltarr) == "NONE") && (!is.null(mlt) && !is.null(mlt$functn) && any(mlt$functn) )  ){
    
    rmlts = lapply(seq_along(dim(mlt$rmlt)), function(x) mlt$rmlt[,,x])
    names(rmlts) = mlt$mltnam
    
    for(i in 1:mlt$nml){
      if(!is.null(mlt$functn) && mlt$functn[i]){
        funct = strsplit(mlt$operator[[i]], split=' ')[[1]]
        for(i in 1:((length(funct)-1)/2)){
          funct = append(append('(', funct), ')', after=i*1+3*i)
        }
        mlt$rmlt[,,i] = eval(parse(text=funct), envir = rmlts)
      }
    }
  }
  
  ##### create data frame  #####
  
  #### 1. parameters defined  ####
  if(!is.null(rch$nprch) && rch$nprch > 0){   
    
    if(is.null(zon) && !all(unlist(rch$zonarr)=='ALL')) stop('Please specify an RMODFLOW zon object')
    if(is.null(mlt) && !all(unlist(rch$mltarr)=='NONE')) stop('Please specify an RMODFLOW mlt object')
    
    ### 1.1 for stress period data ###
    if(!all_parm && rch$nprch > 0){ 
      
      ## 1.1.1 stress data with time-varying parameters ##
      if(!is.null(rch$iname) && !(is.null(rch$iname[[sp]]) || all(is.na(unlist(rch$iname[[sp]]))))){ 
        
        sp_new = tail(subset(which(rch$inrech > 0), which(rch$inrech > 0) <= sp), 1)  # set stress period to last stress period with inrech > 0 before current stress period
        parnams = rch$pname[[sp_new]]
        instnum = unlist(lapply(1:rch$inrech[sp_new], function(x) which(rch$instnam[[which(rch$parnam==parnams[x])]] == rch$iname[[sp_new]][x])))
        
        # lists with rch$nprch elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
        zones = lapply(1:rch$inrech[sp_new], function(x) lapply(1:length(rch$zonarr[[which(rch$parnam == parnams[x])]][instnum[x],]), function(y)  if(rch$zonarr[[which(rch$parnam == parnams[x])]][instnum[x],y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == rch$zonarr[[which(rch$parnam == parnams[x])]][instnum[x],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
        multp = lapply(1:rch$inrech[sp_new], function(x) lapply(1:length(rch$mltarr[[which(rch$parnam == parnams[x])]][instnum[x],]), function(y)  if(rch$mltarr[[which(rch$parnam == parnams[x])]][instnum[x],y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == rch$mltarr[[which(rch$parnam == parnams[x])]][instnum[x],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
        
        ## 1.1.2  stress data with non-time-varying parameters ##
      } else {  
        
        sp_new = tail(subset(which(rch$inrech > 0), which(rch$inrech > 0) <= sp), 1)  # set stress period to last stress period with inrech > 0 before current stress period
        parnams = rch$pname[[sp_new]]
        
        # lists with rch$nprch elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
        zones = lapply(1:rch$inrech[sp_new], function(x) lapply(1:length(rch$zonarr[[which(rch$parnam == parnams[x])]][1,]), function(y)  if(rch$zonarr[[which(rch$parnam == parnams[x])]][1,y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == rch$zonarr[[which(rch$parnam == parnams[x])]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
        multp = lapply(1:rch$inrech[sp_new], function(x) lapply(1:length(rch$mltarr[[which(rch$parnam == parnams[x])]][1,]), function(y)  if(rch$mltarr[[which(rch$parnam == parnams[x])]][1,y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == rch$mltarr[[which(rch$parnam == parnams[x])]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
        
      }
      
      nprch = rch$inrech[sp_new]
      parnam = rch$pname[[sp_new]]
      parval = rch$parval[which(rch$parnam %in% rch$pname[[sp_new]])]
      
      ### 1.2  for parameter data ###
    } else if((all_parm && rch$nprch > 0) ){ 
      
      ## 1.2.1 not time-varying parameter data ##
      if(is.null(instnum)){      
        
        # lists with rch$nprch elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
        zones = lapply(1:rch$nprch, function(x) lapply(1:length(rch$zonarr[[x]][1,]), function(y)  if(rch$zonarr[[x]][1,y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == rch$zonarr[[x]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
        multp = lapply(1:rch$nprch, function(x) lapply(1:length(rch$mltarr[[x]][1,]), function(y)  if(rch$mltarr[[x]][1,y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == rch$mltarr[[x]][1,y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
        
        ## 1.2.2 time varying parameter data ##
      } else if(!is.null(instnum)){
        
        # lists with rch$nprch elements where each element specifies the multiplier and zone arrays; obtained from provided mlt and zon objects
        zones = lapply(1:rch$nprch, function(x) lapply(1:length(rch$zonarr[[x]][rch$instnam[[x]][instnum[x]],]), function(y)  if(rch$zonarr[[x]][rch$instnam[[x]][instnum[x]],y] != "ALL")  rmf_create_array(zon$izon[,,which(zon$zonnam == rch$zonarr[[x]][rch$instnam[[x]][instnum[x]],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("ALL", dim=c(dis$nrow, dis$ncol)) ))
        multp = lapply(1:rch$nprch, function(x) lapply(1:length(rch$mltarr[[x]][rch$instnam[[x]][instnum[x]],]), function(y)  if(rch$mltarr[[x]][rch$instnam[[x]][instnum[x]],y] != "NONE") rmf_create_array(mlt$rmlt[,,which(mlt$mltnam == rch$mltarr[[x]][rch$instnam[[x]][instnum[x]],y])], dim=c(dis$nrow, dis$ncol)) else rmf_create_array("NONE", dim=c(dis$nrow, dis$ncol)) ))
        
      }
      nprch = rch$nprch
      parnam = rch$parnam
      parval = rch$parval
    }
    
    # if multiple clusters are used (rarely the case), than bind these clusters to 1 array [needs to be tested more]
    zones = lapply(zones, function(x) rmf_create_array(unlist(x), dim=c(dis$nrow, dis$ncol)) )
    multp = lapply(multp, function(x) rmf_create_array(unlist(x), dim=c(dis$nrow, dis$ncol)) )
    
    # for each parameter, create an array with final parameter values for the active cells defined by the corresponding zone array and multiply them with the corresponding multiplier array
    for(i in 1:nprch){
      
      if(!is.character(zones[[i]])){ # zone
        if(!is.character(multp[[i]])){ # zone + multiplier
          zones[[i]][which(!(zones[[i]] %in% as.numeric(strsplit(as.character(rch$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = NA
          zones[[i]][which((zones[[i]] %in% as.numeric(strsplit(as.character(rch$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = rch$parval[i]*multp[[i]][which(zones[[i]] %in% as.numeric(strsplit(as.character(rch$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]]))]
          
        } else { # zone no multiplier
          zones[[i]][which(!(zones[[i]] %in% as.numeric(strsplit(as.character(rch$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = NA
          zones[[i]][which((zones[[i]] %in% as.numeric(strsplit(as.character(rch$iz[[i]][if(is.null(instnum)) 1 else instnum[i],]), split=' ')[[1]])))] = rch$parval[i]
        }
      } else { # no zone
        if(!is.character(multp[[i]])){ # no zone + multiplier
          zones[[i]] = rch$parval[i]*multp[[i]]
        } else { # no zone + no multiplier
          zones[[i]] = rmf_create_array(rch$parval[i], dim=c(dis$nrow, dis$ncol))
        }
      } 
      
    }
    
    # create data frame from nprch arrays
    rch_df = data.frame(id = rep((1:(dis$nrow*dis$ncol)), nprch), 
                        parnam = rep(parnam, each=dis$nrow*dis$ncol),
                        parval=rep(parval, each=dis$nrow*dis$ncol),
                        recharge = unlist(zones))
    rch_df = cbind(rch_df, rmf_convert_id_to_ijk(rch_df$id, dis=dis, type='modflow'))
    
    # # add multiplier and zone names
    # if(variable %in% c('zonnam', 'mltnam')){
    #   if(any(rch$nclu > 1)){
    #     stop('Plotting of multiple zone/multiplier names for a single parameter not yet implemented. Please specify a different variable to plot.')
    #   } else {
    #     rch_df = cbind(rch_df, )
    #   }
    # } else if(variable == 'iz'){
    #   
    # } 
    
    # remove NA rows
    rch_df = na.omit(rch_df)
    
  } else { 
    #### 2. no parameters defined (so stress period data) ####
    
    if(variable %in% c('parnam', 'parval', 'instnam', 'mltnam', 'zonnam', 'iz')){
      stop('No parameter data specified for this stress period. Please choose a different variable to plot.')
    }
    sp_new = tail(subset(which(rch$inrech >= 0), which(rch$inrech >= 0) <= sp), 1)  # set stress period to last stress period with inrech >= 0 before current stress period
    rech = rmf_create_array(rch$rech[,,sp_new], dim=c(dis$nrow, dis$ncol))
    rch_df = data.frame(id = 1:(dis$nrow*dis$ncol), 
                        recharge = unlist(rech))
    rch_df = cbind(rch_df, rmf_convert_id_to_ijk(rch_df$id, dis=dis, type='modflow'))
  } 
  
  
  ##### transform data frame of parameter data into rmf_array #####
  
  # adjust id for nrchop != 1
  if(rch$nrchop == 2){
    sp_new = tail(subset(which(rch$inirch >= 0), which(rch$inirch >= 0) <= sp), 1)  # set stress period to last stress period with inirch >= 0 before current stress period
    irch = rch$irch[,,sp_new]
    rch_df$k = irch[rch_df$id]
    
  } else if(rch$nrchop == 3 ){
    if(!("bas" %in% names(list(...)) )){
      warning('nrchop = 3; please specify an RMODFLOW bas object to correctly plot the highest active recharge cells')
    } else {
      
      bas = list(...)$bas
      
      # highest active cells
      high_act = data.frame(active = c(bas$ibound), id = 1:(dis$nrow*dis$ncol*dis$nlay))
      high_act = cbind(high_act, rmf_convert_id_to_ijk(id=high_act$id, dis=dis, type='modflow'))
      high_act$id2d = rep(1:(dis$nrow*dis$ncol), dis$nlay)
      high_act = subset(high_act, high_act$active != 0)
      high_act = do.call(rbind, lapply(split(high_act, high_act$id2d), function(x) x[which.min(x$k), c('id','i','j','k', 'id2d')]))
      high_act = high_act[order(high_act$id),]
      
      # create new id2d column in rch_df and merge with high_active$k on that column
      rch_df$id2d = rmf_convert_ijk_to_id(i=rch_df$i, j=rch_df$j, k=1, dis=dis, type='modflow')
      rch_df = rch_df[which(rch_df$id2d %in% high_act$id2d),] # only use cells that are defined in bas$ibound (via high_act)
      rch_df$k = high_act$k[match(rch_df$id2d, high_act$id2d)] # replace values
      
    }
  }
  
  id =  rmf_convert_ijk_to_id(i=rch_df$i, j=rch_df$j, k=rch_df$k, dis=dis, type='modflow')
  
  # additive parameters (can be a lot less verbose with dplyr)
  if(variable == 'recharge' && any(duplicated(id))){
    
    rch_df$id = id
    id_dupl = id[duplicated(id)]
    rch_df_dupl = rch_df[id %in% id_dupl,]
    aggr = aggregate(rch_df_dupl[[variable]], by=list(rch_df_dupl$id), FUN=sum)
    
    for(i in 1:nrow(aggr)){
      rch_df[id==aggr[i, 1], variable] = aggr[i, 2]
    }
    
    rch_df = rch_df[!duplicated(id),]
    id =  rmf_convert_ijk_to_id(i=rch_df$i, j=rch_df$j, k=rch_df$k, dis=dis, type='modflow')
    
  }
  
  # create rmf_array
  rmf_array = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    rmf_array[id] = 1
  }  else if(variable %in% c('parnam', 'instnam', 'mltnam', 'zonnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
    names = factor(rep(seq_along(unique(rch_df[,variable])), as.vector(table(factor(rch_df[,variable], levels=as.character(unique(rch_df[,variable])))))), labels = unique(rch_df[,variable]))
    rmf_array[id] = names
  } else {
    rmf_array[id] = unlist(rch_df[variable])
  }
  
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam', 'mltnam', 'zonnam'))  rmf_plot(rmf_array, dis=dis, type='factor', levels=levels(names), ...) else rmf_plot(rmf_array, dis=dis, ...) 
  
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
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
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
                         active_only = FALSE,
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
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask)
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
                                  crop = FALSE) {
  
  
  
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
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=factor(value), group=id),data=datapoly,alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_discrete('value',labels=rmfi_ifelse0(is.null(levels),levels(factor(datapoly$value)),levels), na.value = NA)))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=factor(value), group=id),alpha=alpha, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_discrete('value',labels=rmfi_ifelse0(is.null(levels),levels(factor(datapoly$value)),levels), na.value = NA) +
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
        ylim = c(min(xy$y, na.rm = T), max(xy$xy, na.rm = T))
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
#' @param crop logical; should plot be cropped by dropping NA values (as set by mask)
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
                                  crop = FALSE,
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
      if(add) {
        return(list(ggplot2::geom_polygon(ggplot2::aes(x=x,y=y,fill=factor(value), group=id),data=datapoly, colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))),
                    ggplot2::scale_fill_discrete('value',labels=rmfi_ifelse0(is.null(levels),levels(factor(datapoly$value)),levels), na.value = NA)))
      } else {
        return(ggplot2::ggplot(datapoly, ggplot2::aes(x=x, y=y)) +
                 ggplot2::geom_polygon(ggplot2::aes(fill=factor(value), group=id), colour = ifelse(gridlines==TRUE,'black',ifelse(gridlines==FALSE,NA,gridlines))) +
                 ggplot2::scale_fill_discrete('value',labels=rmfi_ifelse0(is.null(levels),levels(factor(datapoly$value)),levels), na.value = NA) +
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
#' @param active_only logical; indicating if only the active cells should be plotted. Non-active cells are set to NA. Defaults to FALSE.
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
                         active_only = FALSE,
                         fun = sum,
                         ...) {
  
  rmfi_plot_bc(obj = wel, dis = dis, kper = kper, variable = variable, i=i, j=j, k=k, active_only = active_only, fun = fun, ...)
  
}
