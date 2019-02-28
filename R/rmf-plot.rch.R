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