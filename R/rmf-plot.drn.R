#' Plot a 2D section of an RMODFLOW drn object
#' 
#' \code{rmf_plot.drn} plots a 2D section of an \code{RMODFLOW} drn object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param drn an \code{RMODFLOW} drn object
#' @param dis an \code{RMODFLOW} dis object
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (default; plots the drain locations), "layer", "row", "column", "elevation", "condfact" (for parameter data), "conductance", "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "identity"
#' @param instnum numeric vector of length \code{npdrn} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot drn

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.drn = function(drn,
                        dis,
                        all_parm = F, 
                        variable = 'identity',
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
  
  if(all_parm && (is.null(drn$npdrn) || (!is.null(drn$npdrn) && drn$npdrn == 0))) stop('RMODFLOW drn object does not have parameters. Please specify parameters or set all_parm to FALSE')
  
  
  ##### create data frame  #####
  
  
  if(all_parm){ #### plot only parameters (independent of stress period) ####
    if(is.null(instnum)){ # not time-varying
      drn_df = data.frame(layer = unlist(drn$layer_parm), row = unlist(drn$row_parm), column = unlist(drn$column_parm), elevation = unlist(drn$elevation_parm), condfact = unlist(drn$condfact_parm) )
      
      # add conductance values & parameter names & parameter values
      drn_df$conductance = unlist(lapply(seq_along(drn$condfact_parm), function(x) drn$parval[x]*drn$condfact_parm[[x]][1,]))
      drn_df$parnam = rep(drn$parnam, drn$nlst) # as.character(unlist(lapply(seq_along(drn$parnam), function(x) rep(drn$parnam[x], drn$nlst[x]))))   
      drn_df$parval = rep(drn$parval, drn$nlst) # as.numeric(unlist(lapply(seq_along(drn$parval), function(x) rep(drn$parval[x], drn$nlst[x]))))   
      
      
    } else { # time varying
      drn_df = data.frame(layer = unlist(lapply(seq_along(drn$layer_parm), function(x) drn$layer_parm[[x]][instnum[x],])), row = unlist(lapply(seq_along(drn$row_parm), function(x) drn$row_parm[[x]][instnum[x],])), column = unlist(lapply(seq_along(drn$column_parm), function(x) drn$column_parm[[x]][instnum[x],])), elevation = unlist(lapply(seq_along(drn$elevation_parm), function(x) drn$elevation_parm[[x]][instnum[x],])), condfact = unlist(lapply(seq_along(drn$condfact_parm), function(x) drn$condfact_parm[[x]][instnum[x],])) )
      
      # add conductance values & parameter names & instance names & parameter values
      drn_df$conductance = unlist(lapply(seq_along(drn$condfact_parm), function(x) drn$parval[x]*drn$condfact_parm[[x]][instnum[x],]))
      drn_df$parnam = rep(drn$parnam, drn$nlst)  # as.character(unlist(lapply(seq_along(drn$parnam), function(x) rep(drn$parnam[x], drn$nlst[x]))))
      drn_df$instnam = as.character(unlist(lapply(seq_along(drn$instnam)), function(x) rep(drn$instnam[[x]][instnum[x]], drn$nlst[x])))
      drn_df$parval = rep(drn$parval, drn$nlst)  # as.numeric(unlist(lapply(seq_along(drn$parval), function(x) rep(drn$parval[x], drn$nlst[x]))))  
      
    }
    
    
  } else { ####  data in use for the specified stress period ####
    
    if(drn$np[sp] > 0){ # parameter data in use
      
      # not time-varying
      if(is.null(drn$iname) || (!is.null(drn$iname) && (is.null(drn$iname[[sp]]) || all(is.na(unlist(drn$iname[[sp]])))) )){
        
        drn_df_parm = data.frame(layer = unlist(drn$layer_parm[which(drn$parnam %in% drn$pname[[sp]])]), row = unlist(drn$row_parm[which(drn$parnam %in% drn$pname[[sp]])]), column = unlist(drn$column_parm[which(drn$parnam %in% drn$pname[[sp]])]), elevation = unlist(drn$elevation_parm[which(drn$parnam %in% drn$pname[[sp]])]), condfact = unlist(drn$condfact_parm[which(drn$parnam %in% drn$pname[[sp]])]) )
        
        # add conductance values & parameter names & parameter values
        drn_df_parm$conductance = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$parval[which(drn$parnam == drn$pname[[sp]][x])]*unlist(drn$condfact_parm[[which(drn$parnam == drn$pname[[sp]][x])]][1,])))
        drn_df_parm$parnam = as.character(unlist(lapply(seq_along(drn$pname[[sp]]), function(x) rep(drn$pname[[sp]][x], drn$nlst[which(drn$parnam == drn$pname[[sp]][x])]) )))  # long code instead of a simple rep(drn$pname[[sp]], drn$nlst[which(drn$parnam %in% drn$pname[[sp]])]) because of possible ordering issues in drn$pname relative to drn$nlst
        drn_df_parm$parval = as.numeric(unlist(lapply(seq_along(drn$pname[[sp]]), function(x) rep(drn$parval[which(drn$parnam == drn$pname[[sp]][x])], drn$nlst[which(drn$parnam == drn$pname[[sp]][x])]) )))  
        
        
      } else { # time-varying
        drn_df_parm = data.frame(layer = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$layer_parm[[which(drn$parnam == drn$pname[[sp]][x])]][ifelse(is.null(drn$iname[[sp]][x]) || is.na(drn$iname[[sp]][x]), 1, which(drn$instnam[[which(drn$parnam == drn$pname[[sp]][x])]] == drn$iname[[sp]][x]) ), ])), row = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$row_parm[[which(drn$parnam == drn$pname[[sp]][x])]][ifelse(is.null(drn$iname[[sp]][x]) || is.na(drn$iname[[sp]][x]), 1, which(drn$instnam[[which(drn$parnam == drn$pname[[sp]][x])]] == drn$iname[[sp]][x]) ), ])), column = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$column_parm[[which(drn$parnam == drn$pname[[sp]][x])]][ifelse(is.null(drn$iname[[sp]][x]) || is.na(drn$iname[[sp]][x]), 1, which(drn$instnam[[which(drn$parnam == drn$pname[[sp]][x])]] == drn$iname[[sp]][x]) ), ])), elevation = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$elevation_parm[[which(drn$parnam == drn$pname[[sp]][x])]][ifelse(is.null(drn$iname[[sp]][x]) || is.na(drn$iname[[sp]][x]), 1, which(drn$instnam[[which(drn$parnam == drn$pname[[sp]][x])]] == drn$iname[[sp]][x]) ), ])), condfact = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$condfact_parm[[which(drn$parnam == drn$pname[[sp]][x])]][ifelse(is.null(drn$iname[[sp]][x]) || is.na(drn$iname[[sp]][x]), 1, which(drn$instnam[[which(drn$parnam == drn$pname[[sp]][x])]] == drn$iname[[sp]][x]) ), ])) )
        
        # add conductance values & parameter names & instance names & parameter values
        drn_df_parm$conductance = unlist(lapply(seq_along(drn$pname[[sp]]), function(x) drn$parval[which(drn$parnam == drn$pname[[sp]][x])]*unlist(drn$condfact_parm[[which(drn$parnam == drn$pname[[sp]][x])]][ifelse(is.null(drn$iname[[sp]][x]) || is.na(drn$iname[[sp]][x]), 1, which(drn$instnam[[which(drn$parnam == drn$pname[[sp]][x])]] == drn$iname[[sp]][x]) ), ])))
        drn_df_parm$parnam = as.character(unlist(lapply(seq_along(drn$pname[[sp]]), function(x) rep(drn$pname[[sp]][x], drn$nlst[which(drn$parnam == drn$pname[[sp]][x])]) )))  # long code instead of a simple rep(drn$pname[[sp]], drn$nlst[which(drn$parnam %in% drn$pname[[sp]])]) because of possible ordering issues in drn$pname relative to drn$nlst
        drn_df_parm$instnam = as.character(unlist(lapply(seq_along(drn$iname[[sp]]), function(x) rep(drn$iname[[sp]][x], drn$nlst[which(drn$parnam == drn$pname[[sp]][x])]) )))
        drn_df_parm$parval = as.numeric(unlist(lapply(seq_along(drn$pname[[sp]]), function(x) rep(drn$parval[which(drn$parnam == drn$pname[[sp]][x])], drn$nlst[which(drn$parnam == drn$pname[[sp]][x])]) )))  
        
      }
      
      drn_df_parm$type = 'parameter'
      
    } # non-parameter data in use
    if(drn$itmp[sp] != 0){
      sp = tail(subset(which(drn$itmp >= 0), which(drn$itmp >= 0) <= sp), 1)  # set stress period to last stress period with itmp >= 0 before current stress period
      
      if(drn$itmp[sp] > 0){
        drn_df_sp = data.frame(layer = unlist(drn$layer_sp[[sp]]), row = unlist(drn$row_sp[[sp]]), column = unlist(drn$column_sp[[sp]]), elevation = unlist(drn$elevation_sp[[sp]]), conductance = unlist(drn$cond_sp[[sp]]) )
        drn_df_sp$type = 'non-parameter'
      }
    } 
    
    # bind drn_df_parm & drn_df_sp into drn_df (check if they exist first)
    if(exists('drn_df_parm') && exists('drn_df_sp')){
      
      drn_df = rbind(drn_df_parm[colnames(drn_df_sp)], drn_df_sp)  # only use mutual column names. This will drop certain columns but only when both parameter AND non-parameter data is being used in the same stress period
      
    } else if(!exists('drn_df_parm')){
      drn_df = drn_df_sp
    } else if(!exists('drn_df_sp')){
      drn_df = drn_df_parm
    }
  }
  
  ##### transform data frame into rmf_array #####
  id =  rmf_convert_ijk_to_id(i=drn_df$row, j=drn_df$column, k=drn_df$layer, dis=dis, type='r')

  # additive parameters (can be a lot less verbose with dplyr)
  if(variable == 'conductance' && any(duplicated(id))){
    
    drn_df$id = id
    id_dupl = id[duplicated(id)]
    drn_df_dupl = drn_df[id %in% id_dupl,]
    aggr = aggregate(drn_df_dupl[[variable]], by=list(drn_df_dupl$id), FUN=sum)
    
    for(i in 1:nrow(aggr)){
      drn_df[id==aggr[i, 1], variable] = aggr[i, 2]
    }
    
    drn_df = drn_df[!duplicated(id),]
    id =  rmf_convert_ijk_to_id(i=drn_df$row, j=drn_df$column, k=drn_df$layer, dis=dis, type='r')
    
  }

  rmf_array = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    rmf_array[id] = 1
  }  else if(variable %in% c('parnam', 'instnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
    names = factor(rep(seq_along(unique(drn_df[,variable])), as.vector(table(factor(drn_df[,variable], levels=as.character(unique(drn_df[,variable])))))), labels = unique(drn_df[,variable]))
    rmf_array[id] = names
  } else {
    rmf_array[id] = unlist(drn_df[variable])
  }
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam'))  rmf_plot(rmf_array, dis=dis, type='factor', levels=levels(names), ...) else rmf_plot(rmf_array, dis=dis, ...)
  
}