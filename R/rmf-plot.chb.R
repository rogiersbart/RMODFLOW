#' Plot a 2D section of an RMODFLOW chd object
#' 
#' \code{rmf_plot.chd} plots a 2D section of an \code{RMODFLOW} chd object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param chd an \code{RMODFLOW} chd object
#' @param dis an \code{RMODFLOW} dis object
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (default; plots the constant-head cells' locations), "layer", "row", "column", "shead", "ehead", "shdfact" (for parameter data), "ehdfact" (for parameter data), "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "identity"
#' @param instnum numeric vector of length \code{npchd} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot chd

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.chd = function(chd,
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
  
  if(all_parm && (is.null(chb$npchb) || (!is.null(chb$npchb) && chb$npchb == 0))) stop('RMODFLOW chb object does not have parameters. Please specify parameters or set all_parm to FALSE')
  
  
  ##### create data frame  #####
  
  
  if(all_parm){ #### plot only parameters (independent of stress period) ####
    if(is.null(instnum)){ # not time-varying
      chd_df = data.frame(layer = unlist(chd$layer_parm), row = unlist(chd$row_parm), column = unlist(chd$column_parm), shdfact = unlist(chd$shdfact_parm), ehdfact = unlist(chd$ehdfact_parm) )
      
      # add head values & parameter names & parameter values
      chd_df$shead = unlist(lapply(seq_along(chd$shdfact_parm), function(x) chd$parval[x]*chd$shdfact_parm[[x]][1,]))
      chd_df$ehead = unlist(lapply(seq_along(chd$ehdfact_parm), function(x) chd$parval[x]*chd$ehdfact_parm[[x]][1,]))
      chd_df$parnam = rep(chd$parnam, chd$nlst) # as.character(unlist(lapply(seq_along(chd$parnam), function(x) rep(chd$parnam[x], chd$nlst[x]))))   
      chd_df$parval = rep(chd$parval, chd$nlst) # as.numeric(unlist(lapply(seq_along(chd$parval), function(x) rep(chd$parval[x], chd$nlst[x]))))   
      
      
    } else { # time varying
      chd_df = data.frame(layer = unlist(lapply(seq_along(chd$layer_parm), function(x) chd$layer_parm[[x]][instnum[x],])), row = unlist(lapply(seq_along(chd$row_parm), function(x) chd$row_parm[[x]][instnum[x],])), column = unlist(lapply(seq_along(chd$column_parm), function(x) chd$column_parm[[x]][instnum[x],])), shdfact = unlist(lapply(seq_along(chd$shdfact_parm), function(x) chd$shdfact_parm[[x]][instnum[x],])), ehdfact = unlist(lapply(seq_along(chd$ehdfact_parm), function(x) chd$ehdfact_parm[[x]][instnum[x],])) )
      
      # add head values & parameter names & instance names & parameter values
      chd_df$shead = unlist(lapply(seq_along(chd$shdfact_parm), function(x) chd$parval[x]*chd$shdfact_parm[[x]][instnum[x],]))
      chd_df$ehead = unlist(lapply(seq_along(chd$ehdfact_parm), function(x) chd$parval[x]*chd$ehdfact_parm[[x]][instnum[x],]))
      chd_df$parnam = rep(chd$parnam, chd$nlst)  # as.character(unlist(lapply(seq_along(chd$parnam), function(x) rep(chd$parnam[x], chd$nlst[x]))))
      chd_df$instnam = as.character(unlist(lapply(seq_along(chd$instnam)), function(x) rep(chd$instnam[[x]][instnum[x]], chd$nlst[x])))
      chd_df$parval = rep(chd$parval, chd$nlst)  # as.numeric(unlist(lapply(seq_along(chd$parval), function(x) rep(chd$parval[x], chd$nlst[x]))))  
      
    }
    
    
  } else { ####  data in use for the specified stress period ####
    
    if(chd$np[sp] > 0){ # parameter data in use
      
      # not time-varying
      if(is.null(chd$iname) || (!is.null(chd$iname) && (is.null(chd$iname[[sp]]) || all(is.na(unlist(chd$iname[[sp]])))) )){
        
        chd_df_parm = data.frame(layer = unlist(chd$layer_parm[which(chd$parnam %in% chd$pname[[sp]])]), row = unlist(chd$row_parm[which(chd$parnam %in% chd$pname[[sp]])]), column = unlist(chd$column_parm[which(chd$parnam %in% chd$pname[[sp]])]), shdfact = unlist(chd$shdfact_parm[which(chd$parnam %in% chd$pname[[sp]])]), ehdfact = unlist(chd$ehdfact_parm[which(chd$parnam %in% chd$pname[[sp]])]) )
        
        # add head values & parameter names & parameter values
        chd_df_parm$shead = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$parval[which(chd$parnam == chd$pname[[sp]][x])]*unlist(chd$shdfact_parm[[which(chd$parnam == chd$pname[[sp]][x])]][1,])))
        chd_df_parm$ehead = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$parval[which(chd$parnam == chd$pname[[sp]][x])]*unlist(chd$ehdfact_parm[[which(chd$parnam == chd$pname[[sp]][x])]][1,])))
        chd_df_parm$parnam = as.character(unlist(lapply(seq_along(chd$pname[[sp]]), function(x) rep(chd$pname[[sp]][x], chd$nlst[which(chd$parnam == chd$pname[[sp]][x])]) )))  # long code instead of a simple rep(chd$pname[[sp]], chd$nlst[which(chd$parnam %in% chd$pname[[sp]])]) because of possible ordering issues in chd$pname relative to chd$nlst
        chd_df_parm$parval = as.numeric(unlist(lapply(seq_along(chd$pname[[sp]]), function(x) rep(chd$parval[which(chd$parnam == chd$pname[[sp]][x])], chd$nlst[which(chd$parnam == chd$pname[[sp]][x])]) )))  
        
        
      } else { # time-varying
        chd_df_parm = data.frame(layer = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$layer_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])), row = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$row_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])), column = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$column_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])), shdfact = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$shdfact_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])), ehdfact = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$ehdfact_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])) )
        
        # add head values & parameter names & instance names & parameter values
        chd_df_parm$shead = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$parval[which(chd$parnam == chd$pname[[sp]][x])]*unlist(chd$shdfact_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])))
        chd_df_parm$ehead = unlist(lapply(seq_along(chd$pname[[sp]]), function(x) chd$parval[which(chd$parnam == chd$pname[[sp]][x])]*unlist(chd$ehdfact_parm[[which(chd$parnam == chd$pname[[sp]][x])]][ifelse(is.null(chd$iname[[sp]][x]) || is.na(chd$iname[[sp]][x]), 1, which(chd$instnam[[which(chd$parnam == chd$pname[[sp]][x])]] == chd$iname[[sp]][x]) ), ])))
        chd_df_parm$parnam = as.character(unlist(lapply(seq_along(chd$pname[[sp]]), function(x) rep(chd$pname[[sp]][x], chd$nlst[which(chd$parnam == chd$pname[[sp]][x])]) )))  # long code instead of a simple rep(chd$pname[[sp]], chd$nlst[which(chd$parnam %in% chd$pname[[sp]])]) because of possible ordering issues in chd$pname relative to chd$nlst
        chd_df_parm$instnam = as.character(unlist(lapply(seq_along(chd$iname[[sp]]), function(x) rep(chd$iname[[sp]][x], chd$nlst[which(chd$parnam == chd$pname[[sp]][x])]) )))
        chd_df_parm$parval = as.numeric(unlist(lapply(seq_along(chd$pname[[sp]]), function(x) rep(chd$parval[which(chd$parnam == chd$pname[[sp]][x])], chd$nlst[which(chd$parnam == chd$pname[[sp]][x])]) )))  
        
      }
      
      chd_df_parm$type = 'parameter'
      
    } # non-parameter data in use
    if(chd$itmp[sp] != 0){
      sp = tail(subset(which(chd$itmp >= 0), which(chd$itmp >= 0) <= sp), 1)  # set stress period to last stress period with itmp >= 0 before current stress period
      
      if(chd$itmp[sp] > 0){
        chd_df_sp = data.frame(layer = unlist(chd$layer_sp[[sp]]), row = unlist(chd$row_sp[[sp]]), column = unlist(chd$column_sp[[sp]]), shead = unlist(chd$shead_sp[[sp]]), ehead = unlist(chd$ehead_sp[[sp]]) )
        chd_df_sp$type = 'non-parameter'
      }
    } 
    
    # bind chd_df_parm & chd_df_sp into chd_df (check if they exist first)
    if(exists('chd_df_parm') && exists('chd_df_sp')){
      
      chd_df = rbind(chd_df_parm[colnames(chd_df_sp)], chd_df_sp)  # only use mutual column names. This will drop certain columns but only when both parameter AND non-parameter data is being used in the same stress period
      
    } else if(!exists('chd_df_parm')){
      chd_df = chd_df_sp
    } else if(!exists('chd_df_sp')){
      chd_df = chd_df_parm
    }
  }
  
  ##### transform data frame into rmf_array #####
  id =  rmf_convert_ijk_to_id(i=chd_df$row, j=chd_df$column, k=chd_df$layer, dis=dis, type='r')

  # additive parameters (can be a lot less verbose with dplyr)
  if(variable %in% c('shead', 'ehead') && any(duplicated(id))){
    
    chb_df$id = id
    id_dupl = id[duplicated(id)]
    chb_df_dupl = chb_df[id %in% id_dupl,]
    aggr = aggregate(chb_df_dupl[[variable]], by=list(chb_df_dupl$id), FUN=sum)
    
    for(i in 1:nrow(aggr)){
      chb_df[id==aggr[i, 1], variable] = aggr[i, 2]
    }
    
    chb_df = chb_df[!duplicated(id),]
    id =  rmf_convert_ijk_to_id(i=chb_df$row, j=chb_df$column, k=chb_df$layer, dis=dis, type='r')
    
  }

  rmf_array = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    rmf_array[id] = 1
  }  else if(variable %in% c('parnam', 'instnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
    names = factor(rep(seq_along(unique(chd_df[,variable])), as.vector(table(factor(chd_df[,variable], levels=as.character(unique(chd_df[,variable])))))), labels = unique(chd_df[,variable]))
    rmf_array[id] = names
  } else {
    rmf_array[id] = unlist(chd_df[variable])
  }
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam'))  rmf_plot(rmf_array, dis=dis, type='factor', levels=levels(names), ...) else rmf_plot(rmf_array, dis=dis, ...)
  
}