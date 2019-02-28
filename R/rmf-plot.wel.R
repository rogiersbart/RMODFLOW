#' Plot a 2D section of an RMODFLOW wel object
#' 
#' \code{rmf_plot.wel} plots a 2D section of an \code{RMODFLOW} wel object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param wel an \code{RMODFLOW} wel object
#' @param dis an \code{RMODFLOW} dis object
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (default; plots the well locations), "layer", "row", "column", "qfact" (for parameter data), "q", "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "identity"
#' @param instnum numeric vector of length \code{npwel} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot wel

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.wel = function(wel,
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
  
  if(all_parm && (is.null(wel$npwel) || (!is.null(wel$npwel) && wel$npwel == 0))) stop('RMODFLOW wel object does not have parameters. Please specify parameters or set all_parm to FALSE')
  
  ##### create data frame  #####
  
  
  if(all_parm){ #### plot only parameters (independent of stress period) ####
    if(is.null(instnum)){ # not time-varying
      wel_df = data.frame(layer = unlist(wel$layer_parm), row = unlist(wel$row_parm), column = unlist(wel$column_parm),  qfact = unlist(wel$qfact_parm) )
      
      # add q values & parameter names & parameter values
      wel_df$q = unlist(lapply(seq_along(wel$qfact_parm), function(x) wel$parval[x]*wel$qfact_parm[[x]][1,]))
      wel_df$parnam = rep(wel$parnam, wel$nlst) # as.character(unlist(lapply(seq_along(wel$parnam), function(x) rep(wel$parnam[x], wel$nlst[x]))))   
      wel_df$parval = rep(wel$parval, wel$nlst) # as.numeric(unlist(lapply(seq_along(wel$parval), function(x) rep(wel$parval[x], wel$nlst[x]))))   
      
      
    } else { # time varying
      wel_df = data.frame(layer = unlist(lapply(seq_along(wel$layer_parm), function(x) wel$layer_parm[[x]][instnum[x],])), row = unlist(lapply(seq_along(wel$row_parm), function(x) wel$row_parm[[x]][instnum[x],])), column = unlist(lapply(seq_along(wel$column_parm), function(x) wel$column_parm[[x]][instnum[x],])),  qfact = unlist(lapply(seq_along(wel$qfact_parm), function(x) wel$qfact_parm[[x]][instnum[x],])) )
      
      # add q values & parameter names & instance names & parameter values
      wel_df$q = unlist(lapply(seq_along(wel$qfact_parm), function(x) wel$parval[x]*wel$qfact_parm[[x]][instnum[x],]))
      wel_df$parnam = rep(wel$parnam, wel$nlst)  # as.character(unlist(lapply(seq_along(wel$parnam), function(x) rep(wel$parnam[x], wel$nlst[x]))))
      wel_df$instnam = as.character(unlist(lapply(seq_along(wel$instnam)), function(x) rep(wel$instnam[[x]][instnum[x]], wel$nlst[x])))
      wel_df$parval = rep(wel$parval, wel$nlst)  # as.numeric(unlist(lapply(seq_along(wel$parval), function(x) rep(wel$parval[x], wel$nlst[x]))))  
      
    }
    
    
  } else { ####  data in use for the specified stress period ####
    
    if(wel$np[sp] > 0){ # parameter data in use
      
      # not time-varying
      if(is.null(wel$iname) || (!is.null(wel$iname) && (is.null(wel$iname[[sp]]) || all(is.na(unlist(wel$iname[[sp]])))) )){
        
        wel_df_parm = data.frame(layer = unlist(wel$layer_parm[which(wel$parnam %in% wel$pname[[sp]])]), row = unlist(wel$row_parm[which(wel$parnam %in% wel$pname[[sp]])]), column = unlist(wel$column_parm[which(wel$parnam %in% wel$pname[[sp]])]) )
        
        # add q values & parameter names & parameter values
        wel_df_parm$q = unlist(lapply(seq_along(wel$pname[[sp]]), function(x) wel$parval[which(wel$parnam == wel$pname[[sp]][x])]*unlist(wel$qfact_parm[[which(wel$parnam == wel$pname[[sp]][x])]][1,])))
        wel_df_parm$parnam = as.character(unlist(lapply(seq_along(wel$pname[[sp]]), function(x) rep(wel$pname[[sp]][x], wel$nlst[which(wel$parnam == wel$pname[[sp]][x])]) )))  # long code instead of a simple rep(wel$pname[[sp]], wel$nlst[which(wel$parnam %in% wel$pname[[sp]])]) because of possible ordering issues in wel$pname relative to wel$nlst
        wel_df_parm$parval = as.numeric(unlist(lapply(seq_along(wel$pname[[sp]]), function(x) rep(wel$parval[which(wel$parnam == wel$pname[[sp]][x])], wel$nlst[which(wel$parnam == wel$pname[[sp]][x])]) )))  
        
        
      } else { # time-varying
        wel_df_parm = data.frame(layer = unlist(lapply(seq_along(wel$pname[[sp]]), function(x) wel$layer_parm[[which(wel$parnam == wel$pname[[sp]][x])]][ifelse(is.null(wel$iname[[sp]][x]) || is.na(wel$iname[[sp]][x]), 1, which(wel$instnam[[which(wel$parnam == wel$pname[[sp]][x])]] == wel$iname[[sp]][x]) ), ])), row = unlist(lapply(seq_along(wel$pname[[sp]]), function(x) wel$row_parm[[which(wel$parnam == wel$pname[[sp]][x])]][ifelse(is.null(wel$iname[[sp]][x]) || is.na(wel$iname[[sp]][x]), 1, which(wel$instnam[[which(wel$parnam == wel$pname[[sp]][x])]] == wel$iname[[sp]][x]) ), ])), column = unlist(lapply(seq_along(wel$pname[[sp]]), function(x) wel$column_parm[[which(wel$parnam == wel$pname[[sp]][x])]][ifelse(is.null(wel$iname[[sp]][x]) || is.na(wel$iname[[sp]][x]), 1, which(wel$instnam[[which(wel$parnam == wel$pname[[sp]][x])]] == wel$iname[[sp]][x]) ), ])), qfact = unlist(lapply(seq_along(wel$pname[[sp]]), function(x) wel$qfact_parm[[which(wel$parnam == wel$pname[[sp]][x])]][ifelse(is.null(wel$iname[[sp]][x]) || is.na(wel$iname[[sp]][x]), 1, which(wel$instnam[[which(wel$parnam == wel$pname[[sp]][x])]] == wel$iname[[sp]][x]) ), ])) )
        
        # add q values & parameter names & instance names & parameter values
        wel_df_parm$q = unlist(lapply(seq_along(wel$pname[[sp]]), function(x) wel$parval[which(wel$parnam == wel$pname[[sp]][x])]*unlist(wel$qfact_parm[[which(wel$parnam == wel$pname[[sp]][x])]][ifelse(is.null(wel$iname[[sp]][x]) || is.na(wel$iname[[sp]][x]), 1, which(wel$instnam[[which(wel$parnam == wel$pname[[sp]][x])]] == wel$iname[[sp]][x]) ), ])))
        wel_df_parm$parnam = as.character(unlist(lapply(seq_along(wel$pname[[sp]]), function(x) rep(wel$pname[[sp]][x], wel$nlst[which(wel$parnam == wel$pname[[sp]][x])]) )))  # long code instead of a simple rep(wel$pname[[sp]], wel$nlst[which(wel$parnam %in% wel$pname[[sp]])]) because of possible ordering issues in wel$pname relative to wel$nlst
        wel_df_parm$instnam = as.character(unlist(lapply(seq_along(wel$iname[[sp]]), function(x) rep(wel$iname[[sp]][x], wel$nlst[which(wel$parnam == wel$pname[[sp]][x])]) )))
        wel_df_parm$parval = as.numeric(unlist(lapply(seq_along(wel$pname[[sp]]), function(x) rep(wel$parval[which(wel$parnam == wel$pname[[sp]][x])], wel$nlst[which(wel$parnam == wel$pname[[sp]][x])]) )))  
        
      }
      
      wel_df_parm$type = 'parameter'
      
    } # non-parameter data in use
    if(wel$itmp[sp] != 0){
      sp = tail(subset(which(wel$itmp >= 0), which(wel$itmp >= 0) <= sp), 1)  # set stress period to last stress period with itmp >= 0 before current stress period
      
      if(wel$itmp[sp] > 0){
        wel_df_sp = data.frame(layer = unlist(wel$layer_sp[[sp]]), row = unlist(wel$row_sp[[sp]]), column = unlist(wel$column_sp[[sp]]), q = unlist(wel$q_sp[[sp]]) )
        wel_df_sp$type = 'non-parameter'
      }
    } 
    
    # bind wel_df_parm & wel_df_sp into wel_df (check if they exist first)
    if(exists('wel_df_parm') && exists('wel_df_sp')){
      
      wel_df = rbind(wel_df_parm[colnames(wel_df_sp)], wel_df_sp)  # only use mutual column names. This will drop certain columns but only when both parameter AND non-parameter data is being used in the same stress period
      
    } else if(!exists('wel_df_parm')){
      wel_df = wel_df_sp
    } else if(!exists('wel_df_sp')){
      wel_df = wel_df_parm
    }
  }
  
  ##### transform data frame into rmf_array #####
  id =  rmf_convert_ijk_to_id(i=wel_df$row, j=wel_df$column, k=wel_df$layer, dis=dis, type='r')

  # additive parameters (can be a lot less verbose with dplyr)
  if(variable == 'q' && any(duplicated(id))){
    
    wel_df$id = id
    id_dupl = id[duplicated(id)]
    wel_df_dupl = wel_df[id %in% id_dupl,]
    aggr = aggregate(wel_df_dupl[[variable]], by=list(wel_df_dupl$id), FUN=sum)
    
    for(i in 1:nrow(aggr)){
      wel_df[id==aggr[i, 1], variable] = aggr[i, 2]
    }
    
    wel_df = wel_df[!duplicated(id),]
    id =  rmf_convert_ijk_to_id(i=wel_df$row, j=wel_df$column, k=wel_df$layer, dis=dis, type='r')
    
  }

  rmf_array = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    rmf_array[id] = 1
  }  else if(variable %in% c('parnam', 'instnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
    names = factor(rep(seq_along(unique(wel_df[,variable])), as.vector(table(factor(wel_df[,variable], levels=as.character(unique(wel_df[,variable])))))), labels = unique(wel_df[,variable]))
    rmf_array[id] = names
  } else {
    rmf_array[id] = unlist(wel_df[variable])
  }
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam'))  rmf_plot(rmf_array, dis=dis, type='factor', levels=levels(names), ...) else rmf_plot(rmf_array, dis=dis, ...)
  
}