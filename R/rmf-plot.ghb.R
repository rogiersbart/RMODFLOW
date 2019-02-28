#' Plot a 2D section of an RMODFLOW ghb object
#' 
#' \code{rmf_plot.ghb} plots a 2D section of an \code{RMODFLOW} ghb object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param ghb an \code{RMODFLOW} ghb object
#' @param dis an \code{RMODFLOW} dis object
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (default; plots the head-boundary locations), "layer", "row", "column", "bhead", "condfact" (for parameter data), "conductance", "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "identity"
#' @param instnum numeric vector of length \code{npghb} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot ghb

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.ghb = function(ghb,
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
  
  if(all_parm && (is.null(ghb$npghb) || (!is.null(ghb$npghb) && ghb$npghb == 0))) stop('RMODFLOW ghb object does not have parameters. Please specify parameters or set all_parm to FALSE')
  
  
  ##### create data frame  #####
  
  
  if(all_parm){ #### plot only parameters (independent of stress period) ####
    if(is.null(instnum)){ # not time-varying
      ghb_df = data.frame(layer = unlist(ghb$layer_parm), row = unlist(ghb$row_parm), column = unlist(ghb$column_parm), bhead = unlist(ghb$bhead_parm), condfact = unlist(ghb$condfact_parm) )
      
      # add conductance values & parameter names & parameter values
      ghb_df$conductance = unlist(lapply(seq_along(ghb$condfact_parm), function(x) ghb$parval[x]*ghb$condfact_parm[[x]][1,]))
      ghb_df$parnam = rep(ghb$parnam, ghb$nlst) # as.character(unlist(lapply(seq_along(ghb$parnam), function(x) rep(ghb$parnam[x], ghb$nlst[x]))))   
      ghb_df$parval = rep(ghb$parval, ghb$nlst) # as.numeric(unlist(lapply(seq_along(ghb$parval), function(x) rep(ghb$parval[x], ghb$nlst[x]))))   
      
      
    } else { # time varying
      ghb_df = data.frame(layer = unlist(lapply(seq_along(ghb$layer_parm), function(x) ghb$layer_parm[[x]][instnum[x],])), row = unlist(lapply(seq_along(ghb$row_parm), function(x) ghb$row_parm[[x]][instnum[x],])), column = unlist(lapply(seq_along(ghb$column_parm), function(x) ghb$column_parm[[x]][instnum[x],])), bhead = unlist(lapply(seq_along(ghb$bhead_parm), function(x) ghb$bhead_parm[[x]][instnum[x],])), condfact = unlist(lapply(seq_along(ghb$condfact_parm), function(x) ghb$condfact_parm[[x]][instnum[x],])) )
      
      # add conductance values & parameter names & instance names & parameter values
      ghb_df$conductance = unlist(lapply(seq_along(ghb$condfact_parm), function(x) ghb$parval[x]*ghb$condfact_parm[[x]][instnum[x],]))
      ghb_df$parnam = rep(ghb$parnam, ghb$nlst)  # as.character(unlist(lapply(seq_along(ghb$parnam), function(x) rep(ghb$parnam[x], ghb$nlst[x]))))
      ghb_df$instnam = as.character(unlist(lapply(seq_along(ghb$instnam)), function(x) rep(ghb$instnam[[x]][instnum[x]], ghb$nlst[x])))
      ghb_df$parval = rep(ghb$parval, ghb$nlst)  # as.numeric(unlist(lapply(seq_along(ghb$parval), function(x) rep(ghb$parval[x], ghb$nlst[x]))))  
      
    }
    
    
  } else { ####  data in use for the specified stress period ####
    
    if(ghb$np[sp] > 0){ # parameter data in use
      
      # not time-varying
      if(is.null(ghb$iname) || (!is.null(ghb$iname) && (is.null(ghb$iname[[sp]]) || all(is.na(unlist(ghb$iname[[sp]])))) )){
        
        ghb_df_parm = data.frame(layer = unlist(ghb$layer_parm[which(ghb$parnam %in% ghb$pname[[sp]])]), row = unlist(ghb$row_parm[which(ghb$parnam %in% ghb$pname[[sp]])]), column = unlist(ghb$column_parm[which(ghb$parnam %in% ghb$pname[[sp]])]), bhead = unlist(ghb$bhead_parm[which(ghb$parnam %in% ghb$pname[[sp]])]), condfact = unlist(ghb$condfact_parm[which(ghb$parnam %in% ghb$pname[[sp]])]) )
        
        # add conductance values & parameter names & parameter values
        ghb_df_parm$conductance = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$parval[which(ghb$parnam == ghb$pname[[sp]][x])]*unlist(ghb$condfact_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][1,])))
        ghb_df_parm$parnam = as.character(unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) rep(ghb$pname[[sp]][x], ghb$nlst[which(ghb$parnam == ghb$pname[[sp]][x])]) )))  # long code instead of a simple rep(ghb$pname[[sp]], ghb$nlst[which(ghb$parnam %in% ghb$pname[[sp]])]) because of possible ordering issues in ghb$pname relative to ghb$nlst
        ghb_df_parm$parval = as.numeric(unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) rep(ghb$parval[which(ghb$parnam == ghb$pname[[sp]][x])], ghb$nlst[which(ghb$parnam == ghb$pname[[sp]][x])]) )))  
        
        
      } else { # time-varying
        ghb_df_parm = data.frame(layer = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$layer_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][ifelse(is.null(ghb$iname[[sp]][x]) || is.na(ghb$iname[[sp]][x]), 1, which(ghb$instnam[[which(ghb$parnam == ghb$pname[[sp]][x])]] == ghb$iname[[sp]][x]) ), ])), row = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$row_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][ifelse(is.null(ghb$iname[[sp]][x]) || is.na(ghb$iname[[sp]][x]), 1, which(ghb$instnam[[which(ghb$parnam == ghb$pname[[sp]][x])]] == ghb$iname[[sp]][x]) ), ])), column = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$column_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][ifelse(is.null(ghb$iname[[sp]][x]) || is.na(ghb$iname[[sp]][x]), 1, which(ghb$instnam[[which(ghb$parnam == ghb$pname[[sp]][x])]] == ghb$iname[[sp]][x]) ), ])), bhead = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$bhead_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][ifelse(is.null(ghb$iname[[sp]][x]) || is.na(ghb$iname[[sp]][x]), 1, which(ghb$instnam[[which(ghb$parnam == ghb$pname[[sp]][x])]] == ghb$iname[[sp]][x]) ), ])), condfact = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$condfact_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][ifelse(is.null(ghb$iname[[sp]][x]) || is.na(ghb$iname[[sp]][x]), 1, which(ghb$instnam[[which(ghb$parnam == ghb$pname[[sp]][x])]] == ghb$iname[[sp]][x]) ), ])) )
        
        # add conductance values & parameter names & instance names & parameter values
        ghb_df_parm$conductance = unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) ghb$parval[which(ghb$parnam == ghb$pname[[sp]][x])]*unlist(ghb$condfact_parm[[which(ghb$parnam == ghb$pname[[sp]][x])]][ifelse(is.null(ghb$iname[[sp]][x]) || is.na(ghb$iname[[sp]][x]), 1, which(ghb$instnam[[which(ghb$parnam == ghb$pname[[sp]][x])]] == ghb$iname[[sp]][x]) ), ])))
        ghb_df_parm$parnam = as.character(unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) rep(ghb$pname[[sp]][x], ghb$nlst[which(ghb$parnam == ghb$pname[[sp]][x])]) )))  # long code instead of a simple rep(ghb$pname[[sp]], ghb$nlst[which(ghb$parnam %in% ghb$pname[[sp]])]) because of possible ordering issues in ghb$pname relative to ghb$nlst
        ghb_df_parm$instnam = as.character(unlist(lapply(seq_along(ghb$iname[[sp]]), function(x) rep(ghb$iname[[sp]][x], ghb$nlst[which(ghb$parnam == ghb$pname[[sp]][x])]) )))
        ghb_df_parm$parval = as.numeric(unlist(lapply(seq_along(ghb$pname[[sp]]), function(x) rep(ghb$parval[which(ghb$parnam == ghb$pname[[sp]][x])], ghb$nlst[which(ghb$parnam == ghb$pname[[sp]][x])]) )))  
        
      }
      
      ghb_df_parm$type = 'parameter'
      
    } # non-parameter data in use
    if(ghb$itmp[sp] != 0){
      sp = tail(subset(which(ghb$itmp >= 0), which(ghb$itmp >= 0) <= sp), 1)  # set stress period to last stress period with itmp >= 0 before current stress period
      
      if(ghb$itmp[sp] > 0){
        ghb_df_sp = data.frame(layer = unlist(ghb$layer_sp[[sp]]), row = unlist(ghb$row_sp[[sp]]), column = unlist(ghb$column_sp[[sp]]), bhead = unlist(ghb$bhead_sp[[sp]]), conductance = unlist(ghb$cond_sp[[sp]]) )
        ghb_df_sp$type = 'non-parameter'
      }
    } 
    
    # bind ghb_df_parm & ghb_df_sp into ghb_df (check if they exist first)
    if(exists('ghb_df_parm') && exists('ghb_df_sp')){
      
      ghb_df = rbind(ghb_df_parm[colnames(ghb_df_sp)], ghb_df_sp)  # only use mutual column names. This will drop certain columns but only when both parameter AND non-parameter data is being used in the same stress period
      
    } else if(!exists('ghb_df_parm')){
      ghb_df = ghb_df_sp
    } else if(!exists('ghb_df_sp')){
      ghb_df = ghb_df_parm
    }
  }
  
  ##### transform data frame into rmf_array #####
  id =  rmf_convert_ijk_to_id(i=ghb_df$row, j=ghb_df$column, k=ghb_df$layer, dis=dis, type='r')

  # additive parameters (can be a lot less verbose with dplyr)
  if(variable == 'conductance' && any(duplicated(id))){
    
    ghb_df$id = id
    id_dupl = id[duplicated(id)]
    ghb_df_dupl = ghb_df[id %in% id_dupl,]
    aggr = aggregate(ghb_df_dupl[[variable]], by=list(ghb_df_dupl$id), FUN=sum)
    
    for(i in 1:nrow(aggr)){
      ghb_df[id==aggr[i, 1], variable] = aggr[i, 2]
    }
    
    ghb_df = ghb_df[!duplicated(id),]
    id =  rmf_convert_ijk_to_id(i=ghb_df$row, j=ghb_df$column, k=ghb_df$layer, dis=dis, type='r')
    
  }
  
  rmf_array = rmf_create_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    rmf_array[id] = 1
  }  else if(variable %in% c('parnam', 'instnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
    names = factor(rep(seq_along(unique(ghb_df[,variable])), as.vector(table(factor(ghb_df[,variable], levels=as.character(unique(ghb_df[,variable])))))), labels = unique(ghb_df[,variable]))
    rmf_array[id] = names
  } else {
    rmf_array[id] = unlist(ghb_df[variable])
  }
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam'))  rmf_plot(rmf_array, dis=dis, type='factor', levels=levels(names), ...) else rmf_plot(rmf_array, dis=dis, ...)
  
}