#' Plot a 2D section of an RMODFLOW riv object
#' 
#' \code{rmf_plot.riv} plots a 2D section of an \code{RMODFLOW} riv object using \code{rmf_plot.rmf-3d-array}
#' 
#' @param riv an \code{RMODFLOW} riv object
#' @param dis an \code{RMODFLOW} dis object
#' @param all_parm logical, should all parameters defined by plotted (i.e. indepedent of stress periods); defaults to FALSE
#' @param variable character, what data should be plotted. Possible values are: "identity" (default; plots the reach locations), "layer", "row", "column", "stage", "condfact" (for parameter data), "rbot", "conductance", "parnam" (for parameter data), "instnam" (for time-varying parameter data) and "parval" (for parameter data); defaults to "identity"
#' @param instnum numeric vector of length \code{npriv} holding the instance numbers for each time-varying parameter which need to be plotted. Only one instance per parameter is allowed. If a certain parameter \code{i} is not time-varying, specify instnum[i] as '1'; defaults to NULL
#' @param l time step number for selecting which stress period to plot; defaults to NULL (last stress period)
#' @param sp optional stress period number to plot; will override the stress period calculated from \code{l}; defaults to NULL
#' @param ... additional arguments passed to \code{rmf_plot.rmf-3d-array}
#' 
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @export
#' @method rmf_plot riv

# Function can benefit from a standardized function which transforms stress packages to data frames (might also be useful for data analysis)

rmf_plot.riv = function(riv,
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

  
  ##### create data frame  #####
  
  
  if(all_parm){ #### plot only parameters (independent of stress period) ####
    if(is.null(instnum)){ # not time-varying
      riv_df = data.frame(layer = unlist(riv$layer_parm), row = unlist(riv$row_parm), column = unlist(riv$column_parm), stage = unlist(riv$stage_parm), condfact = unlist(riv$condfact_parm), rbot = unlist(riv$rbot_parm) )
     
      # add river conductance values & parameter names & parameter values
      riv_df$conductance = unlist(lapply(seq_along(riv$condfact_parm), function(x) riv$parval[x]*riv$condfact_parm[[x]][1,]))
      riv_df$parnam = rep(riv$parnam, riv$nlst) # as.character(unlist(lapply(seq_along(riv$parnam), function(x) rep(riv$parnam[x], riv$nlst[x]))))   
      riv_df$parval = rep(riv$parval, riv$nlst) # as.numeric(unlist(lapply(seq_along(riv$parval), function(x) rep(riv$parval[x], riv$nlst[x]))))   
      
      
    } else { # time varying
      riv_df = data.frame(layer = unlist(lapply(seq_along(riv$layer_parm), function(x) riv$layer_parm[[x]][instnum[x],])), row = unlist(lapply(seq_along(riv$row_parm), function(x) riv$row_parm[[x]][instnum[x],])), column = unlist(lapply(seq_along(riv$column_parm), function(x) riv$column_parm[[x]][instnum[x],])), stage = unlist(lapply(seq_along(riv$stage_parm), function(x) riv$stage_parm[[x]][instnum[x],])), condfact = unlist(lapply(seq_along(riv$condfact_parm), function(x) riv$condfact_parm[[x]][instnum[x],])), rbot = unlist(lapply(seq_along(riv$rbot_parm), function(x) riv$rbot_parm[[x]][instnum[x],])) )
     
      # add river conductance values & parameter names & instance names & parameter values
      riv_df$conductance = unlist(lapply(seq_along(riv$condfact_parm), function(x) riv$parval[x]*riv$condfact_parm[[x]][instnum[x],]))
      riv_df$parnam = rep(riv$parnam, riv$nlst)  # as.character(unlist(lapply(seq_along(riv$parnam), function(x) rep(riv$parnam[x], riv$nlst[x]))))
      riv_df$instnam = as.character(unlist(lapply(seq_along(riv$instnam)), function(x) rep(riv$instnam[[x]][instnum[x]], riv$nlst[x])))
      riv_df$parval = rep(riv$parval, riv$nlst)  # as.numeric(unlist(lapply(seq_along(riv$parval), function(x) rep(riv$parval[x], riv$nlst[x]))))  
      
     }

  
  } else { ####  data in use for the specified stress period ####
    
    if(riv$np[sp] > 0){ # parameter data in use
      
      # not time-varying
      if(is.null(riv$iname) || (!is.null(riv$iname) && (is.null(riv$iname[[sp]]) || all(is.na(unlist(riv$iname[[sp]])))) )){
        
        riv_df_parm = data.frame(layer = unlist(riv$layer_parm[which(riv$parnam %in% riv$pname[[sp]])]), row = unlist(riv$row_parm[which(riv$parnam %in% riv$pname[[sp]])]), column = unlist(riv$column_parm[which(riv$parnam %in% riv$pname[[sp]])]), stage = unlist(riv$stage_parm[which(riv$parnam %in% riv$pname[[sp]])]), condfact = unlist(riv$condfact_parm[which(riv$parnam %in% riv$pname[[sp]])]), rbot = unlist(riv$rbot_parm[which(riv$parnam %in% riv$pname[[sp]])]) )
        
        # add conductance values & parameter names & parameter values
        riv_df_parm$conductance = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$parval[which(riv$parnam == riv$pname[[sp]][x])]*unlist(riv$condfact_parm[[which(riv$parnam == riv$pname[[sp]][x])]][1,])))
        riv_df_parm$parnam = as.character(unlist(lapply(seq_along(riv$pname[[sp]]), function(x) rep(riv$pname[[sp]][x], riv$nlst[which(riv$parnam == riv$pname[[sp]][x])]) )))  # long code instead of a simple rep(riv$pname[[sp]], riv$nlst[which(riv$parnam %in% riv$pname[[sp]])]) because of possible ordering issues in riv$pname relative to riv$nlst
        riv_df_parm$parval = as.numeric(unlist(lapply(seq_along(riv$pname[[sp]]), function(x) rep(riv$parval[which(riv$parnam == riv$pname[[sp]][x])], riv$nlst[which(riv$parnam == riv$pname[[sp]][x])]) )))  
        
      
      } else { # time-varying
        riv_df_parm = data.frame(layer = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$layer_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])), row = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$row_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])), column = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$column_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])), stage = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$stage_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])), condfact = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$condfact_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])), rbot = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$rbot_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])) )
        
        # add conductance values & parameter names & instance names & parameter values
        riv_df_parm$conductance = unlist(lapply(seq_along(riv$pname[[sp]]), function(x) riv$parval[which(riv$parnam == riv$pname[[sp]][x])]*unlist(riv$condfact_parm[[which(riv$parnam == riv$pname[[sp]][x])]][ifelse(is.null(riv$iname[[sp]][x]) || is.na(riv$iname[[sp]][x]), 1, which(riv$instnam[[which(riv$parnam == riv$pname[[sp]][x])]] == riv$iname[[sp]][x]) ), ])))
        riv_df_parm$parnam = as.character(unlist(lapply(seq_along(riv$pname[[sp]]), function(x) rep(riv$pname[[sp]][x], riv$nlst[which(riv$parnam == riv$pname[[sp]][x])]) )))  # long code instead of a simple rep(riv$pname[[sp]], riv$nlst[which(riv$parnam %in% riv$pname[[sp]])]) because of possible ordering issues in riv$pname relative to riv$nlst
        riv_df_parm$instnam = as.character(unlist(lapply(seq_along(riv$iname[[sp]]), function(x) rep(riv$iname[[sp]][x], riv$nlst[which(riv$parnam == riv$pname[[sp]][x])]) )))
        riv_df_parm$parval = as.numeric(unlist(lapply(seq_along(riv$pname[[sp]]), function(x) rep(riv$parval[which(riv$parnam == riv$pname[[sp]][x])], riv$nlst[which(riv$parnam == riv$pname[[sp]][x])]) )))  
        
      }
      
      riv_df_parm$type = 'parameter'

    } # non-parameter data in use
    if(any(riv$itmp > 0)){
      sp = tail(subset(which(riv$itmp > 0), which(riv$itmp > 0) <= sp), 1)  # set stress period to last stress period with itmp > 0 before current stress period
      
      riv_df_sp = data.frame(layer = unlist(riv$layer_sp[[sp]]), row = unlist(riv$row_sp[[sp]]), column = unlist(riv$column_sp[[sp]]), stage = unlist(riv$stage_sp[[sp]]), conductance = unlist(riv$cond_sp[[sp]]), rbot = unlist(riv$rbot_sp[[sp]]) )
      riv_df_sp$type = 'non-parameter'
    } 
    
    # bind riv_df_parm & riv_df_sp into riv_df (check if they exist first)
    if(exists('riv_df_parm') && exists('riv_df_sp')){
      
      riv_df = rbind(riv_df_parm[colnames(riv_df_sp)], riv_df_sp)  # only use mutual column names. This will drop certain columns but only when both parameter AND non-parameter data is being used in the same stress period
      
    } else if(!exists('riv_df_parm')){
      riv_df = riv_df_sp
    } else if(!exists('riv_df_sp')){
      riv_df = riv_df_parm
    }
  }
  
  ##### transform data frame into rmf_array #####
  id =  convert_ijk_to_id(i=riv_df$row, j=riv_df$column, k=riv_df$layer, dis=dis, type='r')
  rmf_array = create_rmodflow_array(dim=c(dis$nrow, dis$ncol, dis$nlay))
  if(variable == 'identity'){
    	rmf_array[id] = 1
  	}  else if(variable %in% c('parnam', 'instnam')){   # add changes for character vectors (parnam & instnam) because of incompatability with default mask (=numeric) in rmf_plot function
       names = factor(rep(seq_along(unique(riv_df[,variable])), as.vector(table(factor(riv_df[,variable], levels=as.character(unique(riv_df[,variable])))))), labels = unique(riv_df[,variable]))
       rmf_array[id] = names
  	} else {
  		rmf_array[id] = unlist(riv_df[variable])
    }
  
  ##### plot #####
  if(variable %in% c('parnam', 'instnam'))  rmf_plot(rmf_array, dis=dis, type='factor', ...) else rmf_plot(rmf_array, dis=dis, ...) # add levels for factor plots?

}