#' Run a MODFLOW model
#' 
#' \code{run_modflow} runs a MODFLOW model.
#' 
#' @param modflow modflow object
#' @param version MODFLOW version to use; 2005 (default) is currently the only option
#' @param executable name of the MODFLOW executable to use; if not provided, the executable distributed with RMODFLOW is used, corresponding to version, machine and sysname
#' @param par vector of parameter value file parameter values to run the model with
#' 
#' @rdname run_modflow
#' @method run_modflow modflow
#' @export
run_modflow.modflow <- function(modflow,
                                version = 2005,
                                executable = NULL,
                                par = NULL) {
  
  # temporary directory
    old <- setwd(tempdir())
    on.exit(setwd(old), add = TRUE)
  
  # write all files
    write_nam(modflow$nam, file = 'input.nam')
    for(i in 1:nrow(modflow$nam)) {
      if(modflow$nam$ftype[i] %in% c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF')) {
        object_class <- c('hob','pvl','dis','zon','mlt','bas','huf','oc','wel','ghb','pcg','kdep','lpf')[which(c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF') == modflow$nam$ftype[i])]
        if(object_class %in% 'lpf') {
          get(paste0('write_',object_class))(modflow[[object_class]], file = modflow$nam$fname[i], dis = modflow$dis)
        } else {
          get(paste0('write_',object_class))(modflow[[object_class]], file = modflow$nam$fname[i])  
        }
      }
    }

  # run modflow
    run_modflow('input.nam', version = version, executable = executable, par = par)
  
  # read all output

}
