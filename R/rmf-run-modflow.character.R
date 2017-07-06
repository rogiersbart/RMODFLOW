#' Run a MODFLOW model
#' 
#' \code{run_modflow} runs a MODFLOW model.
#' 
#' @param file path to name file; typically '*.nam'
#' @param version MODFLOW version to use; 2005 (default) is currently the only option
#' @param executable name of the MODFLOW executable to use; if not provided, the executable distributed with RMODFLOW is used, corresponding to version, machine and sysname
#' @param par vector of parameter value file parameter values to run the model with
#' 
#' @rdname rmf_run_modflow
#' @method rmf_run_modflow character
#' @export
rmf_run_modflow.character <- function(file,
                                      version = 2005,
                                      executable = NULL,
                                      par = NULL) {
  
  # add argument intern = FALSE, so it can be set to TRUE for printing MODFLOW terminal output in vignettes
  
  # select appropriate executable
    if(is.null(executable)) {
      if(version == '2005' | version == 2005) {
        executable <- system.file(paste0('bin/MODFLOW-2005_v.1.11.00_',Sys.info()['sysname'],'_',Sys.info()['machine'],'.exe'),package='RMODFLOW')
      }
    }
  
  # get directory and filename
    dir <- dirname(file)
    file <- basename(file)
    
  # set initial parameters if provided  
    if(!is.null(par)) {
      nam <- rmf_read_nam(paste0(dir,'/',file))
      pvl <- rmf_read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
      pvl$parval <- par
      write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
    }
    
  # run modflow
    if(Sys.info()['sysname']=='Linux') system(paste('cd',dir,'&',executable,file))
    if(Sys.info()['sysname']=='Windows') shell(paste('cd',dir,'&',executable,file),mustWork=TRUE) # , intern=TRUE
}
