#' Install external software
#'
#' @param name character; name of the code to download. If \code{'all'}, all codes will be downloaded. Defaults to \code{'MODFLOW-2005'}
#' @param dir path to directory in which to install the software. Defaults to the "exe" folder in the \code{RMODFLOW} package directory (or creates it silently if not present)
#' @param verbose logical; should information about downloading and installing be printed to the console? Defaults to TRUE
#' @param prompt logical; in case the code is already installed in \code{dir}, should the user be asked to overwrite? If FALSE, the code is overwritten with a warning
#' 
#' @details Currently supported software: All MODFLOW variants documented at
#' \url{https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/}, except for
#' MODFLOW-2000.I.e. MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR and
#' MODFLOW-CFP. The zip files with windows binaries hosted at the USGS websites
#' are downloaded and extracted in \code{dir} each time. The main folder
#' names are modified in order to have more consistency.
#' 
#' The latest version is downloaded and installed. If a \code{name} folder is already present in \code{dir},
#' a terminal prompt will ask if you want to overwrite unless \code{prompt} is FALSE in which case the code is overwritten with a warning.
#' @export

rmf_install <- function(name = c("MODFLOW-2005", "MODFLOW-NWT", "MODFLOW-OWHM", "MODFLOW-LGR", "MODFLOW-CFP", "all"),
                        dir = NULL,
                        verbose = TRUE,
                        prompt = verbose) {
  
  # TODO install examples
  
  os <- Sys.info()['sysname']
  if(!grepl('MODFLOW', name[1]) && name[1] != 'all') name <- paste0('MODFLOW-', name[1])
  name <- match.arg(name)
  
  if(is.null(dir)) { # use exe folder in RMODFLOW
    dir <- system.file('exe', package = 'RMODFLOW')
    if(!dir.exists(dir)) { # create if not present
      dir.create(file.path(system.file(package = 'RMODFLOW'), 'exe'))
      dir <- system.file('exe', package = 'RMODFLOW')
    } 
  } else if(!dir.exists(dir)) {
    stop(dir, ' is not a valid directory', call. = FALSE)
  }
  
  # installer function
  
  install_mf <- function(name, dir) {
    
    if(verbose) message('Downloading and installing ', name, ' ...')
    
    # set url
    if(name == "MODFLOW-2005") {
      if(os == 'Windows') {
        x <- "https://water.usgs.gov/water-resources/software/MODFLOW-2005/MF2005.1_12.zip"
      } else {
        x <- "https://water.usgs.gov/water-resources/software/MODFLOW-2005/MF2005.1_12u.zip"
        warning('Please make sure to compile ', name, ' before running the executable', call. = FALSE)
      }
      folder <- gsub('\\.zip', '', basename(x))
    } else if(name == 'MODFLOW-NWT') {
      if(os == 'Windows') {
        x <- "https://water.usgs.gov/water-resources/software/MODFLOW-NWT/MODFLOW-NWT_1.1.4.zip"
      } else {
        stop(name, ' is not available for your operating system.', call. = FALSE)
      }
      folder <- gsub('\\.zip', '', basename(x))
    } else if(name == 'MODFLOW-OWHM') {
      if(os == 'Windows') {
        x <- "https://ca.water.usgs.gov/modeling-software/one-water-hydrologic-model/MF_OWHM_v1_0_win.zip"
      } else {
        x <- "https://ca.water.usgs.gov/modeling-software/one-water-hydrologic-model/MF_OWHM_v1_0.zip"
        warning('Donwloaded pre-compiled binary of ', name, ' might not work on all unix systems. If so, try re-compiling the code', 
                call. = FALSE)
      }
      folder <- gsub('_win|\\.zip', '', basename(x))
    } else if(name == 'MODFLOW-LGR') {
      if(os == 'Windows') {
        x <- "https://water.usgs.gov/ogw/modflow-lgr/modflow-lgr-v2.0.0/mflgrv2_0_00.zip"
      } else {
        stop(name, ' is not available for your operating system.', call. = FALSE)
      }
      folder <- 'mflgr.2_0'
    } else if(name == 'MODFLOW-CFP') {
      if(os == 'Windows') {
        x <- "https://water.usgs.gov/water-resources/software/CFP/CFPv1.8.00-rel20110223.zip"
      } else {
        stop(name, ' is not available for your operating system.', call. = FALSE)
      }
      folder <- NULL # CFP download does not contain top-level folder or bin folder
    }
    
    mf_dir <- file.path(dir, name)
    
    # install, if already installed ask what to do
    if(dir.exists(mf_dir)) {
      if(prompt) {
        message("You have already installed ", name, " in ", mf_dir)
        install <- readline("Do you want to reinstall? (y/n) ")
      } else {
        warning("Overwriting existing ", name, ' version in ', mf_dir, call. = FALSE)
        install <- 'y'
      }

    } else {
      install <- 'y'
    }
    
    if(install == 'y') {
      if(dir.exists(mf_dir)) unlink(mf_dir, recursive = TRUE, force = TRUE)
      temp <- tempfile()
      if(verbose) message("Downloading file ...")
      download.file(x, temp, quiet = !verbose)
      if(verbose) message("Installing ...")
      unzip(temp, exdir = ifelse(name == 'MODFLOW-CFP', mf_dir, dir))
      unlink(temp, force = TRUE)
      if(name == 'MODFLOW-CFP') { # CFP download does not contain top-level folder or bin folder
        succes <- dir.exists(mf_dir)
        if(!succes) mf_dir <- dir
        dir.create(file.path(mf_dir, 'bin'))
        succes <- succes*file.rename(file.path(mf_dir, 'mf2005cfp.exe'), file.path(mf_dir, 'bin', 'mf2005cfp.exe'))
      } else {
        succes <- file.rename(file.path(dir, folder), mf_dir)
        if(!succes) mf_dir <- file.path(dir, folder)  
      }
      if(verbose) message("You have succesfully installed ", name, " at:\n", mf_dir)
    } else {
      if(verbose) message('Aborting install of ', name)
    }
    
  }
  
  # call install_mf
  if(name == 'all') {
    name <- eval(formals(rmf_install)$name)
    for(i in name[-which(name == 'all')]) install_mf(i, dir)
  } else {
    install_mf(name, dir)
  }
}
