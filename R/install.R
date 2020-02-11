#' Install external codes
#'
#' @param code Character vector with the codes to install, "all" or "none".
#' @param overwrite logical. What to do when the code is already installed. If 
#' not set, the user is asked what to do in an interactive session. An 
#' error message is issued otherwise. 
#' 
#' @details Currently supported software: All MODFLOW variants documented at
#' \url{https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/}, except for
#' MODFLOW-2000.I.e. MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR and
#' MODFLOW-CFP. The zip files with windows binaries hosted at the USGS websites
#' are downloaded and extracted in \code{dir} each time. The main folder
#' names are modified in order to have more consistency.
#' 
#' The latest version is downloaded and installed. If a \code{name} folder is
#' already present in \code{dir}, and \code{overwrite} is not set, a terminal
#' prompt will ask if you want to overwrite. In a non-interactive session, this
#' will result in an error.
#' 
#' @export
#' @examples
#' rmf_install() # install all codes
#' rmf_install("2005") # install MODFLOW-2005
#' rmf_install("MODFLOW-2005") # install MODFLOW-2005
rmf_install <- function(
  code = "all",
  overwrite = NULL
) {

  # Codes ----
    if (code == "all") {
      purrr::walk(supported_codes, rmfi_install_code, overwrite = overwrite)
    } else if (code != "none") {
      codes <- supported_codes %>% c(stringr::str_remove(., "MODFLOW-"))
      if (!all(code %in% c(codes, tolower(codes))))
        ui_stop("Installing codes other than MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not supported.")
      purrr::walk(code, rmfi_install_code, overwrite = overwrite)
    }
  invisible()
}

rmfi_install_code <- function(
  code,
  overwrite
) {
  os <- Sys.info()['sysname']
  path <- getOption("RMODFLOW.path")
  if (any(grepl("2005", code)))
    rmfi_download_code("MODFLOW-2005", path, os, overwrite)
  if (any(grepl("OWHM", code, ignore.case = TRUE))) 
    rmfi_download_code("MODFLOW-OWHM", path, os, overwrite)
  if (any(grepl("NWT", code, ignore.case = TRUE))) 
    rmfi_download_code("MODFLOW-NWT", path, os, overwrite)
  if (any(grepl("LGR", code, ignore.case = TRUE))) 
    rmfi_download_code("MODFLOW-LGR", path, os, overwrite)
  if (any(grepl("CFP", code, ignore.case = TRUE))) 
    rmfi_download_code("MODFLOW-CFP", path, os, overwrite)
}
rmfi_download_code <- function(code, dir, os, overwrite) {

  # set url
  if(code == "MODFLOW-2005") {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/MODFLOW-2005/MF2005.1_12.zip"
    } else {
      x <- "https://water.usgs.gov/water-resources/software/MODFLOW-2005/MF2005.1_12u.zip"
      ui_warn("Please make sure to compile {code} before running the executable.")
    }
    folder <- gsub('\\.zip', '', basename(x))
  } else if(code == 'MODFLOW-NWT') {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/MODFLOW-NWT/MODFLOW-NWT_1.1.4.zip"
    } else {
      ui_stop("{code} is not available for your operating system.")
    }
    folder <- gsub('\\.zip', '', basename(x))
  } else if(code == 'MODFLOW-OWHM') {
    if(os == 'Windows') {
      x <- "https://ca.water.usgs.gov/modeling-software/one-water-hydrologic-model/MF_OWHM_v1_0_win.zip"
    } else {
      x <- "https://ca.water.usgs.gov/modeling-software/one-water-hydrologic-model/MF_OWHM_v1_0.zip"
      ui_warn("Donwloaded pre-compiled binary of {code} might not work on all unix systems. If so, try re-compiling the code")
    }
    folder <- gsub('_win|\\.zip', '', basename(x))
  } else if(code == 'MODFLOW-LGR') {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/ogw/modflow-lgr/modflow-lgr-v2.0.0/mflgrv2_0_00.zip"
    } else {
      ui_stop("{code} is not available for your operating system.")
    }
    folder <- 'mflgr.2_0'
  } else if(code == 'MODFLOW-CFP') {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/CFP/CFPv1.8.00-rel20110223.zip"
    } else {
      ui_stop("{code} is not available for your operating system.")
    }
    folder <- NULL # CFP download does not contain top-level folder or bin folder
  }
  
  mf_dir <- file.path(dir, code)
  
  # install, if already installed ask what to do
  if(dir.exists(mf_dir)) {
    if(is.null(overwrite) & interactive()) {
      ui_alert("You have already installed {code} in {mf_dir}")
      install <- ui_ask("Do you want to reinstall?")
    } else if (is.null(overwrite)) {
      ui_stop(c("{code} version already exists in {mf_dir}.",
                "Set overwrite to TRUE if you want replace it."))
    } else if (overwrite) {
      install <- TRUE
    } else {
      install <- FALSE
    }
    
  } else {
    install <- TRUE
  }
  
  if(install) {
    if(dir.exists(mf_dir)) unlink(mf_dir, recursive = TRUE, force = TRUE)
    temp <- tempfile()
    ui_start("Downloading")
    download.file(x, temp, quiet = TRUE)
    ui_update("Installing")
    unzip(temp, exdir = ifelse(code == 'MODFLOW-CFP', mf_dir, dir))
    unlink(temp, force = TRUE)
    if(code == 'MODFLOW-CFP') { # CFP download does not contain top-level folder or bin folder
      succes <- dir.exists(mf_dir)
      if(!succes) mf_dir <- dir
      dir.create(file.path(mf_dir, 'bin'))
      succes <- succes*file.rename(file.path(mf_dir, 'mf2005cfp.exe'), file.path(mf_dir, 'bin', 'mf2005cfp.exe'))
    } else {
      succes <- file.rename(file.path(dir, folder), mf_dir)
      if(!succes) mf_dir <- file.path(dir, folder)  
    }
    ui_end_done()
    ui_info("You can find {code} at: {mf_dir}")
  } else {
    ui_fail("Aborting install of {code}")
  }
}
supported_codes <- c("MODFLOW-2005", "MODFLOW-OWHM", "MODFLOW-NWT", "MODFLOW-LGR", "MODFLOW-CFP")