#' Install external codes
#' 
#' This function installs external codes that are used by [RMODFLOW]. 
#' 
#' ## Supported software
#' All MODFLOW variants documented at the [Online Guide to
#' MODFLOW](https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/), except
#' for MODFLOW-2000. *I.e.* MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR
#' and MODFLOW-CFP. The zip files with windows binaries hosted at the USGS
#' websites are downloaded and extracted in the installation directory. The main
#' folder names are modified in order to have more consistency.
#' 
#' ## Installation location
#' The default installation location is `file.path(system.file(package =
#' "RMODFLOW"), "code")`, but it can be altered by setting option
#' `RMODFLOW.path`.
#'
#' @param code Character vector with the codes to install, or `"all"` (default).
#' @param overwrite Logical. Overwrite when the code is already installed? If
#'   `NULL` (default), the user is asked what to do in an interactive session.
#'   An error message is issued otherwise.
#' @export
#' @seealso [rmf_guide()] for launching the [Online Guide to
#'   MODFLOW](https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/) or
#'   specific pages thereof.
#' @examples
#' \dontrun{
#' rmf_install() # Install all codes.
#' rmf_install("2005") # Install MODFLOW-2005.
#' rmf_install("MODFLOW-2005") # Install MODFLOW-2005.
#' rmf_install(c(2005, "MODFLOW-NWT", "cfp")) # Install different variants.
#' }
rmf_install <- function(code = "all", overwrite = NULL) {
  if (code[1] == "all") {
    rmfi_install_code(rmfd_supported_codes, overwrite = overwrite)
    return(invisible())
  }
  codes <- rmfd_supported_codes %>% c(stringr::str_remove(., "MODFLOW-"))
  code <- stringr::str_remove(toupper(code), "MODFLOW-")
  if (!all(code %in% codes)) {
    rui::alert("Installing codes other than MODFLOW-2005, MODFLOW-OWHM,",
             "MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not",
             "supported.")
    rui::stop("Issue with code name.")
  }
  rmfi_install_code(code, overwrite = overwrite)
  invisible()
}

#' @rdname rmf_install
#' @export
#' @details [rmf_installed_codes()] shows which codes are installed in the default installation location as
#'  set by the `RMODFLOW.path` option.
#' @examples
#' \dontrun{
#' rmf_installed_codes()
#' }
rmf_installed_codes <- function() {
  loc <- getOption('RMODFLOW.path')
  codes <- vapply(list.dirs(loc, recursive = FALSE), basename, 'text')
  if(length(codes) == 0) {
    rui::disapprove('No codes have been installed in {loc}')
  } else {
    rui::approve('Following codes have been installed in {loc}:')
    for(i in codes) rui::inform(i)
  }
}

#' Install codes
#'
#' @inheritParams rmf_install
rmfi_install_code <- function(code, overwrite) {
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
  invisible()
}

#' Download a code
#'
#' @inheritParams rmf_install
#' @param dir Installation directory.
#' @param os Operating system.
rmfi_download_code <- function(code, dir, os, overwrite) {
  # set url
  if(code == "MODFLOW-2005") {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/MODFLOW-2005/MF2005.1_12.zip"
    } else {
      x <- "https://water.usgs.gov/water-resources/software/MODFLOW-2005/MF2005.1_12u.zip"
      rui::warn("Please make sure to compile {code} before running the",
              "executable.")
    }
    folder <- gsub('\\.zip', '', basename(x))
  } else if(code == 'MODFLOW-NWT') {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/MODFLOW-NWT/MODFLOW-NWT_1.1.4.zip"
    } else {
      rui::stop("{code} is not available for your operating system.")
    }
    folder <- gsub('\\.zip', '', basename(x))
  } else if(code == 'MODFLOW-OWHM') {
    if(os == 'Windows') {
      x <- "https://ca.water.usgs.gov/modeling-software/one-water-hydrologic-model/MF_OWHM_v1_0_win.zip"
    } else {
      x <- "https://ca.water.usgs.gov/modeling-software/one-water-hydrologic-model/MF_OWHM_v1_0.zip"
      rui::warn("Donwloaded pre-compiled binary of {code} might not work on",
              "all unix systems. If so, try re-compiling the code")
    }
    folder <- gsub('_win|\\.zip', '', basename(x))
  } else if(code == 'MODFLOW-LGR') {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/ogw/modflow-lgr/modflow-lgr-v2.0.0/mflgrv2_0_00.zip"
    } else {
      rui::stop("{code} is not available for your operating system.")
    }
    folder <- 'mflgr.2_0'
  } else if(code == 'MODFLOW-CFP') {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/CFP/CFPv1.8.00-rel20110223.zip"
    } else {
      rui::stop("{code} is not available for your operating system.")
    }
    # NOTE CFP download does not contain top-level folder or bin folder
    folder <- NULL 
  }
  mf_dir <- file.path(dir, code)
    
  # install, if already installed ask what to do
  if(dir.exists(mf_dir)) {
    if(is.null(overwrite) & interactive()) {
      rui::alert("You have already installed {code} in {mf_dir}")
      install <- rui::ask("Do you want to reinstall?")
    } else if (is.null(overwrite)) {
      rui::stop(c("{code} version already exists in {mf_dir}.",
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
    rui::begin("Downloading {code}")
    download.file(x, temp, quiet = TRUE)
    rui::update("Installing {code}")
    unzip(temp, exdir = ifelse(code == 'MODFLOW-CFP', mf_dir, dir))
    unlink(temp, force = TRUE)
    if(code == 'MODFLOW-CFP') {
      # NOTE CFP download does not contain top-level folder or bin folder
      succes <- dir.exists(mf_dir)
      if(!succes) mf_dir <- dir
      dir.create(file.path(mf_dir, 'bin'))
      succes <- succes*file.rename(file.path(mf_dir, 'mf2005cfp.exe'),
                                   file.path(mf_dir, 'bin', 'mf2005cfp.exe'))
    } else {
      # fs::file_move(file.path(dir, folder), mf_dir)
      file.rename(file.path(dir, folder), mf_dir)
    }
    rui::succeed()
    rui::inform("You can find {code} at: {.path {mf_dir}}")
  } else {
    rui::disapprove("Aborting install of {code}")
  }
  invisible()
}
