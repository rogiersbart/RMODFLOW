#' Install codes and examples
#'
#' @param code Character vector with the codes to install, "all" or "none".
#' @param example Character vector with the examples to install, "all" or "none".
#' @details Currently supported codes: MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT,
#'   MODFLOW-LGR and MODFLOW-CFP. The zip files with windows binaries hosted at
#'   the USGS websites are downloaded and extracted in the RMODFLOW package
#'   folder.
#' @export
#' @examples
#' rmf_install() # install all codes
#' rmf_install("2005") # install MODFLOW-2005
#' rmf_install("MODFLOW-2005") # install MODFLOW-2005
rmf_install <- function(
  code = "all",
  example = "none"
) {

  # Codes ----
  
    # TODO enable a choice in console and make that default behaviour
    codes <- c("MODFLOW-2005", "MODFLOW-OWHM", "MODFLOW-NWT", "MODFLOW-LGR", "MODFLOW-CFP")
    if (code == "all") {
      purrr::walk(codes, rmfi_install_code)
    } else if (code != "none") {
      codes <- codes %>% c(stringr::str_remove(., "MODFLOW-"))
      if (!all(code %in% c(codes, tolower(codes))))
        stop("Installing codes other than MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not supported.")
      purrr::walk(code, rmfi_install_code)
    }
    
  # Examples ----
    
    # Not implemented yet
    if (! example %in% "none")
      stop("Installing examples is currently not supported.")
    
  return(invisible())
}

rmfi_install_code <- function(
  code
) {
  pkg_home <- system.file(package = "RMODFLOW")
  code_home <- paste0(pkg_home, "/code")
  if (any(grepl("2005", code))) {
    url <- "https://www.usgs.gov/software/modflow-2005-usgs-three-dimensional-finite-difference-ground-water-model"
    file_url <- xml2::read_html(url) %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      stringr::str_subset("MF2005") %>% 
      .[1]
    version <- stringr::str_split(basename(file_url), "\\.")[[1]][2] %>% 
      stringr::str_replace_all("_", ".")
    folder <- stringr::str_remove(basename(file_url), ".zip")
    message("Found MODFLOW-2005 (v", version, ") at:\n", url)
    rmfi_download_code("MODFLOW-2005", file_url, version, folder)
  }
  if (any(grepl("OWHM", code, ignore.case = TRUE))) {
    url <- "https://www.usgs.gov/software/modflow-owhm-one-water-hydrologic-flow-model"
    file_url <- xml2::read_html(url) %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      subset(grepl("_win.zip", .) & grepl("MF_OWHM", .))
    version <- stringr::str_remove(basename(file_url), "_win.zip") %>%
      stringr::str_remove("MF_OWHM_v") %>% 
      stringr::str_replace_all("_", ".")
    folder <- basename(file_url) %>% stringr::str_remove("_win.zip")
    message("Found MODFLOW-OWHM version ", version, " at:\n", url)
    rmfi_download_code("MODFLOW-OWHM", file_url, version, folder)
  }
  if (any(grepl("NWT", code, ignore.case = TRUE))) {
    url <- "https://www.usgs.gov/software/modflow-nwt-a-newton-formulation-modflow-2005"
    file_url <- xml2::read_html(url) %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      subset(grepl(".zip", .) & grepl("MODFLOW-NWT", .))
    version <- stringr::str_remove(basename(file_url), ".zip") %>%
      stringr::str_remove("MODFLOW-NWT_") %>% 
      stringr::str_replace_all("_", ".")
    folder <- basename(file_url) %>% stringr::str_remove(".zip")
    message("Found MODFLOW-NWT version ", version, " at:\n", url)
    rmfi_download_code("MODFLOW-NWT", file_url, version, folder)
  }
  if (any(grepl("LGR", code, ignore.case = TRUE))) {
    url <- "https://water.usgs.gov/ogw/modflow-lgr/"
    file_url <- xml2::read_html(url) %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      subset(grepl(".zip", .) & grepl("mflgr", .)) %>% 
      paste0(url, .)
    version <- stringr::str_remove(basename(file_url), ".zip") %>%
      stringr::str_remove("mflgrv") %>% 
      stringr::str_replace_all("_", ".")
    folder <- "mflgr.2_0"
    message("Found MODFLOW-LGR version ", version, " at:\n", url)
    rmfi_download_code("MODFLOW-LGR", file_url, version, folder)
  }
  if (any(grepl("CFP", code, ignore.case = TRUE))) {
    url <- "https://www.usgs.gov/software/conduit-flow-process-cfp-a-program-simulate-turbulent-or-laminar-groundwater-flow"
    file_url <- xml2::read_html(url) %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      subset(grepl(".zip", .) & grepl("CFP", .) & !grepl("ogw", .)) %>% 
      .[1]
    version <- stringr::str_remove(basename(file_url), ".zip") %>%
      stringr::str_split("-") %>%
      purrr::pluck(1, 1) %>% 
      stringr::str_remove("CFPv") %>% 
      stringr::str_replace_all("_", ".")
    message("Found MODFLOW-CFP version ", version, " at:\n", url)
    rmfi_download_code("MODFLOW-CFP", file_url, version, "MODFLOW-CFP")
  }
}
rmfi_download_code <- function(
  code_name,
  file_url,
  version,
  folder
) {
  if (!fs::dir_exists(fs::path(code_home, code_name))) {
    temp <- tempfile()
    message("Downloading file ...")
    download.file(file_url, temp, quiet = TRUE)
    message("Installing ...")
    if (code_name == "MODFLOW-CFP") {
      unzip(temp, exdir = fs::path(code_home, code_name))
    } else {
      unzip(temp, exdir = code_home)
      fs::file_move(fs::path(code_home, folder), fs::path(code_home, code_name))
    }
    unlink(temp, force = TRUE)
    cat(paste0(version, "\n"), file = fs::path(code_home, code_name, "version-number-for-RMODFLOW"))
    message("You have succesfully installed ", code_name, " (v",
            version, ") at:\n", fs::path(code_home, code_name))
  } else {
    installed_version <- readLines(fs::path(code_home, code_name, "version-number-for-RMODFLOW"))
    compare_versions <- compareVersion(version, installed_version)
    if (compare_versions == 0) {
      message("You have already installed this version at:\n",
              fs::path(code_home, code_name))
      install <- readline("Do you want to reinstall? (y/n) ")
    } else if (compare_versions == 1) {
      message("You have installed version ", installed_version, " at:\n",
              fs::path(code_home, code_name))
      install <- readline("Do you want to overwrite it? (y/n) ")
    } else {
      message("It seems like you have a later version (", installed_version, ") than the one found online.")
      install <- readline("Do you want to overwrite it? (y/n) ")
    }
    if (install == "y") {
      fs::file_delete(fs::path(code_home, code_name))
      rmf_install(code_name)
    }
  }
}
