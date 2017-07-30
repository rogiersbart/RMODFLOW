#' Install external software
#'
#' @param name Character.
#' 
#' @details Currently supported software: All MODFLOW variants documented at
#' \url{https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/}, except for
#' MODFLOW-2000.I.e. MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR and
#' MODFLOW-CFP. The zip files with windows binaries hosted at the USGS websites
#' are downloaded and extracted in \file{C:/WRDAPP/} each time. The main folder
#' names are modified in order to have more consistency.
#'
#' @export
#' @examples
#' rmf_install("MODFLOW-2005")
rmf_install <- function(name = "MODFLOW-2005") {
  
  # make sure directory to install MODFLOW-related software exists
  
    if (!dir.exists("C:/WRDAPP")) dir.create("C:/WRDAPP")
  
  # do the installation
  
    if (name == "MODFLOW-2005") {
      
      # get the link to the latest version
      
        x <- xml2::read_html("https://water.usgs.gov/ogw/modflow/MODFLOW.html") %>% 
          rvest::html_nodes("a") %>% 
          rvest::html_attr("href") %>% 
          subset(grepl(".zip", .) & !grepl("u", .) & grepl("MODFLOW-2005", .))
        version <- substr(x[1], 15, 21)
        folder <- substr(basename(x[1]), 1, nchar(basename(x))-4)
        message("Found MODFLOW-2005 version ", version, " at:\nhttps://water.usgs.gov/ogw/modflow/MODFLOW.html")
        x <- paste0("https://water.usgs.gov/ogw/modflow/", x[1])
        
      # install, if already installed ask what to do
        
        if (!dir.exists("C:/WRDAPP/MODFLOW-2005")) {
          temp <- tempfile()
          message("Downloading file ...")
          download.file(x, temp, quiet = TRUE)
          message("Installing ...")
          unzip(temp, exdir = "C:/WRDAPP")
          unlink(temp, force = TRUE)
          file.rename(paste0("C:/WRDAPP/", folder), "C:/WRDAPP/MODFLOW-2005")
          cat(paste0(version, "\n"), file = "C:/WRDAPP/MODFLOW-2005/version.txt")
          message("You have succesfully installed MODFLOW-2005 at:\nC:/WRDAPP/MODFLOW-2005/")
        } else {
          installed_version <- readLines("C:/WRDAPP/MODFLOW-2005/version.txt")
          compare_versions <- compareVersion(version, installed_version)
          if (compare_versions == 0) {
            message("You have already installed this version at:\nC:/WRDAPP/MODFLOW-2005/")
            install <- readline("Do you want to reinstall? (y/n) ")
          } else if (compare_versions == 1) {
            message("You have installed version ", installed_version, " at:\nC:/WRDAPP/MODFLOW-2005/")
            install <- readline("Do you want to overwrite it? (y/n) ")
          } else {
            message("It seems like you have a later version (", installed_version, ") than the one found online. I strongly suggest you to check https://water.usgs.gov/ogw/modflow/MODFLOW.html and make changes manually if required.")
            install <- readline("Do you want to overwrite it? (y/n) ")
          }
          if (install == "y") {
            unlink("C:/WRDAPP/MODFLOW-2005", recursive = TRUE, force = TRUE)
            rmf_install("MODFLOW-2005")
          }
        }
        
    } else if (name == "MODFLOW-OWHM") {
      
      # get the link to the latest version
      
        x <- xml2::read_html("https://water.usgs.gov/ogw/modflow-owhm/") %>% 
          rvest::html_nodes("a") %>% 
          rvest::html_attr("href") %>% 
          subset(grepl(".zip", .) & grepl("MF_OWHM", .))
        version <- gsub("_", "\\.", substr(x, 10, 12))
        folder <- substr(basename(x), 1, nchar(basename(x))-4)
        message("Found MODFLOW-OWHM version ", version, " at:\nhttps://water.usgs.gov/ogw/modflow-owhm/")
        x <- paste0("https://water.usgs.gov/ogw/modflow-owhm/", x)
        
      # install, if already installed ask what to do
      
        if (!dir.exists("C:/WRDAPP/MODFLOW-OWHM")) {
          temp <- tempfile()
          message("Downloading file ...")
          download.file(x, temp, quiet = TRUE)
          message("Installing ...")
          unzip(temp, exdir = "C:/WRDAPP")
          unlink(temp, force = TRUE)
          file.rename(paste0("C:/WRDAPP/", folder), "C:/WRDAPP/MODFLOW-OWHM")
          cat(paste0(version, "\n"), file = "C:/WRDAPP/MODFLOW-OWHM/version.txt")
          message("You have succesfully installed MODFLOW-OWHM at:\nC:/WRDAPP/MODFLOW-OWHM/")
        } else {
          installed_version <- readLines("C:/WRDAPP/MODFLOW-OWHM/version.txt")
          compare_versions <- compareVersion(version, installed_version)
          if (compare_versions == 0) {
            message("You have already installed this version at:\nC:/WRDAPP/MODFLOW-OWHM/")
            install <- readline("Do you want to reinstall? (y/n) ")
          } else if (compare_versions == 1) {
            message("You have installed version ", installed_version, " at:\nC:/WRDAPP/MODFLOW-OWHM/")
            install <- readline("Do you want to overwrite it? (y/n) ")
          } else {
            message("It seems like you have a later version (", installed_version, ") than the one found online. I strongly suggest you to check https://water.usgs.gov/ogw/modflow-owhm/ and make changes manually if required.")
            install <- readline("Do you want to overwrite it? (y/n) ")
          }
          if (install == "y") {
            unlink("C:/WRDAPP/MODFLOW-OWHM", recursive = TRUE, force = TRUE)
            rmf_install("MODFLOW-OWHM")
          }
        }
      
    } else if (name == "MODFLOW-NWT") {
      
      # get the link to the latest version
      
      x <- xml2::read_html("https://water.usgs.gov/ogw/modflow-nwt/") %>% 
        rvest::html_nodes("a") %>% 
        rvest::html_attr("href") %>% 
        subset(grepl(".zip", .) & grepl("MODFLOW-NWT", .))
      version <- gsub("_", "\\.", substr(x, 13, 17))
      folder <- substr(basename(x), 1, nchar(basename(x))-4)
      message("Found MODFLOW-NWT version ", version, " at:\nhttps://water.usgs.gov/ogw/modflow-nwt/")
      x <- paste0("https://water.usgs.gov/ogw/modflow-nwt/", x)
      
      # install, if already installed ask what to do
      
      if (!dir.exists("C:/WRDAPP/MODFLOW-NWT")) {
        temp <- tempfile()
        message("Downloading file ...")
        download.file(x, temp, quiet = TRUE)
        message("Installing ...")
        unzip(temp, exdir = "C:/WRDAPP")
        unlink(temp, force = TRUE)
        file.rename(paste0("C:/WRDAPP/", folder), "C:/WRDAPP/MODFLOW-NWT")
        cat(paste0(version, "\n"), file = "C:/WRDAPP/MODFLOW-NWT/version.txt")
        message("You have succesfully installed MODFLOW-NWT at:\nC:/WRDAPP/MODFLOW-NWT/")
      } else {
        installed_version <- readLines("C:/WRDAPP/MODFLOW-NWT/version.txt")
        compare_versions <- compareVersion(version, installed_version)
        if (compare_versions == 0) {
          message("You have already installed this version at:\nC:/WRDAPP/MODFLOW-NWT/")
          install <- readline("Do you want to reinstall? (y/n) ")
        } else if (compare_versions == 1) {
          message("You have installed version ", installed_version, " at:\nC:/WRDAPP/MODFLOW-NWT/")
          install <- readline("Do you want to overwrite it? (y/n) ")
        } else {
          message("It seems like you have a later version (", installed_version, ") than the one found online. I strongly suggest you to check https://water.usgs.gov/ogw/modflow-nwt/ and make changes manually if required.")
          install <- readline("Do you want to overwrite it? (y/n) ")
        }
        if (install == "y") {
          unlink("C:/WRDAPP/MODFLOW-NWT", recursive = TRUE, force = TRUE)
          rmf_install("MODFLOW-NWT")
        }
      }
      
    } else if (name == "MODFLOW-LGR") {
      
      # get the link to the latest version
      
      x <- xml2::read_html("https://water.usgs.gov/ogw/modflow-lgr/") %>% 
        rvest::html_nodes("a") %>% 
        rvest::html_attr("href") %>% 
        subset(grepl(".zip", .) & grepl("mflgr", .))
      version <- gsub("_", "\\.", substr(x, 14, 18))
      message("Found MODFLOW-LGR version ", version, " at:\nhttps://water.usgs.gov/ogw/modflow-lgr/")
      x <- paste0("https://water.usgs.gov/ogw/modflow-lgr/", x)
      
      # install, if already installed ask what to do
      
      if (!dir.exists("C:/WRDAPP/MODFLOW-LGR")) {
        temp <- tempfile()
        message("Downloading file ...")
        download.file(x, temp, quiet = TRUE)
        message("Installing ...")
        unzip(temp, exdir = "C:/WRDAPP")
        folder <- unzip(temp, list = TRUE)$Name[1] %>% # folder name is different from zip file name!
          substr(1, nchar(.) - 1)
        unlink(temp, force = TRUE)
        file.rename(paste0("C:/WRDAPP/", folder), "C:/WRDAPP/MODFLOW-LGR")
        cat(paste0(version, "\n"), file = "C:/WRDAPP/MODFLOW-LGR/version.txt")
        message("You have succesfully installed MODFLOW-LGR at:\nC:/WRDAPP/MODFLOW-LGR/")
      } else {
        installed_version <- readLines("C:/WRDAPP/MODFLOW-LGR/version.txt")
        compare_versions <- compareVersion(version, installed_version)
        if (compare_versions == 0) {
          message("You have already installed this version at:\nC:/WRDAPP/MODFLOW-LGR/")
          install <- readline("Do you want to reinstall? (y/n) ")
        } else if (compare_versions == 1) {
          message("You have installed version ", installed_version, " at:\nC:/WRDAPP/MODFLOW-LGR/")
          install <- readline("Do you want to overwrite it? (y/n) ")
        } else {
          message("It seems like you have a later version (", installed_version, ") than the one found online. I strongly suggest you to check https://water.usgs.gov/ogw/modflow-lgr/ and make changes manually if required.")
          install <- readline("Do you want to overwrite it? (y/n) ")
        }
        if (install == "y") {
          unlink("C:/WRDAPP/MODFLOW-LGR", recursive = TRUE, force = TRUE)
          rmf_install("MODFLOW-LGR")
        }
      }
      
    } else if (name == "MODFLOW-CFP") {
      
      # get the link to the latest version
      
      x <- xml2::read_html("https://water.usgs.gov/ogw/cfp/cfp.htm") %>% 
        rvest::html_nodes("a") %>% 
        rvest::html_attr("href") %>% 
        subset(grepl(".zip", .) & grepl("CFP", .) & !grepl("ogw", .))
      version <- gsub("_", "\\.", substr(x, 5, 10))
      message("Found MODFLOW-CFP version ", version, " at:\nhttps://water.usgs.gov/ogw/cfp/cfp.htm")
      x <- paste0("https://water.usgs.gov/ogw/cfp/", x)
      folder <- "C:/WRDAPP/MODFLOW-CFP"
      
      # install, if already installed ask what to do
      
      if (!dir.exists(folder)) {
        temp <- tempfile()
        message("Downloading file ...")
        download.file(x, temp, quiet = TRUE)
        message("Installing ...")
        dir.create(folder)
        unzip(temp, exdir = folder)
        unlink(temp, force = TRUE)
        cat(paste0(version, "\n"), file = "C:/WRDAPP/MODFLOW-CFP/version.txt")
        message("You have succesfully installed MODFLOW-CFP at:\n", folder, "/")
      } else {
        installed_version <- readLines("C:/WRDAPP/MODFLOW-CFP/version.txt")
        compare_versions <- compareVersion(version, installed_version)
        if (compare_versions == 0) {
          message("You have already installed this version at:\nC:/WRDAPP/MODFLOW-CFP/")
          install <- readline("Do you want to reinstall? (y/n) ")
        } else if (compare_versions == 1) {
          message("You have installed version ", installed_version, " at:\nC:/WRDAPP/MODFLOW-CFP/")
          install <- readline("Do you want to overwrite it? (y/n) ")
        } else {
          message("It seems like you have a later version (", installed_version, ") than the one found online. I strongly suggest you to check https://water.usgs.gov/ogw/cfp/cfp.htm and make changes manually if required.")
          install <- readline("Do you want to overwrite it? (y/n) ")
        }
        if (install == "y") {
          unlink("C:/WRDAPP/MODFLOW-CFP", recursive = TRUE, force = TRUE)
          rmf_install("MODFLOW-CFP")
        }
      }
      
    } else {
      stop("Installing software other than MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not supported.")
    }
}
