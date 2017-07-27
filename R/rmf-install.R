#' Install external software
#'
#' @param software character
#' 
#' @details Currently supported software: MODFLOW-2005.
#'
#' @examples
#' rmf_install("MODFLOW-2005")
rmf_install <- function(software = "MODFLOW-2005") {
  if(software == "MODFLOW-2005") {
    x <- xml2::read_html("https://water.usgs.gov/ogw/modflow/MODFLOW.html")
    x <- x %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      subset(grepl(".zip", .) & !grepl("u", .) & grepl("MODFLOW-2005", .))
    version <- substr(x[1], 15, 21)
    message("Found MODFLOW-2005 version ", version, ".")
    if(system.file("MODFLOW-2005", package = "RMODFLOW") == "") {
      x <- paste0("https://water.usgs.gov/ogw/modflow/", x[1])
      temp <- tempfile()
      download.file(x, temp)
      unzip(temp, exdir = system.file(package = "RMODFLOW"))
      unlink(temp)
      file.rename(system.file(substr(basename(x), 1, nchar(basename(x))-4), package = "RMODFLOW"),
                  paste0(system.file(package = "RMODFLOW"), "/MODFLOW-2005"))
      cat(version, "\n", file = paste0(system.file("MODFLOW-2005", package = "RMODFLOW"), "/version.txt"))
    } else {
      current_version <- readLines(system.file("MODFLOW-2005/version.txt", package = "RMODFLOW"))
      compVersion <- compareVersion(version, current_version)
      if (compVersion == 0) {
        message("You have already installed the latest version.")
        reinstall <- readline("Do you want to reinstall? (y/n) ")
        if (reinstall == "y") {
          unlink(system.file("MODFLOW-2005", package = "RMODFLOW"), recursive = TRUE)
          rmf_install("MODFLOW-2005")
        }
      } else if (compVersion == -1) {
        message("Your current version is later than the one we find online.")
        downgrade <- readline("Do you want to downgrade? (y/n) ")
        if (downgrade == "y") {
          unlink(system.file("MODFLOW-2005", package = "RMODFLOW"), recursive = TRUE)
          rmf_install("MODFLOW-2005")
        }
      }
    }
  } else {
    stop("Installing software other than MODFLOW-2005 is currently not supported.")
  }
}
