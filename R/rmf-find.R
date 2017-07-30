#' Find paths to executables
#' 
#' \code{rmf_find} finds the path to external software executables.
#' 
#' The function first looks for the executable in the current working
#' directory. If not there, it looks in \file{C:/WRDAPP/}, where the
#' software might have been installed by \code{\link{rmf_install}}. If the
#' executable cannot be found, the system path variable is used in an attempt
#' to locate it. If it still cannot be located, you should first install the
#' software or add the folder with the executable to the system path.
#'
#' @param name Character. Name of the software. Currently supported values are \code{"MODFLOW-2005"}, \code{"MODFLOW-OWHM"}, \code{"MODFLOW-NWT"}, \code{"MODFLOW-LGR"} and \code{"MODFLOW-CFP"}.
#' @param precision Character. Can be \code{"single"} or \code{"double"}. Only relevant for MODFLOW-2005.
#'
#' @return Path to the executable.
#' @export
#'
#' @examples
#' rmf_find("MODFLOW-2005")
rmf_find <- function(name = "MODFLOW-2005",
                     precision = "single") {
  if (name == "MODFLOW-2005") {
    executable <- ifelse(precision == "single", "mf2005.exe", "mf2005dbl.exe")
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-OWHM") {
    executable <- "MF_OWHM.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-NWT") {
    executable <- "MODFLOW-NWT_64.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-LGR") {
    executable <- "mflgr.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/bin/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else if (name == "MODFLOW-CFP") {
    executable <- "mf2005cfp.exe"
    folder <- ""
    rmf_install_bin_folder <- paste0("C:/WRDAPP/", name, "/")
    if (!file.exists(executable)) {
      if (file.exists(paste0(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        stop("Path to ", name, " executable not found.")
      }
    }
    return(paste0(folder, executable))
  } else {
    stop("Finding paths to the executables of software other than MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or MODFLOW-CFP is currently not supported.")
  }
}