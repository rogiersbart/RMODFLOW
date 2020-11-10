#' Browse the RMODFLOW pkgdown website
#' 
#' This function can be used to launch the [RMODFLOW pkgdown
#' website](https://rogiersbart.github.io/RMODFLOW/) from the R console.
#'
#' @export
#' @seealso [rmf_guide()] for browsing the online guide pages.
#' @examples
#' rmf_browse()
rmf_browse <- function() {
  # TODO add external code webpages with similar shortcuts as above. For
  # instance for MODFLOW variants.
  rui::begin("Launching")
  Sys.sleep(.1) # NOTE status bar doesn't appear otherwise
  browseURL("https://rogiersbart.github.io/RMODFLOW/")
  rui::succeed()
  invisible()
}

#' Launch the online guide pages
#' 
#' This function can be used to launch the [Online Guide to
#' MODFLOW](https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/) or
#' specific pages thereof. Other online guides are available as well, but this
#' one should be the most complete version with information on all different
#' MODFLOW 2005 variants that are part of the RMODFLOW scope. Some shortcuts are
#' available as well, to more easily access some of the pages through the
#' MODFLOW file type or ModelMuse file extension conventions.
#'
#' @param name Name of the `.htm` file linked to in the online guide contents or
#'   index menu. If `NULL` (default), the introduction page is launched.
#' @export
#' @seealso [rmf_browse()] for launching the [RMODFLOW pkgdown
#'   website](https://rogiersbart.github.io/RMODFLOW/).
#' @examples
#' rmf_guide() # Launches the introduction page.
#' rmf_guide("name_file") # Launches the "name_file.htm" page.
#' rmf_guide("nam") # Launches the same page using the "nam" shortcut.
rmf_guide <- function(name = NULL) {
  rui::begin("Launching")
  Sys.sleep(.1) # NOTE status bar doesn't appear otherwise
  online_guide_url <- "https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/"
  if (!is.null(name)) online_guide_url <- rmfi_guide_url(name)
  online_guide_url %>% browseURL()
  rui::succeed()
  invisible()
}

#' Complete guide url from page name or shortcut
#' 
#' @param name Name of the online guide page to launch, or a shortcut from
#'   `rmfd_guide_shortcuts` that can be translated to one.
#' @return Complete URL of the online guide page.
rmfi_guide_url <- function(name) {
  # TODO extend the list of shortcuts
  df <- rmfd_guide_shortcuts
  if (name %in% df$shortcut)
    name <- df$name[which(df$shortcut == name)]
  paste0(
    "https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?",
    name,
    ".htm"
  )
}
