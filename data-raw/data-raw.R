rmfd_guide_shortcuts <- tibble::tribble(
  ~shortcut, ~name,
  "pval", "parameter_value_file",
  "nam", "name_file"
)
rmfd_supported_codes <- c("MODFLOW-2005", "MODFLOW-OWHM", "MODFLOW-NWT",
                          "MODFLOW-LGR", "MODFLOW-CFP")
usethis::use_data(
  rmfd_supported_codes,
  rmfd_guide_shortcuts,
  internal = TRUE,
  overwrite = TRUE
)
