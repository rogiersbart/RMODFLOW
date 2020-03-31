.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.RMODFLOW <- list(
    RMODFLOW.path = paste0(system.file(package = "RMODFLOW"), "/code"),
    RMODFLOW.ui = "verbose",
    RMODFLOW.theme = "RMODFLOW"
  )
  toset <- !(names(opts.RMODFLOW) %in% names(opts))
  if(any(toset)) options(opts.RMODFLOW[toset])
  ui_alert("{.pkg RMODFLOW} is still in its experimental lifecycle stage.")
  ui_alert("Use at your own risk, and submit issues here:")
  ui_alert("{.url https://github.com/rogiersbart/RMODFLOW/issues}")
  invisible()
}

# options
# path: enable users to set the code installation directory
# ui: allow for a hierarchy of verbosity
#   verbose: everything
#   quiet: only warn and code output
#   silent: only code output
#   none: nothing
# theme: ggplot2 theme, scales and template
#   RMODFLOW: recommended theme, encouraging users to tweak things
#   ggplot2: ggplot2 default behaviour

# TODO: this has to enter documentation somewhere!