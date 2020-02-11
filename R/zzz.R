.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.RMODFLOW <- list(
    RMODFLOW.path = paste0(system.file(package = "RMODFLOW"), "/code"),
    RMODFLOW.ui = "verbose"
  )
  toset <- !(names(opts.RMODFLOW) %in% names(opts))
  if(any(toset)) options(opts.RMODFLOW[toset])
  invisible()
}

# options
# path: enable users to set the code installation directory
# ui: allow for a hierarchy of verbosity
#   verbose: everything
#   quiet: only warn and code output
#   silent: only code output
#   none: nothing

# TODO: this has to enter documentation somewhere!