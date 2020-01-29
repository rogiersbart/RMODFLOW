.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.RMODFLOW <- list(
    RMODFLOW.path = paste0(system.file(package = "RMODFLOW"), "/code")
  )
  toset <- !(names(opts.RMODFLOW) %in% names(opts))
  if(any(toset)) options(opts.RMODFLOW[toset])
  invisible()
}