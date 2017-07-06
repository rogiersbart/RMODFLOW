#' Generic function to export tables from RMODFLOW arrays
#' 
#' @rdname rmf_export_table
#' @export
rmf_export_table <- function(...) {
  UseMethod('rmf_export_table')
}

#' @describeIn rmf_export_table Deprecated function name
export_table <- function(...) {
  .Deprecated(new = "rmf_export_table", old = "export_table")
  rmf_export_table(...)
}
