#' Generic function to export tables from RMODFLOW arrays
#' 
#' @rdname export_table
#' @export
export_table <- function(...) {
  UseMethod('export_table')
}
