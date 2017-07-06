#' Convert a hob object to a time series data frame
#' 
#' @param hob hob object
#' @param dis dis object
#' @param prj prj object
#' @return time series data frame containing name, time and head columns
#' @export
rmf_convert_hob_to_time_series <- function(hob,
                                           dis,
                                           prj) {
  toffset <- lubridate::days(ifelse(hob$irefsp==1,0,cumsum(dis$perlen)[hob$irefsp-1]) + hob$toffset * hob$tomulth)
  time_series <- data.frame(name = hob$obsloc, time = prj$starttime + toffset, head = hob$hobs)
  return(time_series)
}

#' @describeIn rmf_convert_hob_to_time_series Deprecated function name
convert_hob_to_time_series <- function(...) {
  .Deprecated(new = "rmf_convert_hob_to_time_series", old = "convert_hob_to_time_series")
  rmf_convert_hob_to_time_series(...)
}
