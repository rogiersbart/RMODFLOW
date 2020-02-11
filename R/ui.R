# inline styling options: {.style text}
# .emph .strong .code .pkg .fn .fun .arg .key .kbd .file .path .email .url
# .envvar .var .val

# TODO make all of these behave according to RMODFLOW.ui option
ui_text <- function(...) cli::cli_text(..., .envir = parent.frame())
ui_title <- function(x) cli::cli_text(cli::col_yellow("#"), " ", x, .envir = parent.frame())
ui_info <- function(x) cli::cli_text(cli::col_cyan("i"), " ", x, .envir = parent.frame())
ui_done <- function(x) cli::cli_text(cli::col_green("v"), " ", x, .envir = parent.frame())
ui_alert <- function(x) cli::cli_text(cli::col_red("!"), " ", x, .envir = parent.frame())
ui_fail <- function(x) cli::cli_text(cli::col_red("x"), " ", x, .envir = parent.frame())
ui_start <- function(x) {
  cli::cli_status(msg = paste0("{cli::col_yellow('~')} ", x, " ..."),
                  msg_done = paste0("{cli::col_green('v')} ", x, " ... done"),
                  msg_failed = paste0("{cli::col_red('x')} ", x, " ... failed"),
                  .envir = parent.frame())
}
ui_update <- function(x) {
  cli::cli_status_update(msg = paste0("{cli::col_yellow('~')} ", x, " ..."),
                         msg_done = paste0("{cli::col_green('v')} ", x, " ... done"),
                         msg_failed = paste0("{cli::col_red('x')} ", x, " ... failed"),
                         .envir = parent.frame())
}
ui_end <- cli::cli_status_clear
ui_end_done <- cli::cli_process_done
ui_end_fail <- cli::cli_process_failed
ui_code <- usethis::ui_code_block
ui_todo <- function(x) cli::cli_text(cli::col_red('O'), " ", x, .envir = parent.frame())
ui_warn <- usethis::ui_warn
ui_stop <- usethis::ui_stop
ui_ask <- function(x) {
  # based on usethis::ui_yeah
  cli::cli_text(cli::col_yellow('?'), " ", x, .envir = parent.frame())
  if (!interactive()) {
    ui_stop("User input required, but session is not interactive.")
  }
  ayes <- c("Yes", "Definitely", "For sure", 
            "Yup", "Yeah", "I agree", "Absolutely")
  nays <- c("No way", "Not now", "Negative", 
            "No", "Nope", "Absolutely not")
  qs <- c(sample(ayes, 1), sample(nays, 2))
  ord <- sample(length(qs))
  out <- utils::menu(qs[ord])
  out != 0L && (ord == 1)[[out]]
}
# test_ui <- function() {
#   ui_title("UI function testing")
#   ui_info("ui_info")
#   ui_done("ui_done")
#   ui_alert("ui_alert")
#   ui_fail("ui_fail")
#   ui_start("Downloading")
#   Sys.sleep(0.2)
#   ui_update("Installing")
#   a <- sample(1:3, 1)
#   ui_info("This is some information during a status bar.")
#   Sys.sleep(0.2)
#   switch(a,
#          ui_end_done(),
#          ui_end_fail(),
#          ui_end()
#   )
#   ui_code("ui_code")
#   ui_todo("ui_todo")
#   ui_ask("ui_ask")
#   ui_warn("ui_warn")
#   ui_stop("ui_stop")
#   invisible()
# }