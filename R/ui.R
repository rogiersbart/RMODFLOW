# Notes ----
  
  # NOTE available inline styling options: {.style text}
  # .emph .strong .code .pkg .fn .fun .arg .key .kbd .file .path .email .url
  # .envvar .var .val
  
  # NOTE glue expressions should work in all of these

  # TODO make all of these behave according to RMODFLOW.ui option

# Standard text messages ----

  ui_text <- function(...) cli::cli_text(paste(...), .envir = parent.frame())
  ui_title <- function(...) cli::cli_text(cli::col_yellow("#"), " ", paste(...),
                                          .envir = parent.frame())
  
    # Use for:
    # - Code name when executing
  
  ui_info <- function(...) cli::cli_text(cli::col_cyan("i"), " ", paste(...), .envir = parent.frame())
  
    # Use for:
    # - Additional information provided by external code
  
  ui_done <- function(...) cli::cli_text(cli::col_green("v"), " ", paste(...), .envir = parent.frame())
  
    # Use for:
    # - Confirmation of succesful completion by external code
  
  ui_alert <- function(...) cli::cli_text(cli::col_red("!"), " ", paste(...), .envir = parent.frame())
  
    # Use for:
    # - Alerts at package startup
    # - Messages with styling before signaling conditions with ui_warn or ui_stop
  
  ui_fail <- function(...) cli::cli_text(cli::col_red("x"), " ", paste(...), .envir = parent.frame())
  
    # Use for:
    # - Message for unsuccesful completion by external code

# Status information ----

  ui_start <- function(...) {
    cli::cli_status(msg = paste0("{cli::col_yellow('~')} ", paste(...), " ..."),
                    msg_done = paste0("{cli::col_green('v')} ", paste(...), " ... done"),
                    msg_failed = paste0("{cli::col_red('x')} ", paste(...), " ... failed"),
                    .envir = parent.frame())
  }
  ui_update <- function(...) {
    cli::cli_status_update(msg = paste0("{cli::col_yellow('~')} ", paste(...), " ..."),
                           msg_done = paste0("{cli::col_green('v')} ", paste(...), " ... done"),
                           msg_failed = paste0("{cli::col_red('x')} ", paste(...), " ... failed"),
                           .envir = parent.frame())
  }
  ui_end <- cli::cli_status_clear
  ui_end_done <- cli::cli_process_done
  ui_end_fail <- cli::cli_process_failed
  
  # Use for:
  # - Downloads?
  # - Optimization?
  # - Sensitivity analysis?
  
# User interaction ----
  
  ui_code <- usethis::ui_code_block
  ui_todo <- function(...) cli::cli_text(cli::col_red('O'), " ", paste(...), .envir = parent.frame())
  ui_ask <- function(...) {
    # NOTE this is based on usethis::ui_yeah, but uses text symbols only for
    # consistent behaviour on all consoles/operating systems
    cli::cli_text(cli::col_yellow('?'), " ", paste(...), .envir = parent.frame())
    if (!interactive()) {
      ui_stop("User input required, but session is not interactive.")
    }
    selection <- utils::menu(c("Yes please.", "No thanks."))
    if (selection == 0) ui_stop("A choice is required.")
    as.logical(2 - selection)
  }

# Conditions ----

  ui_warn <- function(...) usethis::ui_warn(paste(...))
  ui_stop <- function(...) usethis::ui_stop(paste(...))
  # NOTE for styled messages, use ui_alert before signaling conditions
  
# Testing all of the above ----
  
  ui_test_messages <- function() {
    message("Standard text messages:")
    ui_title("ui_title")
    ui_info("ui_info")
    ui_done("ui_done")
    ui_alert("ui_alert")
    ui_fail("ui_fail")
    message("Status information:")
    ui_start("Status that will succeed")
    Sys.sleep(2)
    ui_end_done()
    ui_start("Status that will fail")
    Sys.sleep(2)
    ui_end_fail()
    ui_start("Status that will disappear")
    Sys.sleep(2)
    ui_end()
    ui_start("Status")
    Sys.sleep(2)
    ui_info("Information provided during a status bar.")
    Sys.sleep(2)
    ui_update("Status - Almost there")
    Sys.sleep(2)
    ui_end_done()
    message("User interaction:")
    ui_code("ui_code")
    ui_todo("ui_todo")
    ui_ask("ui_ask")
    message("Conditions:")
    ui_alert("{.fun ui_warn} will now provide a warning message")
    ui_warn("ui_warn")
    ui_alert("{.fun ui_stop} will now provide an error message")
    ui_stop("ui_stop")
    invisible()
  }
  
# Theme, scales and template for ggplot2 ----
  
  ui_plot <- function(p, type = "default", class = "unknown") {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(p)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      p <- p + ggplot2::labs(
          subtitle = paste0('RMODFLOW "', type, '" plot for "', class, '" object'),
          caption = "We strongly recommend tweaking this plot to your specific needs."
        )
      return(p)
    }
    ui_alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    ui_stop("Issue with RMODFLOW options.")
  }
  ui_theme <- function(...) {
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(ggplot2::theme_minimal(
        base_size = 13,
        base_line_size = .8,
        base_rect_size = .8
      ) +
        ggplot2::theme(...))
    }
    if (getOption("RMODFLOW.theme") == "ggplot2") {
      return(ggplot2::theme(...))
    }
    ui_alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    ui_stop("Issue with RMODFLOW options.")
  }
  ui_fill_c <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::fill_c(...))
    }
    ui_alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    ui_stop("Issue with RMODFLOW options.")
  }
  ui_fill_d <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::fill_d(...))
    }
    ui_alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    ui_stop("Issue with RMODFLOW options.")
  }
  ui_colour_c <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::colour_c(...))
    }
    ui_alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    ui_stop("Issue with RMODFLOW options.")
  }
  ui_colour_d <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::colour_d(...))
    }
    ui_alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    ui_stop("Issue with RMODFLOW options.")
  }

