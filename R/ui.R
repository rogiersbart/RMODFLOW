# Console UI ----

  # use rui functions

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
    rui::alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    rui::error("Issue with RMODFLOW options.")
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
    rui::alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    rui::error("Issue with RMODFLOW options.")
  }
  ui_fill_c <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::fill_c(...))
    }
    rui::alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    rui::error("Issue with RMODFLOW options.")
  }
  ui_fill_d <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::fill_d(...))
    }
    rui::alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    rui::error("Issue with RMODFLOW options.")
  }
  ui_colour_c <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::colour_c(...))
    }
    rui::alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    rui::error("Issue with RMODFLOW options.")
  }
  ui_colour_d <- function(...) {
    if (getOption("RMODFLOW.theme") == "ggplot2") return(NULL)
    if (getOption("RMODFLOW.theme") == "RMODFLOW") {
      return(spectralscale::colour_d(...))
    }
    rui::alert('Option {.arg RMODFLOW.theme} should be {.val "RMODFLOW"} or {.val "ggplot2"}.')
    rui::error("Issue with RMODFLOW options.")
  }

