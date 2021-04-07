#' Execute a MODFLOW model
#' 
#' These functions execute MODFLOW models.
#' 
#' The `preprocess` argument can be used for different purposes:
#' - Implementing derived parameters: One can make some of the pval parameters
#'   depend on others. The `preprocess` function should in this case just
#'   modify the pval object, and return it. Note that this means that the values
#'   of derived parameters specified in `evaluate` might be changed in this
#'   case.
#' - Implementing any kind of parameter that is not directly supported by
#'   MODFLOW. This means the `preprocess` function should have some side
#'   effects, modifying some of the MODFLOW files. For this purpose, it is
#'   convenient to add extra parameters at the end of the pval file, while
#'   keeping the number of parameters (np) constant, and equal to the number
#'   of MODFLOW-supported parameters.
#' 
#' @param code Name of the MODFLOW variant to use, or path to the executable.
#' @param evaluate Vector of PVAL file parameter values to evaluate. This should
#'   be a named vector if not all parameters are provided in their order of
#'   occurrence, where the names (can be regular expressions to) match the
#'   parameter names. Parameters that are not mentioned take the value from the
#'   PVAL file. If `NULL` (default), no values are changed in the original
#'   PVAL file.
#' @param convergence Character. The message in the terminal output used
#'   to check for convergence.
#' @return Invisible list with start and end time, elapsed run time, a logical
#'   indicating normal termination, and the stdout output, when done for an on
#'   disk model. Full `modflow` object including all results otherwise (in
#'   memory model).
#' @export
#' @seealso
#' [rmf_install()] for external code installation,\cr
#' [rmf_analyze()] for local sensitivity analysis, and\cr
#' [rmf_optimize()] for local calibration.
rmf_execute <- function(...) {
  UseMethod('rmf_execute')
}

#' @rdname rmf_execute
#' @param path Path to the NAM file. Typically with extension `.nam`.
#' @param backup Logical. Should a backup (with `.old` suffix) of the original
#'   PVAL file be created? Defaults to `FALSE`.
#' @param preprocess Function to do preprocessing, which takes the model pval
#'   object as input, and returns another pval object. See details for how to
#'   use this. Defaults to NULL.
#' @param ui If NULL (default), MODFLOW output is shown in the R console. If
#'   `"none"`, the output is suppressed.
#' @export
rmf_execute.character <- function(
  path,
  code = "2005",
  evaluate = NULL,
  backup = FALSE,
  preprocess = NULL,
  ui = NULL,
  convergence = "Normal termination"
) {
  # NOTE The convergence argument was foreseen for custom executables.
  code <- rmfi_find(code)
  
  # get directory and filename
  dir <- dirname(path)
  file <- basename(path)

  # set initial parameters if provided  
  if(!is.null(evaluate) | !is.null(preprocess)) {
    nam <- rmf_read_nam(path)
    if('PVAL' %in% nam$ftype) {
      pval <- rmf_read_pval(rmfi_look_for_path(dir, nam, "pval"))
      
      # pval file backup
        if (backup) rmfi_backup_pval(dir, nam)
      
      # replace evaluate values in parval
        if (!is.null(evaluate)) {
          pval$data$parval <- rmfi_replace_in_vector(pval$data$parnam, pval$data$parval, evaluate)
        }
      
      # preprocess pval file
        if (!is.null(preprocess)) pval <- preprocess(pval)
        
      rmf_write_pval(pval, file.path(dir, nam$fname[which(nam$ftype == 'PVAL')]))
    } else {
      rui::alert('Model does not have pval input file. Ignoring evaluate argument.')
      rui::warn("Issue with PVAL file.")
    }
  }
  
  # run modflow
  mf_stdout <- ! getOption("RMODFLOW.ui") == "none"
  if (!is.null(ui)) mf_stdout <- ! ui == "none"
  out <- processx::run(
    code, file, wd = dir,
    stdout_line_callback = if (mf_stdout) rmfi_line_callback else NULL
  )
  out$stdout <- out$stdout %>%
    stringr::str_split("\\r\\n") %>%
    purrr::pluck(1)
  rmf_execute <- list(
    start = out$stdout %>%
      stringr::str_subset("Run start date and time") %>%
      stringr::str_extract("(?<=: ).*") %>% 
      readr::parse_datetime(format = "%Y/%m/%d %H:%M:%S",
                            locale = readr::locale(tz = Sys.timezone())),
    end = out$stdout %>%
      stringr::str_subset("Run start date and time") %>%
      stringr::str_extract("(?<=: ).*") %>% 
      readr::parse_datetime(format = "%Y/%m/%d %H:%M:%S",
                            locale = readr::locale(tz = Sys.timezone())),
    time = out$stdout %>%
      stringr::str_subset("Elapsed run time") %>%
      stringr::str_extract("(?<=: ).*") %>%
      tolower() %>%
      lubridate::duration(),
    normal_termination = any(grepl(convergence, out$stdout)),
    stdout = out$stdout
  )
  invisible(rmf_execute)
}

#' @rdname rmf_execute
#' @param modflow modflow object
#' @export
rmf_execute.modflow <- function(
  modflow,
  code = "2005",
  evaluate = NULL
) {
  # TODO change class and top-level S3 to "rmf_model" instead of modflow
  # TODO append rmf_execute results to top level list?
  code <- rmfi_find(code)
  
  # temporary directory
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  
  # write all files
  # TODO replace code below with rmf_write()
  rmf_write_nam(modflow$nam, file = 'input.nam')
  for(i in 1:nrow(modflow$nam)) {
    if(modflow$nam$ftype[i] %in% c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF')) {
      object_class <- c('hob','pval','dis','zon','mlt','bas','huf','oc','wel','ghb','pcg','kdep','lpf')[which(c('HOB','PVAL','DIS','ZONE','MULT','BAS6','HUF2','OC','WEL','GHB','PCG','KDEP','LPF') == modflow$nam$ftype[i])]
      if(object_class %in% 'lpf') {
        get(paste0('rmf_write_',object_class))(modflow[[object_class]], file = modflow$nam$fname[i], dis = modflow$dis)
      } else {
        get(paste0('rmf_write_',object_class))(modflow[[object_class]], file = modflow$nam$fname[i])  
      }
    }
  }
  
  # run modflow
  rmf_execute('input.nam', code = code, evaluate = evaluate, backup = FALSE)
  
  # read all output
  
}

#' Analyze a MODFLOW model
#'
#' This function performs local sensitivity analysis of a MODFLOW model .
#'
#' Only works with models using MODFLOW parameters and having a head
#' predictions output file as defined in the HOB object
#'
#' @inheritParams rmf_execute
#' @param include Character vector indicating which PVAL file parameters should
#'   be included. Regular expressions can be used to include multiple parameters
#'   at once. Parameters that are not mentioned are not included. If `NULL`
#'   (default), all parameters are included.
#' @param transform Character vector of transformations. This should be a named
#'   vector if not all parameters are transformed, and listed in their order of
#'   occurrence, where the names (can be regular expressions to) match the
#'   parameter names. Parameters that are not mentioned are not transformed. If
#'   `NULL` (default), no transformations are done. Currently only `"log"` is
#'   supported.
#' @param backup Logical. Should a backup (with `.old` suffix) of the original
#'   PVAL file be created? Defaults to `TRUE`.
#' @param restore Logical. Should the original PVAL file be restored? Defaults
#'   to `FALSE`.
#' @param visualize Logical. Should the results be visualized during the
#'   analysis? Defaults to `TRUE` in an interactive session; `FALSE` otherwise.
#' @param ... Optional arguments passed to [rmf_execute()].
#' @return An `rmf_analyze` object, which is a list with dimensionless scaled
#'   sensitivities (dss) and composite scaled sensitivies (css).
#' @export
#' @seealso
#' [rmf_execute()] for executing a MODFLOW model.\cr
#' [rmf_optimize()] for optimizing a MODFLOW model.
rmf_analyze <- function(path,
                        code = "2005",
                        evaluate = NULL,
                        include = NULL,
                        transform = NULL,
                        backup = TRUE,
                        restore = FALSE,
                        visualize = interactive(),
                        ...) {
  # TODO allow for controlling some settings for the gradient approximation
  # like the step size and one/two-sided approximation etc?
  # TODO consider parallel options here when rmf_execute.modflow works
  # TODO better include restoring in on.exit?
  code <- rmfi_find(code)
  dir <- dirname(path)
  nam <- rmf_read_nam(path)
  
  # pval file backup
  if (backup) rmfi_backup_pval(dir, nam)
  
  # read pval and hob
  if(!('PVAL' %in% nam$ftype) || !('HOB' %in% nam$ftype)) {
    rui::alert("{.fun rmf_analyze} only works with PVAL and HOB file types.")
    rui::error("Issue with model structure.")
  }
  pval <- pval_org <- rmfi_look_for_path(dir, nam, "pval") %>% rmf_read_pval()
  hob <- rmfi_look_for_path(dir, nam, "hob") %>% rmf_read_hob()
  if(hob$iuhobsv == 0 || toupper(nam$ftype[which(nam$nunit == hob$iuhobsv)]) != 'DATA') {
    rui::alert("{.fun rmf_analyze} only works with a HOB output file.",
             "This can be created by setting {.arg iuhobsv} of the HOB file to",
             "a non-zero value, and including a corresponding DATA type entry",
             "in the NAM file.")
    rui::error('Issue with model structure.')
  }
  
  # evaluate, include, transform
  if (is.null(evaluate)) {
    evaluate <- pval$data$parval
  } else {
    evaluate <- rmfi_replace_in_vector(pval$data$parnam, pval$data$parval, evaluate)
  }
  if (is.null(include)) {
    include <- rep(TRUE,length(evaluate))
  } else {
    regex_to_include <- rep(TRUE, length(include))
    names(regex_to_include) <- include
    include <- rep(FALSE, length(pval$data$parnam))
    include <- rmfi_replace_in_vector(pval$data$parnam, include, regex_to_include)
  }
  if(!is.null(transform)) {
    if (!all(transform %in% "log")) {
      rui::alert('Use {.val "log"} in {.arg transform} for logarithmic',
               "transformations. Other options are not available yet.")
      rui::error("Issue with transformation definition.")
    }
    transform[transform == "log"] <- TRUE
    transform <- as.logical(transform) %>% setNames(names(transform))
    transform <- rmfi_replace_in_vector(pval$data$parnam, rep(FALSE, length(pval$data$parnam)), transform)
    evaluate[transform] <- log(evaluate[transform])
  }
  
  # analyze
  rui::begin("Analyzing")
  
  # initial run
  pval$data$parval <- evaluate
  if (any(transform)) pval$data$parval[transform] <- exp(pval$data$parval[transform])
  rmf_write_pval(pval, rmfi_look_for_path(dir, nam, type = "pval"))
  rmf_execute(path = path, code = code, ui = "none", ...)
  hob_out_orig <- rmfi_look_for_path(dir, nam, unit = hob$iuhobsv) %>% 
    rmf_read_hob_out()
  rmf_analyze <- list()
  rmf_analyze$dss <- matrix(NA_real_,
                    nrow = hob$nh,
                    ncol = length(pval$data$parnam))
  rmf_analyze$css <- rep(NA_real_, length(pval$data$parnam))
  rmf_analyze$data$parnam <- pval$data$parnam
  
  # dss & css
  for(i in which(include)) {
    pval$data$parval <- evaluate
    
    # perturbation
    pval$data$parval[i] <- pval$data$parval[i]*1.01
    if (any(transform)) pval$data$parval[transform] <- exp(pval$data$parval[transform])
    
    # evaluation
    rmf_write_pval(pval, rmfi_look_for_path(dir, nam, type = "pval"))
    rmf_execute(path = path, code = code, ui = "none", ...)
    hob_out <- rmf_read_hob_out(rmfi_look_for_path(dir, nam, unit = hob$iuhobsv))
    
    # assignment
    rmf_analyze$dss[,i] <- (hob_out$simulated - hob_out_orig$simulated)/(0.01)
    rmf_analyze$css[i] <- sqrt(sum(rmf_analyze$dss[,i]^2)/hob$nh)
    
    # visualize
    if (visualize) {
      print(rmf_plot.rmf_analyze(rmf_analyze) +
              ggplot2::labs(subtitle = "RMODFLOW progress visualization ...",
                            caption = "This plot reflects a temporary state during analysis."))
    }
  }
  rui::succeed()
  
  # restore original pval
  if (restore) {
    rui::begin("Restoring")
    rmf_write_pval(pval_org, rmfi_look_for_path(dir, nam, type = "pval"))
    rmf_execute(path = path, code = code, ...)
    rui::succeed()
  }

  class(rmf_analyze) <- c("rmf_analyze", class(rmf_analyze))
  rmf_analyze
}

#' Optimize a MODFLOW model
#' 
#' This function performs local optimization of a MODFLOW model.
#'
#' Only works with models using MODFLOW parameters and having a head
#' predictions output file as defined in the HOB object The only method
#' currently available is "Nelder-Mead". See \code{\link{optim}}
#' 
#' @inheritParams rmf_execute
#' @inheritParams rmf_analyze
#' @param start,lower,upper Vectors of PVAL file parameter values to start the
#'   optimization, and corresponding lower and upper limits. These should be
#'   named vectors if not all parameters are provided in their order of
#'   occurrence, where the names (can be regular expressions to) match the
#'   parameter names. Parameters that are not mentioned take the starting value
#'   from the PVAL file, and/or have no constraints. If NULL (default), no
#'   values are changed in the original PVAL file, and/or no constraints are
#'   imposed. `lower` and `upper` also take named lists of functions and/or
#'   numeric values, where the functions are then applied to the `start` or
#'   original PVAL values. Note support for lower and upper limits is achieved
#'   with the Nelder and Mead method by pretending the model cannot be evaluated
#'   when violating them.
#' @param iterate Integer. Maximum number of iterations.
#' @param tolerate Double. Relative convergence tolerance.
#' @param cost Character. The performance measure that should be used as the
#'   cost function. Possible values are those supported by [rmf_performance()].
#'   Defaults to `"ssq"`.
#' @param export Optional file path to export intermediate results to after each
#'   iteration.
#' @param continue To continue from the last parameter set recorded in the
#'   export file, or not. Defaults to FALSE.
#' @param ... Optional arguments passed to [rmf_execute()].
#' @return Invisible list with [optim()] results and the full parameter list.
#' @export
#' @seealso
#' [rmf_execute()] for executing a MODFLOW model.\cr
#' [rmf_analyze()] for analyzing a MODFLOW model.
rmf_optimize <- function(
  path,
  code = "2005",
  start = NULL,
  lower = -Inf, # TODO think about this default value ...
  upper = Inf,
  include = NULL,
  transform = NULL,
  cost = "ssq",
  backup = TRUE,
  restore = FALSE,
  visualize = interactive(),
  export = NULL,
  continue = FALSE,
  iterate = 50,
  tolerate = 1E-4,
  ...
) {
  # TODO add api for choosing which observation file to use
  # TODO add rmf class to returned object? and foresee print/plot methods?
  code <- rmfi_find(code)
  dir <- dirname(path)
  nam <- rmf_read_nam(path)
  
  # pval file backup
  if (backup) rmfi_backup_pval(dir, nam)
  
  # read pval and hob
  if(!('PVAL' %in% nam$ftype) || !('HOB' %in% nam$ftype)) {
    rui::alert("{.fun rmf_optimize} only works with PVAL and HOB file types.")
    rui::error("Issue with model structure.")
  }
  pval <- pval_org <- rmfi_look_for_path(dir, nam, "pval") %>% rmf_read_pval()
  hob <- rmfi_look_for_path(dir, nam, "hob") %>% rmf_read_hob()
  if(hob$iuhobsv == 0 || toupper(nam$ftype[which(nam$nunit == hob$iuhobsv)]) != 'DATA') {
    rui::alert("{.fun rmf_optimize} only works with a HOB output file.",
             "This can be created by setting {.arg iuhobsv} of the HOB file to",
             "a non-zero value, and including a corresponding DATA type entry",
             "in the NAM file.")
    rui::error('Issue with model structure.')
  }

  # continue from previous optimization
  if (continue) {
    if (is.null(export)) {
      rui::alert("You want to continue a previous optimization, but you have",
                 "not provided an export file path.")
      rui::error("Issue with optimization.")
    }
    start <- readr::read_tsv(export) %>%
      dplyr::select(-1, -2) %>%
      dplyr::slice(nrow(.)) %>%
      unlist()
  }
  
  # if restart, remove export file first
  if (!continue & fs::file_exists(export)) fs::file_delete(export)
    
  # start, include, transform, lower, upper
  if (is.null(start)) {
    start <- pval$data$parval
  } else {
    start <- rmfi_replace_in_vector(pval$data$parnam, pval$data$parval, start)
  }
  if (is.null(include)) {
    include <- rep(TRUE,length(start))
  } else {
    regex_to_include <- rep(TRUE, length(include))
    names(regex_to_include) <- include
    include <- rep(FALSE, length(pval$data$parnam))
    include <- rmfi_replace_in_vector(pval$data$parnam, include, regex_to_include)
  }
  if (is.list(lower)) {
    lower <- rmfi_replace_in_vector(pval$data$parnam, rep(-Inf, length(pval$data$parnam)), lower,
                                    start = start)
  } else if (length(lower) == 1 & is.infinite(lower[1])) {
    lower <- rep(lower, length(pval$data$parnam))
  } else {
    lower <- rmfi_replace_in_vector(pval$data$parnam, rep(-Inf, length(pval$data$parnam)), lower)
  }
  if (is.list(upper)) {
    upper <- rmfi_replace_in_vector(pval$data$parnam, rep(Inf, length(pval$data$parnam)), upper,
                                    start = start)
  } else if (length(upper) == 1 & is.infinite(upper[1])) {
    upper <- rep(upper, length(pval$data$parnam))
  } else {
    upper <- rmfi_replace_in_vector(pval$data$parnam, rep(Inf, length(pval$data$parnam)), upper)
  }
  if(!is.null(transform)) {
    if (!all(transform %in% "log")) rui::error("Only logarithmic transforms are currently implemented.")
    transform[transform == "log"] <- TRUE
    transform <- as.logical(transform) %>% setNames(names(transform))
    transform <- rmfi_replace_in_vector(pval$data$parnam, rep(FALSE, length(pval$data$parnam)), transform)
    start[transform] <- log(start[transform])
    lower[transform] <- log(lower[transform])
    upper[transform] <- log(upper[transform])
  }
  
  # optimize
  rui::begin("Optimizing")
  run <- 0
  optimization_history <- matrix(ncol = length(pval$data$parnam) + 1, nrow = 0)
  optim_modflow <- function(included_parval) {
    run <<- run + 1
    
    # adjust values
    pval$data$parval <- start
    pval$data$parval[include] <- included_parval
    if (any(lower > upper)) {
      rui::alert("{.arg lower} contains values larger than {.arg upper}.")
      rui::error("Issue with bounds.")
    }
    
    # check bounds and run modflow
    out_of_bounds <- FALSE
    if (any(pval$data$parval > upper) | any(pval$data$parval < lower)) out_of_bounds <- TRUE
    if(!is.null(transform)) {
      pval$data$parval[transform] <- exp(pval$data$parval[transform])
    } 
    if (out_of_bounds) {
      converged <- FALSE
    } else {
      # write values
      rmf_write_pval(pval,
                     rmfi_look_for_path(dir, nam, "pval"))
      converged <- rmf_execute(path = path, code = code,
                               ui = "none", ...)$normal_termination
    }
    
    # get cost
    if (converged) {
      hob_out <- rmf_read_hob_out(rmfi_look_for_path(dir, nam, unit = hob$iuhobsv))
      cost_value <- rmf_performance(hob_out)[[cost]]
    } else { # return large cost when not converging
      cost_value <- Inf
    }
    
    # keep new step, export, visualize
    new_step <- TRUE # (run + sum(include)*2)%%(sum(include)*2 +1) == 0
    if (new_step) {
      optimization_history <<- optimization_history %>% rbind(c(cost_value, pval$data$parval))
      # print 
      if (! visualize) {
        rui::inform(
          paste(stringr::str_to_upper(cost),
                format(cost_value, scientific = TRUE, digits = 4),
                ifelse(converged, "Converged.", "No convergence."))
        )
      }
    }
    if (!is.null(export)) {
      export_exists <- fs::file_exists(export)
      tibble::tibble(
        cost = cost_value,
        converged = converged,
        parnam = pval$data$parnam,
        parval = pval$data$parval
      ) %>%
        tidyr::spread("parnam", "parval") %>%
        readr::write_tsv(export,
                         append = export_exists,
                         col_names = !export_exists)
    }
    if (visualize & new_step) {
      optimization_history <- as.data.frame(optimization_history)
      names(optimization_history) <- c("cost", pval$data$parnam)
      p <- optimization_history %>% 
        dplyr::mutate(run = 1:nrow(.)) %>% 
        dplyr::mutate_at(pval$data$parnam[transform], log) %>% 
        tidyr::gather("parameter", "value", -run, -1) %>% 
        dplyr::filter(parameter %in% pval$data$parnam[include]) %>% 
        dplyr::mutate(parameter = ifelse(parameter %in% pval$data$parnam[transform],
                                         paste0("log(", parameter, ")"),
                                         parameter)) %>% 
        ggplot2::ggplot() +
        ggplot2::aes(run, value) +
        (if (run > 1) ggplot2::geom_line(colour = "grey90") else NULL) +
        ggplot2::geom_point(ggplot2::aes(colour = cost), size = 3) +
        ggplot2::facet_grid(rows = vars(parameter), scales = "free_y") +
        ui_theme() +
        ui_colour_c("cold", rev = TRUE)
      print(ui_plot(p) +
              ggplot2::labs(
                title = "Optimization trace",
                subtitle = "RMODFLOW progress visualization ...",
                caption = "This plot reflects a temporary state during optimization.",
                x = "Iteration number",
                y = "Parameter value",
                colour = paste0("Cost\n(", toupper(cost), ")")))
      # TODO think of including local sensitivity based on sensitivity runs
      # TODO rethink use of ui_plot for non-S3 methods
    }
    
    cost_value
  }
  rmf_optimize <- optim(start[include],
                        optim_modflow,
                        method = 'Nelder-Mead',
                        control = list(maxit = iterate,
                                       reltol = tolerate))
  rmf_optimize$included <- include
  rmf_optimize$parnam <- pval$data$parnam
  rmf_optimize$parval <- start
  rmf_optimize$parval[include] <- rmf_optimize$par
  if (!is.null(transform)) {
    rmf_optimize$parval[transform] <- exp(rmf_optimize$parval[transform])
    # TODO include lower, upper etc.? with correct backtransform?
  }
  rmf_optimize$trace <- optimization_history %>% 
    tibble::as_tibble() %>%
    purrr::set_names(c("cost", pval$data$parnam)) %>% 
    dplyr::mutate(run = 1:nrow(.))
  switch(rmf_optimize$convergence + 1, rui::succeed(), rui::fail())
  if (rmf_optimize$convergence == 1) {
    rui::alert("Optimization has not converged in {.arg maxit} iterations!")
    rui::warn("Issue with optimization.")
  }
  
  # restore original pval
  if (restore) {
    rui::begin("Restoring")
    rmf_write_pval(pval_org, rmfi_look_for_path(dir, nam, type = "pval"))
    rmf_execute(path = path, code = code, ...)
    rui::succeed()
  }
  
  invisible(rmf_optimize)
}

#' Find paths to executables
#'
#' This function tries to locate external code executables.
#'
#' It first looks for the executable in the current working directory. If not
#' there, it looks in the bin subfolder of `getOption("RMODFLOW.path")`, where
#' the software might have been installed by [rmf_install()]. If the executable
#' cannot be found, a final attempt is made by checking the system path
#' variable. If it still cannot be located, an error is thrown.
#' 
#' @inheritParams rmf_execute
#' @param precision Character. Can be \code{"single"} or \code{"double"}. Only
#'   relevant for MODFLOW-2005.
#' @return Path to the executable.
rmfi_find <- function(
  code = "2005",
  precision = "single"
) {
  # TODO automatically select 32 bit executables if available, otherwise throw
  # error on 32 bit systems
  if (file.exists(code)) return(code)
  if (grepl("2005", code)) {
    if (grepl("dbl", code)) precision <- "double"
    code <- "MODFLOW-2005"
    executable <- ifelse(precision == "single", "mf2005.exe", "mf2005dbl.exe")
    folder <- ""
    rmf_install_bin_folder <- file.path(getOption("RMODFLOW.path"), code,
                                     "bin")
    if (!file.exists(executable)) {
      if (file.exists(file.path(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        rui::error("Path to {code} executable not found.")
      }
    }
    return(file.path(folder, executable))
  }
  if (grepl("owhm", code, ignore.case = TRUE)) {
    code <- "MODFLOW-OWHM"
    executable <- "MF_OWHM.exe"
    folder <- ""
    rmf_install_bin_folder <- file.path(getOption("RMODFLOW.path"), code,
                                     "bin")
    if (!file.exists(executable)) {
      if (file.exists(file.path(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        rui::error("Path to {code} executable not found.")
      }
    }
    return(file.path(folder, executable))
  }
  if (grepl("nwt", code, ignore.case = TRUE)) {
    code <- "MODFLOW-NWT"
    executable <- "MODFLOW-NWT_64.exe"
    folder <- ""
    rmf_install_bin_folder <- file.path(getOption("RMODFLOW.path"), code,
                                     "bin")
    if (!file.exists(executable)) {
      if (file.exists(file.path(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        rui::error("Path to {code} executable not found.")
      }
    }
    return(file.path(folder, executable))
  } 
  if (grepl("lgr", code, ignore.case = TRUE)) {
    code <- "MODFLOW-LGR"
    executable <- "mflgr.exe"
    folder <- ""
    rmf_install_bin_folder <- file.path(getOption("RMODFLOW.path"), code,
                                     "bin")
    if (!file.exists(executable)) {
      if (file.exists(file.path(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        rui::error("Path to {code} executable not found.")
      }
    }
    return(file.path(folder, executable))
  } 
  if (grepl("cfp", code, ignore.case = TRUE)) {
    code <- "MODFLOW-CFP"
    executable <- "mf2005cfp.exe"
    folder <- ""
    rmf_install_bin_folder <- file.path(getOption("RMODFLOW.path"), code,
                                     "bin")
    if (!file.exists(executable)) {
      if (file.exists(file.path(rmf_install_bin_folder, executable))) {
        folder <- rmf_install_bin_folder
      } else if (Sys.which(executable) == "") {
        rui::error("Path to {code} executable not found.")
      }
    }
    return(file.path(folder, executable))
  }
  rui::alert("Finding paths to the executables of codes other than ",
           "MODFLOW-2005, MODFLOW-OWHM, MODFLOW-NWT, MODFLOW-LGR or ",
           "MODFLOW-CFP is currently not supported.")
  rui::error("Issue with code path.")
}

#' Look for a file path in a NAM file
#'
#' @param dir Character. Path to directory containing the NAM file.
#' @param nam Character. File name of the NAM file.
#' @param type Character. File type (ftype) of the entry to look for.
#' @param unit Integer. Unit number of the entry to look for.
#' @return
rmfi_look_for_path <- function(dir, nam, type = NULL, unit = NULL) {
  if (!is.null(type)) {
    return(file.path(dir, nam$fname[which(nam$ftype == stringr::str_to_upper(type))]))
  }
  if (!is.null(unit)) {
    return(file.path(dir, nam$fname[which(nam$nunit == unit)]))
  }
  rui::alert("Either {.arg type} or {.arg unit} should be provided.")
  rui::error("Issue with arguments.")
}

rmfi_line_callback <- function(line, process) {
  line <- stringr::str_squish(line)
  if (line == "") return(invisible())
  if (line %in% c("MODFLOW-2005", "MODFLOW-LGR2", "MODFLOW-NWT-SWR1",
                  "OWHM 1.0")) {
    rui::entitle(line)
    return(invisible())
  }
  if (grepl("Normal termination of simulation", line)) {
    rui::approve(line)
    return(invisible())
  }
  if (grepl("FAILED TO MEET SOLVER CONVERGENCE CRITERIA", line)) {
    rui::disapprove(line)
    return(invisible())
  }
  if (grepl("Can't find name file", line)) {
    rui::alert(line)
    rui::error("Issue with the name file path.")
    return(invisible())
  }
  if (grepl("NAME FILE IS EMPTY", line)) {
    rui::alert(line)
    rui::error("Issue with the name file.")
    return(invisible())
  }
  if (grepl("Solving: Stress period: 1 Time step: 1", line, fixed = TRUE)) {
    rui::begin(line)
    return(invisible())
  }
  if (grepl("Solving: ", line, fixed = TRUE)) {
    rui::proceed(line)
    return(invisible())
  }
  if (grepl("Run end ", line)) rui::clear()
  rui::inform(line)
  invisible()
}

#' Replace values in a vector with corresponding parameter names
#' 
#' This function is a helper for processing the arguments of [rmf_execute()],
#' [rmf_analyze()] and [rmf_optimize()] that can be named vectors, a named
#' lists of functions.
#'
#' @param parnam Character vector of parameter names from a PVAL file.
#' @param parval Vector of values. Can be numeric as in PVAL file, but also
#'   character for *e.g.* the transformation. 
#' @param new Named numeric vector, or named list of functions and/or numeric
#'   values.
#' @return
rmfi_replace_in_vector <- function(parnam, parval, new, start = parval) {
  if (is.null(names(new)) & length(new) == length(parnam)) return(new)
  if (!is.null(names(new))) {
    if (is.list(new)) {
      for (i in 1:length(new)) {
        if (is.function(new[[i]])) {
          parval[grepl(names(new)[i], parnam)] <- new[[i]](
            start[grepl(names(new)[i], parnam)])
        } else {
          parval[grepl(names(new)[i], parnam)] <- new[[i]]
        }
      }
      return(parval)
    } else {
      for (i in 1:length(new)) {
        parval[grepl(names(new)[i], parnam)] <- new[i]
      }
      return(parval)
    }
  }
  rui::alert("Length of one of the vector arguments does not equal the number of",
           "parameters in the PVAL file, and the vector is not named.")
  rui::error("Issue with the function arguments.")
}

#' Backup a PVAL file
#'
#' @inheritParams rmfi_look_for_path
rmfi_backup_pval <- function(dir, nam) {
  if (file.exists(paste0(rmfi_look_for_path(dir, nam, "pval"), ".old"))) {
    rui::warn("Backup of the original PVAL file is already there.")
  } else {
    path <- rmfi_look_for_path(dir, nam, "pval")
    file.copy(
      path,
      paste0(path, ".old")
    )
  }
  invisible()
}
