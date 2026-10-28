no_default_function_provided_debug <- function(args_list, fun_name) {
  # Get the class of each argument as a string (fixed CHARACTER to character)
  arg_classes <- vapply(
    args_list,
    function(x) {
      if (is.null(x)) "NULL" else paste(class(x), collapse = "/")
    },
    character(1)
  )

  # Format into a readable string: "arg1 (class), arg2 (class), ..."
  formatted_args <- sprintf("%s (%s)", names(arg_classes), arg_classes)
  error_details <- paste(formatted_args, collapse = "\n  ")

  stop(
    paste0(
      "Generic '",
      fun_name,
      "' function cannot be called directly.\n",
      "Received arguments:\n  ",
      error_details
    ),
    call. = FALSE
  )
}

#_______________________________________________________________________________
#----                              run_analysis                             ----
#_______________________________________________________________________________

#' Run a forest plot or sensitivity analysis.
#'
#' @param object OAT analysis, a forest plot or a sensitivity analysis to execute.
#' @param outputs outputs, character vector with the model parameters to look at (e.g. 'CL') or a NCA analysis
#' @param model Campsis model
#' @param dataset Campsis dataset.
#' For sensitivity analyses: treatment, observations and covariates are reused (covariates must be fixed).
#' For forest plots: treatment and observations are reused (covariates are automatically excluded).
#' @param arm_index arm index, default is 1L (the first arm). Allows to select a specific arm from the dataset.
#' @param settings Campsis settings
#' @return OAT analysis including the results
#' @export
#' @rdname run_analysis
run_analysis <- function(object, outputs, model, dataset, arm_index, settings) {
  stop("No default function is provided")
}

# @importFrom campsis Observations Settings set_subjects
setGeneric("run_analysis", function(object, outputs, model, dataset = NULL, arm_index = NULL, settings = NULL) {
  if (is.null(dataset)) {
    dataset <- campsis::Dataset(1) |>
      add(campsis::Observations(times = 0))
  }
  if (is.null(settings)) {
    settings <- campsis::Settings()
  }
  if (is.null(arm_index)) {
    arm_index <- 1L
  } else {
    arm_index <- as.integer(arm_index)
  }

  # Select specific arm
  dataset@arms@list <- dataset@arms@list[arm_index]

  # Number of subjects in arm must be one
  dataset <- dataset |>
    campsis::set_subjects(1)

  standardGeneric("run_analysis")
})

#_______________________________________________________________________________
#----                              get_label                                ----
#_______________________________________________________________________________

#' Get label.
#'
#' @param object generic object
#' @param ... extra arguments
#' @return a label
#' @rdname get_label
get_label <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("get_label", function(object, ...) {
  standardGeneric("get_label")
})

#_______________________________________________________________________________
#----                            get_forest_plot                            ----
#_______________________________________________________________________________

#' Get forest plot.
#'
#' @param object generic object
#' @param index output number, 1 by default
#' @param relative relative change or absolute value, logical
#' @param show_caption show caption about reference subject and CI percentage
#' @param show_labels show the numeric labels
#' @param show_ref show vertical reference (at 1 or at the baseline)
#' @param show_range show vertical dashed lines, logical
#' @param range vertical dashed lines range, default is c(0.8, 1.25)
#' @param ci confidence interval, default is 0.9, which corresponds to a 90\% CI
#' @param limits limits of the forest plot, numeric vector (2 values)
#' @param ... extra arguments like 'geom_label_vjust', 'geom_label_nudge_x', 'geom_label_nudge_y', 'geom_label_size', 'label_nsig', 'geom_hline_color'
#' @return ggplot
#' @export
#' @rdname get_forest_plot
get_forest_plot <- function(
  object,
  index,
  relative,
  show_caption,
  show_labels,
  show_ref,
  show_range,
  range,
  ci,
  limits,
  ...
) {
  no_default_function_provided_debug(mget(names(formals()), envir = environment()), "get_forest_plot")
}

setGeneric(
  "get_forest_plot",
  function(
    object,
    index = NULL,
    relative = NULL,
    show_caption = NULL,
    show_labels = NULL,
    show_ref = NULL,
    show_range = NULL,
    range = NULL,
    ci = NULL,
    limits = NULL,
    ...
  ) {
    if (is.null(index)) {
      index <- 1L
    } else {
      index <- as.integer(index)
    }
    if (is.null(relative)) {
      relative <- TRUE
    }
    if (is.null(show_caption)) {
      show_caption <- TRUE
    }
    if (is.null(show_labels)) {
      show_labels <- TRUE
    }
    if (is.null(show_ref)) {
      show_ref <- TRUE
    }
    if (is.null(show_range)) {
      show_range <- TRUE
    }
    if (is.null(range)) {
      range <- c(0.8, 1.25)
    }
    if (is.null(ci)) {
      ci <- 0.9
    }
    if (is.null(limits)) {
      limits <- as.numeric(NA)
    }
    standardGeneric("get_forest_plot")
  }
)

#_______________________________________________________________________________
#----                            get_tornado_plot                            ----
#_______________________________________________________________________________

#' Get tornado plot.
#'
#' @param object generic object
#' @param index output number, 1 by default
#' @param relative relative change or absolute value, logical
#' @param show_labels show the numeric labels
#' @param show_ref show vertical reference (at 0 or at the baseline)
#' @param limits limits of the tornado plot, numeric vector (2 values)
#' @param ... extra arguments like 'geom_bar_width', 'geom_text_nudge_y', 'label_nsig', 'geom_hline_color', 'geom_text_size'
#' @return ggplot
#' @export
#' @rdname get_tornado_plot
get_tornado_plot <- function(object, index, relative, show_labels, show_ref, limits, ...) {
  no_default_function_provided_debug(mget(names(formals()), envir = environment()), "get_tornado_plot")
}

setGeneric(
  "get_tornado_plot",
  function(object, index = NULL, relative = NULL, show_labels = NULL, show_ref = NULL, limits = NULL, ...) {
    if (is.null(index)) {
      index <- 1L
    } else {
      index <- as.integer(index)
    }
    if (is.null(relative)) {
      relative <- TRUE
    }
    if (is.null(show_labels)) {
      show_labels <- TRUE
    }
    if (is.null(show_ref)) {
      show_ref <- TRUE
    }
    if (is.null(limits)) {
      limits <- as.numeric(NA)
    }
    standardGeneric("get_tornado_plot")
  }
)
