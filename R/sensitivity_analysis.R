#_______________________________________________________________________________
#----                       sensitivity_analysis class                      ----
#_______________________________________________________________________________

#' Sensitivity analysis class.
#'
#' @export
setClass(
  "sensitivity_analysis",
  representation(
    items = "sensitivity_analysis_items"
  ),
  contains = "oat_analysis"
)

#' Create an OAT-based sensitivity analysis object.
#'
#' @param json path to JSON sensitivity analysis file or JSON content in string form
#' @return an OAT-based sensibility analysis object
#' @importFrom campsis Dataset Observations Settings
#' @export
SensitivityAnalysis <- function(json = NULL) {
  if (is.null(json)) {
    return(new("sensitivity_analysis"))
  } else {
    return(load_from_json(new("sensitivity_analysis"), json = json))
  }
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature = c("sensitivity_analysis", "sensitivity_analysis_item"), definition = function(object, x) {
  object@items <- object@items |> add(x)
  return(object)
})

#_______________________________________________________________________________
#----                          create_scenarios                             ----
#_______________________________________________________________________________

#' @rdname create_scenarios
#' @param model Campsis model
#' @importFrom campsis Scenarios Scenario
setMethod(
  "create_scenarios",
  signature = c("sensitivity_analysis", "campsis_model", "dataset"),
  definition = function(object, model, dataset) {
    # Note, IIV, VARCOV_OMEGA, VARCOV_SIGMA was already disabled in the given model, see oat_analysis.R, run_analysis()
    # Because simulations are not replicated, variance-covariance matrix is not used by default in CAMPSIS
    # If sensitivity analysis is replicated: nothing happens because the updated model is part of the scenario
    scenarios <- campsis::Scenarios()
    items <- object@items

    for (item in items@list) {
      changes <- item@changes
      modelUp <- model # First direction of tornado plot (direction up)
      modelDown <- model # Other direction of tornado plot (direction down)

      for (change in changes@list) {
        name <- change |> get_name()
        where <- change@where

        if (where == "equation") {
          equation <- model |> find(Equation(name))
          if (is.null(equation)) {
            stop(paste0("Equation ", name, " cannot be found in model"))
          }
          equationUp <- equation
          equationDown <- equation
          if (change@up_down_as_factor) {
            equationUp@rhs <- paste0("(", equationUp@rhs, ") * ", change@up) # Multiplication
            equationDown@rhs <- paste0("(", equationDown@rhs, ") / ", change@down) # Division
          } else {
            # Use values as is
            equationUp@rhs <- change@up |> as.character()
            equationDown@rhs <- change@down |> as.character()
          }

          modelUp <- modelUp |> campsismod::replace(equationUp)
          modelDown <- modelDown |> campsismod::replace(equationDown)
        } else if (where == "theta") {
          theta <- model |> find(Theta(name))
          if (is.null(theta)) {
            stop(paste0("Theta ", name, " cannot be found in model"))
          }
          thetaUp <- theta
          thetaDown <- theta
          if (change@up_down_as_factor) {
            if (change@log) {
              thetaUp@value <- log(exp(theta@value) * change@up) # Multiplication
              thetaDown@value <- log(exp(theta@value) / change@down) # Division
            } else {
              thetaUp@value <- theta@value * change@up # Multiplication
              thetaDown@value <- theta@value / change@down # Division
            }
          } else {
            # Use values as is
            if (change@log) {
              # Arbitrary but more convenient for the user to provide boundaries in the linear scale
              thetaUp@value <- log(change@up)
              thetaDown@value <- log(change@down)
            } else {
              thetaUp@value <- change@up
              thetaDown@value <- change@down
            }
          }

          modelUp <- modelUp |> campsismod::replace(thetaUp)
          modelDown <- modelDown |> campsismod::replace(thetaDown)
        } else {
          # Should not happen
          stop(paste0("Unknown where argument: ", where))
        }
      }
      scenarios <- scenarios |>
        add(campsis::Scenario(name = paste0(item |> get_name(), ", up"), model = modelUp)) |>
        add(campsis::Scenario(name = paste0(item |> get_name(), ", down"), model = modelDown))
    }
    return(scenarios)
  }
)

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("sensitivity_analysis", "json_element"), definition = function(object, json) {
  object@items@list <- json@data$items %>%
    purrr::map(~ load_from_json(new("sensitivity_analysis_item"), JSONElement(.x)))
  return(object)
})

setMethod("load_from_json", signature = c("sensitivity_analysis", "character"), definition = function(object, json) {
  schema <- system.file("extdata", "sensitivity_analysis.schema.json", package = "campsismisc")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})

setMethod("load_from_json", signature = c("sensitivity_analysis", "list"), definition = function(object, json) {
  schema <- system.file("extdata", "sensitivity_analysis.schema.json", package = "campsismisc")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})

#_______________________________________________________________________________
#----                             get_tornado_plot                            ----
#_______________________________________________________________________________

#' @rdname get_tornado_plot
#' @param geom_bar_width width argument of method geom_bar
#' @param geom_text_nudge_y nudge_y argument of method geom_text
#' @param label_nsig number of significant digits in label
#' @param geom_hline_color color argument of method geom_hline
#' @param geom_text_size size argument of method geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom tidyr separate
#' @importFrom dplyr mutate group_by across summarise arrange ungroup pull
#' @importFrom ggplot2 ggplot aes coord_flip geom_bar xlab labs geom_hline ylab scale_y_continuous
setMethod(
  "get_tornado_plot",
  signature = c("sensitivity_analysis", "integer", "logical", "logical", "logical", "numeric"),
  definition = function(
    object,
    index,
    relative,
    show_labels,
    show_ref,
    limits,
    geom_bar_width = 0.5,
    geom_text_nudge_y = 1,
    label_nsig = 3,
    geom_hline_color = "grey",
    geom_text_size = 3,
    ...
  ) {
    is_sensitivity_analysis <- is(object, "sensitivity_analysis")
    output <- object@transient_outputs@metrics@list[[index]]
    results <- object@transient_results |> dplyr::filter(.data$output_index == index)
    baseline <- results$baseline[1]

    # Check limits argument
    if (any(is.na(limits))) {
      limits <- NULL
    }

    # Tornado plots are bidirectional, forest plots aren't
    if (is_sensitivity_analysis) {
      results <- results |>
        tidyr::separate(col = "SCENARIO", sep = ", ", into = c("ITEM_NAME", "DIRECTION"))
    } else {
      results <- results |>
        dplyr::mutate(ITEM_NAME = .data$SCENARIO, DIRECTION = "up")
    }

    formula <- preprocess_function(fun = ~ (.x - .y) * 100 / .y, name = "tornado_plot_fct")

    if ("replicate" %in% colnames(results)) {
      warning("Multiple replicates detected, median value will be used in tornado plot")
    }

    # Always compute change from baseline because arrange will always called on the absolute change
    results$CHANGE <- formula(results$value, baseline)

    summary <- results |>
      dplyr::group_by(dplyr::across(c("ITEM_NAME", "DIRECTION"))) |>
      dplyr::summarise(MED_CHANGE = median(.data$CHANGE), MED_VALUE = median(.data$value)) |> # If several replicates
      dplyr::group_by(dplyr::across("ITEM_NAME")) |>
      dplyr::mutate(MAX_MED_CHANGE = max(abs(.data$MED_CHANGE))) |>
      dplyr::arrange(abs(.data$MAX_MED_CHANGE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(DIRECTION = factor(.data$DIRECTION, levels = c("up", "down"), labels = c("Up", "Down")))

    # Customize item name if sensitivity analysis object
    if (is_sensitivity_analysis) {
      item_names <- object@items |> get_names()
      item_labels <- object@items@list |> purrr::map_chr(\(x) x |> get_label(object@transient_model))
      order <- order(match(item_names, unique(summary$ITEM_NAME)))
      item_names <- item_names[order]
      item_labels <- item_labels[order]
      summary <- summary |>
        dplyr::mutate(ITEM_NAME = factor(.data$ITEM_NAME, levels = item_names, labels = item_labels))
    } else {
      summary <- summary |>
        dplyr::mutate(ITEM_NAME = factor(.data$ITEM_NAME, levels = unique(.data$ITEM_NAME)))
    }

    # Nudge y-axis
    summary$NUDGE_Y <- ifelse(summary$MED_CHANGE > 0, geom_text_nudge_y, -geom_text_nudge_y)

    shift_trans <- function(d = 0) {
      scales::trans_new("shift", transform = function(x) x - d, inverse = function(x) x + d)
    }

    # Relative or absolute
    summary <- summary |>
      dplyr::mutate(
        TORNADO_VALUE = if (relative) {
          .data$MED_CHANGE
        } else {
          .data$MED_VALUE
        }
      )

    # Label
    summary$LABEL <- paste0(signif(summary |> dplyr::pull(.data$TORNADO_VALUE), digits = label_nsig))

    plot <- ggplot2::ggplot(
      data = summary,
      mapping = ggplot2::aes(x = .data$ITEM_NAME, y = .data$TORNADO_VALUE, fill = .data$DIRECTION, label = .data$LABEL)
    ) +
      ggplot2::coord_flip(ylim = limits) +
      ggplot2::geom_bar(stat = "identity", position = "identity", width = geom_bar_width)

    if (show_labels) {
      plot <- plot +
        ggrepel::geom_text_repel(nudge_y = summary$NUDGE_Y, size = geom_text_size)
    }

    plot <- plot +
      ggplot2::xlab(NULL) +
      ggplot2::labs(fill = "Direction")

    if (show_ref) {
      plot <- plot + ggplot2::geom_hline(yintercept = ifelse(relative, 0, baseline), color = geom_hline_color)
    }

    if (relative) {
      plot <- plot +
        ggplot2::ylab(paste0("Change in ", output |> get_name(), " (%)"))
    } else {
      plot <- plot +
        ggplot2::scale_y_continuous(trans = shift_trans(baseline)) +
        ggplot2::ylab(paste0(output |> get_name()))
    }

    return(plot)
  }
)
