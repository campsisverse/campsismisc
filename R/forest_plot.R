#_______________________________________________________________________________
#----                           forest_plot class                           ----
#_______________________________________________________________________________

#' Forest plot class.
#'
#' @export
setClass(
  "forest_plot",
  representation(
    items = "forest_plot_items"
  ),
  contains = "oat_analysis")

#' Create a forest plot.
#'
#' @param json path to JSON forest plot file or JSON content in string form
#' @return a forest plot object
#' @importFrom campsis Dataset Observations Settings
#' @export
ForestPlot <- function(json = NULL) {
  if (is.null(json)) {
    return(new("forest_plot"))
  } else {
    return(load_from_json(new("forest_plot"), json = json))
  }
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature = c("forest_plot", "forest_plot_item"), definition = function(object, x) {
  object@items <- object@items |> add(x)
  return(object)
})

#_______________________________________________________________________________
#----                          create_scenarios                             ----
#_______________________________________________________________________________

#' @rdname create_scenarios
#' @param dataset Campsis dataset
#' @importFrom campsis Scenarios Scenario
setMethod(
  "create_scenarios",
  signature = c("forest_plot", "campsis_model", "dataset"),
  definition = function(object, model, dataset) {
    scenarios <- campsis::Scenarios()
    items <- object@items

    for (item in items@list) {
      covariates <- item@covariates
      dataset_ <- dataset
      for (covariate in covariates@list) {
        dataset_ <- dataset_ |> campsismod::replace(covariate)
      }
      scenarios <- scenarios |>
        add(campsis::Scenario(name = item |> get_label(model), dataset = dataset_))
    }
    return(scenarios)
  }
)

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("forest_plot", "json_element"), definition = function(object, json) {
  object@items@list <- json@data$items %>%
    purrr::map(~ load_from_json(new("forest_plot_item"), JSONElement(.x)))
  return(object)
})

setMethod("load_from_json", signature = c("forest_plot", "character"), definition = function(object, json) {
  schema <- system.file("extdata", "forest_plot.schema.json", package = "campsismisc")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})

setMethod("load_from_json", signature = c("forest_plot", "list"), definition = function(object, json) {
  schema <- system.file("extdata", "forest_plot.schema.json", package = "campsismisc")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})

#_______________________________________________________________________________
#----                               get_forest_plot                           ----
#_______________________________________________________________________________

#' @rdname get_forest_plot
#' @param geom_label_vjust vjust argument of method geom_label
#' @param geom_label_nudge_x nudge_x argument of method geom_label
#' @param geom_label_nudge_y nudge_y argument of method geom_label
#' @param geom_label_size size argument of method geom_label
#' @param label_nsig number of significant digits in label
#' @param geom_hline_color color argument of method geom_hline
#' @importFrom dplyr group_by across summarise
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline geom_label coord_flip ylab xlab
setMethod(
  "get_forest_plot",
  signature = c("forest_plot", "integer", "logical", "logical", "logical", "logical", "numeric", "numeric", "numeric"),
  definition = function(
    object,
    index,
    relative,
    show_labels,
    show_ref,
    show_range,
    range,
    ci,
    limits,
    geom_label_vjust = 0,
    geom_label_nudge_x = 0.15,
    geom_label_nudge_y = 0,
    geom_label_size = 3,
    label_nsig = 3,
    geom_hline_color = "darkblue",
    ...
  ) {
    alpha <- (1 - ci) / 2
    formula <- preprocess_function(fun = ~ .x / .y, name = "forest_plot_fct")
    output <- object@transient_outputs@metrics@list[[index]]
    results <- object@transient_results |> dplyr::filter(.data$output_index == index)
    baseline <- results$baseline[1]

    # Check limits argument
    if (any(is.na(limits))) {
      limits <- NULL
    }

    # Recompute value as relative value if relative is TRUE
    if (relative) {
      results$value <- formula(results$value, baseline)
    }
    summary <- results |>
      dplyr::group_by(dplyr::across("SCENARIO")) |>
      dplyr::summarise(
        LOW = quantile(.data$value, alpha),
        MED = median(.data$value),
        UP = quantile(.data$value, 1 - alpha)
      )

    # Add label column
    summary$LABEL <- paste0(
      signif(summary$MED, label_nsig),
      ' (',
      signif(summary$LOW, label_nsig),
      '-',
      signif(summary$UP, label_nsig),
      ')'
    )

    plot <- ggplot2::ggplot(summary, ggplot2::aes(x = .data$SCENARIO, y = .data$MED)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$LOW, ymax = .data$UP), width = 0.2)

    if (show_ref) {
      plot <- plot + ggplot2::geom_hline(yintercept = ifelse(relative, 1, baseline), color = geom_hline_color)
    }
    if (show_range) {
      plot <- plot + ggplot2::geom_hline(yintercept = ifelse(relative, 1, baseline) * range, linetype = 2)
    }
    if (show_labels) {
      # Note when hjust not set, geom_labels are automatically aligned with data
      plot <- plot +
        ggplot2::geom_label(
          ggplot2::aes(label = .data$LABEL),
          vjust = geom_label_vjust,
          nudge_x = geom_label_nudge_x,
          nudge_y = geom_label_nudge_y,
          size = geom_label_size,
          linewidth = 0
        )
    }

    plot <- plot +
      ggplot2::coord_flip(ylim = limits) +
      ggplot2::ylab(paste0(ifelse(relative, "Relative ", ""), output |> get_name())) +
      ggplot2::xlab(NULL)

    return(plot)
  }
)
