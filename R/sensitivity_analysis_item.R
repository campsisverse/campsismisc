#_______________________________________________________________________________
#----                         forest_plot_item class                        ----
#_______________________________________________________________________________

#' Forest plot item: one or more labeled covariates.
#'
#' @export
setClass(
  "sensitivity_analysis_item",
  representation(
    changes = "sensitivity_analysis_changes",
    label = "character"
  ),
  contains = "pmx_element"
)

#'
#' Create a sensitivity analysis item.
#'
#' @param ... all changes
#' @return a sensitivity analysis item
#' @export
SensitivityAnalysisItem <- function(...) {
  args <- list(...)
  are_changes <- args |> purrr::map_lgl(\(x) is(x, "sensitivity_analysis_change"))
  assertthat::assert_that(all(are_changes), msg = "Sensitivity analysis item can only accept one or multiple changes")
  changes <- SensitivityAnalysisChanges() |> add(args)
  return(new("sensitivity_analysis_item", changes = changes))
}

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

setMethod("get_name", signature = c("sensitivity_analysis_item"), definition = function(x) {
  names <- x@changes@list |> purrr::map_chr(\(x) x@parameter)
  return(paste0(names, collapse = "/"))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("sensitivity_analysis_item", "json_element"), definition = function(object, json) {
  object@changes@list <- json@data$changes |>
    purrr::map(~ load_from_json(new("sensitivity_analysis_change"), JSONElement(.x)))
  return(object)
})

#_______________________________________________________________________________
#----                              get_label                                ----
#_______________________________________________________________________________

#' @rdname get_label
#' @param model Campsis model
setMethod("get_label", signature = c("sensitivity_analysis_item"), definition = function(object, model) {
  individual_labels <- object@changes@list |>
    purrr::map_chr(.f = function(change) {
      parameter_name <- change |> get_name()
      where <- change@where

      parameter_label <- parameter_name
      if (where == "theta") {
        theta <- model |> find(Theta(parameter_name))
        if (!is.null(theta) && !is.na(theta@label) && theta@label != "") {
          parameter_label <- theta@label
        }
      }
      parameter_label <- gsub("\\\\n", "\n", parameter_label)

      if (change@up_down_as_factor) {
        return(paste0(parameter_label, " (\U00F7", change@down, ", ", "\U00D7", change@up, ")"))
      } else {
        return(paste0(parameter_label, " (", change@down, ", ", change@up, ")"))
      }
    })

  return(paste0(individual_labels, collapse = " / "))
})
