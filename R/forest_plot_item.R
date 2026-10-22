#_______________________________________________________________________________
#----                         forest_plot_item class                        ----
#_______________________________________________________________________________

#' Forest plot item: one or more labeled covariates.
#'
#' @export
setClass(
  "forest_plot_item",
  representation(
    covariates = "covariates"
  ),
  contains = "pmx_element"
)

#'
#' Create a forest plot item.
#'
#' @param ... all constant covariates
#' @return a forest plot item
#' @export
ForestPlotItem <- function(...) {
  args <- list(...)
  are_constant_covariates <- args |> purrr::map_lgl(\(x) is(x, "fixed_covariate"))
  assertthat::assert_that(all(are_constant_covariates), msg = "Forest plot item can only accept fixed covariates")
  covariates <- new("covariates") |> add(args)
  return(new("forest_plot_item", covariates = covariates))
}

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

setMethod("get_name", signature = c("forest_plot_item"), definition = function(x) {
  names <- x@covariates@list |> purrr::map_chr(\(x) paste0(x@name, ":", x@distribution@value))
  return(paste0(names, collapse = "/"))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("forest_plot_item", "json_element"), definition = function(object, json) {
  object@covariates@list <- json@data$covariates |>
    purrr::map(~ campsis::Covariate(name = .x$name, distribution = .x$value))
  return(object)
})

#_______________________________________________________________________________
#----                              get_label                                ----
#_______________________________________________________________________________

#' @rdname get_label
#' @param model Campsis model
setMethod("get_label", signature = c("forest_plot_item"), definition = function(object, model) {
  if (!is(model, "campsis_model")) {
    stop("Please provide a Campsis model")
  }
  individual_labels <- object@covariates@list |>
    purrr::map_chr(.f = function(covariate) {
      cov_def <- model@covariates |> find(covariate)
      if (is.null(cov_def)) {
        stop(paste0("Covariate '", covariate@name, "' was not found in the model"))
      }
      label <- if (is(cov_def, "categorical_covariate_definition")) {
        paste0(
          cov_def@label,
          ": ",
          names(which(cov_def@categories == covariate@distribution@value))
        )
      } else {
        if (!is.na(cov_def@unit) && cov_def@unit != "") {
          paste0(cov_def@label, ": ", covariate@distribution@value, " ", cov_def@unit)
        } else {
          paste0(cov_def@label, ": ", covariate@distribution@value)
        }
      }
      return(gsub("\\\\n", "\n", label))
    })

  return(paste0(individual_labels, collapse = " / "))
})
