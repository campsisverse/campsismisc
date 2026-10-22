#_______________________________________________________________________________
#----                           oat_analysis class                          ----
#_______________________________________________________________________________

#' One-at-a-time analysis structure.
#'
#' @export
setClass(
  "oat_analysis",
  representation(
    transient_results = "data.frame",
    transient_outputs = "ANY", # Store the NCA analysis
    transient_model = "campsis_model" # Transient field used to store the model
  ),
  prototype = prototype(
    transient_results = data.frame(),
    transient_model = campsismod::CampsisModel()
  )
)

#_______________________________________________________________________________
#----                          compute_baseline                             ----
#_______________________________________________________________________________

#' Compute baseline.
#'
#' @param object simulation results of the baseline, tibble
#' @param output OAT output, one or several
#' @return the baseline, double or list of double
#' @rdname compute_baseline
compute_baseline <- function(object, output) {
  stop("No default function is provided")
}

setGeneric("compute_baseline", function(object, output) {
  standardGeneric("compute_baseline")
})

#' @importFrom campsisnca calculate NCAAnalysis NCATable
#' @importFrom dplyr pull
#' @rdname compute_baseline
setMethod("compute_baseline", signature = c("tbl_df", "nca_metric"), definition = function(object, output) {
  nca <- campsisnca::NCAAnalysis() |>
    add(output)
  table <- campsisnca::NCATable() |>
    add(nca)
  baseline <- table |>
    campsisnca::calculate(x = object) |>
    campsismod::export(dest = "dataframe", type = "individual")
  return(baseline |> dplyr::pull(.data$value))
})

#' @rdname compute_baseline
setMethod("compute_baseline", signature = c("tbl_df", "nca_analysis"), definition = function(object, output) {
  baselines <- output@metrics@list |>
    purrr::map(function(m) {
      if (is.na(m@variable)) {
        m@variable <- output@variable
      }
      compute_baseline(object = object, output = m)
    })
  return(baselines)
})

#_______________________________________________________________________________
#----                          create_scenarios                             ----
#_______________________________________________________________________________

#' Create scenarios.
#'
#' @param object OAT analysis, type forest plot or sensitivity analysis
#' @param dataset Campsis dataset
#' @param model Campsis model
#' @return a list of CAMPSIS scenarios to be run
#' @rdname create_scenarios
create_scenarios <- function(object, model, dataset) {
  stop("No default function is provided")
}

setGeneric("create_scenarios", function(object, model, dataset) {
  standardGeneric("create_scenarios")
})

#_______________________________________________________________________________
#----                                utilities                              ----
#_______________________________________________________________________________

#' Process OAT outputs.
#'
#' @param outputs outputs as character, nca_metric or nca_analysis
#' @return an nca_analysis object
#' @importFrom campsisnca NCAAnalysis Last
#' @keywords internal
process_outputs <- function(outputs) {
  if (is.character(outputs)) {
    params <- outputs
    outputs <- campsisnca::NCAAnalysis()
    for (param in params) {
      outputs <- outputs |> add(campsisnca::Last(variable = param, name = param))
    }
  }
  if (is(outputs, "nca_metric")) {
    outputs <- campsisnca::NCAAnalysis() |> add(outputs)
  }
  return(outputs)
}

#_______________________________________________________________________________
#----                               run_analysis                            ----
#_______________________________________________________________________________

#' @importFrom dplyr filter select all_of mutate relocate any_of
post_process_scenarios <- function(object, output) {
  results <- object |>
    dplyr::filter(.data$metric == output |> get_name()) |>
    dplyr::select(-dplyr::all_of(c("discrete_value", "metric"))) |> # TODO
    dplyr::mutate(SCENARIO = factor(.data$SCENARIO, levels = unique(.data$SCENARIO) |> rev())) |>
    dplyr::relocate(dplyr::any_of(c("replicate", "id", "value", "SCENARIO")))
  return(results)
}

#' @rdname run_analysis
#' @importFrom campsis Outfun Outfuns NCATableOutfun simulate Scenario Covariate
#' @importFrom campsisnca NCAAnalysis NCATable
#' @importFrom progressr with_progress without_progress
setMethod(
  "run_analysis",
  signature = c("oat_analysis", "ANY", "campsis_model", "dataset", "simulation_settings"),
  definition = function(object, outputs, model, dataset, settings) {
    # Process outputs
    outputs <- process_outputs(outputs)

    # Store model in object
    object@transient_model <- model

    # Fix IIV, VARCOV_OMEGA, VARCOV_SIGMA at the beginning!
    # This way, calling disable is not needed in the scenarios
    model_no_var <- model |>
      disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA"))

    base_dataset <- dataset
    if (is(object, "forest_plot")) {
      for (cov_def in model_no_var@covariates@list) {
        base_dataset <- base_dataset |>
          add(campsis::Covariate(name = cov_def@name, distribution = cov_def@default_value))
      }
    }
    items <- object@items

    # Preparing settings
    settings_base_scenario <- settings
    settings_base_scenario@hardware@cpu <- 1L
    settings_base_scenario@hardware@replicate_parallel <- FALSE
    settings_base_scenario@default@replicates <- 1L 

    # Getting all necessary 'outvars' across all outputs
    # This in order to make 1 call to the simulate method
    outvars <- outputs@metrics@list |>
      purrr::map_chr(\(x) if (is.na(x@variable)) outputs@variable else x@variable) |>
      unique()

    # Storing outvars in the default arguments
    settings_base_scenario@default@outvars <- outvars
    settings@default@outvars <- outvars

    # Compute and store baseline value of each output
    # Progress from campsis is deactivated for the base scenario
    base_scenario <- progressr::without_progress(
      campsis::simulate(
        model = model_no_var,
        dataset = base_dataset,
        settings = settings_base_scenario
      )
    )
    baselines <- base_scenario |>
      compute_baseline(output = outputs)

    # Generate scenarios
    scenarios <- object |>
      create_scenarios(model = model_no_var, dataset = base_dataset)

    # Prepare outfun function
    nca <- outputs
    nca@strata <- c(SCENARIO = "all")
    nca@force_strat_vars <- "SCENARIO" # Even if there is only 1 level

    outfun <- campsis::NCATableOutfun(table = campsisnca::NCATable() |> add(nca), export_type = "individual")
    outfuns <- campsis::Outfuns() |>
      add(outfun)

    # Storing outfuns in the default arguments
    settings@default@outfuns <- outfuns

    # Simulate
    all_results <- campsis::simulate(
      model = model_no_var,
      dataset = base_dataset,
      scenarios = scenarios,
      settings = settings
    )

    # Post-processing
    transient_results <- purrr::imap_dfr(.x = outputs@metrics@list, .f = function(output, index) {
      res <- post_process_scenarios(object = all_results, output = output)
      res$output_index <- index
      res$baseline <- baselines[[index]]
      return(res)
    })

    # Save
    object@transient_results <- transient_results
    object@transient_outputs <- outputs

    return(object)
  }
)
