library(testthat)
library(campsis)
library(campsisnca)

context("Test the forest plot feature")

source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Forest plot: effect of METAB (0/1) and WT on CL (rxode2/mrgsolve)", {
  model <- get_model("metaboliser_effect_on_cl") %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight", unit = "kg")) %>%
    add(CategoricalCovariateDefinition(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    ))
  reg_filename <- "meta_effect_cl_output"

  object <- ForestPlot() %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% get_names(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 10, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = "CL", model = model, settings = settings))
  )

  test <- expression(
    oat_analysis_regression_test(object = object, filename = reg_filename),
    object %>% get_forest_plot(), # Default forest plot
    object %>% get_forest_plot(limits = c(0.7, 1.5)) + ggplot2::scale_y_continuous(breaks = c(0.8, 1, 1.25))
    # class(object) <- "sensitivity_analysis",
    # # A tornado plot should work as well but a warning is thrown because it has more than 1 replicate
    # expect_warning(object %>% get_tornado_plot(), regexp="Multiple replicates detected")
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC0-24 (rxode2/mrgsolve)", {
  model <- get_model("metaboliser_effect_on_cl") %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight", unit = "kg")) %>%
    add(CategoricalCovariateDefinition(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    ))
  reg_filename <- "meta_effect_auc_output"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = 0:24))

  outputs <- AUC(variable = "CONC")
  object <- ForestPlot() %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% get_names(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 10, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = outputs, model = model, dataset = dataset, settings = settings))
  )

  test <- expression(
    oat_analysis_regression_test(object = object, filename = reg_filename),
    object %>% get_forest_plot() # Default forest plot
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC144-168 (rxode2/mrgsolve)", {
  model <- get_model("metaboliser_effect_on_cl") %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight", unit = "kg")) %>%
    add(CategoricalCovariateDefinition(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    ))
  reg_filename <- "meta_effect_aucd7_output"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1, ii = 24, addl = 6)) %>%
    add(Observations(times = 0:168))

  outputs <- AUC(variable = "CONC", window = TimeWindow(144, 168))
  object <- ForestPlot() %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% get_names(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 2, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = outputs, model = model, dataset = dataset, settings = settings))
  )

  test <- expression(
    oat_analysis_regression_test(object = object, filename = reg_filename),
    object %>% get_forest_plot() # Default forest plot
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Forest plots must work when only 1 item is simulated (rxode2/mrgsolve)", {
  model <- get_model("metaboliser_effect_on_cl") %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight")) %>%
    add(CategoricalCovariateDefinition(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    ))

  object <- ForestPlot() %>%
    add(ForestPlotItem(Covariate("WT", 60)))

  expect_equal(object@items %>% get_names(), c("WT:60"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 2, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = "CL", model = model, settings = settings))
  )

  test <- expression(
    results1 <- object@transient_results %>% dplyr::filter(output_index == 1),
    expect_equal(object@transient_outputs@metrics %>% length(), 1),
    expect_equal(results1$replicate, c(1, 2)),
    expect_true(is.numeric(results1$value)),
    expect_equal(as.character(results1$SCENARIO), rep("Weight: 60", 2))
  )
  campsismisc_test(simulation, test, env = environment())
})
