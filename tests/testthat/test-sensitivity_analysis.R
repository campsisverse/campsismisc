library(testthat)
library(campsis)
library(campsisnca)

context("Test the sensitivity analysis feature")

source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on AUC (several replicates)", {
  model <- get_model("metaboliser_effect_on_cl")
  reg_filename <- "effect_of_parameters_on_auc"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = 0:24)) %>%
    add(Covariate("METAB", 0)) %>%
    add(Covariate("WT", 70))

  results <- simulate(model = model %>% disable("IIV"), dataset = dataset)
  spaghettiPlot(results, "CONC")

  outputs <- AUC(variable = "CONC")
  object <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("DUR", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VC", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VP", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("Q", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("CL", up = 2, down = 2, log = TRUE)))

  expect_equal(object@items %>% get_names(), c("DUR", "VC", "VP", "Q", "CL"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 3, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = outputs, model = model, dataset = dataset, settings = settings))
  )

  test <- expression(
    oat_analysis_regression_test(object = object, filename = reg_filename),
    expect_warning(object %>% get_tornado_plot(), regexp = "Multiple replicates detected")
    # class(object) <- "forest_plot",
    # object %>% get_forest_plot(relative=FALSE)
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on AUC (single replicate)", {
  model <- get_model("metaboliser_effect_on_cl")
  reg_filename <- "effect_of_parameters_on_auc_1rep"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = 0:24)) %>%
    add(Covariate("METAB", 0)) %>%
    add(Covariate("WT", 70))

  results <- simulate(model = model %>% disable("IIV"), dataset = dataset)
  spaghettiPlot(results, "CONC")

  outputs <- AUC(variable = "CONC")
  object <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("DUR", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VC", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VP", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("Q", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("CL", up = 2, down = 2, log = TRUE)))

  expect_equal(object@items %>% get_names(), c("DUR", "VC", "VP", "Q", "CL"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 1, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = outputs, model = model, dataset = dataset, settings = settings))
  )

  test <- expression(
    oat_analysis_regression_test(object = object, filename = reg_filename),
    object %>% get_tornado_plot(relative = TRUE),
    object %>% get_tornado_plot(relative = FALSE)
    # class(object) <- "forest_plot",
    # object %>% get_forest_plot(relative=FALSE),
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on Cmax (rxode2/mrgsolve)", {
  model <- get_model("metaboliser_effect_on_cl")
  reg_filename <- "effect_of_parameters_on_cmax"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = 0:24)) %>%
    add(Covariate("METAB", 0)) %>%
    add(Covariate("WT", 70))
 
  results <- simulate(model = model %>% disable("IIV"), dataset = dataset)
  spaghettiPlot(results, "CONC")

  outputs <- Cmax(variable = "CONC")
  object <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("DUR", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VC", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VP", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("Q", up = 2, down = 2, log = TRUE))) %>%
    add(SensitivityAnalysisItem(Change("CL", up = 2, down = 2, log = TRUE)))

  expect_equal(object@items %>% get_names(), c("DUR", "VC", "VP", "Q", "CL"))

  simulation <- expression(
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 3, engine = destEngine, seed = 1)
    ),
    object <- progressr::with_progress(object %>% run_analysis(outputs = outputs, model = model, dataset = dataset, settings = settings))
  )

  test <- expression(
    oat_analysis_regression_test(object = object, filename = reg_filename),
    expect_warning(object %>% get_tornado_plot(), regexp = "Multiple replicates detected")
    # class(object) <- "forest_plot",
    # object %>% get_forest_plot()
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Tornado plot: sensitivity analysis of the 1-cpt-fo model parameters on AUC (rxode2/mrgsolve, change applied on THETAS)", {
  model <- model_suite$pk$'1cpt_fo'
  reg_filename <- "sensitivity_bio_ka_cl_vc_on_auc"

  arm1 <- Arm(subjects = 10, label = "Arm 1") %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1, ii = 24, addl = 0)) %>%
    add(Observations(seq(0, 48, by = 0.1))) %>%
    set_subjects(1) %>%
    set_label(as.character(NA))

  outputs <- AUC(variable = "CONC", window = TimeWindow(0, "last"), method = 1, name = "AUC")

  dataset_sens <- Dataset() %>% add(arm1) %>% add(DatasetConfig(exportTSLD = TRUE, exportTDOS = TRUE))

  sens <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("BIO", up = 2, down = 2))) %>%
    add(SensitivityAnalysisItem(Change("KA", up = 2, down = 2))) %>%
    add(SensitivityAnalysisItem(Change("CL", up = 2, down = 2))) %>%
    add(SensitivityAnalysisItem(Change("VC", up = 2, down = 2)))

  simulation <- expression(
    tictoc::tic(),
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 1, engine = destEngine, seed = 1)
    ),
    sens <- progressr::with_progress(sens %>% run_analysis(outputs = outputs, model = model, dataset = dataset_sens, settings = settings)),
    tictoc::toc(), # rxode2: 15.43 sec, mrgsolve: 7.19 sec
    sens
  )

  test <- expression(
    oat_analysis_regression_test(object = sens, filename = reg_filename),
    sens %>% get_tornado_plot(), # Default plot
    sens %>% get_tornado_plot(limits = c(-15, 100), show_labels = FALSE) # Limits can be set, labels can be hidden, etc.
  )
  campsismisc_test(simulation, test, env = environment())
})

test_that("Tornado plot: sensitivity analysis of the 1-cpt-fo model parameters on AUC(rxode2/mrgsolve, change applied on equations)", {
  model <- model_suite$pk$'1cpt_fo'
  reg_filename <- "sensitivity_bio_ka_cl_vc_on_auc"

  arm1 <- Arm(subjects = 10, label = "Arm 1") %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1, ii = 24, addl = 0)) %>%
    add(Observations(seq(0, 48, by = 0.1))) %>%
    set_subjects(1) %>%
    set_label(as.character(NA))

  outputs <- AUC(variable = "CONC", window = TimeWindow(0, "last"), method = 1, name = "AUC")

  dataset_sens <- Dataset() %>% add(arm1) %>% add(DatasetConfig(exportTSLD = TRUE, exportTDOS = TRUE))

  sens <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("BIO", up = 2, down = 2, where = "equation"))) %>%
    add(SensitivityAnalysisItem(Change("KA", up = 2, down = 2, where = "equation"))) %>%
    add(SensitivityAnalysisItem(Change("CL", up = 2, down = 2, where = "equation"))) %>%
    add(SensitivityAnalysisItem(Change("VC", up = 2, down = 2, where = "equation")))

  simulation <- expression(
    tictoc::tic(),
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 1, engine = destEngine, seed = 1)
    ),
    sens <- progressr::with_progress(sens %>% run_analysis(outputs = outputs, model = model, dataset = dataset_sens, settings = settings)),
    tictoc::toc(), # rxode2: 40.75 sec, mrgsolve: 35.78 sec
    sens
  )

  test <- expression(
    oat_analysis_regression_test(object = sens, filename = reg_filename),
    sens %>% get_tornado_plot(), # Default plot
    sens %>% get_tornado_plot(limits = c(-15, 100), show_labels = FALSE) # Limits can be set, labels can be hidden, etc.
  )
  campsismisc_test(simulation, test, env = environment())
})
