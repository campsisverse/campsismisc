library(testthat)
library(campsis)
library(campsisnca)

context("Test arm selection in run_analysis")

source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Arm selection works for sensitivity analysis", {
  model <- get_model("metaboliser_effect_on_cl")
  # Add covariate definitions to the model
  model@covariates <- model@covariates %>%
    add(campsismod::ContinuousCovariateDefinition("WT", 70)) %>%
    add(campsismod::CategoricalCovariateDefinition("METAB", categories = c(Slow = 0, Fast = 1), default_value = 0))

  # Dataset with 2 arms: 1000mg and 2000mg
  dataset <- Dataset() %>%
    add(Arm(1) %>% add(Infusion(time = 0, amount = 1000))) %>%
    add(Arm(2) %>% add(Infusion(time = 0, amount = 2000))) %>%
    add(Observations(times = 0:48)) %>%
    add(Covariate("WT", 70)) %>%
    add(Covariate("METAB", 0))

  outputs <- AUC(variable = "CONC")
  object <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("CL", up = 1.5, down = 1.5)))

  simulation <- expression({
    settings <- Settings(
      Hardware(cpu = 1, replicate_parallel = FALSE),
      DefaultSettings(replicates = 1, engine = destEngine, seed = 1)
    )
    object1 <- object %>%
      run_analysis(outputs = outputs, model = model, dataset = dataset, arm_index = 1, settings = settings)
    object2 <- object %>%
      run_analysis(outputs = outputs, model = model, dataset = dataset, arm_index = 2, settings = settings)
  })

  test <- expression({
    # Check baseline for Arm 1
    baseline1 <- object1@transient_results$baseline[1]
    # Check baseline for Arm 2
    baseline2 <- object2@transient_results$baseline[1]
    print(baseline1)
    print(baseline2)

    # Expect baseline 2 to be approximately twice baseline 1 (since it's a linear model and double dose)
    expect_equal(baseline2, 2 * baseline1, tolerance = 1e-3)

    # Check that they are not the same
    expect_true(baseline1 != baseline2)

    # Check results are different
    res1 <- object1@transient_results$value
    res2 <- object2@transient_results$value
    expect_true(all(res1 != res2))
  })

  campsismisc_test(simulation, test, env = environment())
})
