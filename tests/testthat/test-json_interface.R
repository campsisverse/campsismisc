library(testthat)
library(campsismisc)
library(campsis)

context("Test the JSON interface")

TEST_FOLDER <- file.path(getwd(), test_path())

test_that("Import forest plot from JSON", {
  imported_fp <- ForestPlot(json = file.path(TEST_FOLDER, "json_examples", "forest_plot_1.json"))

  expected_fp <- ForestPlot() %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(imported_fp, expected_fp)
})

test_that("Import sensitivity analysis from JSON", {
  imported_sa <- SensitivityAnalysis(json = file.path(TEST_FOLDER, "json_examples", "sensitivity_analysis_1.json"))

  expected_sa <- SensitivityAnalysis() %>%
    add(SensitivityAnalysisItem(Change("CL", up = 1.2, down = 1.2))) %>%
    add(SensitivityAnalysisItem(Change("V", up = 1.5, down = 1.5, where = "equation")))

  expect_equal(imported_sa, expected_sa)
})
