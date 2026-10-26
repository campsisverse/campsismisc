library(testthat)
library(campsis)

context("Test all methods from the forest plot class")

test_that("Method get_label is working as expected with continuous labeled covariates", {
  model <- CampsisModel() %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight", unit = "kg"))
  item <- ForestPlotItem(Covariate("WT", 60))

  expect_equal(item %>% get_label(model = model), "Weight: 60 kg")
})

test_that("Method get_label is working as expected with categorical labeled covariates", {
  model <- CampsisModel() %>%
    add(CategoricalCovariateDefinition(
      name = "FOOD",
      default_value = 0,
      label = "Food",
      categories = c(Fasted = 0, Fed = 1)
    ))
  item <- ForestPlotItem(Covariate("FOOD", 0))

  expect_equal(item %>% get_label(model = model), "Food: Fasted")
})

test_that("Unit is optional in the continuous labeled covariate", {
  model <- CampsisModel() %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight"))

  item <- ForestPlotItem(Covariate("WT", 65))

  expect_equal(item %>% get_label(model = model), "Weight: 65")
})
