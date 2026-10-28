library(testthat)

context("Test all methods from the sensitivity analysis item class")

test_that("Method get_label is working as expected", {
  model <- campsismod::model_suite$pk$`1cpt_fo`
  item <- SensitivityAnalysisItem(Change("CL", down = 3, up = 2))
  expect_equal(item %>% get_label(model = model), "Clearance (÷3, ×2)")

  # If corresponding theta is not found, the parameter name is used instead
  model <- campsismod::model_suite$pk$`1cpt_fo`
  item <- SensitivityAnalysisItem(Change("CL2", down = 3, up = 2))
  expect_equal(item %>% get_label(model = model), "CL2 (÷3, ×2)")
})

test_that("Method get_label handles doubled backslashes in labels", {
  model <- campsismod::model_suite$pk$`1cpt_fo`
  # Manually inject a label with doubled backslash
  model@parameters@list[[4]]@label <- "Clearance\\n(CL)"

  item <- SensitivityAnalysisItem(Change("CL", down = 3, up = 2))
  expect_equal(item %>% get_label(model = model), "Clearance\n(CL) (÷3, ×2)")
})
