OVERWRITE_NON_REG_FILES <- FALSE
TEST_ENGINES <- c("rxode2", "mrgsolve")

get_model <- function(name) {
  model <- read.campsis(file.path(getwd(), test_path(), "campsis_models", "/", name))

  # See strange regression in R base: #21
  model@parameters@varcov <- signif(model@parameters@varcov, 5)

  return(model)
}

#' Test there is no regression on the results of the OAT analysis.
#'
#' @param object OAT analysis object
#' @param filename filename of expected results
#' @param index output index to be checked
#' @importFrom tibble as_tibble
oat_analysis_regression_test <- function(object, filename, index = 1) {
  results <- object@transient_results %>%
    dplyr::filter(.data$output_index == index) %>%
    dplyr::select(-dplyr::any_of(c("output_index", "baseline"))) %>%
    strip_metadata() %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.numeric, round, digits = 2)

  file <- file.path(getwd(), test_path(), "non_regression", "oat_analysis", paste0(filename, ".csv"))

  if (OVERWRITE_NON_REG_FILES) {
    write.table(results, file = file, sep = ",", row.names = FALSE)
  }
  expected <- read.csv(file = file) %>% tibble::as_tibble()
  expect_equal(results, expected)
}

no_engine_installed <- function() {
  cond1 <- engine_installed("RxODE")
  cond2 <- engine_installed("rxode2")
  cond3 <- engine_installed("mrgsolve")
  return(!(cond1 || cond2 || cond3))
}

engine_installed <- function(name) {
  return(find.package(name, quiet = TRUE) %>% length() > 0)
}

campsismisc_test <- function(simulation, test, env) {
  # Iteration over all test engines to be tested
  for (testEngine in TEST_ENGINES) {
    env$destEngine <- testEngine
    # Check if package exists (as test engines are suggested packages)
    # This is needed for CRAN when package is tested with `_R_CHECK_DEPENDS_ONLY_`=TRUE
    if (engine_installed(testEngine)) {
      env$results <- eval(simulation, envir = env)
      eval(test, envir = env)
    }
  }
}

convert_campsis_test <- function(env = parent.frame(), debug_engine = "mrgsolve") {
  if (!exists("simulation", envir = env) || !exists("test", envir = env)) {
    stop("Could not find 'simulation' or 'test' expressions in the provided environment.")
  }

  # Get the expressions from the specified environment
  sim_expr <- get("simulation", envir = env)
  test_expr <- get("test", envir = env)

  # Modify the simulation call
  sim_call <- sim_expr[[1]]
  sim_call$dest <- debug_engine
  sim_text <- paste0("results <- ", deparse1(sim_call))

  # Extract all lines from the test expression
  test_text_lines <- vapply(as.list(test_expr), deparse1, character(1))

  # Combine and return
  final_script <- c(sim_text, test_text_lines)
  cat(paste(final_script, collapse = "\n"))
}

strip_metadata <- function(x) {
  # Revert class back to standard tibble components
  class(x) <- c("tbl_df", "tbl", "data.frame")

  # Delete the attribute
  attr(x, "metadata") <- NULL

  return(x)
}
