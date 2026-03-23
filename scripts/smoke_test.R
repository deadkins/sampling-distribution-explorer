source("app/helpers.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

keys <- names(POPULATION_SPECS)
estimators <- c("Mean", "Median")

for (key in keys) {
  population <- generate_population(key, n = 5000)
  meta <- get_population_metadata(key)

  assert_true(length(population) == 5000, sprintf("Population length mismatch for %s", key))
  assert_true(is.list(meta), sprintf("Metadata missing for %s", key))

  for (estimator in estimators) {
    sim <- simulate_repeated_samples(
      population = population,
      n = 25,
      reps = 120,
      estimator = estimator,
      seed = 42,
      keep_last = 20
    )

    assert_true(length(sim$estimates) == 120, sprintf("Estimate count mismatch for %s / %s", key, estimator))
    assert_true(length(sim$last_samples) == 20, sprintf("Last-sample count mismatch for %s / %s", key, estimator))
    assert_true(length(sim$example_sample) == 25, sprintf("Example sample size mismatch for %s / %s", key, estimator))

    truth <- compute_true_parameter(population, estimator)
    assert_true(is.finite(truth), sprintf("True parameter not finite for %s / %s", key, estimator))

    ci_df <- compute_last20_cis(sim$last_samples, estimator = estimator, bootstrap_B = 80)
    assert_true(nrow(ci_df) == 20, sprintf("CI row count mismatch for %s / %s", key, estimator))
    assert_true(all(ci_df$lower <= ci_df$estimate), sprintf("CI lower bound invalid for %s / %s", key, estimator))
    assert_true(all(ci_df$estimate <= ci_df$upper), sprintf("CI upper bound invalid for %s / %s", key, estimator))
  }
}

cat("Smoke tests passed for all populations and estimators.\n")
