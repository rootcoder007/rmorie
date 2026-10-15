# R/ledger_scan.R, the two refusals PR #77 added that no other test reached.

test_that("a classifier that empties a measured group, and Delta not above Gamma, are refused", {
  # sensitivity 0 and specificity 1 put nobody in the measured-exposed group: d1 = 0
  expect_error(morie_misclassified_rd(0.3, 0.1, prevalence = 0.4, sensitivity = 0, specificity = 1),
               "both measured groups must be non-empty", fixed = TRUE)
  # sensitivity 1 and specificity 0 put nobody in the measured-unexposed group: d0 = 0
  expect_error(morie_misclassified_rd(0.3, 0.1, prevalence = 0.4, sensitivity = 1, specificity = 0),
               "both measured groups must be non-empty", fixed = TRUE)
  expect_error(morie_sensitivity_amplification(Delta = 2, Gamma = 2), "`Delta` must exceed `Gamma` (at least 1)",
               fixed = TRUE)
  expect_error(morie_sensitivity_amplification(Delta = 3, Gamma = 0.5), "`Delta` must exceed `Gamma`", fixed = TRUE)
  # the level curve solved for Lambda: (Lambda Delta + 1) / (Lambda + Delta) gives back Gamma
  r <- morie_sensitivity_amplification(Delta = 3, Gamma = 2)
  expect_equal((r$Lambda * 3 + 1) / (r$Lambda + 3), 2)
})
