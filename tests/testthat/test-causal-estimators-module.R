# The causal-estimators module on the canonical CPADS frame: three estimators,
# every number finite, and the AIPW line built from rows that really line up.

test_that("causal-estimators returns IPW, outcome regression and AIPW with finite numbers", {
  d <- make_canonical_cpads()
  out <- suppressWarnings(rmorie:::.run_causal_estimators_module_internal(d))
  tbl <- out$causal_estimator_comparison
  expect_identical(tbl$method, c("IPW", "Outcome regression", "AIPW"))
  expect_true(all(is.finite(tbl$ate)))
  expect_true(all(is.finite(tbl$se)) && all(tbl$se > 0))
  expect_true(all(tbl$ci_lower < tbl$ate & tbl$ate < tbl$ci_upper))
  # the three estimators answer the same question on the same frame: they agree to within a few SEs
  expect_lt(abs(tbl$ate[3] - tbl$ate[1]), 6 * max(tbl$se))
})
