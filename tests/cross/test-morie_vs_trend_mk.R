test_that("MannKendall matches trend::mk.test", {
  skip_if_not_installed("trend")
  u <- .morie_random_uniform(60, seed = 31)
  x <- round(stats::qnorm(u) + 0.03 * seq_along(u), 1)
  for (cc in c(TRUE, FALSE)) {
    ref <- trend::mk.test(x, continuity = cc)
    r <- MannKendall(x, continuity = cc)
    expect_equal(r$statistic, unname(ref$statistic), tolerance = 1e-12)
    expect_equal(r$p_value, ref$p.value, tolerance = 1e-12)
    expect_equal(r$S, unname(ref$estimates["S"]), tolerance = 1e-12)
    expect_equal(r$varS, unname(ref$estimates["varS"]), tolerance = 1e-12)
    expect_equal(r$tau, unname(ref$estimates["tau"]), tolerance = 1e-12)
  }
})
