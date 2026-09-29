# Coverage for the effect-size helpers: Cohen's d (pooled and control-SD
# denominators, with the undefined-SD warning), Hedges' g with the exact
# gamma-function correction J(m), Cramer's V from the uncorrected
# chi-square, and eta squared from an F statistic (checked against aov).

test_that("Cohen's d and Hedges' g", {
  x1 <- c(5.1, 6.3, 5.8, 7.0, 6.1, NA)
  x2 <- c(4.2, 5.0, 4.8, 5.5)
  a <- x1[!is.na(x1)]
  sp <- sqrt((4 * stats::var(a) + 3 * stats::var(x2)) / 7)
  d <- (mean(a) - mean(x2)) / sp
  expect_equal(morie_cohens_d(x1, x2), d, tolerance = 1e-12)
  expect_equal(morie_cohens_d(x1, x2, pooled = FALSE), (mean(a) - mean(x2)) / stats::sd(x2), tolerance = 1e-12)
  J <- gamma(3.5) / (sqrt(3.5) * gamma(3))
  expect_equal(morie_hedges_g(x1, x2), d * J, tolerance = 1e-12)
  # J approaches the familiar 1 - 3 / (4 m - 1) for large m
  big1 <- seq(1, 10, length.out = 200)
  big2 <- seq(2, 12, length.out = 200)
  expect_equal(morie_hedges_g(big1, big2) / morie_cohens_d(big1, big2), 1 - 3 / (4 * 398 - 1), tolerance = 1e-6)
  expect_warning(morie_cohens_d(c(1, 1), c(1, 1)), "undefined")
  expect_warning(expect_identical(morie_hedges_g(1, NA), NaN), "undefined")
})

test_that("Cramer's V and eta squared", {
  tab <- matrix(c(20, 15, 5, 10, 25, 30), 2, byrow = TRUE)
  chi <- unname(stats::chisq.test(tab, correct = FALSE)$statistic)
  expect_equal(morie_cramers_v(tab), sqrt(chi / (sum(tab) * 1)), tolerance = 1e-12)
  t3 <- matrix(c(10, 2, 3, 4, 12, 5, 1, 3, 15), 3)
  expect_equal(morie_cramers_v(t3), sqrt(unname(suppressWarnings(stats::chisq.test(t3))$statistic) / (sum(t3) * 2)), tolerance = 1e-12)
  set.seed(2)
  y <- stats::rnorm(24) + rep(c(0, 0.5, 1.2), each = 8)
  g <- factor(rep(1:3, each = 8))
  a <- summary(stats::aov(y ~ g))[[1]]
  eta <- a[["Sum Sq"]][1] / sum(a[["Sum Sq"]])
  expect_equal(morie_eta_squared(a[["F value"]][1], 2, 21), eta, tolerance = 1e-12)
})
