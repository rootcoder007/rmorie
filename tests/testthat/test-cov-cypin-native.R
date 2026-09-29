# Coverage for the P450-inhibition screen: graph descriptors checked by
# hand on small molecules (molecular weight from standard atomic weights,
# rings, rotatable bonds, H-bond donors/acceptors, halogens, Fsp3,
# charge), the stable logistic, the IRLS logistic fit against stats::glm
# (ridge 0) and the prediction / no-model paths.

.w <- c(H = 1.008, C = 12.011, N = 14.007, O = 15.999, Cl = 35.45)

test_that("descriptors of ethanol, butane, chlorobenzene and pyridinium", {
  d <- morie_cypin_descriptors("CCO")
  expect_equal(d[1], 2 * .w[["C"]] + .w[["O"]] + 6 * .w[["H"]], tolerance = 1e-12)
  expect_equal(d[-1], c(3, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0))
  b <- morie_cypin_descriptors("CCCC")
  expect_equal(b[5], 1)
  expect_equal(b[11], 1)
  cb <- morie_cypin_descriptors("Clc1ccccc1")
  expect_equal(cb[1], 6 * .w[["C"]] + .w[["Cl"]] + 5 * .w[["H"]], tolerance = 1e-12)
  expect_equal(cb[c(2:5, 9, 11)], c(7, 1, 1, 0, 1, 0))
  cy <- morie_cypin_descriptors("C1CCCCC1C")
  expect_equal(cy[c(3, 4, 5, 11)], c(1, 0, 0, 1))
  expect_equal(morie_cypin_descriptors("C[NH3+]")[12], 1)
  expect_error(morie_cypin_descriptors("C[Xe]"), "")
})

test_that("the logistic is evaluated stably on both tails", {
  expect_equal(morie_cypin_logistic(2), stats::plogis(2), tolerance = 1e-15)
  expect_equal(morie_cypin_logistic(-800), stats::plogis(-800))
  expect_equal(morie_cypin_logistic(-3), stats::plogis(-3), tolerance = 1e-15)
})

test_that("IRLS reproduces glm and its score vanishes at the optimum", {
  set.seed(5)
  X <- matrix(stats::rnorm(80), 40)
  y <- stats::rbinom(40, 1, stats::plogis(0.3 + X %*% c(1, -0.7)))
  f <- morie_cypin_fit(asplit(X, 1), y, ridge = 0)
  g <- stats::glm(y ~ X, family = stats::binomial(), control = stats::glm.control(epsilon = 1e-14, maxit = 50))
  expect_equal(f$coefficients, unname(stats::coef(g)), tolerance = 1e-9)
  expect_equal(f$deviance, stats::deviance(g), tolerance = 1e-9)
  expect_lt(max(abs(f$score)), 1e-8)
  r <- morie_cypin_fit(asplit(X, 1), y, ridge = 5)
  # ridge score: X'(y - mu) = ridge * beta off the intercept
  expect_equal(r$score[-1], 5 * r$coefficients[-1], tolerance = 1e-8)
  expect_equal(morie_cypin_predict(X[3, ], f$coefficients), unname(stats::fitted(g)[3]), tolerance = 1e-9)
  expect_error(morie_cypin_fit(list(), numeric(0)), "needs data")
  expect_error(morie_cypin_fit(asplit(X, 1), y[-1]), "one label per compound")
  expect_error(morie_cypin_predict(1:3, 1:3), "plus an intercept")
})

test_that("screening a compound with and without a model", {
  none <- morie_cypin("CCO", "3A4")
  expect_null(none$predicted)
  expect_false(none$has_model)
  expect_match(none$reason, "none are shipped")
  expect_identical(names(none$named)[1], "mw")
  b <- c(-2, 0.01, rep(0, 11))
  m <- morie_cypin("CCO", "2D6", model = list(coefficients = b))
  expect_equal(m$predicted, stats::plogis(-2 + 0.01 * none$descriptors[1]), tolerance = 1e-12)
  expect_false(m$inhibits)
  expect_identical(morie_cypin("CCO", "2D6", model = b)$predicted, m$predicted)
  expect_error(morie_cypin("CCO", "2E1"), "isozyme is one of")
  expect_match(morie_cypin_cheatsheet(), "P450")
})
