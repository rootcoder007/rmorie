# Super learning (van der Laan, Polley & Hubbard 2007; van der Laan &
# Rose 2018 Chap. 3): replayed Fisher-Yates folds, cross-validated risks
# of simple learners computed fold by fold, the discrete selector, the
# convex ensemble and the sequential (iterated-expectation) learner.

sl_mean <- function(X, y) {
  m <- mean(y)
  function(x) m
}
sl_lm <- function(X, y) {
  b <- stats::lm.fit(cbind(1, X), y)$coefficients
  b[is.na(b)] <- 0
  function(x) sum(c(1, x) * b)
}
sl_data <- function() {
  set.seed(8)
  X <- matrix(stats::rnorm(40 * 2), 40)
  y <- 1 + 2 * X[, 1] - X[, 2] + stats::rnorm(40, sd = 0.5)
  list(X = X, y = y)
}

test_that("folds are a seeded Fisher-Yates shuffle dealt round-robin", {
  f <- morie_tlseqsl_cv_folds(11, 3, seed = 2)
  e <- .ghc_rng(2)
  idx <- 0:10
  for (i in 10:1) {
    j <- as.integer(.ghc_unif(e, 1) * (i + 1)) %% (i + 1)
    tmp <- idx[i + 1]
    idx[i + 1] <- idx[j + 1]
    idx[j + 1] <- tmp
  }
  expect_identical(f, lapply(1:3, function(v) idx[seq(v, 11, by = 3)]))
  expect_identical(sort(unlist(f)), 0:10)
  expect_error(morie_tlseqsl_cv_folds(5, 6), "V must lie in 2..5")
})

test_that("cross-validated risk evaluates each fold on a model fitted without it", {
  d <- sl_data()
  folds <- morie_tlseqsl_cv_folds(40, 5, seed = 1)
  pred <- numeric(40)
  for (f in folds) {
    tr <- setdiff(1:40, f + 1)
    pred[f + 1] <- mean(d$y[tr])
  }
  r <- morie_tlseqsl_cv_risk(d$X, d$y, sl_mean, V = 5, seed = 1)
  expect_equal(r$cv_predictions, pred, tolerance = 1e-14)
  expect_equal(r$risk, mean((d$y - pred)^2), tolerance = 1e-14)
  yb <- as.numeric(d$y > 1)
  rl <- morie_tlseqsl_cv_risk(d$X, yb, sl_mean, V = 5, loss = "log", seed = 1)
  p <- pmin(pmax(rl$cv_predictions, 1e-12), 1 - 1e-12)
  expect_equal(rl$risk, -mean(yb * log(p) + (1 - yb) * log(1 - p)), tolerance = 1e-14)
  expect_error(morie_tlseqsl_cv_risk(d$X, d$y, sl_mean, loss = "abs"), "loss must be one of")
})

test_that("discrete and ensemble super learners choose and weight by CV risk", {
  d <- sl_data()
  lib <- list(mean = sl_mean, ols = sl_lm)
  ds <- morie_tlseqsl_discrete_super_learner(d$X, d$y, lib, V = 5, seed = 3)
  rk <- vapply(lib, function(a) morie_tlseqsl_cv_risk(d$X, d$y, a, 5, seed = 3)$risk, 1)
  expect_equal(unlist(ds$risks), rk, tolerance = 1e-15)
  expect_identical(ds$selected, names(which.min(rk)))
  en <- morie_tlseqsl_ensemble_super_learner(d$X, d$y, lib, V = 5, seed = 3)
  P1 <- ds$cv_predictions$mean
  P2 <- ds$cv_predictions$ols
  a <- (0:20) / 20
  risk_a <- vapply(a, function(al) mean((d$y - al * P1 - (1 - al) * P2)^2), 1)
  expect_equal(unname(unlist(en$weights)), c(a[which.min(risk_a)], 1 - a[which.min(risk_a)]),
               tolerance = 1e-15)
  expect_equal(en$cv_risk, min(risk_a), tolerance = 1e-14)
  lib3 <- list(a = sl_mean, b = sl_lm, c = function(X, y) function(x) 0)
  e3 <- morie_tlseqsl_ensemble_super_learner(d$X, d$y, lib3, V = 5, seed = 3)
  r3 <- vapply(lib3, function(al) morie_tlseqsl_cv_risk(d$X, d$y, al, 5, seed = 3)$risk, 1)
  expect_equal(unname(unlist(e3$weights)), unname((1 / r3) / sum(1 / r3)), tolerance = 1e-14)
  e1 <- morie_tlseqsl_ensemble_super_learner(d$X, d$y, list(only = sl_lm), V = 5)
  expect_identical(e1$estimate, 1)
  expect_error(morie_tlseqsl_discrete_super_learner(d$X, d$y, list()), "library is empty")
})

test_that("sequential super learning iterates the conditional expectations backwards", {
  d <- sl_data()
  H <- lapply(1:40, function(i) d$X[i, ])
  lib <- list(mean = sl_mean, ols = sl_lm)
  r <- morie_tlseqsl_sequential_super_learner(H, d$y, lib, T = 2, V = 5, seed = 6)
  step <- function(X, y) {
    en <- morie_tlseqsl_ensemble_super_learner(X, y, lib, 5, "squared", 6)
    ds <- morie_tlseqsl_discrete_super_learner(X, y, lib, 5, "squared", 6)
    en$weights$mean * ds$cv_predictions$mean + en$weights$ols * ds$cv_predictions$ols
  }
  q2 <- step(d$X, d$y)
  q1 <- step(d$X[, 1, drop = FALSE], q2)
  expect_equal(r$estimate, mean(q1), tolerance = 1e-14)
  expect_identical(vapply(r$sequential_fits, `[[`, 1, "t"), c(0, 1))
  expect_equal(morie_tlseqsl_sequentialsuperlearner(H, d$y, lib, T = 2, V = 5, seed = 6)$mean,
               r$mean)
  expect_error(morie_tlseqsl_sequential_super_learner(H, d$y, lib, T = 0), "at least one time")
  expect_match(morie_tlseqsl_cheatsheet(), "CROSS-VALIDATION", fixed = TRUE)
})
