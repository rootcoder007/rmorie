# Coverage for Super Learner IPTW (van der Laan, Polley & Hubbard 2007;
# Pirracchio, Petersen & van der Laan 2015): the default library, the
# cross-validated level-one matrix rebuilt fold by fold with lm.fit / glm,
# the CV risks, the discrete, NNLS-on-the-simplex and OLS meta-learners,
# IPTW weights (plain and stabilised) and the Hajek ATE with its
# influence-function SE.

.dat <- function() {
  set.seed(8)
  n <- 60
  H <- cbind(stats::rnorm(n), stats::rnorm(n))
  A <- stats::rbinom(n, 1, stats::plogis(0.5 * H[, 1] - 0.4 * H[, 2]))
  y <- 1 + 2 * A + H[, 1] + stats::rnorm(n)
  list(H = H, A = A, y = y, n = n)
}

test_that("the default library and CV risks", {
  expect_identical(vapply(default_learners(1), `[[`, "", "name"), c("intercept", "main", "quadratic"))
  expect_identical(vapply(default_learners(3, c(0, 5)), `[[`, "", "name"),
                   c("intercept", "main", "quadratic", "interaction", "interaction+ridge5"))
  Z <- cbind(c(0.2, 0.8, 0.5), c(0.9, 0.1, 0.5))
  y <- c(0, 1, 1)
  expect_equal(cv_risk(y, Z), colMeans((y - Z)^2), tolerance = 1e-12)
  expect_equal(cv_risk(y, Z, "nll"), -colMeans(y * log(Z) + (1 - y) * log(1 - Z)), tolerance = 1e-12)
  expect_error(cv_risk(y, Z, "l1"), "l2 or nll")
})

test_that("the level-one matrix holds held-out fold predictions; meta-learners combine them", {
  d <- .dat()
  lib <- default_learners(2)[1:3]
  sl <- super_learner(d$y, d$H, library = lib, n_folds = 5, meta = "ols")
  folds <- lapply(0:4, function(v) which(seq_len(d$n) %% 5 == v))
  X <- list(matrix(1, d$n, 1), cbind(1, d$H), cbind(1, d$H, d$H^2))
  Z <- matrix(0, d$n, 3)
  for (f in folds) for (j in 1:3) {
    b <- stats::lm.fit(X[[j]][-f, , drop = FALSE], d$y[-f])$coefficients
    Z[f, j] <- X[[j]][f, , drop = FALSE] %*% b
  }
  # the learners add a 1e-8 ridge to X'X
  expect_equal(sl$level_one, Z, tolerance = 1e-7)
  expect_equal(unname(sl$cv_risk), colMeans((d$y - Z)^2), tolerance = 1e-7)
  expect_equal(sl$weight_vector, unname(stats::lm.fit(Z, d$y)$coefficients), tolerance = 1e-6)
  ds <- super_learner(d$y, d$H, library = lib, n_folds = 5, meta = "discrete")
  expect_equal(ds$weight_vector, as.numeric(1:3 == which.min(colMeans((d$y - Z)^2))))
  expect_identical(ds$best_candidate, lib[[which.min(colMeans((d$y - Z)^2))]]$name)
  nn <- super_learner(d$y, d$H, library = lib, n_folds = 5)
  a <- nn$weight_vector
  expect_equal(sum(a), 1, tolerance = 1e-10)
  expect_true(all(a >= 0))
  # simplex-constrained least squares: no feasible direction improves
  obj <- function(w) mean((d$y - Z %*% w)^2)
  for (k in 1:3) {
    e <- as.numeric(1:3 == k)
    expect_gte(obj(0.99 * a + 0.01 * e), obj(a) - 1e-10)
  }
  expect_error(super_learner(d$y, d$H, meta = "stack"), "meta must be one of")
  expect_error(super_learner(d$y[1:5], d$H[1:5, ]), "at least 8")
  expect_error(super_learner(d$y, d$H[-1, ]), "59 covariate rows for 60")
  expect_error(super_learner(d$y, d$H, loss = "nll"), "needs a binary outcome")
})

test_that("IPTW weights and the Hajek ATE", {
  d <- .dat()
  lib <- list(list(name = "main", kind = "main", penalty = 0))
  f <- flexible_iptw(d$A, d$H, library = lib, n_folds = 4, trim = 0.02)
  g <- stats::fitted(stats::glm(d$A ~ d$H, family = stats::binomial()))
  g <- pmin(pmax(g, 0.02), 0.98)
  expect_equal(f$propensity, unname(g), tolerance = 1e-6)
  expect_equal(f$weights, d$A / f$propensity + (1 - d$A) / (1 - f$propensity), tolerance = 1e-12)
  s <- flexible_iptw(d$A, d$H, library = lib, n_folds = 4, stabilize = TRUE)
  pa <- mean(d$A)
  expect_equal(s$weights, d$A * pa / s$propensity + (1 - d$A) * (1 - pa) / (1 - s$propensity), tolerance = 1e-12)
  r <- iptw_ate(d$y, d$A, d$H, library = lib, n_folds = 4)
  w <- r$weights
  m1 <- sum((w * d$y)[d$A == 1]) / sum(w[d$A == 1])
  m0 <- sum((w * d$y)[d$A == 0]) / sum(w[d$A == 0])
  expect_equal(r$estimate, m1 - m0, tolerance = 1e-12)
  ic <- ifelse(d$A == 1, w * (d$y - m1) * 60 / sum(w[d$A == 1]), -w * (d$y - m0) * 60 / sum(w[d$A == 0]))
  expect_equal(r$se, stats::sd(ic) / sqrt(60), tolerance = 1e-12)
  expect_identical(morie_flxipt, flexible_iptw)
  expect_identical(flexibleiptw, flexible_iptw)
  expect_error(flexible_iptw(d$A + 1, d$H), "binary 0/1")
  expect_error(flexible_iptw(rep(1, 60), d$H), "both treatment arms")
  expect_error(flexible_iptw(d$A, d$H, trim = 0.5), "trim must be in")
  expect_error(iptw_ate(d$y[-1], d$A, d$H), "59 outcomes but 60")
})
