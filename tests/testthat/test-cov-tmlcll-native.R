# Cross-lagged panel models and the lagged-intervention TMLE: CLPM
# coefficients by lm() on stacked waves, the within/between split and
# the RI-CLPM, and the TMLE recomputed from its nuisance fits.

cl_panel <- function() {
  set.seed(23)
  n <- 40
  T <- 4
  bx <- stats::rnorm(n)
  X <- Y <- matrix(0, n, T)
  X[, 1] <- bx + stats::rnorm(n)
  Y[, 1] <- 0.5 * bx + stats::rnorm(n)
  for (t in 2:T) {
    X[, t] <- bx + 0.3 * (X[, t - 1] - bx) + stats::rnorm(n, sd = 0.5)
    Y[, t] <- 0.5 * bx + 0.2 * X[, t - 1] + stats::rnorm(n, sd = 0.5)
  }
  list(X = X, Y = Y, n = n, T = T)
}
cl_stack <- function(X, Y) {
  T <- ncol(X)
  idx <- as.matrix(expand.grid(t = 2:T, i = seq_len(nrow(X))))
  data.frame(x = X[idx[, c("i", "t")]], y = Y[idx[, c("i", "t")]],
             xl = X[cbind(idx[, "i"], idx[, "t"] - 1)], yl = Y[cbind(idx[, "i"], idx[, "t"] - 1)])
}

test_that("CLPM coefficients are the stacked lagged regressions", {
  d <- cl_panel()
  s <- cl_stack(d$X, d$Y)
  fx <- stats::coef(stats::lm(x ~ xl + yl, s))
  fy <- stats::coef(stats::lm(y ~ xl + yl, s))
  r <- morie_clpm_coefficients(d$X, d$Y)
  expect_equal(c(r$x_on_x, r$y_on_x), unname(fx[2:3]), tolerance = 1e-8)
  expect_equal(c(r$x_on_y, r$y_on_y), unname(fy[2:3]), tolerance = 1e-8)
  expect_equal(r$cross_lag_x_to_y, r$x_on_y)
  expect_identical(morie_tmlcll(d$X, d$Y)$x_on_x, r$x_on_x)
  expect_error(morie_clpm_coefficients(d$X, d$Y[, 1:3]), "same shape")
  expect_error(morie_clpm_coefficients(d$X[, 1, drop = FALSE], d$Y[, 1, drop = FALSE]), "2 waves")
})

test_that("within/between decomposition and the RI-CLPM on within-person scores", {
  d <- cl_panel()
  wb <- morie_within_between_decomposition(d$X)
  m <- rowMeans(d$X)
  expect_equal(wb$person_means, m, tolerance = 1e-15)
  expect_equal(wb$within, d$X - m, tolerance = 1e-15)
  expect_equal(wb$between_variance, stats::var(m), tolerance = 1e-14)
  expect_equal(wb$within_variance, mean((d$X - m)^2), tolerance = 1e-15)
  ri <- morie_ri_clpm_coefficients(d$X, d$Y)
  s <- cl_stack(d$X - m, d$Y - rowMeans(d$Y))
  fy <- stats::coef(stats::lm(y ~ xl + yl, s))
  expect_equal(ri$cross_lag_x_to_y, unname(fy[2]), tolerance = 1e-8)
  expect_equal(ri$between_variance_y, stats::var(rowMeans(d$Y)), tolerance = 1e-14)
})

test_that("the lagged-intervention TMLE solves its score and rescales to the outcome", {
  set.seed(24)
  n <- 80
  W <- matrix(stats::rnorm(n * 2), n)
  a <- stats::rbinom(n, 1, stats::plogis(0.5 * W[, 1]))
  y <- 2 + a + W[, 2] + stats::rnorm(n)
  tm <- rep(1:4, 20)
  r <- morie_tmle_cross_lagged(y, a, W, tm)
  lo <- min(y)
  hi <- max(y)
  ys <- (y - lo) / (hi - lo)
  g <- pmin(pmax(stats::fitted(stats::glm(a ~ W, family = stats::binomial())), 0.02), 0.98)
  co <- stats::coef(stats::lm(ys ~ a + W))
  q1 <- pmin(pmax(as.numeric(cbind(1, 1, W) %*% co), 1e-6), 1 - 1e-6)
  q0 <- pmin(pmax(as.numeric(cbind(1, 0, W) %*% co), 1e-6), 1 - 1e-6)
  H <- a / g - (1 - a) / (1 - g)
  off <- stats::qlogis(ifelse(a == 1, q1, q0))
  expect_equal(sum(H * (ys - stats::plogis(off + r$epsilon * H))), 0, tolerance = 1e-8)
  q1s <- stats::plogis(stats::qlogis(q1) + r$epsilon / g)
  q0s <- stats::plogis(stats::qlogis(q0) - r$epsilon / (1 - g))
  expect_equal(r$psi, mean(q1s - q0s) * (hi - lo), tolerance = 1e-7)
  expect_identical(r$n_waves, 4L)
  expect_true(r$solves_eic)
  kg <- morie_tmlecrosslagged(y, a, W, tm, g = rep(0.5, n), bounds = c(-5, 10))
  expect_true(is.finite(kg$psi))
  expect_error(morie_tmle_cross_lagged(y, a[-1], W, tm), "differ in length")
  expect_error(morie_tmle_cross_lagged(y, a, W, tm, bounds = c(1, 1)), "degenerate")
})
