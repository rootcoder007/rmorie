# Coverage for tmlcds_native, tmlcou_native and tmlcps_native exports.
# Every expectation is recomputed in the test body.

tml2_data <- function() {
  X <- cbind(c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7),
             c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0))
  D <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0)
  y <- c(2.1, 1.0, 2.9, 1.4, 0.3, 2.2, 1.9, 0.8, 3.0, 1.6, 0.9, 1.5)
  list(X = X, D = D, y = y)
}

tml2_fluct <- function(y, qa, h) {
  stats::uniroot(function(e) sum(h * (y - stats::plogis(stats::qlogis(qa) + e * h))),
                 c(-30, 30), tol = 1e-14)$root
}

test_that("ctmle_sequence starts from an intercept propensity and adds covariates greedily", {
  d <- tml2_data()
  s <- ctmle_sequence(d$y, d$D, d$X)
  lo <- min(d$y)
  rg <- max(d$y) - lo
  ys <- pmin(pmax((d$y - lo) / rg, 1e-8), 1 - 1e-8)
  b <- stats::coef(stats::lm(ys ~ d$D + d$X))
  cl <- function(v) pmin(pmax(v, 1e-8), 1 - 1e-8)
  q1 <- cl(b[1] + b[2] + as.numeric(d$X %*% b[3:4]))
  q0 <- cl(b[1] + as.numeric(d$X %*% b[3:4]))
  qa <- ifelse(d$D == 1, q1, q0)
  fl <- function(qa, q1, q0, g) {
    h <- d$D / g - (1 - d$D) / (1 - g)
    e <- tml2_fluct(ys, qa, h)
    list(qa = cl(stats::plogis(stats::qlogis(qa) + e * h)), q1 = cl(stats::plogis(stats::qlogis(q1) + e / g)),
         q0 = cl(stats::plogis(stats::qlogis(q0) - e / (1 - g))), e = e)
  }
  loss <- function(q) -mean(ys * log(q) + (1 - ys) * log(1 - q))
  f0 <- fl(qa, q1, q0, rep(mean(d$D), 12))
  # the package least squares carries a 1e-10 ridge
  expect_equal(s$steps[[1]]$epsilon, f0$e, tolerance = 1e-8)
  expect_equal(s$steps[[1]]$psi, rg * mean(f0$q1 - f0$q0), tolerance = 1e-8)
  expect_equal(s$steps[[1]]$loss, loss(f0$qa), tolerance = 1e-8)
  cand <- lapply(1:2, function(j) {
    g <- stats::fitted(stats::glm(d$D ~ d$X[, j], family = stats::binomial(), control = list(epsilon = 1e-14)))
    fl(f0$qa, f0$q1, f0$q0, pmin(pmax(g, 0.005), 0.995))
  })
  ls <- vapply(cand, function(f) loss(f$qa), 0)
  expect_equal(s$steps[[2]]$covariates, which.min(ls) - 1L)
  expect_equal(s$steps[[2]]$loss, min(ls), tolerance = 1e-8)
  expect_length(s$steps, 3)
  expect_error(ctmle_sequence(d$y, d$D, d$X, tuning = "grid"), "tuning must be")
  expect_error(ctmle_sequence(d$y, d$D + 1, d$X), "binary 0/1")
  expect_error(ctmle_sequence(d$y, d$D, d$X, q_covariates = 5), "outside the 2 covariates")
})

test_that("tmle_cdrs picks the step with the smallest cross-validated loss", {
  d <- tml2_data()
  r <- tmle_cdrs(d$y, d$D, d$X, n_folds = 3)
  s <- ctmle_sequence(d$y, d$D, d$X)
  k <- which.min(r$cv_loss)
  expect_equal(r$selected, k - 1L)
  expect_equal(r$estimate, s$steps[[k]]$psi, tolerance = 1e-12)
  expect_equal(r$in_sample_loss, vapply(s$steps, function(z) z$loss, 0), tolerance = 1e-12)
  expect_same_function(morie_tmlcds, tmle_cdrs)
  cc <- tmle_cdrs(d$y, d$D, d$X, tuning = "continuous", penalties = c(10, 0), n_folds = 3)
  expect_length(cc$steps, 2)
  expect_equal(cc$selected_penalty, c(10, 0)[which.min(cc$cv_loss)])
})

test_that("morie_tmlcou fluctuates a rescaled count outcome logistically", {
  d <- tml2_data()
  cnt <- c(3, 0, 5, 1, 0, 4, 2, 1, 6, 2, 1, 1)
  r <- morie_tmlcou(cnt, d$D, d$X)
  ys <- cnt / 6
  g <- stats::fitted(stats::glm(d$D ~ d$X, family = stats::binomial()))
  g <- pmin(pmax(g, 0.01), 0.99)
  b <- stats::coef(stats::lm(ys ~ d$D + d$X))
  cl <- function(v) pmin(pmax(v, 1e-6), 1 - 1e-6)
  q1 <- cl(b[1] + b[2] + as.numeric(d$X %*% b[3:4]))
  q0 <- cl(b[1] + as.numeric(d$X %*% b[3:4]))
  H <- d$D / g - (1 - d$D) / (1 - g)
  qa <- ifelse(d$D == 1, q1, q0)
  e <- tml2_fluct(ys, qa, H)
  q1s <- stats::plogis(stats::qlogis(q1) + e / g)
  q0s <- stats::plogis(stats::qlogis(q0) - e / (1 - g))
  psi <- 6 * mean(q1s - q0s)
  ic <- 6 * (H * (ys - ifelse(d$D == 1, q1s, q0s)) + q1s - q0s - mean(q1s - q0s))
  expect_equal(r$epsilon, e, tolerance = 1e-10)
  expect_equal(r$estimate, psi, tolerance = 1e-10)
  expect_equal(r$se, sqrt(sum((ic - mean(ic))^2)) / 12, tolerance = 1e-10)
  expect_equal(r$mean_treated, 6 * mean(q1s), tolerance = 1e-10)
  expect_true(r$solves_eic)
  expect_same_function(morie_tmlcountoutcome, morie_tmlcou)
  tt <- c(1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, 2)
  rr <- morie_tmlcou(cnt, d$D, d$X, offset = tt, g = rep(0.5, 12))
  expect_true(rr$rate_scale)
  expect_equal(rr$scale, range(cnt / tt))
  expect_error(morie_tmlcou(-cnt, d$D, d$X), "cannot be negative")
  expect_error(morie_tmlcou(cnt, d$D, d$X, offset = -tt), "offset must be positive")
  expect_error(morie_tmlcou(cnt, d$D, d$X, lower = 0, upper = 3), "outside the stated bounds")
  lf <- linear_fluctuation_unsafe(c(0.9, 0.2, 0.5), c(2, -1, 1), c(1, 0, 1))
  eps <- sum(c(2, -1, 1) * (c(1, 0, 1) - c(0.9, 0.2, 0.5))) / 6
  expect_equal(lf$epsilon, eps, tolerance = 1e-12)
  expect_equal(lf$out_of_range, sum(c(0.9, 0.2, 0.5) + eps * c(2, -1, 1) > 1 | c(0.9, 0.2, 0.5) + eps * c(2, -1, 1) < 0))
  expect_error(linear_fluctuation_unsafe(1:2, 1, 1), "same length")
  expect_equal(unscale(c(0, 0.25, 1), 2, 6), c(2, 3, 6))
})

test_that("pseudo_outcome and effect_curve give the Kennedy doubly robust curve", {
  d <- tml2_data()
  A <- c(0.3, 1.2, 2.5, 0.8, 1.9, 3.1, 0.1, 2.2, 1.5, 2.8, 0.6, 1.7)
  x <- d$X[, 1]
  po <- pseudo_outcome(d$y, A, x)
  ft <- stats::lm(A ~ x)
  s2 <- sum(stats::residuals(ft)^2) / 10
  mu_a <- stats::fitted(ft)
  fy <- stats::coef(stats::lm(d$y ~ A * x))
  mu <- function(a, xx) fy[1] + fy[2] * a + fy[3] * xx + fy[4] * a * xx
  xi <- vapply(1:12, function(i) {
    m <- mean(stats::dnorm(A[i], mu_a, sqrt(s2)))
    (d$y[i] - mu(A[i], x[i])) * m / stats::dnorm(A[i], mu_a[i], sqrt(s2)) + mean(mu(A[i], x))
  }, 0)
  # both least-squares fits carry an absolute 1e-8 ridge
  expect_equal(po$xi, unname(xi), tolerance = 1e-7)
  n0 <- pseudo_outcome(d$y, A, NULL)
  f1 <- stats::coef(stats::lm(d$y ~ A))
  # without covariates the outcome model is still E[Y | A] = b0 + b1 A
  expect_equal(n0$info$standardized_mu, unname(f1[1] + f1[2] * A), tolerance = 1e-7)
  expect_equal(n0$xi, d$y, tolerance = 1e-7)
  expect_error(pseudo_outcome(d$y, A[-1], x), "differ in length")
  grid <- c(0.5, 1.5, 2.5)
  pc <- effect_curve(xi, A, grid, fit = "polynomial")
  pb <- stats::coef(stats::lm(xi ~ A + I(A^2) + I(A^3)))
  expect_equal(pc$curve, unname(pb[1] + pb[2] * grid + pb[3] * grid^2 + pb[4] * grid^3), tolerance = 1e-9)
  kc <- effect_curve(xi, A, grid, bandwidth = 0.6)
  w <- function(g) exp(-0.5 * ((A - g) / 0.6)^2)
  expect_equal(kc$curve, vapply(grid, function(g) sum(w(g) * xi) / sum(w(g)), 0), tolerance = 1e-12)
  lc <- effect_curve(xi, A, grid, fit = "locallinear", bandwidth = 0.6)
  expect_equal(lc$curve, vapply(grid, function(g) unname(stats::coef(stats::lm(xi ~ I(A - g), weights = w(g)))[1]), 0),
               tolerance = 1e-10)
  cvb <- effect_curve(xi, A, grid)
  expect_true(cvb$info$bandwidth %in% (diff(range(A)) * c(0.02, 0.05, 0.08, 0.12, 0.2, 0.3, 0.5, 0.8)))
  expect_error(effect_curve(xi, A, grid, fit = "spline"), "fit must be one of")
  expect_error(effect_curve(xi, A, grid, bandwidth = -1), "bandwidth must be positive")
  r <- morie_tmlcps(d$y, A, x, a_grid = grid, bandwidth = 0.6)
  expect_equal(r$curve, effect_curve(r$pseudo_outcome, A, grid, bandwidth = 0.6)$curve, tolerance = 1e-12)
  expect_equal(r$estimate, mean(diff(r$curve) / diff(grid)), tolerance = 1e-12)
  expect_equal(r$se, stats::sd(r$pseudo_outcome) / sqrt(12), tolerance = 1e-12)
  expect_same_function(morie_tmlcontinuoustreatment, morie_tmlcps)
  expect_error(morie_tmlcps(d$y, d$D, x), "fewer than 3 distinct")
})
