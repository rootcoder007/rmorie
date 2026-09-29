# Coverage for the forward search (Atkinson & Riani 2000): the truncated-
# normal consistency factor, subset OLS against stats::lm.fit, the
# least-median-of-squares start regenerated from the counter generator,
# each forward step (the m + 1 smallest absolute residuals, the minimum
# deletion residual with leverage), the monitoring plot and outlier
# flagging on planted contamination.

.dat <- function() {
  set.seed(11)
  x <- stats::runif(30, 0, 10)
  y <- 2 + 0.5 * x + stats::rnorm(30, sd = 0.3)
  y[c(4, 17, 25)] <- y[c(4, 17, 25)] + c(6, 7, -6)
  list(X = cbind(1, x), y = y)
}

test_that("the consistency factor is 1 - (2n/m) z phi(z), z = Phi^-1((n+m)/2n)", {
  z <- stats::qnorm((20 + 12) / 40)
  expect_equal(morie_forwsr_consistency_factor(12, 20), 1 - (40 / 12) * z * stats::dnorm(z), tolerance = 1e-12)
  expect_identical(morie_forwsr_consistency_factor(20, 20), 1)
  expect_error(morie_forwsr_consistency_factor(0, 20), "cannot be empty")
})

test_that("subset OLS matches lm.fit and reports residuals for every unit", {
  d <- .dat()
  s <- c(0, 2, 5, 9, 11, 20)
  f <- morie_forwsr_ols_fit(d$X, d$y, s)
  l <- stats::lm.fit(d$X[s + 1, ], d$y[s + 1])
  expect_equal(f$beta, unname(l$coefficients), tolerance = 1e-10)
  expect_equal(f$residuals, as.numeric(d$y - d$X %*% l$coefficients), tolerance = 1e-10)
  expect_equal(f$s2, sum(l$residuals^2) / 4, tolerance = 1e-10)
  expect_identical(morie_forwsr_ols_fit(d$X, d$y)$df, 28L)
  expect_error(morie_forwsr_ols_fit(d$X, d$y, 1), "subset of 1 cannot fit 2")
  expect_error(morie_forwsr_ols_fit(cbind(1, rep(1, 30)), d$y), "rank deficient")
  expect_error(morie_forwsr_ols_fit(d$X, d$y[-1]), "30 rows of X but 29")
  expect_error(morie_forwsr_ols_fit(d$X[1:3, ], d$y[1:3]), "at least four")
})

test_that("the LMS start minimises the median squared residual over the draws", {
  d <- .dat()
  st <- morie_forwsr_lms_start(d$X, d$y, n_draw = 40, seed = 3)
  e <- .ghc_rng(3)
  best <- Inf
  arg <- NULL
  for (k in 1:40) {
    idx <- integer(0)
    for (q in 1:2) {
      j <- as.integer(.ghc_unif(e, 1) * 30) %% 30
      while (j %in% idx) j <- as.integer(.ghc_unif(e, 1) * 30) %% 30
      idx <- c(idx, j)
    }
    b <- stats::lm.fit(d$X[idx + 1, ], d$y[idx + 1])$coefficients
    med <- sort(as.numeric(d$y - d$X %*% b)^2)[16]
    if (med < best) {
      best <- med
      arg <- sort(idx)
    }
  }
  expect_identical(st$subset, as.integer(arg))
  expect_equal(st$median_sq_residual, best, tolerance = 1e-10)
})

test_that("each step adds the smallest residuals and monitors the leverage-scaled deletion residual", {
  d <- .dat()
  steps <- morie_forwsr_forward_search(d$X, d$y, start = c(0, 1, 2))
  expect_identical(vapply(steps, `[[`, 1L, "m"), 3:30)
  for (i in c(1, 10, 20)) {
    s <- steps[[i]]
    f <- morie_forwsr_ols_fit(d$X, d$y, s$subset)
    expect_identical(steps[[i + 1]]$subset, sort(order(abs(f$residuals))[1:(s$m + 1)] - 1L))
    out <- setdiff(0:29, s$subset) + 1
    h <- rowSums((d$X[out, ] %*% solve(crossprod(d$X[s$subset + 1, ]))) * d$X[out, ])
    sig <- f$sigma / sqrt(morie_forwsr_consistency_factor(s$m, 30))
    expect_equal(s$min_deletion_residual, min(abs(f$residuals[out]) / sqrt(1 + h)) / sig, tolerance = 1e-10)
  }
  expect_true(is.na(steps[[28]]$min_deletion_residual))
  fp <- morie_forwsr_forward_plot(steps, "sigma")
  expect_equal(fp$sigma, vapply(steps, `[[`, 1, "sigma"))
  expect_equal(fp$m, as.numeric(3:30))
  expect_error(morie_forwsr_forward_plot(steps, "beta0"), "not monitored")
  expect_error(morie_forwsr_forward_plot(list()), "no steps")
  expect_error(morie_forwsr_forward_search(d$X, d$y, start = 0), "at least 2 observations")
})

test_that("the planted outliers enter last and are flagged after the jump", {
  d <- .dat()
  r <- morie_forwsr(d$X, d$y, n_draw = 100, seed = 2)
  expect_setequal(tail(vapply(r$entry_order, `[[`, 1L, "entered"), 3), c(3L, 16L, 24L))
  expect_true(all(c(3L, 16L, 24L) %in% r$flagged))
  expect_true(r$jump_at_m <= 27)
  expect_equal(r$coefficients, unname(stats::lm.fit(d$X, d$y)$coefficients), tolerance = 1e-10)
  expect_identical(r$monitored_from_m, 7L)
})
