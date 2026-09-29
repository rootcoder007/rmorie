# R twins of the causal front ends ate, g_comp, late, pliv and prsmtd.
# Every value is recomputed here from the estimator's definition; the
# Python arm gives the same numbers to 1e-15 on this design.

.ce_data <- function() {
  n <- 40
  i <- 0:(n - 1)
  x <- ((i * 7919) %% 97) / 97 - 0.5
  z <- as.integer((i * 31) %% 5 < 2)
  t <- as.integer(z + x + ((i * 13) %% 7) / 7 > 1)
  y <- 1 + 2 * t + 0.7 * x + ((i * 17) %% 11) / 11 - 0.5
  w <- 1 + ((i * 3) %% 4) / 4
  data.frame(x = x, z = z, t = t, y = y, w = w)
}

test_that("Ate is the weighted difference in means with its Neyman variance", {
  d <- .ce_data()
  r <- Ate(d, outcome = "y", treatment = "t", weights_col = "w")
  m1 <- sum(d$w[d$t == 1] * d$y[d$t == 1]) / sum(d$w[d$t == 1])
  m0 <- sum(d$w[d$t == 0] * d$y[d$t == 0]) / sum(d$w[d$t == 0])
  expect_equal(r[[1]], m1 - m0, tolerance = 1e-12)
  expect_true(is.finite(r[[2]]) && r[[2]] > 0)
  expect_error(Ate(d, outcome = "y", treatment = "nope", weights_col = "w"))
})

test_that("GComp standardises the fitted outcome model over the sample", {
  d <- .ce_data()
  r <- GComp(d, treatment = "t", outcome = "y", covariates = "x", outcome_model = "linear")
  fit <- stats::lm(y ~ t + x, data = d)
  p1 <- predict(fit, transform(d, t = 1L))
  p0 <- predict(fit, transform(d, t = 0L))
  expect_equal(r$ate, mean(p1 - p0), tolerance = 1e-12)
  expect_error(GComp(d, treatment = "t", outcome = "y", covariates = "x", outcome_model = "cubic"))
})

test_that("Late is two-stage least squares with the homoskedastic standard error", {
  d <- .ce_data()
  r <- Late(d, treatment = "t", outcome = "y", instrument = "z", covariates = "x")
  Z <- cbind(1, d$z, d$x)
  X <- cbind(1, d$t, d$x)
  Pz <- Z %*% solve(crossprod(Z), t(Z))
  Xh <- Pz %*% X
  b <- solve(crossprod(Xh, X), crossprod(Xh, d$y))
  e <- d$y - X %*% b
  s2 <- sum(e^2) / (nrow(d) - ncol(X))
  V <- s2 * solve(crossprod(Xh, X))
  expect_equal(r$late, b[2], tolerance = 1e-12)
  expect_equal(r$se, sqrt(V[2, 2]), tolerance = 1e-12)
  expect_equal(r$ci[1], r$late - stats::qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_equal(r$ci[2], r$late + stats::qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_error(Late(d, treatment = "t", outcome = "y", instrument = "z", se_type = "cluster"))
})

test_that("Pliv reports a normal interval and is reproducible for a seed", {
  d <- .ce_data()
  r <- Pliv(d, treatment = "t", outcome = "y", instrument = "z", covariates = "x", n_folds = 4, random_state = 3)
  expect_equal(r$n_obs, 40)
  expect_equal(r$ci_lower, r$late - stats::qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_equal(r$ci_upper, r$late + stats::qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_equal(r$pval, 2 * stats::pnorm(-abs(r$late / r$se)), tolerance = 1e-9)
  r2 <- Pliv(d, treatment = "t", outcome = "y", instrument = "z", covariates = "x", n_folds = 4, random_state = 3)
  expect_identical(r$late, r2$late)
  expect_true(abs(r$late - 2) < 3 * r$se)
  expect_error(Pliv(d, treatment = "t", outcome = "y", instrument = "z", covariates = "x", n_folds = 1))
})

test_that("Prsmtd matches every initiator to a non-initiator at the same time", {
  A <- rbind(c(0, 1), c(1, 1), c(0, 0), c(0, 0), c(1, 1), c(0, 0), c(0, 1), c(0, 0))
  H <- rbind(c(0.1, 0.9), c(1.2, 1.0), c(-0.3, 0.2), c(0.5, 0.4), c(0.9, 1.1), c(-0.8, -0.1), c(0.4, 1.3), c(0.0, 0.7))
  r <- Prsmtd(A, H)
  # an initiation at time k is a 0 -> 1 switch (or a 1 at the first time)
  starts <- which(A[, 1] == 1)
  later <- which(A[, 1] == 0 & A[, 2] == 1)
  expect_equal(r$n_initiators, length(starts) + length(later))
  m <- r$matched_idx
  expect_equal(nrow(m), r$n_initiators)
  for (i in seq_len(nrow(m))) {
    k <- m[i, 1]
    expect_true(A[m[i, 2], k] == 1)               # the initiator is treated at time k
    expect_true(A[m[i, 3], k] == 0)               # its match is not
    if (k > 1) expect_true(A[m[i, 2], k - 1] == 0) # and the initiator was untreated before
  }
  expect_error(Prsmtd(A[, 1], H), "shape")
})
