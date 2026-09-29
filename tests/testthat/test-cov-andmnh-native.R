# Coverage for Andrews & Monahan (1992) prewhitened kernel HAC. Kernels
# from their definitions, the AR(1) plug-in and bandwidth against
# Andrews' (1991) published constants (1.1447, 2.6614, 1.3221, 1.7462),
# the HAC sum against its autocovariance definition and against
# sandwich::kernHAC, and the VAR prewhitening against lm.fit.

.vdat <- function() {
  set.seed(12)
  n <- 80
  x <- cbind(1, as.numeric(stats::arima.sim(list(ar = 0.5), n)))
  e <- as.numeric(stats::arima.sim(list(ar = 0.4), n))
  list(x = x, e = e, y = as.numeric(x %*% c(1, 2)) + e)
}

test_that("kernels follow their definitions", {
  x <- c(0, 0.3, 0.6, 1, 1.4)
  expect_equal(vapply(x, bartlett_kernel, 1), pmax(1 - x, 0))
  expect_equal(vapply(x, parzen_kernel, 1), ifelse(x <= 0.5, 1 - 6 * x^2 + 6 * x^3, ifelse(x <= 1, 2 * (1 - x)^3, 0)), tolerance = 1e-12)
  expect_equal(vapply(x, tukey_hanning_kernel, 1), ifelse(x <= 1, (1 + cos(pi * x)) / 2, 0), tolerance = 1e-12)
  z <- 6 * pi * x[-1] / 5
  expect_equal(vapply(x[-1], quadratic_spectral_kernel, 1), 25 / (12 * pi^2 * x[-1]^2) * (sin(z) / z - cos(z)), tolerance = 1e-12)
  expect_identical(quadratic_spectral_kernel(0), 1)
  # the QS kernel integrates (squared) to one over the real line
  expect_equal(2 * stats::integrate(function(u) vapply(u, quadratic_spectral_kernel, 1)^2, 0, Inf, subdivisions = 2000L)$value, 1, tolerance = 1e-4)
  expect_equal(2 * stats::integrate(function(u) vapply(u, parzen_kernel, 1)^2, 0, 1)$value, 0.539285, tolerance = 1e-5)
})

test_that("moment vectors, AR(1) fits and the alpha(q) plug-in", {
  d <- .vdat()
  V <- moment_vectors(d$e, d$x)
  expect_equal(V, d$x * d$e)
  expect_error(moment_vectors(d$e[-1], d$x), "residuals but")
  a <- ar1_fit(V[, 2])
  xx <- V[, 2]
  rho <- sum(xx[-1] * xx[-80]) / sum(xx[-80]^2)
  expect_equal(unname(a), c(rho, sum((xx[-1] - rho * xx[-80])^2) / 79), tolerance = 1e-12)
  expect_error(ar1_fit(1:2), "at least 3")
  al <- alpha_ar1(V, q = 2, weights = "drop_first")
  r <- unname(ar1_fit(V[, 2]))
  expect_equal(al$alpha, (4 * r[1]^2 * r[2]^2 / (1 - r[1])^8) / (r[2]^2 / (1 - r[1])^4), tolerance = 1e-12)
  a1 <- alpha_ar1(V, q = 1)
  f <- lapply(1:2, function(j) unname(ar1_fit(V[, j])))
  num <- sum(vapply(f, function(z) 4 * z[1]^2 * z[2]^2 / ((1 - z[1])^6 * (1 + z[1])^2), 1))
  den <- sum(vapply(f, function(z) z[2]^2 / (1 - z[1])^4, 1))
  expect_equal(a1$alpha, num / den, tolerance = 1e-12)
  expect_error(alpha_ar1(V, q = 3), "q = 1 or 2")
  expect_error(alpha_ar1(V, weights = c(1, 1, 1)), "3 weights for 2 series")
})

test_that("the automatic bandwidth reproduces Andrews' (1991) constants", {
  d <- .vdat()
  V <- moment_vectors(d$e, d$x)
  const <- c(bartlett = 1.1447, parzen = 2.6614, qs = 1.3221, `tukey-hanning` = 1.7462)
  for (k in names(const)) {
    q <- if (k == "bartlett") 1 else 2
    ab <- automatic_bandwidth(V, kernel = k)
    al <- alpha_ar1(V, q = q)$alpha
    # the published multipliers are rounded to four decimals
    expect_equal(ab$bandwidth / (al * 80)^(1 / (2 * q + 1)), unname(const[k]), tolerance = 1e-4)
  }
  expect_error(automatic_bandwidth(V, kernel = "daniell"), "kernel must be one of")
})

test_that("kernel HAC is the weighted autocovariance sum and matches sandwich", {
  d <- .vdat()
  V <- moment_vectors(d$e, d$x)
  s <- 4.3
  J <- kernel_hac(V, s, kernel = "bartlett", n_params = 2)
  ref <- crossprod(V) / 80
  for (j in 1:4) {
    G <- crossprod(V[(j + 1):80, ], V[1:(80 - j), ]) / 80
    ref <- ref + (1 - j / s) * (G + t(G))
  }
  expect_equal(J, ref * 80 / 78, tolerance = 1e-12)
  expect_error(kernel_hac(V, 0), "bandwidth must be positive")
  expect_error(kernel_hac(V, 2, n_params = 80), "not larger than")
  skip_if_not_installed("sandwich")
  fit <- stats::lm(d$y ~ d$x[, 2])
  u <- stats::residuals(fit)
  m0 <- andrews_monahan_hac(u, d$x, prewhiten = FALSE, kernel = "qs", bandwidth = 3.7, n_params = 0)
  s0 <- sandwich::kernHAC(fit, prewhite = 0, bw = 3.7, kernel = "Quadratic Spectral", adjust = FALSE, sandwich = FALSE)
  expect_equal(unname(m0$J), unname(s0), tolerance = 1e-10)
  m1 <- andrews_monahan_hac(u, d$x, prewhiten = TRUE, kernel = "qs", bandwidth = 3.7, n_params = 0, adjust = FALSE)
  s1 <- sandwich::kernHAC(fit, prewhite = 1, bw = 3.7, kernel = "Quadratic Spectral", adjust = FALSE, sandwich = FALSE)
  expect_equal(unname(m1$J), unname(s1), tolerance = 1e-10)
})

test_that("VAR(1) prewhitening is least squares, capped through the SVD", {
  d <- .vdat()
  V <- moment_vectors(d$e, d$x)
  pw <- prewhiten_var(V, order = 1, adjust = FALSE)
  B <- stats::lm.fit(V[-80, ], V[-1, ])$coefficients
  expect_equal(pw$A[[1]], t(B), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(pw$residuals, V[-1, ] - V[-80, ] %*% B, tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(pw$D, solve(diag(2) - t(B)), tolerance = 1e-10, ignore_attr = TRUE)
  M <- matrix(c(1.2, 0.3, -0.4, 0.5), 2)
  sv <- svd(M)
  capd <- sv$u %*% diag(pmin(sv$d, 0.97)) %*% t(sv$v)
  expect_equal(singular_value_adjust(M), capd, tolerance = 1e-12)
  expect_equal(max(svd(singular_value_adjust(M, 0.5))$d), 0.5, tolerance = 1e-12)
  expect_error(singular_value_adjust(M, 1), "strictly between 0 and 1")
  pa <- prewhiten_var(V, order = 1)
  expect_lte(max(svd(pa$A[[1]])$d), 0.97 + 1e-12)
  expect_identical(prewhiten_var(V, order = 0)$D, diag(2))
  expect_error(prewhiten_var(V[1:2, ], order = 1), "cannot fit a VAR")
})

test_that("the full estimator recolours J* by D and uses the automatic bandwidth", {
  d <- .vdat()
  h <- andrews_monahan_hac(d$e, d$x)
  expect_s3_class(h, "andmnh")
  expect_equal(h$J, h$D %*% h$J_star %*% t(h$D), tolerance = 1e-12)
  pw <- prewhiten_var(moment_vectors(d$e, d$x), order = 1)
  expect_equal(h$bandwidth, automatic_bandwidth(pw$residuals, "qs", n = 80)$bandwidth, tolerance = 1e-12)
  expect_equal(h$J_star, kernel_hac(pw$residuals, h$bandwidth, "qs", n_params = 2, n = 80), tolerance = 1e-12)
  expect_true(h$bandwidth_automatic)
  expect_identical(morie_andmnh, andrews_monahan_hac)
  expect_output(print(h), "Andrews-Monahan")
})
