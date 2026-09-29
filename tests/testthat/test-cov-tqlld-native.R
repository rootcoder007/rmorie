# Lloyd-Max quantisers (Lloyd 1982; Max 1960): the uniform closed form,
# the Gaussian two-level optimum +-sqrt(2/pi) with distortion 1 - 2/pi,
# the centroid/midpoint fixed point on empirical data, and nearest-code
# quantisation. The Gaussian cells are integrated by a 20000-point
# midpoint rule, accurate to about 1e-7, hence tolerance 1e-6 there.

lm_funs <- list(morie_tqlld, morie_lloyd_max_codebook, morie_tqlld_lloyd_max_codebook,
                morie_tqlld_tqlld, morie_tqlld_turboquant_lloyd_max_codebook)

test_that("the uniform source has the closed-form midpoint codebook", {
  for (f in lm_funs) {
    r <- f(4, "uniform", lo = 0, hi = 2)
    expect_equal(r$codebook, c(0.25, 0.75, 1.25, 1.75), tolerance = 1e-15)
    expect_equal(r$boundaries, c(0.5, 1, 1.5), tolerance = 1e-15)
    expect_equal(r$distortion, 0.5^2 / 12, tolerance = 1e-15)
    expect_error(f(2, "uniform", lo = 1, hi = 1), "hi > lo")
    expect_error(f(0), "levels must be >= 1")
    expect_error(f(2, "laplace"), "source must be one of")
  }
})

test_that("the Gaussian optimum matches Max's two- and four-level tables", {
  for (f in lm_funs) {
    r2 <- f(2, "gaussian")
    expect_equal(r2$codebook, c(-1, 1) * sqrt(2 / pi), tolerance = 1e-6)
    expect_equal(r2$distortion, 1 - 2 / pi, tolerance = 1e-6)
    expect_true(r2$converged)
    r4 <- f(4, "gaussian")
    # centroid condition: y_k = (phi(b_{k-1}) - phi(b_k)) / (Phi(b_k) - Phi(b_{k-1}))
    e <- c(-Inf, r4$boundaries, Inf)
    cen <- (stats::dnorm(e[-5]) - stats::dnorm(e[-1])) / (stats::pnorm(e[-1]) - stats::pnorm(e[-5]))
    expect_equal(r4$codebook, cen, tolerance = 1e-6)
    expect_equal(r4$codebook[3:4], c(0.4528, 1.510), tolerance = 1e-3)
    expect_true(all(diff(r4$distortion_history) <= 1e-12))
  }
})

test_that("the empirical source converges to the centroid/midpoint fixed point", {
  set.seed(4)
  x <- c(stats::rnorm(60, -2), stats::rnorm(60, 1, 0.5), stats::rnorm(30, 4, 0.3))
  for (f in lm_funs) {
    r <- f(3, "empirical", data = x, max_iter = 500, tol = 1e-14)
    cell <- findInterval(x, r$boundaries, left.open = TRUE) + 1
    expect_equal(r$codebook, as.numeric(tapply(x, cell, mean)), tolerance = 1e-12)
    expect_equal(r$distortion, mean((x - r$codebook[cell])^2), tolerance = 1e-12)
    expect_error(f(3, "empirical"), "needs data")
    expect_error(f(3, "empirical", data = 1:2), "samples")
  }
})

test_that("quantisation picks the nearest codeword", {
  cb <- c(-1, 0.2, 1.5)
  x <- c(-3, -0.3, 0.9, 0.84, 2)
  for (f in list(morie_quantize_with_codebook, morie_tqlld_quantize_with_codebook)) {
    q <- f(x, cb)
    idx <- apply(abs(outer(x, cb, "-")), 1, which.min)
    expect_identical(q$indices, idx - 1L)
    expect_equal(q$values, cb[idx])
    expect_equal(q$mse, mean((x - cb[idx])^2), tolerance = 1e-15)
    # a one-level codebook maps everything to its single codeword
    expect_equal(f(x, 0.5)$values, rep(0.5, 5))
    expect_error(f(x, numeric(0)), "empty")
  }
  expect_match(morie_tqlld_cheatsheet(), "E[X | cell k]", fixed = TRUE)
})
