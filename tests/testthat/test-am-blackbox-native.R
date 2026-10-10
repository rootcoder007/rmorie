# Native Aldrich-McKelvey (closed form) and blackbox (EM low-rank) scaling.

am_data <- function(seed, N = 150L) {
  set.seed(seed)
  zt <- c(-1.2, -0.4, 0.1, 0.6, 1.3, -0.9)
  Z <- t(vapply(seq_len(N), function(i) {
    round((zt - stats::rnorm(1, 0, 0.5)) / stats::runif(1, 0.5, 2) +
            stats::rnorm(6, 0, 0.3) + 4)
  }, numeric(6)))
  Z[sample(length(Z), 60)] <- NA
  Z
}

am_loss <- function(Z, z) {
  rows <- which(rowSums(is.na(Z)) == 0 & apply(Z, 1L, stats::var) > 0)
  sum(vapply(rows, function(i) {
    sum(stats::lm.fit(cbind(1, Z[i, ]), z)$residuals^2)
  }, 0))
}

test_that("the native AM stimuli minimise the Aldrich-McKelvey loss", {
  Z <- am_data(1L)
  z <- rmorie:::.sv_am_stimuli(Z)
  expect_equal(mean(z), 0, tolerance = 1e-12)
  expect_equal(stats::sd(z), 1, tolerance = 1e-12)
  expect_lt(z[1], 0)
  best <- am_loss(Z, z)
  set.seed(3)
  for (k in 1:200) {
    v <- z + stats::rnorm(6, 0, 0.05)
    v <- (v - mean(v)) / stats::sd(v)
    expect_gte(am_loss(Z, v), best - 1e-9)
  }
})

test_that("the native AM stimuli equal basicspace::aldmck", {
  skip_if_not_installed("basicspace")
  Z <- am_data(2L)
  ref <- as.numeric(basicspace::aldmck(Z, respondent = 0, polarity = 1,
                                       verbose = FALSE)$stimuli)
  ref <- (ref - mean(ref)) / stats::sd(ref)
  expect_equal(rmorie:::.sv_am_stimuli(Z), ref, tolerance = 1e-10)
})

test_that("AM refuses a single stimulus or no usable respondent", {
  expect_error(morie_spatial_voting_aldrich_mckelvey(matrix(1:3, 3, 1)),
               "at least two stimuli")
  expect_error(rmorie:::.sv_am_stimuli(matrix(c(1, 1, NA, 1), 2, 2)),
               "at least one respondent")
})

test_that("native blackbox with complete data matches basicspace::blackbox", {
  skip_if_not_installed("basicspace")
  # blackbox stops after at most five joint sweeps (|dSSE| < 0.01), as basicspace's BLACKB does,
  # so on weakly structured data it is not the converged truncated SVD
  set.seed(4)
  X <- round(matrix(stats::rnorm(40 * 2), 40, 2) %*% t(matrix(stats::rnorm(6 * 2), 6, 2)) +
               matrix(stats::rnorm(40 * 6, 0, 0.5), 40, 6) + 4)
  dimnames(X) <- list(paste0("r", 1:40), paste0("q", 1:6))
  ref <- basicspace::blackbox(X, dims = 2, minscale = 5, verbose = FALSE)
  rfit <- as.matrix(ref$individuals[[2]][, c("c1", "c2")]) %*%
    t(as.matrix(ref$stimuli[[2]][, c("w1", "w2")]))
  f <- morie_spatial_voting_blackbox(X, n_dims = 2L)
  expect_identical(f$engine, "native")
  # basicspace prints three decimals
  expect_lt(max(abs(rfit - f$ideal_points %*% t(f$stimuli_weights))), 0.01)
})

test_that("native blackbox matches basicspace::blackbox with missing cells", {
  skip_if_not_installed("basicspace")
  set.seed(2)
  n <- 120
  p <- 10
  X <- round(matrix(stats::rnorm(n * 2), n, 2) %*%
               t(matrix(stats::rnorm(p * 2), p, 2)) +
               matrix(stats::rnorm(n * p, 0, 0.5), n, p) + 4)
  X[sample(length(X), 100)] <- NA
  dimnames(X) <- list(paste0("r", 1:n), paste0("q", 1:p))
  ref <- basicspace::blackbox(X, dims = 2, minscale = 8, verbose = FALSE)
  rfit <- as.matrix(ref$individuals[[2]][, c("c1", "c2")]) %*%
    t(as.matrix(ref$stimuli[[2]][, c("w1", "w2")]))
  testthat::local_mocked_bindings(
    requireNamespace = function(...) FALSE, .package = "base")
  f <- morie_spatial_voting_blackbox(X, n_dims = 2L)
  nfit <- f$ideal_points %*% t(f$stimuli_weights)
  # basicspace prints three decimals
  expect_lt(max(abs(rfit - nfit), na.rm = TRUE), 0.01)
  expect_identical(unname(is.na(nfit[, 1])), unname(is.na(rfit[, 1])))
})
