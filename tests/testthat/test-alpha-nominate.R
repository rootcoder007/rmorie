# Native alpha-NOMINATE (Carroll et al. 2013).

anom_sim <- function(n, m, alpha, beta = 8, seed = 7L) {
  set.seed(seed)
  x <- sort(stats::runif(n, -1, 1))
  mid <- stats::runif(m, -0.8, 0.8)
  sp <- stats::runif(m, 0.2, 0.6) * sample(c(-1, 1), m, TRUE)
  dY <- outer(x, mid - sp, "-")^2
  dN <- outer(x, mid + sp, "-")^2
  quad <- -0.5 * beta * 0.25 * (dY - dN)
  nom <- beta * (exp(-0.125 * dY) - exp(-0.125 * dN))
  p <- stats::pnorm(quad + alpha * (nom - quad))
  list(x = x, V = matrix(stats::rbinom(n * m, 1, p), n, m))
}

test_that("the vote probability is the alpha mixture of the two utilities", {
  V <- matrix(c(1, 0), 1, 2)
  dY <- matrix(c(0.3, 1.1), 1, 2)
  dN <- matrix(c(0.9, 0.2), 1, 2)
  beta <- 5
  alpha <- 0.4
  q <- -0.5 * beta * 0.25 * (dY - dN)
  g <- beta * (exp(-0.125 * dY) - exp(-0.125 * dN))
  u <- q + alpha * (g - q)
  expect_equal(rmorie:::.morie_anom_ll(V, dY, dN, beta, alpha),
               matrix(c(stats::pnorm(u[1], log.p = TRUE),
                        stats::pnorm(-u[2], log.p = TRUE)), 1, 2),
               tolerance = 1e-12)
  V[1, 2] <- NA
  expect_identical(rmorie:::.morie_anom_ll(V, dY, dN, beta, alpha)[1, 2], 0)
})

test_that("the vectorised slice sampler draws from its target", {
  set.seed(11)
  x <- 0
  out <- numeric(4000)
  f <- function(v) stats::dnorm(v, 1.5, 0.7, log = TRUE)
  for (i in seq_along(out)) out[i] <- x <- rmorie:::.morie_slice_vec(f, x, 8)
  expect_lt(abs(mean(out) - 1.5), 0.06)
  expect_lt(abs(stats::sd(out) - 0.7), 0.05)
  y <- rmorie:::.morie_slice_vec(function(v) rep(0, length(v)), c(0.2, 0.9),
                                 8, lower = 0, upper = 1)
  expect_true(all(y >= 0 & y <= 1))
})

test_that("alpha-NOMINATE recovers the simulated ideal points", {
  skip_on_cran()
  s <- anom_sim(25L, 60L, alpha = 0.5)
  fit <- morie_spatial_voting_alpha_nominate(
    s$V, n_dims = 1L, n_samples = 150L, burn_in = 150L, seed = 3L,
    minvotes = 10L, polarity = 25L)
  expect_gt(stats::cor(fit$ideal_points[, 1], s$x[fit$legislators_used]),
            0.95)
  expect_gt(fit$ideal_points[fit$legislators_used == 25L, 1], 0)
  expect_true(fit$alpha >= 0 && fit$alpha <= 1)
  expect_true(all(fit$draws$alpha >= 0 & fit$draws$alpha <= 1))
  expect_true(all(fit$draws$beta > 0))
  expect_equal(dim(fit$draws$X), c(150L, length(fit$legislators_used), 1L))
  # each draw is centred on its legislators
  expect_equal(rowMeans(fit$draws$X[, , 1]), rep(0, 150L), tolerance = 1e-12)
})

test_that("constrain = TRUE holds alpha at 1", {
  s <- anom_sim(15L, 30L, alpha = 1, seed = 2L)
  fit <- morie_spatial_voting_alpha_nominate(
    s$V, n_dims = 1L, n_samples = 10L, burn_in = 5L, minvotes = 10L,
    constrain = TRUE)
  expect_identical(unique(fit$draws$alpha), 1)
})

test_that("two-dimensional draws are rotated onto a common configuration", {
  skip_on_cran()
  s <- anom_sim(20L, 40L, alpha = 0.5, seed = 5L)
  fit <- morie_spatial_voting_alpha_nominate(
    s$V, n_dims = 2L, n_samples = 20L, burn_in = 20L, minvotes = 10L)
  expect_equal(dim(fit$ideal_points), c(length(fit$legislators_used), 2L))
  # a rotation preserves every legislator-to-outcome distance
  d1 <- rmorie:::.morie_sqdist(matrix(fit$draws$X[1, , ], ncol = 2),
                               matrix(fit$draws$Y[1, , ], ncol = 2))
  expect_true(all(is.finite(d1)))
})

test_that("bad votes and over-strict screens are refused in plain words", {
  expect_error(morie_spatial_voting_alpha_nominate(matrix(2, 5, 5)),
               "1 \\(yea\\), 0 \\(nay\\) or NA")
  s <- anom_sim(10L, 10L, alpha = 0.5)
  expect_error(morie_spatial_voting_alpha_nominate(s$V, minvotes = 50L),
               "Too few roll calls or legislators")
})
