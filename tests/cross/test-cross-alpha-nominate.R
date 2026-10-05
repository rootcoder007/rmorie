# SPDX-License-Identifier: AGPL-3.0-or-later
test_that("native alpha-NOMINATE agrees with anominate::anominate", {
  skip_if_not_installed("anominate")
  skip_if_not_installed("pscl")
  set.seed(7)
  n <- 40
  m <- 120
  x <- sort(runif(n, -1, 1))
  x[1] <- 0.9
  mid <- runif(m, -0.8, 0.8)
  sp <- runif(m, 0.2, 0.6) * sample(c(-1, 1), m, TRUE)
  dY <- outer(x, mid - sp, "-")^2
  dN <- outer(x, mid + sp, "-")^2
  quad <- -0.5 * 8 * 0.25 * (dY - dN)
  nom <- 8 * (exp(-0.125 * dY) - exp(-0.125 * dN))
  V <- matrix(rbinom(n * m, 1, pnorm(quad + 0.5 * (nom - quad))), n, m)
  fit <- morie_spatial_voting_alpha_nominate(V, n_dims = 1L, n_samples = 400L,
                                             burn_in = 300L, seed = 3L)
  rc <- pscl::rollcall(V, yea = 1, nay = 0, missing = NA,
                       legis.names = paste0("L", seq_len(n)))
  utils::capture.output(ref <- suppressMessages(
    anominate::anominate(rc, dims = 1, nsamp = 700, burnin = 300,
                         polarity = 1, random.starts = TRUE)))
  am <- colMeans(ref$legislators[[1]])
  expect_gt(cor(am, fit$ideal_points[, 1]), 0.99)
  expect_lt(abs(mean(ref$alpha) - fit$alpha), 0.1)
  expect_lt(abs(mean(ref$beta) / fit$beta - 1), 0.1)
})
