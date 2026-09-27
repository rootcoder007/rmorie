test_that("morie_spatial_voting_cjr_irt samples the MCMCpack::MCMCirt1d posterior", {
  skip_if_not_installed("MCMCpack")
  skip_if_not_installed("coda")
  U <- .morie_random_uniform(1200, seed = 4, stream = 0)
  Z <- .morie_random_normal(600, seed = 4, stream = 1)
  V <- matrix(as.numeric(U[1:600] < stats::pnorm(outer(Z[1:40], 0.8 + 0.2 * Z[101:115]) -
                                                   0.3 * Z[201:215][col(matrix(0, 40, 15))])), 40, 15)
  st <- morie_spatial_voting_em_irt(V)$ideal_points
  # prior variance 1 on the bill parameters: both Gibbs samplers then mix
  # well enough (ESS near 200) for a z-test on posterior means
  f <- morie_spatial_voting_cjr_irt(V, n_samples = 5000L, burn_in = 500L, start = st, seed = 1,
                                    beta_prior_var = 1)
  m <- MCMCpack::MCMCirt1d(V, theta.start = as.numeric(st), burnin = 500, mcmc = 5000, seed = 11,
                           T0 = 1, AB0 = 1, store.item = TRUE, verbose = 0)
  ref <- as.matrix(m)
  ref <- ref[, c(grep("^theta", colnames(ref)), grep("^alpha", colnames(ref)), grep("^beta", colnames(ref)))]
  # location and scale of the latent axis are pinned only by the N(0, 1)
  # prior and mix slowly, so compare the invariant functionals per draw:
  # standardised ideal points, alpha - beta * mean(theta), beta * sd(theta)
  inv <- function(th, a, b) {
    m <- rowMeans(th)
    s <- apply(th, 1, stats::sd)
    cbind((th - m) / s, a - b * m, b * s)
  }
  our <- inv(f$ideal_point_chain[, , 1], f$alpha_chain, f$beta_chain[, , 1])
  ref <- inv(ref[, 1:40], ref[, 41:55], ref[, 56:70])
  tse <- function(ch) apply(ch, 2, stats::sd) / sqrt(coda::effectiveSize(coda::mcmc(ch)))
  z <- (colMeans(our) - colMeans(ref)) / sqrt(tse(our)^2 + tse(ref)^2)
  expect_lt(max(abs(z)), 4)
  expect_lt(mean(z^2), 2)
})
