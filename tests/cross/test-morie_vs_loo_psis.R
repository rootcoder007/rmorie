test_that("psis_loo reproduces loo::loo with r_eff = 1", {
  skip_if_not_installed("loo")
  set.seed(19)
  for (S in c(40, 300)) {
    mu <- rnorm(S, 0.2, 0.3)
    s <- sqrt(1 / rgamma(S, 6, 5))
    y <- c(rnorm(15), 4.2)
    ll <- sapply(y, function(v) dnorm(v, mu, s, log = TRUE))
    ref <- suppressWarnings(loo::loo(ll, r_eff = rep(1, ncol(ll))))
    r <- psis_loo(ll)
    est <- ref$estimates
    expect_equal(r$elpd_loo, est["elpd_loo", "Estimate"], tolerance = 1e-10)
    expect_equal(r$p_loo, est["p_loo", "Estimate"], tolerance = 1e-10)
    expect_equal(r$se, est["elpd_loo", "SE"], tolerance = 1e-10)
    expect_equal(r$k_hat, unname(ref$diagnostics$pareto_k), tolerance = 1e-10)
  }
})
