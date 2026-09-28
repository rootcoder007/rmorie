test_that("SubmaxTest critical constant equals mvtnorm::qmvnorm", {
  skip_if_not_installed("mvtnorm")
  for (p in 1:3) {
    C <- SubmaxComparisons(p)
    G <- ncol(C)
    r <- SubmaxTest(rep(1, G), rep(0, G), rep(1, G), C, nsim = 40000)
    set.seed(1)
    q <- mvtnorm::qmvnorm(0.95, tail = "lower.tail", corr = r$rho)$quantile
    expect_lt(abs(r$kappa - q), 0.03)
  }
})
