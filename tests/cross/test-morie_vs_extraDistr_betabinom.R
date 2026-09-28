test_that("BetaBinomialPmf equals extraDistr::dbbinom", {
  skip_if_not_installed("extraDistr")
  for (pr in list(c(20, 0.7, 2.3), c(33, 4.1, 1.2), c(25, 1, 1))) {
    k <- 0:pr[1]
    ours <- vapply(k, function(j) BetaBinomialPmf(j, pr[1], pr[2], pr[3]), 0)
    expect_equal(ours, extraDistr::dbbinom(k, pr[1], pr[2], pr[3]), tolerance = 1e-12)
  }
})
