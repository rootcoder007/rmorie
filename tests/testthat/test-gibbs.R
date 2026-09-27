test_that("GibbsPseudolikelihood matches ppm values", {
  U <- .morie_random_uniform(120, seed = 21, stream = 0)
  P <- cbind(2 * U[seq(1, 119, 2)], U[seq(2, 120, 2)])
  f <- GibbsPseudolikelihood(P, c(0, 2, 0, 1), "geyer", r = 0.1, sat = 2)
  expect_lt(max(abs(f$theta - c(3.658567001, -0.2152837095))), 1e-8)
  f <- GibbsPseudolikelihood(P, c(0, 2, 0, 1), "softcore", kappa = 0.5)
  expect_lt(max(abs(f$se - c(0.13038334, 0.54554565))), 1e-8)
  f <- GibbsPseudolikelihood(P, c(0, 2, 0, 1), "diggle_gratton", delta = 0.005, rho = 0.1)
  expect_lt(max(abs(f$theta - c(3.4691805813, 0.2088573719))), 1e-8)
})

test_that("SequentialInhibition matches the Python arm", {
  r <- SequentialInhibition(0.2, c(0, 1, 0, 1), n = 5, seed = 1)
  expect_identical(r$proposals, 8)
  expect_lt(max(abs(r$points[1, ] - c(0.8902591728838161, 0.8946847162442282))), 1e-15)
  expect_gte(min(stats::dist(r$points)), 0.2)
})
