.cd_u <- .morie_random_uniform(100, seed = 73, stream = 0)
.cd_th <- (0.8 + 1.2 * (.cd_u[1:30] - 0.5) + ifelse((0:29) %% 7 == 0, pi, 0)) %% (2 * pi)
.cd_X <- rbind(c(1, 2, 7, 3), c(2, 2, 6, 1), c(3, 1, 6, 2), c(1.5, 2.5, 4, 2), c(2.2, 0.8, 5, 3), c(0.9, 3.1,
    4.4, 1.6))

test_that("CircularSummary and VonmisesMle equal the circular package", {
  s <- CircularSummary(.cd_th)
  expect_equal(unname(unlist(s)), c(0.6502944378784097, 0.626856758131225, 0.373143241868775,
      0.9664752665022612, 2.264567274783553e-06), tolerance = 1e-12)
  m <- VonmisesMle(.cd_th)
  expect_equal(c(m$mu, m$kappa, m$se_mu, m$se_kappa, VonmisesMle(.cd_th, TRUE)$kappa, m$loglik),
      c(0.6502944378784096, 1.6237033656887356, 0.1811991711262737, 0.38578422618001035, 1.5826449629808073,
      -41.83229321296985),
               tolerance = 1e-12)
})

test_that("compositional summaries and Dirichlet models by definition", {
  s <- AitchisonClrCovariance(.cd_X)
  expect_equal(s$total_variance, sum(s$variation) / 8)
  expect_equal(CompositionalMad(rbind(c(1, 2, 4), c(2, 2, 2), c(4, 2, 1))), c(log(2), 0, log(2)))
  expect_equal(CompositionalPielou(c(3, 3, 3)), 1)
  expect_equal(sum(AitchisonBiplot(.cd_X)$explained), 1)
  expect_equal(DirichletFitMom(rbind(c(1, 2, 7), c(2, 2, 6), c(3, 1, 6)))$alpha, c(3, 2.5, 9.5))
  expect_equal(rowSums(DirichletSample(c(1.5, 2, 0.7), 20, seed = 9)), rep(1, 20))
  expect_error(CompositionalPielou(c(1, 0)), "positive")
})
