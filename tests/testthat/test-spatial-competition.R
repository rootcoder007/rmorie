test_that("PolicyMotivatedEquilibrium matches the symmetric closed form", {
  for (p in list(c(1, 0.5), c(2, 1), c(0.5, 0.1))) {
    r <- PolicyMotivatedEquilibrium(-p[1], p[1], 0, p[2])
    x <- p[1] / (1 + 2 * p[1] * stats::dnorm(0) / p[2])
    expect_lt(max(abs(c(r$x1, r$x2) - c(-x, x))), 1e-12)
  }
  r <- PolicyMotivatedEquilibrium(c(-1, 0), c(1, 0), c(0, 0), 0.5)
  expect_lt(abs(r$x2[1] - 1 / (1 + 2 * stats::dnorm(0) / 0.5)), 1e-12)
})

test_that("PolicyMotivatedEquilibrium solves the first-order conditions and matches Python", {
  r <- PolicyMotivatedEquilibrium(c(-1, 0.5), c(1.5, 1), c(0.2, -0.3), 0.7)
  ref <- c(-0.30714734126985294, 0.062815566635104833, 0.71908169996883797, 0.18770075462936342)
  expect_lt(max(abs(c(r$x1, r$x2) - ref)), 1e-10)
  expect_lt(max(abs(unlist(r$gradients))), 1e-10)
})

test_that("PluralityCompetition gives the Downs and Eaton-Lipsey equilibria", {
  r <- PluralityCompetition(c(0, 1, 2, 7, 9))
  expect_identical(r$positions, c(2, 2))
  expect_true(r$is_equilibrium)
  expect_gt(PluralityCompetition(c(0, 1, 2, 7, 9), c(1, 2))$gain[1], 0.09)
  U <- (seq_len(120) - 0.5) / 120
  expect_true(PluralityCompetition(U, c(0.25, 0.25, 0.75, 0.75))$is_equilibrium)
  expect_true(PluralityCompetition(U, c(1 / 6, 1 / 6, 0.5, 5 / 6, 5 / 6))$is_equilibrium)
  expect_false(PluralityCompetition(U, c(0.25, 0.5, 0.75))$is_equilibrium)
})

test_that("LogitCompetition follows Schofield's Hessian and matches Python", {
  Z <- .morie_random_normal(80, seed = 3, stream = 0)
  X <- cbind(Z[seq(1, 79, 2)], 0.6 * Z[seq(2, 80, 2)])
  r <- LogitCompetition(X, c(0, 0.5, 1), 0.2)
  expect_lt(max(abs(r$positions - matrix(colMeans(X), 3, 2, byrow = TRUE))), 1e-10)
  for (j in 1:3) {
    rho <- r$mean_shares[j]
    want <- sort(2 * 0.2 * rho * (1 - rho) * eigen(r$characteristic_matrices[[j]], only.values = TRUE)$values)
    expect_lt(max(abs(sort(r$hessian_eigenvalues[[j]]) - want)), 1e-6)
  }
  r <- LogitCompetition(X, c(0, 0.2, 1.5), 1.5)
  expect_true(r$is_local_nash)
  ref <- rbind(c(-0.593141209637, -0.1798003389928), c(0.864816154602, 0.0849780285912), c(0.168313932046, -0.0407506927967))
  expect_lt(max(abs(r$positions - ref)), 1e-9)
})
