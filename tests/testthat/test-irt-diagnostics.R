test_that("Lordzs, Semthe and Ave", {
  S <- matrix(c(0.05, 0.01, 0.01, 0.04), 2)
  d <- c(0.3, 0.2)
  r <- Lordzs(c(1.2, 0.3), c(0.9, 0.1), matrix(c(0.02, 0.004, 0.004, 0.015), 2), matrix(c(0.03, 0.006, 0.006, 0.025), 2))
  expect_equal(r$statistic, as.numeric(t(d) %*% solve(S) %*% d), tolerance = 1e-12)
  P <- plogis(1.2 * 0.5)
  Q <- plogis(0.8 * (0.5 - 1))
  expect_equal(Semthe(0.5, rbind(c(1.2, 0), c(0.8, 1)))$se, 1 / sqrt(1.44 * P * (1 - P) + 0.64 * Q * (1 - Q)))
  expect_equal(Ave(c(0.7, 0.8, 0.9)), (0.49 + 0.64 + 0.81) / 3)
})

test_that("Hsirt solves the item probit score equations", {
  x <- -1.5 + 3 * (0:15) / 15
  V <- rbind(c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1), c(0, 1, 0, 1, 0, 1, 1, 1, 0, 1), c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    c(1, 1, 0, 1, 1, 1, 0, 1, 0, 1), c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1), c(1, 0, 0, 1, 1, 0, 0, 1, 1, 0),
    c(1, 1, 1, 0, 0, 1, 1, 0, 0, 1), c(0, 0, 1, 1, 0, 0, 1, 1, 1, 0), c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1),
    c(1, 0, 1, 1, 0, 0, 1, 1, 0, 0), c(1, 0, 1, 1, 1, 0, 1, 1, 1, 1), c(1, 0, 1, 0, 0, 0, 1, 0, 0, 0),
    c(0, 0, 1, 1, 1, 0, 1, 1, 1, 0), c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0), c(1, 0, 1, 1, 0, 0, 1, 1, 0, 0),
    c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
  r <- Hsirt(V, x, max_iter = 200)
  expect_equal(sum(log(r$psi)), 0, tolerance = 1e-12)
  for (j in 1:10) {
    z <- (r$beta[j] * x - r$alpha[j]) / r$psi
    pz <- pmin(pmax(pnorm(z), 1e-9), 1 - 1e-9)
    s <- dnorm(z) * (V[, j] - pz) / (pz * (1 - pz))
    expect_equal(c(sum(s * -1 / r$psi) - r$alpha[j] / 25, sum(s * x / r$psi) - r$beta[j] / 25), c(0, 0), tolerance = 1e-6)
  }
})
