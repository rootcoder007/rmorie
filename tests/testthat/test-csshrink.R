test_that("PRS-CS posterior mean is (D + Psi^-1)^-1 beta_hat", {
  bh <- c(0.12, -0.05, 0.30)
  D <- matrix(c(1, 0.4, 0.1, 0.4, 1, 0.3, 0.1, 0.3, 1), 3, 3)
  psi <- c(0.5, 0.02, 2)
  exp <- as.numeric(solve(D + diag(1 / psi), bh))
  expect_equal(Csshrink(bh, D, psi, n = 5000)$beta, exp, tolerance = 1e-13)
  expect_equal(Csshrink(bh, D, psi, n = 50)$beta, exp, tolerance = 1e-13)
  expect_equal(Csshrink(0.2, 1, 0.25, n = 1000)$beta, 0.04, tolerance = 1e-15)
})
