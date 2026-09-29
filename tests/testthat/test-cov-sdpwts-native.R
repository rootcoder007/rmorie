# Semidefinite programming by the barrier method (Boyd & Vandenberghe
# 2004, Secs. 11.2-11.3; Vandenberghe & Boyd 1996): the LMI map, the PSD
# test, the -log det barrier, the m/t gap, and the lambda_min SDP.

sd_F0 <- matrix(c(2, 0.5, 0, 0.5, 1, 0.2, 0, 0.2, 3), 3)
sd_Fs <- list(diag(3), matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3))

test_that("the LMI map is F0 + sum x_i F_i", {
  x <- c(0.3, -0.4)
  expect_equal(sdpwts_lmi(x, sd_F0, sd_Fs), sd_F0 + 0.3 * sd_Fs[[1]] - 0.4 * sd_Fs[[2]])
  expect_error(sdpwts_lmi(1, sd_F0, sd_Fs), "1 variables but 2 matrices")
  expect_error(sdpwts_lmi(c(1, 1), sd_F0, list(diag(3), diag(2))), "is not 3x3")
})

test_that("PSD test and barrier use the eigenvalues", {
  ps <- sdpwts_is_psd(sd_F0)
  ev <- eigen(sd_F0, symmetric = TRUE)$values
  expect_equal(ps$eigenvalues, ev, tolerance = 1e-15)
  expect_true(ps$psd && ps$strictly_feasible)
  expect_false(sdpwts_is_psd(diag(c(1, -1)))$psd)
  expect_true(sdpwts_is_psd(diag(c(1, 0)))$psd)
  x <- c(0.1, 0.2)
  b <- sdpwts_barrier(x, sd_F0, sd_Fs)
  expect_equal(b$value, -as.numeric(determinant(sdpwts_lmi(x, sd_F0, sd_Fs))$modulus), tolerance = 1e-13)
  out <- sdpwts_barrier(c(-5, 0), sd_F0, sd_Fs)
  expect_identical(out$value, Inf)
  expect_false(out$feasible)
})

test_that("the central-path gap is m / t", {
  g <- sdpwts_central_path_gap(50, 3)
  expect_equal(g$gap, 3 / 50)
  expect_error(sdpwts_central_path_gap(0, 3), "t must be positive")
  expect_error(sdpwts_central_path_gap(1, 0), "at least 1")
  expect_match(sdpwts_cheatsheet(), "-log det F(x)", fixed = TRUE)
})

test_that("the barrier method recovers lambda_min as an SDP", {
  A <- matrix(c(4, 1, 0.5, 1, 3, 0.2, 0.5, 0.2, 2), 3)
  r <- sdpwts_min_eigenvalue_sdp(A, tol = 1e-9)
  expect_equal(r$t, min(eigen(A, symmetric = TRUE)$values), tolerance = 1e-7)
  s <- morie_sdpwts(-1, A, list(-diag(3)), min(eigen(A)$values) - 1, tol = 1e-8)
  expect_lte(s$gap, 1e-8)
  expect_error(sdpwts_solve_sdp(-1, A, list(-diag(3)), 10), "STRICTLY feasible")
  expect_error(sdpwts_solve_sdp(-1, A, list(-diag(3)), 0, mu = 1), "mu must exceed 1")
})
