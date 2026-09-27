test_that("morie_spatial_voting_em_irt matches emIRT::binIRT", {
  skip_if_not_installed("emIRT")
  U <- .morie_random_uniform(600, seed = 3, stream = 0)
  Z <- .morie_random_normal(600, seed = 3, stream = 1)
  V <- matrix(as.numeric(U[1:300] < stats::pnorm(outer(Z[1:25], 1 + Z[101:112]) + Z[201:212][col(matrix(0, 25, 12))])), 25, 12)
  V[U[301:600] < 0.05] <- NA
  st <- morie_spatial_voting_em_irt(V, max_iter = 0L)
  r <- morie_spatial_voting_em_irt(V, max_iter = 20000L, tol = 1e-13, conv = "abs")
  Y <- ifelse(is.na(V), 0, ifelse(V == 1, 1, -1))
  invisible(utils::capture.output(e <- emIRT::binIRT(
    .rc = list(votes = Y),
    .starts = list(alpha = matrix(0, 12, 1), beta = matrix(0, 12, 1), x = st$ideal_points),
    .priors = emIRT::makePriors(25, 12, 1), .D = 1L,
    .control = list(threads = 1, verbose = FALSE, thresh = 1e-13, convtype = 2, maxit = 20000)
  )))
  expect_lt(max(abs(r$ideal_points - e$means$x)), 1e-8)
  expect_lt(max(abs(cbind(r$difficulty, r$discrimination) - e$means$beta)), 1e-8)
})
