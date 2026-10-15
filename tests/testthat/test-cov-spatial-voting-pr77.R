# R/spatial_voting.R, the lines PR #77 added that no other test reached: the blackbox
# pseudo-inverse of a matrix whose eigenvalues all fall under the 1e-4 cut, and the refusal of an
# issue no scaled respondent answered.

test_that("the eigen pseudo-inverse drops every eigenvalue at or under 1e-4", {
  pinv <- get(".sv_bb_pinv_sym", envir = environment(morie_spatial_voting_blackbox))
  expect_identical(pinv(diag(c(5e-5, -1e-5))), matrix(0, 2, 2))
  Q <- qr.Q(qr(matrix(c(2, 1, 1, 3), 2)))
  A <- Q %*% diag(c(4, 1e-6)) %*% t(Q)
  expect_equal(pinv(A), Q[, 1] %*% t(Q[, 1]) / 4, tolerance = 1e-10)
})

test_that("an issue without a response among the scaled respondents is refused", {
  set.seed(1)
  X <- matrix(stats::rnorm(30 * 9), 30, 9)
  X[, 9] <- NA
  X[1, 9] <- 1
  X[1, 1:7] <- NA  # the only respondent on issue 9 answers too few issues to be scaled
  expect_error(morie_spatial_voting_blackbox(X, n_dims = 2, minscale = 8),
               "Every issue needs at least one response among the scaled respondents.", fixed = TRUE)
})
