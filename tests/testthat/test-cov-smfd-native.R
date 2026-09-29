# P-splines (Eilers & Marx 1996): the evenly spaced knot sequence, the
# Cox-de Boor basis against splines::splineDesign, difference penalties
# against diff(), and the penalised fit with its smoother trace.

test_that("knots extend degree segments beyond the data range", {
  k <- smfd_knot_sequence(0, 1, nseg = 4, degree = 3)
  expect_equal(k, seq(-0.75, 1.75, by = 0.25), tolerance = 1e-15)
  expect_error(smfd_knot_sequence(0, 1, 0), "nseg")
  expect_error(smfd_knot_sequence(0, 1, 2, -1), "degree")
  expect_error(smfd_knot_sequence(1, 1), "xmax must exceed")
})

test_that("the B-spline basis matches splineDesign and sums to one", {
  x <- c(0, 0.13, 0.5, 0.77, 0.99)
  for (deg in 0:3) {
    k <- smfd_knot_sequence(0, 1, 5, deg)
    B <- smfd_bspline_basis(x, k, deg)
    ref <- splines::splineDesign(k, x, ord = deg + 1, outer.ok = TRUE)
    expect_equal(B, ref, tolerance = 1e-14)
    expect_equal(rowSums(B), rep(1, 5), tolerance = 1e-14)
  }
  expect_equal(smfd_bspline_one(0.3, 2, 1, c(0, 0.25, 0.5, 0.75)), (0.3 - 0.25) / 0.25, tolerance = 1e-15)
  expect_error(smfd_bspline_basis(x, c(0, 1), 3), "too short")
})

test_that("difference penalties are diff() of the identity", {
  expect_equal(smfd_difference_matrix(6, 0), diag(6))
  for (d in 1:3) expect_equal(smfd_difference_matrix(6, d), diff(diag(6), differences = d))
  expect_error(smfd_difference_matrix(3, 3), "more than 3 coefficients")
  expect_error(smfd_difference_matrix(3, -1), "negative")
})

test_that("the P-spline solves (B'WB + lambda D'D) a = B'Wy and reports tr(H)", {
  set.seed(6)
  x <- sort(stats::runif(40))
  y <- sin(6 * x) + stats::rnorm(40, sd = 0.2)
  w <- stats::runif(40, 0.5, 2)
  f <- morie_smfd(x, y, nseg = 8, lam = 2, weights = w)
  B <- splines::splineDesign(smfd_knot_sequence(min(x), max(x), 8, 3), x, ord = 4, outer.ok = TRUE)
  D <- diff(diag(ncol(B)), differences = 2)
  A <- crossprod(B, w * B) + 2 * crossprod(D)
  a <- solve(A, crossprod(B, w * y))
  expect_equal(f$coefficients, as.numeric(a), tolerance = 1e-10)
  H <- B %*% solve(A) %*% t(B) %*% diag(w)
  expect_equal(f$hat_diagonal, diag(H), tolerance = 1e-10)
  expect_equal(f$effective_dimension, sum(diag(H)), tolerance = 1e-10)
  expect_equal(f$sigma2, f$rss / (40 - sum(diag(H))), tolerance = 1e-10)
  # lambda -> infinity with a second-order penalty leaves the LS line
  lin <- morie_smfd(x, 1 + 2 * x, lam = 1e6)
  expect_equal(lin$fitted, 1 + 2 * x, tolerance = 1e-6)
  expect_error(morie_smfd(x, y[-1]), "same length")
  expect_error(morie_smfd(1, 1), "at least two")
  expect_error(morie_smfd(x, y, lam = -1), "negative")
})
