# R/logit_separation.R, the lines PR #77 added that no other test reached: the tableau simplex
# refuses a negative right-hand side and reports the iteration cap; the LP it stops on is
# lpSolve's, whose optimum it reaches when allowed to.

test_that("negative b is refused and the iteration cap gives status 4", {
  skip_if_not_installed("lpSolve")
  sx <- get(".lsep_simplex_max", envir = environment(morie_logit_separation))
  A <- rbind(c(1, 1), c(1, 3), c(2, 1))
  b <- c(4, 6, 6)
  cv <- c(3, 2)
  expect_error(sx(cv, A, c(1, -1, 2)), ".lsep_simplex_max needs b >= 0", fixed = TRUE)
  capped <- sx(cv, A, b, max_iter = 1L)
  expect_identical(capped$status, 4L)
  expect_null(capped$solution)
  ref <- lpSolve::lp("max", cv, A, rep("<=", 3), b)
  full <- sx(cv, A, b)
  expect_identical(full$status, 0L)
  expect_equal(full$objval, ref$objval, tolerance = 1e-12)
  expect_equal(full$solution, ref$solution, tolerance = 1e-12)
  expect_gt(full$iterations, 1L)
})
