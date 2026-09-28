test_that("utility maximisation recomputes", {
  r <- QuadraticUtilityLagrange(c(2, 1), diag(2), matrix(c(1, 1), 1), 1)
  expect_equal(r$x, c(1, 0), tolerance = 1e-12)
  q <- QuadraticUtilityConstrained(c(2, 2), diag(2), rbind(c(1, 0), c(0, 1)), c(1, 3))
  expect_equal(q$x, c(1, 2), tolerance = 1e-12)
  expect_equal(VcgMechanism(rbind(c(5, 0), c(0, 3), c(0, 4)))$payments, c(0, 1, 2))
})
