test_that("cvbnd's width is B sqrt(log(2/delta)/(2n))", {
  r <- cvbnd(c(0.21, 0.35, 0.28, 0.30, 0.25), n = 400, delta = 0.1, loss_bound = 2)
  w <- 2 * sqrt(log(20) / 800)
  expect_equal(r$bound_width, w, tolerance = 1e-14)
  expect_equal(r$upper_bound, 0.278 + w, tolerance = 1e-14)
  expect_equal(r$cv_se, sd(c(0.21, 0.35, 0.28, 0.30, 0.25)) / sqrt(5), tolerance = 1e-14)
  expect_error(cvbnd(numeric(0), 10), "non-empty")
})
