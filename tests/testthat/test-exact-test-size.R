test_that("Exactsize returns the largest attainable size not above alpha", {
  pmf <- choose(5, 0:5) / 32
  r <- Exactsize(pmf, alpha = 0.2)
  expect_equal(r$sizes, rev(cumsum(rev(pmf))), tolerance = 1e-15)
  expect_equal(r$alpha_exact, 6 / 32)
  expect_equal(r$cut, 4L)
  lo <- Exactsize(pmf, alpha = 0.2, upper = FALSE)
  expect_equal(lo$alpha_exact, 6 / 32)
  expect_equal(lo$cut, 1L)
  expect_true(is.nan(Exactsize(pmf, alpha = 0.01)$alpha_exact))
})
