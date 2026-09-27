test_that("archetypes satisfy the simplex constraints and match the Python arm", {
  i <- 0:49
  X <- cbind(sin(1.7 * i) * 2 + cos(i), cos(2.3 * i) + 0.5 * sin(0.3 * i * i))
  r <- morie_esl_archetypes(X, 3)
  expect_true(min(r$W) >= 0 && min(r$B) >= 0)
  expect_equal(rowSums(r$W), rep(1, 50), tolerance = 1e-12)
  expect_equal(r$archetypes, r$B %*% X, tolerance = 1e-12)
  expect_equal(r$rss, sum((X - r$W %*% r$archetypes)^2), tolerance = 1e-12)
  expect_true(all(diff(r$rss_path) <= 1e-12))
  expect_equal(r$rss, 5.1809409499909655, tolerance = 1e-9)
  expect_equal(morie_esl_archetypes(X, 1)$archetypes[1, ], colMeans(X), tolerance = 1e-10)
  C <- rbind(c(0, 0), c(4, 0), c(4, 3), c(0, 3), c(1, 1), c(2, 2), c(3, 1))
  expect_lt(morie_esl_archetypes(C, 4)$rss, 1e-24)
})
