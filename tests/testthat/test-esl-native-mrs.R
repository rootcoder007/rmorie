test_that("MARS forward pass starts like earth and the backward pass minimises GCV", {
  i <- 1:60
  X <- cbind(((7 * i) %% 59) / 59 * 3, ((11 * i) %% 61) / 61 * 2)
  y <- pmax(X[, 1] - 1, 0) + 0.5 * pmax(1.2 - X[, 2], 0) + 0.05 * cos(9 * i)
  r <- morie_esl_mars(X, y, query = rbind(c(0.5, 0.3), c(2.5, 1.8)))
  expect_equal(r$forward_terms[[2]][[1]]$t, 0.864406779661017, tolerance = 1e-14)
  expect_equal(r$forward_terms[[4]][[1]]$t, 1.27868852459016, tolerance = 1e-12)
  M <- length(r$terms) + 2 * (length(r$terms) - 1) / 2
  expect_equal(r$gcv, r$rss / 60 / (1 - M / 60)^2, tolerance = 1e-14)
  expect_equal(r$prediction, c(0.4488790282832523, 1.4919671466355267), tolerance = 1e-10)
  expect_lt(r$gcv, 0.00206728103486534)
})
