test_that("AUC, ridge, Poisson and k-means recompute", {
  k <- 0:29
  y <- as.integer(sin(3.1 * k) > 0)
  s <- round(cos(1.3 * k) + 0.5 * y, 1)
  pos <- s[y == 1]
  neg <- s[y == 0]
  expect_equal(aurroc(y, s)$value, mean(outer(pos, neg, ">") + 0.5 * outer(pos, neg, "==")))
  X <- cbind(sin(0:14), cos(0.7 * (0:14)))
  yy <- 1 + 2 * X[, 1] - X[, 2] + 0.1 * sin(3 * (0:14))
  Xc <- scale(X, scale = FALSE)
  b <- solve(crossprod(Xc) + 0.7 * diag(2), crossprod(Xc, yy - mean(yy)))
  expect_equal(rdgr(X, yy, alpha = 0.7)$coef, as.vector(b), tolerance = 1e-12)
  Xp <- cbind((0:19) / 5, cos(0:19))
  yp <- floor(abs(sin(1.7 * (0:19))) * 4 + (0:19) / 5)
  g <- glmpoi(Xp, yp)
  ref <- stats::glm(yp ~ Xp, family = stats::poisson(), control = stats::glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(g$coef, unname(stats::coef(ref)), tolerance = 1e-9)
  expect_equal(g$se, unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-7)
  expect_equal(g$aic, stats::AIC(ref), tolerance = 1e-9)
  P <- cbind(cos(0:23) + 4 * ((0:23) %% 3), sin(0:23) + 3 * ((0:23) %% 3 == 1))
  r <- kmeans2(P, 3, n_init = 5, random_state = 7)
  expect_equal(r$inertia, sum((P - r$centroids[r$labels + 1, ])^2), tolerance = 1e-12)
})
