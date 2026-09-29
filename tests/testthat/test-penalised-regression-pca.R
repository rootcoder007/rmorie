X <- cbind(1:7, c(0.5, -1, 0.2, 1.5, -0.3, 0.8, -1.1), c(2, 0.1, -1.2, 0.3, 1.1, -0.4, 0.9))
Y <- c(1.1, 2.3, 2.8, 4.4, 4.9, 6.2, 6.8)

test_that("Elnetr and Lasr satisfy the KKT conditions", {
  for (a in list(c(0.1, 0.5), c(0.5, 0.3), c(2, 0.9), c(0.2, 1))) {
    r <- Elnetr(X, Y, alpha = a[1], l1_ratio = a[2])
    res <- Y - r$intercept - drop(X %*% r$coef)
    expect_lt(abs(sum(res)), 1e-10)
    g <- drop(crossprod(X, res)) / length(Y) - a[1] * (1 - a[2]) * r$coef
    lam <- a[1] * a[2]
    act <- r$coef != 0
    expect_equal(g[act], lam * sign(r$coef[act]), tolerance = 1e-6)
    expect_true(all(abs(g[!act]) <= lam + 1e-6))
  }
  expect_equal(Lasr(X, Y, alpha = 0.2)$coef, Elnetr(X, Y, alpha = 0.2, l1_ratio = 1)$coef)
})

test_that("Lasr with one predictor is the soft threshold; ridge end is closed form", {
  x <- X[, 1]
  n <- length(Y)
  sxy <- sum((x - mean(x)) * (Y - mean(Y)))
  b <- sign(sxy) * max(abs(sxy) - n * 0.2, 0) / sum((x - mean(x))^2)
  expect_equal(Lasr(cbind(x), Y, alpha = 0.2)$coef, b, tolerance = 1e-12)
  Xc <- scale(X, scale = FALSE)
  br <- drop(solve(crossprod(Xc) + n * 0.3 * diag(3), crossprod(Xc, Y - mean(Y))))
  expect_equal(Elnetr(X, Y, alpha = 0.3, l1_ratio = 0)$coef, br, tolerance = 1e-7)
})

test_that("Pcaprx matches prcomp on the correlation matrix", {
  Z <- X[1:6, ]
  r <- Pcaprx(Z)
  ref <- stats::prcomp(Z, scale. = TRUE)
  expect_equal(r$explained_variance, ref$sdev^2, tolerance = 1e-12)
  V <- ref$rotation
  for (c in 1:3) if (V[which.max(abs(V[, c])), c] < 0) V[, c] <- -V[, c]
  expect_equal(unname(t(r$components)), unname(V), tolerance = 1e-10)
  expect_equal(unname(r$scores), unname(scale(Z) %*% V), tolerance = 1e-10)
  expect_equal(sum(r$explained_variance_ratio), 1)
})
