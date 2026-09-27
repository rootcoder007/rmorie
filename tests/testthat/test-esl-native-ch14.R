esl14_X <- function() {
  i <- 1:40
  cbind(sin(i) + 0.3 * cos(3 * i), cos(2 * i) + 0.5 * sin(i), sin(3 * i) - 0.2 * sin(i), cos(i) + 0.4 * sin(2 * i),
        0.6 * sin(i) + 0.2 * cos(5 * i))
}

test_that("PCA equals prcomp and factor analysis the tight factanal solution", {
  X <- esl14_X()
  m <- morie_esl_pca_svd(X, 3)
  p <- prcomp(X)
  expect_equal(m$eigenvalues, p$sdev[1:3]^2, tolerance = 1e-12)
  expect_equal(abs(m$components), abs(t(p$rotation[, 1:3])), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(morie_esl_pca_transform(m, X), m$scores, tolerance = 1e-12)
  f <- factor_analysis_ml(X, 1, tol = 1e-14, max_iter = 1e5)
  u <- 1 - f$communalities / diag(cov(X))
  ref <- factanal(X, 1, rotation = "none", control = list(opt = list(factr = 1, pgtol = 0, maxit = 1e4), lower = 1e-8))
  expect_equal(u, unname(ref$uniquenesses), tolerance = 1e-8)
})

test_that("KL NMF never increases the divergence and reaches a stationary point", {
  X <- outer(1:8, 1:6, function(i, j) abs(sin(i * j / 3)) * 3 + (i + j) %% 3)
  r <- morie_esl_nmf(X, 2, loss = "kl", max_iter = 5000, tol = 1e-14)
  expect_true(all(diff(r$divergence_path) <= 1e-12))
  Rm <- X / (r$W %*% r$H)
  expect_lt(max(abs(crossprod(r$W, Rm) - crossprod(r$W, matrix(1, 8, 6)))), 1e-4)
  expect_equal(r$loglik + sum(X - ifelse(X > 0, X * log(X), 0)), -r$kl_divergence, tolerance = 1e-10)
})
