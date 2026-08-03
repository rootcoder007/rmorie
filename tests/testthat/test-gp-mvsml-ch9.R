# MVSML chapter 9: SVM.  Example 9.1 p.350 is the anchor.

EX_X <- matrix(c(0.5,1, -0.5,1, -0.5,-1), nrow = 3, byrow = TRUE)
EX_Y <- c(-1, -1, 1)

test_that("Example 9.1 label matrix and Gram match the book", {
  Q <- morie_svm_label_matrix(EX_X, EX_Y)
  expect_equal(Q, matrix(c(-0.5,-1, 0.5,-1, -0.5,-1), nrow = 3,
                         byrow = TRUE), tolerance = 1e-12)
  expect_equal(Q %*% t(Q),
               matrix(c(1.25,0.75,1.25, 0.75,1.25,0.75,
                        1.25,0.75,1.25), nrow = 3, byrow = TRUE),
               tolerance = 1e-12)                      # p.350
})

test_that("eq (9.5) decision values and (9.28) weights", {
  f <- morie_svm_decision(EX_X, 0, c(1, -1))
  expect_equal(f, c(-0.5, -1.5, 0.5), tolerance = 1e-12)
  expect_true(all(EX_Y * f > 0))                       # p.341
  alpha <- c(0, 2, 2)
  b <- morie_svm_beta(alpha, EX_X, EX_Y)
  expect_equal(b, as.numeric(t(EX_X) %*% (alpha * EX_Y)),
               tolerance = 1e-12)                      # eq 9.28
})

test_that("eq (9.32) objective matches the hand computation", {
  alpha <- c(1, 1, 2)
  Q <- morie_svm_label_matrix(EX_X, EX_Y)
  hand <- sum(alpha) - 0.5 * sum(outer(alpha, alpha) * (Q %*% t(Q)))
  expect_equal(morie_svm_dual_objective(alpha, EX_X, EX_Y),
               hand, tolerance = 1e-12)
  # eq (9.29): this alpha is balanced
  expect_equal(sum(alpha * EX_Y), 0, tolerance = 1e-12)
})

test_that("the fitted dual separates the data and obeys the KKT", {
  r <- morie_svm_fit_dual(EX_X, EX_Y)
  expect_true(all(r$alpha >= -1e-8))                   # eq 9.33
  expect_equal(sum(r$alpha * EX_Y), 0, tolerance = 1e-6)
  pred <- sign(morie_svm_decision(EX_X, r$beta0, r$beta))
  expect_equal(pred, EX_Y)
  # support vectors sit on the margin y_i f(x_i) = 1 (p.348)
  f <- morie_svm_decision(EX_X, r$beta0, r$beta)
  for (i in r$support_vectors) {
    expect_lt(abs(EX_Y[i] * f[i] - 1), 1e-2)
  }
})

test_that("the dual depends on the data only through inner products", {
  alpha <- c(1, 1, 2)
  G <- EX_X %*% t(EX_X)
  expect_equal(morie_svm_dual_objective(alpha, EX_X, EX_Y),
               morie_svm_dual_objective(alpha, EX_X, EX_Y, G),
               tolerance = 1e-12)                      # p.349
})
