esl12_data <- function() {
  X <- cbind(sin(1:40), cos(3 * (1:40)) + ((1:40) %% 3) * 0.5)
  list(X = X, y = ifelse(sin(2 * (1:40)) + X[, 1] > 0.2, 1, -1))
}

test_that("the SMO reaches libsvm's optimum for the linear and kernel classifiers", {
  d <- esl12_data()
  r <- morie_esl_svc(d$X, d$y, C = 1, tol = 1e-10)
  expect_equal(c(r$w, r$b), c(1.213774815839871, 0.123255850258580, -0.218576046711221), tolerance = 1e-6)
  obj <- function(w, b) 0.5 * sum(w^2) + sum(pmax(0, 1 - d$y * (d$X %*% w + b)))
  expect_lt(obj(r$w, r$b), 28.0982623 + 1e-6)
  k <- morie_esl_svm_kernel(d$X, d$y, C = 1, kernel = "rbf", gamma = 0.7, tol = 1e-10)
  expect_equal(k$b, -0.398046272259155, tolerance = 1e-6)
})

test_that("support-vector regression equals e1071 eps-regression", {
  X <- cbind(sin(1:40), cos(3 * (1:40)))
  y <- 2 * X[, 1] - X[, 2] + 0.3 * cos(7 * (1:40))
  r <- morie_esl_svr(X, y, C = 1, epsilon = 0.2, kernel = "rbf", gamma = 0.7, tol = 1e-10,
                     newdata = rbind(c(0.2, 0.5), c(-0.5, 0.9)))
  expect_equal(c(r$b, r$prediction), c(0.0802678275040895, -0.2376537861308012, -1.8255432832230458), tolerance = 1e-6)
  expect_equal(r$n_support, 20)
  inside <- abs(y - r$fitted) < 0.2 - 1e-6
  expect_true(all(abs(r$coef[inside]) < 1e-9))
})
