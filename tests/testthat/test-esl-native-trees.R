esl10_data <- function() {
  i <- 1:60
  X <- cbind(((7 * i) %% 59) / 59 * 3, ((11 * i) %% 61) / 61 * 2)
  list(i = i, X = X, y = sin(2 * X[, 1]) + (X[, 2] > 1.1) + 0.1 * cos(9 * i), Q = rbind(c(0.5, 0.3), c(2.5, 1.8), c(1.4, 1.5)))
}

test_that("the regression tree reproduces rpart (anova, cp = 0)", {
  skip_if_not_installed("rpart")
  d <- esl10_data()
  m <- morie_esl_decision_tree(d$X, d$y, max_depth = 2, min_leaf = 5)
  df <- data.frame(x1 = d$X[, 1], x2 = d$X[, 2], y = d$y)
  f <- rpart::rpart(y ~ ., data = df, method = "anova",
                    control = rpart::rpart.control(cp = 0, minsplit = 10, minbucket = 5, maxdepth = 2, xval = 0))
  expect_equal(m$fitted, unname(predict(f)), tolerance = 1e-12)
  expect_equal(morie_esl_tree_predict(m, d$Q), unname(predict(f, data.frame(x1 = d$Q[, 1], x2 = d$Q[, 2]))),
               tolerance = 1e-12)
})

test_that("gradient boosting with stumps equals gbm without subsampling", {
  skip_if_not_installed("gbm")
  d <- esl10_data()
  m <- morie_esl_gbm(d$X, d$y, M = 20, nu = 0.1, max_depth = 1, min_leaf = 5)
  expect_equal(morie_esl_gbm_predict(m, d$Q), c(0.673313781462843, 0.206052062699831, 1.136199473261762),
               tolerance = 1e-12)
})

test_that("AdaBoost.M1 leaves each stump at weighted error one half", {
  d <- esl10_data()
  yc <- ifelse(d$y > 0.5, 1, -1)
  a <- morie_esl_adaboost(d$X, yc, M = 5)
  w <- rep(1 / 60, 60)
  for (k in seq_along(a$stumps)) {
    s <- a$stumps[[k]]
    pred <- ifelse(d$X[, s$feature] <= s$threshold, s$sign, -s$sign)
    err <- sum(w[pred != yc])
    expect_equal(a$alphas[k], log((1 - err) / err), tolerance = 1e-12)
    w <- w * exp(a$alphas[k] * (pred != yc))
    w <- w / sum(w)
    expect_equal(sum(w[pred != yc]), 0.5, tolerance = 1e-12)
  }
  expect_equal(morie_esl_adaboost_predict(a, d$X), a$prediction)
})
