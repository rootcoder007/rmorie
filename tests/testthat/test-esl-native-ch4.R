esl4_data <- function() {
  i <- 1:40
  X <- cbind(sin(i) + 0.1 * i / 10, cos(3 * i) + (i %% 3) * 0.8)
  list(i = i, X = X, yb = as.integer(sin(3 * i) + X[, 1] + 0.4 * X[, 2] > 0.5), Q = rbind(c(0.2, 0.5), c(-0.5, 1.9)))
}

test_that("LDA and QDA discriminants equal the formulas and MASS predictions", {
  d <- esl4_data()
  g <- d$i %% 3
  a <- morie_esl_lda_disc(d$X, g, d$Q)
  expect_equal(as.numeric(t(a$discriminants)), c(-1.1710168903230160, -0.85303945435103545, -2.2368809300709271,
                                                 -1.6901568327291563, 1.10915113996086401, 2.2635147568254093),
               tolerance = 1e-12)
  skip_if_not_installed("MASS")
  expect_equal(a$prediction, as.numeric(as.character(predict(MASS::lda(d$X, g), d$Q)$class)))
  b <- morie_esl_qda(d$X, g, d$Q)
  expect_equal(as.numeric(t(b$discriminants)), c(-0.90784632592712999, -0.20829140007268876, -2.10592636219517715,
                                                 -6.85752565013955362, -1.99902648735511757, -0.95671668016566891),
               tolerance = 1e-12)
  expect_equal(b$prediction, as.numeric(as.character(predict(MASS::qda(d$X, g), d$Q)$class)))
})

test_that("multi-output and indicator regression equal lm", {
  d <- esl4_data()
  Y <- cbind(d$X[, 1] + 0.5 * d$X[, 2] + 0.2 * cos(5 * d$i), d$X[, 2] - d$X[, 1] + 0.3 * sin(7 * d$i))
  f <- lm(Y ~ d$X)
  m <- morie_esl_multi_output_ls(d$X, Y)
  expect_equal(m$coefficients, unname(coef(f)), tolerance = 1e-12)
  expect_equal(m$residual_covariance, unname(crossprod(resid(f)) / 37), tolerance = 1e-12)
  g <- (d$i * 7) %% 3
  r <- morie_esl_indicator_regression(d$X, g, d$Q)
  Yi <- sapply(0:2, function(k) as.numeric(g == k))
  expect_equal(r$fitted, unname(cbind(1, d$Q) %*% coef(lm(Yi ~ d$X))), tolerance = 1e-12)
  expect_equal(r$prediction, c(0, 2))
})

test_that("multinomial logit equals nnet::multinom and L1 logistic equals glmnet", {
  d <- esl4_data()
  g <- (d$i * 7) %% 3
  m <- morie_esl_multinomial_logit(d$X, g)
  expect_true(m$converged)
  expect_equal(m$coefficients, rbind(c(2.9242944380902962, 0.005467889849221046, -3.6695368955215137),
                                     c(2.4176999205822489, -0.191029152082225651, -1.8377786718215035)),
               tolerance = 1e-8)
  expect_equal(m$se[1, ], c(1.1408228105325795, 0.82568338710031663, 1.03359770228320280), tolerance = 1e-8)
  expect_equal(m$loglik, -29.753547780862426, tolerance = 1e-12)
  l <- morie_esl_l1_logistic(d$X, d$yb, 2)
  expect_equal(c(l$intercept, l$beta), c(-0.89013527368866274, 1.53994694644996089, 0.60509690422156026),
               tolerance = 1e-10)
  expect_equal(abs(l$score[l$active_set]), rep(2, 2), tolerance = 1e-10)
})
