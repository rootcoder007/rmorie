# Coverage tests for R/esl_native.R (ESL 2nd ed.): EM for Gaussian
# mixtures, IWLS / logistic regression (glm), cross-validation, sure
# independence screening, MDL, dropout and Haar wavelet shrinkage.

esl_ldmvn <- function(X, mu, S) {
  d <- sweep(X, 2, mu)
  -0.5 * (ncol(X) * log(2 * pi) + log(det(S)) + rowSums((d %*% solve(S)) * d))
}

esl_mixdat <- function() {
  i <- 1:40
  cbind(c(sin(i[1:20]) / 3, 3 + cos(i[21:40]) / 3), c(cos(2 * i[1:20]) / 3, 2 + sin(3 * i[21:40]) / 3))
}

test_that("EM mixture: returned parameters reproduce the log-likelihood and responsibilities", {
  X <- esl_mixdat()
  f <- morie_esl_em_gmm(X, k = 2, tol = 1e-10, seed = 1)
  comp <- sapply(1:2, function(j) log(f$pi[j]) + esl_ldmvn(X, f$mu[j, ], f$sigma[, , j]))
  lse <- apply(comp, 1, function(r) max(r) + log(sum(exp(r - max(r)))))
  expect_equal(f$loglik, sum(lse), tolerance = 1e-9)
  expect_equal(f$resp, exp(comp - lse), tolerance = 1e-9)
  expect_true(all(diff(f$loglik_path) > -1e-9))
  npar <- 1 + 4 + 6
  expect_equal(f$aic, 2 * npar - 2 * f$loglik, tolerance = 1e-12)
  expect_equal(f$bic, npar * log(40) - 2 * f$loglik, tolerance = 1e-12)
  expect_equal(sort(tabulate(f$labels + 1, 2)), c(20L, 20L))
  expect_error(morie_esl_em_gmm(X, k = 41), "exceeds")
  gm <- morie_esl_gaussian_mixture(X, k = 2, newdata = rbind(c(0, 0), c(3, 2)), tol = 1e-10, seed = 1)
  Z <- rbind(c(0, 0), c(3, 2))
  dens <- exp(esl_ldmvn(Z, f$mu[1, ], f$sigma[, , 1])) * f$pi[1] + exp(esl_ldmvn(Z, f$mu[2, ], f$sigma[, , 2])) * f$pi[2]
  expect_equal(gm$density, dens, tolerance = 1e-9)
  expect_error(morie_esl_gaussian_mixture(X, newdata = matrix(0, 1, 3)), "3 columns")
})

test_that("IWLS equals glm for binomial and Poisson", {
  x <- cbind(c(-1.2, -0.5, 0.3, 0.8, 1.5, -0.9, 0.1, 2.2, -1.8, 0.6))
  yb <- c(0, 0, 1, 0, 1, 1, 0, 1, 0, 1)
  ctl <- glm.control(epsilon = 1e-14, maxit = 100)
  g <- glm(yb ~ x, family = binomial(), control = ctl)
  r <- morie_esl_iwls(x, yb, tol = 1e-12)
  expect_equal(r$beta, unname(coef(g)), tolerance = 1e-9)
  expect_equal(r$se, unname(summary(g)$coefficients[, 2]), tolerance = 1e-7)
  expect_equal(r$loglik, as.numeric(logLik(g)), tolerance = 1e-9)
  yp <- c(0, 1, 2, 1, 4, 0, 2, 7, 0, 3)
  gp <- glm(yp ~ x, family = poisson(), control = ctl)
  rp <- morie_esl_iwls(x, yp, family = "poisson", tol = 1e-12)
  expect_equal(rp$beta, unname(coef(gp)), tolerance = 1e-9)
  expect_equal(rp$loglik, as.numeric(logLik(gp)), tolerance = 1e-9)
  expect_true(morie_esl_iwls(x, as.numeric(x > 0))$separated)
  expect_error(morie_esl_iwls(x, yp, family = "gamma"), "family must be")
  expect_error(morie_esl_iwls(x, yb + 1), "0/1")
  lr <- morie_esl_logistic_reg(x, yb, tol = 1e-12)
  p <- plogis(cbind(1, x) %*% r$beta)
  expect_equal(lr$prob, as.numeric(p), tolerance = 1e-12)
  cls <- as.integer(p >= 0.5)
  expect_equal(lr$accuracy, mean(cls == yb))
  expect_equal(lr$confusion, matrix(c(sum(yb == 0 & cls == 0), sum(yb == 0 & cls == 1), sum(yb == 1 & cls == 0), sum(yb == 1 & cls == 1)), 2, byrow = TRUE))
  expect_equal(lr$odds_ratio, exp(r$beta), tolerance = 1e-12)
  expect_error(morie_esl_logistic_reg(x, yb, threshold = 1), "threshold")
})

test_that("k-fold cross-validation reuses the returned folds", {
  x <- cbind(1:12 / 3, sin(1:12))
  y <- 1 + 2 * x[, 1] - x[, 2] + cos(1:12) / 4
  cv <- morie_esl_cv_score(x, y, k = 4, seed = 3)
  pred <- numeric(12)
  for (j in 0:3) {
    te <- cv$fold_id == j
    b <- coef(lm(y[!te] ~ x[!te, ]))
    pred[te] <- cbind(1, x[te, , drop = FALSE]) %*% b
  }
  expect_equal(cv$predictions, pred, tolerance = 1e-10)
  expect_equal(cv$cv, mean((y - pred)^2), tolerance = 1e-10)
  expect_equal(as.numeric(table(cv$fold_id)), rep(3, 4))
  mae <- morie_esl_cv_score(x, y, k = 4, loss = "mae", seed = 3)
  expect_equal(mae$cv, mean(abs(y - pred)), tolerance = 1e-10)
  yc <- rep(0:1, 6)
  st <- morie_esl_cv_score(x, yc, model = function(a, b, z) rep(1, nrow(z)), k = 3, loss = "01", stratify = TRUE)
  expect_equal(st$cv, 0.5)
  expect_equal(as.numeric(table(st$fold_id, yc)), rep(2, 6))
  expect_error(morie_esl_cv_score(x, y, k = 1), "between 2 and n")
  expect_error(morie_esl_cv_score(x, y, loss = "huber"), "loss must be")
})

test_that("sure independence screening ranks by absolute marginal correlation", {
  X <- cbind(sin(1:15), 1:15, cos(1:15), (1:15)^2 / 10)
  y <- 0.5 * (1:15) + sin(1:15)
  s <- morie_esl_sis_screening(X, y, d = 2)
  om <- abs(cor(X, y))[, 1]
  expect_equal(s$omega, om, tolerance = 1e-12)
  expect_equal(s$selected, order(-om)[1:2])
  expect_equal(s$dropped, order(-om)[3:4])
  expect_error(morie_esl_sis_screening(X, y, d = 5), "between 1 and p")
})

test_that("minimum description length, dropout and wavelet shrinkage", {
  m <- morie_esl_mdl(-120, 4, n = 50)
  expect_equal(m$mdl, 120 + 2 * log(50), tolerance = 1e-12)
  expect_equal(m$bic, 4 * log(50) + 240, tolerance = 1e-12)
  expect_equal(m$bits, m$mdl / log(2), tolerance = 1e-12)
  th <- c(0.5, -1, 2)
  mp <- morie_esl_mdl(-80, th, prior_sd = 2)
  expect_equal(mp$model_cost, 0.5 * sum(th^2) / 4 + 3 * log(2 * sqrt(2 * pi)), tolerance = 1e-12)
  expect_equal(morie_esl_mdl(-80, 3)$model_cost, 1.5 * log(2 * pi), tolerance = 1e-12)
  expect_error(morie_esl_mdl(-1, th, prior_sd = 0), "positive")
  X <- matrix(1:12, 3)
  d <- morie_esl_dropout(X, p = 0.8, seed = 2)
  expect_equal(d$output, X * d$mask / 0.8)
  expect_true(all(d$mask %in% c(0, 1)))
  expect_equal(morie_esl_dropout(X, training = FALSE)$output, X)
  expect_error(morie_esl_dropout(X, p = 0), "KEEP probability")
  y <- c(4, 2, 5, 7, 1, 3, 8, 6)
  w0 <- morie_esl_wavelet_smooth(y, threshold = 0)
  expect_equal(w0$signal, y, tolerance = 1e-12)
  h1 <- (y[c(1, 3, 5, 7)] - y[c(2, 4, 6, 8)]) / sqrt(2)
  ws <- morie_esl_wavelet_smooth(y, threshold = 1, levels = 1)
  expect_equal(ws$coefficients[[1]], sign(h1) * pmax(abs(h1) - 1, 0), tolerance = 1e-12)
  wh <- morie_esl_wavelet_smooth(y, mode = "hard", threshold = 1.5, levels = 1)
  expect_equal(wh$coefficients[[1]], ifelse(abs(h1) > 1.5, h1, 0), tolerance = 1e-12)
  expect_equal(ws$sigma, median(abs(h1 - median(h1))) / 0.6745, tolerance = 1e-12)
  expect_length(morie_esl_wavelet_smooth(y[1:6], threshold = 0)$signal, 6L)
  expect_equal(morie_esl_wavelet_smooth(y[1:6], threshold = 0)$signal, y[1:6], tolerance = 1e-12)
  expect_error(morie_esl_wavelet_smooth(y, mode = "garrote"), "soft")
})
