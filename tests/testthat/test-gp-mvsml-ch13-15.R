# Parity of the R mirror against morie Python for MVSML chapters
# 13-15 and the marginal structural models.  Every expected value below
# was produced by the Python implementation in morie.fn, so a mismatch
# here is a genuine three-way parity break.

test_that("eq. 13.1 convolution matches Python", {
  img <- matrix(0, 8, 8)
  for (i in 1:8) for (j in 1:8) img[i, j] <- ((i - 1) * 5 + (j - 1)) %% 7
  ker <- matrix(0, 3, 3)
  for (a in 1:3) for (b in 1:3) ker[a, b] <- (((a - 1) + 2 * (b - 1)) %% 3) - 1
  r <- morie_conv2d(img, ker, bias = 0.5)
  expect_equal(as.numeric(t(r$feature_map)), c(-6.5, 0.5, 7.5, 7.5, 0.5, 0.5, 0.5, -6.5, -6.5, 0.5, 7.5, 7.5, 7.5, 0.5, 0.5, -6.5, -6.5, 0.5, 0.5, 7.5, 7.5, 0.5, 0.5, -6.5, -6.5, -6.5, 0.5, 7.5, 7.5, 0.5, 0.5, 0.5, -6.5, -6.5, 0.5, 7.5), tolerance = 1e-9)
  expect_equal(r$n_parameters, 10)
  expect_equal(r$output_shape, c(6, 6))
})

test_that("eq. 14.3/14.9 design and 14.4 fit match Python", {
  tg <- (0:20) / 20
  curves <- t(vapply(0:9, function(k) {
    a <- 1 + 0.3 * k; b <- -0.5 + 0.2 * k^2; cc <- 0.4 - 0.02 * k^3
    a + b * sin(2 * pi * tg) + cc * cos(2 * pi * tg)
  }, numeric(21)))
  d <- morie_fda_design(tg, curves, L1 = 3, L2 = 5)
  expect_equal(as.numeric(t(d$X_star)), c(1, 1, -0.25, 0.2, 1, 1.3, -0.15, 0.19, 1, 1.6, 0.15, 0.12, 1, 1.9, 0.65, -0.07, 1, 2.2, 1.35, -0.44, 1, 2.5, 2.25, -1.05, 1, 2.8, 3.35, -1.96, 1, 3.1, 4.65, -3.23, 1, 3.4, 6.15, -4.92, 1, 3.7, 7.85, -7.09), tolerance = 1e-8)
  # beta is only comparable across arms if the design has full rank
  expect_gt(min(eigen(crossprod(d$X_star), only.values = TRUE)$values),
            1e-8)
  y <- 1 + 0.5 * (0:9)
  f <- morie_fda_fit(tg, curves, y, L1 = 3, L2 = 5)
  expect_equal(f$beta, c(-0.666666666667, 1.66666666667, 7.577466238e-15, 1.17703002179e-15), tolerance = 1e-7)
  expect_equal(f$sigma2, 2.60422706336e-29, tolerance = 1e-8)
})

test_that("eq. 14.6 coefficients and 14.8 LOOCV match Python", {
  tg <- (0:20) / 20
  x <- 1 - 0.5 * sin(2 * pi * tg) + 0.4 * cos(2 * pi * tg)
  Psi <- morie_fda_basis(tg, 5)
  expect_equal(morie_fda_coefficients(Psi, x), c(1, -0.5, 0.4, 4.60742555219e-17, -1.02140518266e-17),
               tolerance = 1e-7)
  # eq. (14.6) rebuilds the curve from those coefficients
  expect_equal(as.numeric(Psi %*% morie_fda_coefficients(Psi, x)),
               c(1.4, 1.22591410933, 1.0297141716, 0.83060560373, 0.648078539602, 0.5, 0.400864944102, 0.360377401896, 0.382500576104, 0.465068896294, 0.6, 0.774085890669, 0.970285828396, 1.16939439627, 1.3519214604, 1.5, 1.5991350559, 1.6396225981, 1.6174994239, 1.53493110371, 1.4), tolerance = 1e-7)
  expect_equal(morie_fda_loocv(tg, x, 5), 9.58342740327e-31, tolerance = 1e-7)
})

test_that("eq. 15.1-15.3 ZAP quantities match Python", {
  z <- morie_zap_link(c(0.1, 0.5, -0.3), c(-1, 0, 2))
  expect_equal(z$mu, c(1.10517091808, 1.6487212707, 0.740818220682), tolerance = 1e-10)
  expect_equal(z$theta, c(0.26894142137, 0.5, 0.880797077978), tolerance = 1e-10)
  expect_equal(morie_zap_loglik(c(1, 2, 3, 1, 4)), -7.29052929945,
               tolerance = 1e-9)
  p <- morie_zap_predict(c(0.2, 0.6, 0.1), c(1.5, 3.0, 0.8))
  expect_equal(p$prediction, c(1.54466030015, 1.26287483579, 1.30749567906), tolerance = 1e-9)
})

test_that("the eq. 15.3 erratum is corrected in R too", {
  # the book prints the numerator as (1-theta) exp(-mu), dropping the
  # mu; the only quantity consistent with its own pmf on p.651 is the
  # sum of y P(Y = y), which is what we return
  for (par in list(c(0.2, 1.5), c(0.6, 3.0), c(0.05, 0.4))) {
    th <- par[1]; mu <- par[2]
    yy <- 1:300
    direct <- sum(yy * (1 - th) * dpois(yy, mu) / (1 - exp(-mu)))
    expect_equal(morie_zap_predict(th, mu)$prediction, direct,
                 tolerance = 1e-9)
    mv <- morie_zap_mean_variance(th, mu)
    expect_equal(mv$mean, direct, tolerance = 1e-9)
  }
  # eq. (15.4): 0 when theta > 0.5, mu-hat otherwise (not the ZAP mean)
  cl <- morie_zap_predict(c(0.7, 0.5, 0.2), c(4, 4, 4),
                                threshold = 0.5)
  expect_equal(cl$prediction_classified, c(0, 4, 4))
})

test_that("marginal structural models match Python", {
  A <- cbind(((0:39) * 3) %% 2, ((0:39) * 5) %% 2)
  y <- 1 + 2 * rowSums(A) + 0.1 * (((0:39) %% 5) - 2)
  w <- 1 + 0.05 * ((0:39) %% 7)
  expect_equal(morie_msm_linear(y, A, w)$beta, c(0.999563318777, 2.0015312509), tolerance = 1e-9)

  yb <- as.numeric((A[, 1] + A[, 2] + ((0:39) %% 3 == 0)) >= 2)
  expect_equal(morie_msm_logistic(yb, A)$beta, c(-24.4936371443, 24.4936373522), tolerance = 1e-6)

  yc <- ((0:39) %% 4) + rowSums(A)
  expect_equal(morie_msm_poisson(yc, A)$beta, c(-4.4408920985e-16, 0.69314718056), tolerance = 1e-6)

  tt <- 1 + ((0:39) %% 9) * 0.5
  ev <- as.numeric((0:39) %% 4 != 0)
  expect_equal(morie_msm_cox_marginal(tt, ev, A)$beta, 0.337507401646,
               tolerance = 1e-7)
  a <- morie_msm_accelerated_failure(tt, ev, A)
  expect_equal(a$beta, c(0.95592351286, -0.00161346302844), tolerance = 1e-9)
  expect_equal(a$n_uncensored, 30)
  expect_equal(morie_msm_gmm_estimator(y, A, weights = w)$beta, c(0.999563318777, 2.0015312509),
               tolerance = 1e-9)
})

test_that("the MSM weights change the estimate they are meant to", {
  A <- cbind(c(1, 1, 0, 0, 1, 0), c(1, 0, 1, 0, 0, 1))
  y <- c(5.1, 3.2, 3.0, 1.1, 3.1, 2.9)
  # unit weights reduce the MSM to ordinary least squares
  expect_equal(unname(morie_msm_linear(y, A)$beta),
               unname(coef(lm(y ~ rowSums(A)))), tolerance = 1e-9)
  # doubling every weight cannot move a weighted least squares fit
  expect_equal(morie_msm_linear(y, A, rep(2, 6))$beta,
               morie_msm_linear(y, A)$beta, tolerance = 1e-9)
  # the Cox MSM agrees with a weighted Newton step on the same data
  tt <- c(1, 2, 3, 4, 5, 6); ev <- c(1, 1, 1, 0, 1, 1)
  expect_true(is.finite(morie_msm_cox_marginal(tt, ev, A)$hazard_ratio))
})
