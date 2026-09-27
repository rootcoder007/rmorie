test_that("modified regression and graphical lasso equal glasso", {
  skip_if_not_installed("glasso")
  S <- matrix(c(10, 1, 5, 4, 1, 10, 2, 6, 5, 2, 10, 3, 4, 6, 3, 10), 4)
  A <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0), 4)
  g <- morie_esl_ggm_fit(S, A)
  ref <- suppressWarnings(glasso::glasso(S, rho = 0, zero = rbind(c(1, 3), c(2, 4)), thr = 1e-14, maxit = 1e5))
  expect_equal(g$Sigma, ref$w, tolerance = 1e-12)
  expect_equal(g$Theta, ref$wi, tolerance = 1e-12)
  expect_equal(round(g$Sigma[1, 3], 2), 1.31)
  i <- 1:60
  Z <- cbind(sin(i), cos(2 * i) + 0.5 * sin(i), sin(3 * i), cos(i) - 0.3 * cos(2 * i))
  Sd <- cov(Z) * 59 / 60
  gl <- morie_esl_graphical_lasso(Sd, 0.1)
  expect_equal(gl$Theta, glasso::glasso(Sd, rho = 0.1, thr = 1e-14, maxit = 1e5)$wi, tolerance = 1e-10)
  pr <- morie_esl_precision_regression(g$Theta)
  expect_equal(pr$partial_correlation[1, 3], 0)
  expect_equal(pr$coefficients[1, ], -g$Theta[1, ] / g$Theta[1, 1] * c(0, 1, 1, 1), tolerance = 1e-14)
})

test_that("Ising MLE equals the Poisson log-linear fit and EM equals norm::em.norm", {
  t <- 1:200
  Xb <- cbind(as.integer(sin(t) > 0), as.integer(sin(t) + cos(3 * t) > 0.2), as.integer(cos(2 * t) > 0),
              as.integer(sin(5 * t) + sin(t) > 0))
  f <- morie_esl_ising_fit(Xb, rbind(c(1, 2), c(2, 3), c(3, 4), c(1, 4)))
  st <- expand.grid(x1 = 0:1, x2 = 0:1, x3 = 0:1, x4 = 0:1)
  st$count <- as.numeric(table(factor(Xb %*% c(1, 2, 4, 8), levels = 0:15)))
  pg <- glm(count ~ x1 + x2 + x3 + x4 + x1:x2 + x2:x3 + x3:x4 + x1:x4, family = poisson, data = st,
            control = glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(c(f$main, f$edges[, 3]), unname(coef(pg)[-1]), tolerance = 1e-10)
  skip_if_not_installed("norm")
  i <- 1:60
  Z <- cbind(sin(i), cos(2 * i) + 0.5 * sin(i), sin(3 * i))
  Z[i %% 5 == 0, 2] <- NA
  Z[i %% 7 == 0, 3] <- NA
  e <- morie_esl_mvn_em_missing(Z)
  s <- norm::prelim.norm(Z)
  pr <- norm::getparam.norm(s, norm::em.norm(s, showits = FALSE, criterion = 1e-14, maxits = 10000))
  expect_equal(e$mean, pr$mu, tolerance = 1e-10)
  expect_equal(e$cov, pr$sigma, tolerance = 1e-10, ignore_attr = TRUE)
})
