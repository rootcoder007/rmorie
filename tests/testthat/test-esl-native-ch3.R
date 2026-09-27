esl3_data <- function() {
  i <- 1:30
  X <- cbind(sin(i), cos(2 * i) + 0.3 * sin(i), log(i), ((13 * i) %% 11) / 11)
  list(X = X, y = 1 + 2 * X[, 1] - X[, 2] + 0.5 * X[, 3] + 0.3 * cos(5 * i))
}

test_that("OLS, standard errors, z-scores and the F test equal lm and anova", {
  d <- esl3_data()
  Xi <- cbind(1, d$X)
  f <- lm(d$y ~ d$X)
  s <- summary(f)$coefficients
  o <- morie_esl_ols_normal_equations(Xi, d$y)
  expect_equal(o$beta, unname(coef(f)), tolerance = 1e-12)
  expect_equal(o$se, unname(s[, 2]), tolerance = 1e-12)
  z <- morie_esl_z_score(Xi, d$y, o$beta)
  expect_equal(z$z, unname(s[, 3]), tolerance = 1e-12)
  expect_equal(z$p_t, unname(s[, 4]), tolerance = 1e-10)
  a <- anova(lm(d$y ~ d$X[, 1:2]), f)
  ft <- morie_esl_f_test(1:2, 1:4, d$X, d$y)
  expect_equal(c(ft$statistic, ft$p_value), c(a$F[2], a$`Pr(>F)`[2]), tolerance = 1e-12)
  expect_equal(ft$statistic, 66.262923419164778, tolerance = 1e-12)
})

test_that("ridge equals the closed form and its hat-matrix trace", {
  d <- esl3_data()
  Xi <- cbind(1, d$X)
  r <- morie_esl_ridge(Xi, d$y, 2)
  P <- diag(c(0, 1, 1, 1, 1))
  expect_equal(r$beta, drop(solve(crossprod(Xi) + 2 * P, crossprod(Xi, d$y))), tolerance = 1e-12)
  sv <- svd(scale(d$X, scale = FALSE))$d
  expect_equal(r$effective_df, 1 + sum(sv^2 / (sv^2 + 2)), tolerance = 1e-12)
})

test_that("lasso equals glmnet with lambda / n", {
  skip_if_not_installed("glmnet")
  d <- esl3_data()
  r <- morie_esl_lasso(cbind(1, d$X), d$y, 1.5)
  expect_equal(r$beta, c(1.20771080274177578, 1.78698515947479208, -0.87307341356985324,
                         0.42033601326284487, 0), tolerance = 1e-10)
  expect_equal(r$active_set, 1:4)
})

test_that("PCR equals pls::pcr and effective dof is the trace", {
  d <- esl3_data()
  r <- morie_esl_pcr(d$X, d$y, 2)
  expect_equal(c(r$beta, r$intercept), c(0.300266243113946452, 0.366667070497064096, 0.021281516408693080,
                                         0.025330623817156995, 2.244155835906887386), tolerance = 1e-12)
  X <- cbind(1, 0:3)
  e <- morie_esl_effective_dof(X %*% solve(crossprod(X), t(X)))
  expect_equal(e$estimate, 2, tolerance = 1e-12)
  expect_true(e$is_projection)
})
