# Coverage for the ESL chapter-3 helpers and first-order solvers
# (esl_native_ch3.R, eslfst.R, esllso.R, eslmht.R, enobj.R, acclso.R,
# admmop.R, auglag.R, bfgsop.R): least-squares quantities against lm,
# lasso solvers at their KKT conditions, Holm against p.adjust.

es_X <- cbind(1, c(0.5, 1.2, -0.3, 2.1, 1.7, 0.1, 2.8, 0.9, 1.9, -0.6, 2.4, 1.1),
              c(1.1, 0.4, 0.8, -0.2, 0.6, 1.5, 0.3, -0.7, 0.9, 1.2, -0.4, 0.5))
es_y <- as.numeric(es_X %*% c(1, 2, -1)) +
  c(0.3, -0.2, 0.1, 0.5, -0.4, 0.2, -0.1, 0.6, -0.3, 0.2, -0.5, 0.1)

test_that("ESL residual sum of squares, Var(beta-hat) and standard errors", {
  f <- lm(es_y ~ es_X - 1)
  b <- unname(coef(f))
  r <- morie_esl_residual_sum_squares(es_X, es_y, b)
  expect_equal(r$estimate, sum(resid(f)^2), tolerance = 1e-10)
  expect_equal(r$mean_squared_error, sum(resid(f)^2) / 12, tolerance = 1e-10)
  expect_error(morie_esl_residual_sum_squares(es_X, es_y[-1], b), "rows but y has")
  expect_error(morie_esl_residual_sum_squares(es_X, es_y, b[-1]), "columns but beta")
  v <- morie_esl_var_beta_hat(es_X, 0.5)
  expect_equal(v$covariance, 0.5 * solve(crossprod(es_X)), tolerance = 1e-10)
  expect_error(morie_esl_var_beta_hat(es_X, 0), "positive")
  expect_error(morie_esl_var_beta_hat(cbind(es_X, es_X[, 2]), 1), "singular")
  s <- morie_esl_se_beta(es_X, es_y, b)
  expect_equal(s$se, unname(summary(f)$coefficients[, 2]), tolerance = 1e-10)
  expect_equal(morie_esl_se_beta(es_X, as.numeric(es_X %*% b), b)$se, rep(0, 3))
  expect_error(morie_esl_se_beta(es_X[1:3, ], es_y[1:3], b), "n > p")
})

test_that("Fnested is the nested-model F test", {
  Z <- es_X[, 2:3]
  r <- Fnested(1, c(1, 2), Z, es_y)
  a <- anova(lm(es_y ~ Z[, 1]), lm(es_y ~ Z))
  expect_equal(r$statistic, a$F[2], tolerance = 1e-10)
  expect_equal(r$p_value, a$`Pr(>F)`[2], tolerance = 1e-10)
  r0 <- Fnested(integer(0), 2, Z, es_y)
  expect_equal(r0$statistic, anova(lm(es_y ~ 1), lm(es_y ~ Z[, 2]))$F[2], tolerance = 1e-10)
  expect_error(Fnested(3, 1, Z, es_y), "nested")
  expect_error(Fnested(1, 1, Z, es_y), "p1 > p0")
})

test_that("Esllso, Fistalasso and Admmlasso solve the same lasso", {
  lam <- 1.5
  kkt <- function(b, pen) {
    g <- as.numeric(crossprod(es_X, es_y - es_X %*% b))
    nz <- b != 0
    c(max(abs(g[nz] - pen[nz] * sign(b[nz])), 0), max(abs(g[!nz]) - pen[!nz], 0))
  }
  e <- Esllso(es_X, es_y, lam)
  expect_true(e$converged)
  expect_lt(max(kkt(e$beta, c(0, lam, lam))), 1e-9)
  expect_equal(e$objective, 0.5 * sum((es_y - es_X %*% e$beta)^2) + lam * sum(abs(e$beta[2:3])),
               tolerance = 1e-12)
  big <- Esllso(es_X, es_y, 1e4)
  expect_equal(big$beta[2:3], c(0, 0))
  expect_equal(big$beta[1], mean(es_y), tolerance = 1e-10)
  expect_error(Esllso(es_X, es_y[-1], 1), "rows but y has")
  expect_error(Esllso(es_X, es_y, -1), "non-negative")
  expect_error(Esllso(cbind(es_X, 0), es_y, 1), "all-zero column")
  Zc <- es_X[, 2:3]
  ref <- Esllso(Zc, es_y, lam)
  fi <- Fistalasso(Zc, es_y, lam, steps = 20000)
  expect_equal(fi$beta, ref$beta, tolerance = 1e-8)
  expect_equal(fi$lipschitz, max(eigen(crossprod(Zc))$values), tolerance = 1e-9)
  expect_equal(fi$objective, 0.5 * sum((es_y - Zc %*% fi$beta)^2) + lam * sum(abs(fi$beta)),
               tolerance = 1e-12)
  expect_error(Fistalasso(Zc, es_y[-1], lam), "one row")
  expect_error(Fistalasso(Zc, es_y, -1), "non-negative")
  expect_error(Fistalasso(Zc, es_y, 1, steps = -1), "non-negative")
  ad <- Admmlasso(Zc, es_y, lam, rho = 2, steps = 5000)
  expect_equal(ad$z, ref$beta, tolerance = 1e-8)
  expect_lt(ad$primalres, 1e-8)
  expect_error(Admmlasso(Zc, es_y[-1], lam), "one row")
  expect_error(Admmlasso(Zc, es_y, -1), "non-negative")
  expect_error(Admmlasso(Zc, es_y, 1, rho = 0), "strictly positive")
})

test_that("Holmadj matches p.adjust(method = 'holm')", {
  p <- c(0.01, 0.04, 0.03, 0.2, 0.001, 0.049)
  r <- Holmadj(p, alpha = 0.05)
  expect_equal(r$p_adjusted, p.adjust(p, "holm"), tolerance = 1e-12)
  expect_equal(r$reject, p.adjust(p, "holm") <= 0.05)
  expect_equal(r$n_reject, sum(p.adjust(p, "holm") <= 0.05))
  expect_error(Holmadj(numeric(0)), "empty")
})

test_that("Enetobj is the elastic-net penalised RSS", {
  b <- c(0.5, 1.8, -0.9)
  r <- Enetobj(es_X[, 2:3], es_y, b, lam = 0.7, alpha = 0.3)
  rss <- sum((es_y - es_X %*% b)^2)
  pen <- 0.7 * (0.5 * 0.7 * sum(b[2:3]^2) + 0.3 * sum(abs(b[2:3])))
  expect_equal(r$prss, rss + pen, tolerance = 1e-12)
  r0 <- Enetobj(es_X, es_y, b, lam = 1, alpha = 1, add_intercept = FALSE)
  expect_equal(r0$l1, sum(abs(b)), tolerance = 1e-12)
  expect_error(Enetobj(es_X[, 2:3], es_y[-1], b, 1, 0.5), "one row")
  expect_error(Enetobj(es_X[, 2:3], es_y, b[-1], 1, 0.5), "one entry per column")
  expect_error(Enetobj(es_X[, 2:3], es_y, b, -1, 0.5), "non-negative")
  expect_error(Enetobj(es_X[, 2:3], es_y, b, 1, 2), "\\[0, 1\\]")
})

test_that("Auglag evaluates the augmented Lagrangian and its multiplier update", {
  f <- function(x) sum(x^2)
  g <- function(x) c(x[1] + x[2] - 1, x[1] - 2 * x[2])
  x <- c(0.3, 0.4)
  r <- Auglag(f, g, x, lam = c(0.5, -0.2), mu = 3)
  gv <- g(x)
  expect_equal(r$value, 0.25 + sum(c(0.5, -0.2) * gv) + 1.5 * sum(gv^2), tolerance = 1e-12)
  expect_equal(r$lambda, c(0.5, -0.2) + 3 * gv, tolerance = 1e-12)
  expect_equal(r$violation, max(abs(gv)), tolerance = 1e-12)
  expect_equal(Auglag(f, g, x)$linear, 0)
  expect_error(Auglag(f, g, x, lam = 1), "one entry per constraint")
  expect_error(Auglag(f, g, x, mu = 0), "strictly positive")
})

test_that("Bfgsupd satisfies the secant equation", {
  H <- matrix(c(2, 0.3, 0.3, 1), 2)
  s <- c(0.4, -0.2)
  y <- c(0.9, 0.1)
  r <- Bfgsupd(H, s, y)
  rho <- 1 / sum(y * s)
  ref <- (diag(2) - rho * s %o% y) %*% H %*% (diag(2) - rho * y %o% s) + rho * s %o% s
  expect_equal(r$M, ref, tolerance = 1e-12)
  expect_equal(as.numeric(r$M %*% y), s, tolerance = 1e-12)
  d <- Bfgsupd(H, s, y, inverse = FALSE)
  expect_equal(as.numeric(d$M %*% s), y, tolerance = 1e-12)
  expect_equal(solve(d$M), Bfgsupd(solve(H), s, y)$M, tolerance = 1e-10)
  expect_error(Bfgsupd(H[, 1, drop = FALSE], s, y), "square")
  expect_error(Bfgsupd(H, s[1], y), "match the dimension")
  expect_error(Bfgsupd(H, s, -y), "curvature")
  expect_error(Bfgsupd(-H, s, y, inverse = FALSE), "strictly positive")
})
