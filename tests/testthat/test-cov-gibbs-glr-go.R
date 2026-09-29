# Coverage for the Gibbs sampler, GISS anomalies, the Page/GLR CUSUMs, the
# Bayesian GLM, the Kendall trend test, the simplex LP, grand-mean
# centering, GSEA, goal-conditioned values and GO enrichment; expectations
# are recomputed in the test body.

vdc_u <- function(i) {
  k <- i + 1
  f <- 1
  r <- 0
  while (k > 0) {
    f <- f / 2
    r <- r + f * (k %% 2)
    k <- k %/% 2
  }
  r
}

test_that("Gibbsm updates each block with a van der Corput uniform", {
  rho <- 0.6
  cond <- list(function(x, u) rho * x[2] + sqrt(1 - rho^2) * qnorm(u),
               function(x, u) rho * x[1] + sqrt(1 - rho^2) * qnorm(u))
  r <- Gibbsm(cond, c(0.5, -0.5), n_iter = 6, burn = 2)
  x <- c(0.5, -0.5)
  D <- matrix(0, 6, 2)
  cnt <- 0
  for (s in 1:6) {
    for (i in 1:2) {
      x[i] <- cond[[i]](x, vdc_u(cnt + 1))
      cnt <- cnt + 1
    }
    D[s, ] <- x
  }
  expect_equal(r$draws, D, tolerance = 1e-12)
  expect_equal(r$mean, colMeans(D[3:6, ]), tolerance = 1e-12)
  expect_error(Gibbsm(cond, 1), "one conditional per block")
  expect_error(Gibbsm(cond, c(0, 0), n_iter = 3, burn = 3), "burn")
  expect_error(Gibbsm(list(1, 2), c(0, 0)), "callable")
})

test_that("giss and morie_giss_anomaly remove base-period means and fit a trend", {
  T <- rbind(c(14.1, 14.3, 14.2, 14.6, 14.8),
             c(9.0, 9.4, 9.1, 9.3, 9.9))
  yr <- c(1950, 1960, 1970, 1980, 1990)
  r <- giss(T, years = yr, base = c(1955, 1980), dist = c(300, 900), radius = 1200)
  A <- T - rowMeans(T[, 2:4])
  w <- c(0.75, 0.25) / sum(c(0.75, 0.25))
  ser <- colSums(A * w)
  expect_equal(r$anomaly, ser, tolerance = 1e-12)
  expect_equal(r$baseline, sum(w * rowMeans(T[, 2:4])), tolerance = 1e-12)
  expect_equal(r$trend, unname(coef(lm(ser ~ yr))[2]), tolerance = 1e-9)
  expect_equal(r$nbase, 3L)
  r2 <- morie_giss_anomaly(T)
  ser2 <- colMeans(T - rowMeans(T))
  expect_equal(r2$anomaly, ser2, tolerance = 1e-12)
  expect_equal(r2$trend, unname(coef(lm(ser2 ~ I(0:4)))[2]), tolerance = 1e-9)
  r3 <- giss(T[1, ], years = yr, base = c(2000, 2010))
  expect_equal(r3$nbase, 5L)
})

test_that("the Page CUSUM entry points agree with the explicit recursion", {
  x <- c(0, 0, 1, 0, 1, 1, 1, 0, 1, 1)
  a <- log(0.7 / 0.2)
  b <- log(0.3 / 0.8)
  z <- ifelse(x == 1, a, b)
  S <- cumsum(z)
  W <- S - pmin(cummin(S), 0)
  r <- morie_glr_test(x, 0.2, 0.7, threshold = 2)
  expect_equal(r$scores, z, tolerance = 1e-12)
  expect_equal(r$estimate, W, tolerance = 1e-12)
  expect_equal(r$kl, 0.7 * a + 0.3 * b, tolerance = 1e-12)
  expect_equal(r$stop_index, which(W >= 2)[1] - 1L)
  expect_true(r$detected)
  expect_equal(r$expected_delay, 2 / r$kl, tolerance = 1e-12)
  # changepoint = 0-based index of the first observation after the running minimum
  expect_equal(r$changepoint, which.min(c(0, S)[seq_len(which.max(W) + 1)]) - 1L)
  expect_equal(page_cusum(x, 0.2, 0.7)$estimate, W, tolerance = 1e-12)
  expect_equal(glrtest(x, 0.2, 0.7, threshold = 2)$stop_index, r$stop_index)
  g2 <- morie_glrtest(x, 0.2, 0.7, threshold = 2)
  expect_equal(g2$estimate, W, tolerance = 1e-12)
  expect_equal(g2$changepoint, r$changepoint)
  expect_equal(g2$stop_index, r$stop_index)

  xn <- c(0.1, -0.3, 0.2, 1.4, 0.9, 1.3)
  zn <- (1 - 0) * (xn - 0.5) / 0.25
  Sn <- cumsum(zn)
  expect_equal(morie_glr_test(xn, 0, 1, family = "normal", sd = 0.5)$estimate,
               Sn - pmin(cummin(Sn), 0), tolerance = 1e-12)
  expect_equal(morie_glrtest(xn, 0, 1, family = "normal", sd = 0.5)$kl, 1 / (2 * 0.25), tolerance = 1e-12)
  xp <- c(1, 0, 2, 5, 4, 6)
  zp <- xp * log(4 / 1.5) - 2.5
  expect_equal(morie_glr_test(xp, 1.5, 4, family = "poisson")$scores, zp, tolerance = 1e-12)
  expect_equal(morie_glrtest(xp, 1.5, 4, family = "poisson")$scores, zp, tolerance = 1e-12)
  expect_false(morie_glrtest(xp, 1.5, 4, threshold = 1e3, family = "poisson")$detected)

  expect_error(morie_glr_test(x, 0.2, 0.2), "must differ")
  expect_error(morie_glr_test(c(0, 2), 0.2, 0.5), "0 or 1")
  expect_error(morie_glr_test(x, 0.2, 0.5, family = "gamma"), "family")
  expect_error(morie_glrtest(c(1.5), 1, 2, family = "poisson"), "non-negative integers")
  expect_error(morie_glrtest(numeric(0), 1, 2), "at least one")
})

test_that("morie_glmbay_bayesian_glm is the ridge posterior mode with Laplace covariance", {
  X <- cbind(c(0.2, -1, 0.5, 1.3, -0.4, 0.9), c(1, 0, 1, 0, 1, 1))
  y <- c(1.1, -0.8, 0.9, 1.7, 0.1, 1.2)
  g <- morie_glmbay_bayesian_glm(X, y, family = "gaussian", prior_sd = 2)
  Xi <- cbind(1, X)
  A <- crossprod(Xi) + diag(1 / 4, 3)
  b <- solve(A, crossprod(Xi, y))
  expect_equal(g$coefficients, as.numeric(b), tolerance = 1e-9)
  expect_equal(g$posterior_sd, sqrt(diag(solve(A))), tolerance = 1e-9)
  V <- diag(6) + 4 * Xi %*% t(Xi)
  lm_ <- -0.5 * (6 * log(2 * pi) + as.numeric(determinant(V)$modulus) + sum(y * solve(V, y)))
  expect_equal(g$log_marginal, lm_, tolerance = 1e-9)

  yb <- c(1, 0, 1, 1, 0, 1)
  bb <- morie_glmbay_bayesian_glm(X, yb, family = "binomial", prior_sd = 1.5)
  mu <- plogis(as.numeric(Xi %*% bb$coefficients))
  expect_lt(max(abs(crossprod(Xi, yb - mu) - bb$coefficients / 2.25)), 1e-8)
  H <- crossprod(Xi * (mu * (1 - mu)), Xi) + diag(1 / 2.25, 3)
  expect_equal(bb$posterior_sd, sqrt(diag(solve(H))), tolerance = 1e-6)
  expect_equal(bb$loglik, sum(dbinom(yb, 1, mu, log = TRUE)), tolerance = 1e-9)
  expect_true(bb$converged)

  yp <- c(2, 0, 3, 5, 1, 2)
  pp <- morie_glmbay_bayesian_glm(X[, 1], yp, family = "poisson", prior_sd = 3)
  Xp <- cbind(1, X[, 1])
  mp <- exp(as.numeric(Xp %*% pp$coefficients))
  expect_lt(max(abs(crossprod(Xp, yp - mp) - pp$coefficients / 9)), 1e-8)
  expect_equal(pp$loglik, sum(dpois(yp, mp, log = TRUE)), tolerance = 1e-9)

  expect_error(morie_glmbay_bayesian_glm(X, y[-1]), "responses")
  expect_error(morie_glmbay_bayesian_glm(X, y, family = "gamma"), "family must be")
  expect_error(morie_glmbay_bayesian_glm(X, y, prior_sd = 0), "positive")
})

test_that("Ktrend is Kendall's tau-b with the Mann-Kendall normal approximation", {
  t <- c(3, 1, 2, 5, 4, 6, 7)
  x <- c(2.0, 1.5, 1.5, 3.1, 2.9, 3.0, 4.2)
  r <- Ktrend(t, x)
  xs <- x[order(t)]
  pr <- combn(7, 2)
  sg <- sign(xs[pr[2, ]] - xs[pr[1, ]])
  S <- sum(sg)
  expect_equal(r$S, as.integer(S))
  expect_equal(r$tau, cor(t, x, method = "kendall"), tolerance = 1e-12)
  expect_equal(r$tau_a, S / sum(sg != 0), tolerance = 1e-12)
  vS <- (7 * 6 * 19 - 2 * 1 * 9) / 18
  expect_equal(r$var_S, vS, tolerance = 1e-12)
  expect_equal(r$z, (S - 1) / sqrt(vS), tolerance = 1e-12)
  expect_equal(r$p_value, 2 * pnorm(-abs((S - 1) / sqrt(vS))), tolerance = 1e-12)
  expect_equal(Ktrend(1:4, 4:1)$z, (-6 + 1) / sqrt(4 * 3 * 13 / 18), tolerance = 1e-12)
  expect_error(Ktrend(1:2, 1:2), "n >= 3")
  expect_error(Ktrend(1:3, 1:4), "same length")
})

test_that("Glpopt solves min c'x, Ax <= b, x >= 0 with its duals", {
  cv <- c(-3, -5)
  A <- rbind(c(1, 0), c(0, 2), c(3, 2))
  b <- c(4, 12, 18)
  r <- Glpopt(cv, A, b)
  # optimum of the textbook Wyndor problem is x = (2, 6), value -36
  verts <- rbind(c(0, 0), c(4, 0), c(4, 3), c(2, 6), c(0, 6))
  feas <- apply(verts, 1, function(v) all(A %*% v <= b + 1e-12))
  vals <- verts[feas, ] %*% cv
  expect_equal(r$objective, min(vals), tolerance = 1e-12)
  expect_equal(r$x, verts[feas, ][which.min(vals), ], tolerance = 1e-12)
  expect_equal(r$dual_objective, r$objective, tolerance = 1e-12)
  expect_true(all(r$dual <= 1e-12))
  expect_equal(r$status, "optimal")
  u <- Glpopt(c(-1, 0), rbind(c(0, 1)), 3)
  expect_equal(u$status, "unbounded")
  expect_error(Glpopt(cv, A, c(-1, 1, 1)), "b >= 0")
  expect_error(Glpopt(cv, A, b[-1]), "row counts")
})

test_that("Gmcenter subtracts the grand mean", {
  y <- c(3, 7, 2, 8)
  r <- Gmcenter(y)
  expect_equal(r$centered, y - 5, tolerance = 1e-12)
  expect_equal(r$sd, sd(y), tolerance = 1e-12)
  expect_true(is.nan(Gmcenter(4)$sd))
})

test_that("Gnsetenr is the weighted Kolmogorov-Smirnov running sum", {
  r <- c(0.9, -0.2, 0.5, 0.1, -0.7, 0.3, -0.4, 0.8)
  inset <- c(1, 0, 1, 0, 0, 1, 0, 0)
  g <- Gnsetenr(r, inset, p = 1)
  o <- order(-r)
  hit <- inset[o] == 1
  step <- ifelse(hit, abs(r[o]) / sum(abs(r[o][hit])), -1 / 5)
  run <- cumsum(step)
  expect_equal(g$running, run, tolerance = 1e-12)
  expect_equal(g$es, run[which.max(abs(run))], tolerance = 1e-12)
  expect_equal(g$arg_es, which.max(abs(run)) - 1L)
  g0 <- Gnsetenr(r, inset, p = 0)
  expect_equal(g0$running, cumsum(ifelse(hit, 1 / 3, -1 / 5)), tolerance = 1e-12)
  gp <- Gnsetenr(r, inset, nperm = 30, seed = 4)
  set.seed(4)
  es_of <- function(m) {
    rr <- cumsum(ifelse(m, abs(r[o]) / sum(abs(r[o][m])), -1 / 5))
    rr[which.max(abs(rr))]
  }
  # the observed set can be redrawn; R's cumsum accumulates in long double,
  # so compare |ES| with a 1e-12 allowance rather than bit equality
  es0 <- es_of(hit)
  same <- ext <- 0
  for (b in 1:30) {
    pm <- rep(FALSE, 8)
    pm[sample.int(8, 3)] <- TRUE
    ep <- es_of(pm)
    if ((es0 >= 0 && ep >= 0) || (es0 < 0 && ep < 0)) {
      same <- same + 1
      if (abs(ep) >= abs(es0) - 1e-12) ext <- ext + 1
    }
  }
  expect_equal(gp$pvalue, ext / same, tolerance = 1e-12)
  expect_error(Gnsetenr(r, inset[-1]), "equal length")
  expect_error(Gnsetenr(r, rep(1, 8)), "proper nonempty subset")
})

test_that("Goalcond gives -steps (or the discounted sum) to each goal", {
  env <- rbind(c(0, 0, 1), c(1, 0, 2), c(2, 0, 3), c(1, 1, 0), c(3, 0, 3))
  r <- Goalcond(env, goal_dist = cbind(c(3, 0), c(3, 1)))
  d3 <- c(3, 2, 1, 0)
  d0 <- c(0, 1, -Inf, -Inf)
  expect_equal(r$v[, 1], -d3)
  expect_equal(r$v[, 2], c(0, -1, -Inf, -Inf))
  expect_equal(r$expected_value, (3 * -d3 + 1 * c(0, -1, 0, 0)) / 4, tolerance = 1e-12)
  expect_equal(r$reachable, 6 / 8, tolerance = 1e-12)
  g <- Goalcond(env, goal_dist = matrix(3), gamma = 0.9, step_cost = -2)
  expect_equal(g$v[, 1], vapply(d3, function(d) sum(-2 * 0.9^seq(0, length.out = d)), 0), tolerance = 1e-12)
  expect_equal(Goalcond(env)$n_states, 4L)
})

test_that("Goenr is the hypergeometric upper tail", {
  r <- Goenr(c(4, 1, 0), list_size = 10, term_size = c(12, 30, 5), background_size = 100)
  expect_equal(r$pvalue, phyper(c(4, 1, 0) - 1, c(12, 30, 5), 100 - c(12, 30, 5), 10, lower.tail = FALSE),
               tolerance = 1e-12)
  expect_equal(r$expected, 10 * c(12, 30, 5) / 100, tolerance = 1e-12)
  expect_equal(r$fold_enrichment, (c(4, 1, 0) / 10) / (c(12, 30, 5) / 100), tolerance = 1e-12)
  b <- Goenr(c(4, 1), 10, 12, 100, correction = "bonferroni")
  expect_equal(b$padj, pmin(1, 2 * phyper(c(3, 0), 12, 88, 10, lower.tail = FALSE)), tolerance = 1e-12)
  expect_error(Goenr(11, 10, 20, 100), "cannot exceed")
  expect_error(Goenr(1, 200, 20, 100), "list_size")
  expect_error(Goenr(c(1, 2), 10, c(3, 4, 5), 100), "equal length")
})
