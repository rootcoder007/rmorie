# Coverage for npbcox .. odgrev exports (Npbcox, Npbqr, Npbsr, Nprphet,
# Nrgmwd, Nrmprc, Lctest, morie_nypd_all_analyses, Nystromap, Objfair,
# Ocmtmd, Outbrkdet). Every expectation is recomputed in the test body.

test_that("Npbcox fits the Breslow partial likelihood and the gamma-process baseline", {
  skip_if_not_installed("survival")
  tt <- c(2, 3, 3, 5, 6, 7, 8, 9, 11, 12, 14, 15)
  ev <- c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0)
  x1 <- c(0.5, -1.2, 0.3, 1.1, -0.4, 0.9, -0.7, 0.2, 1.4, -0.1, 0.6, -1.0)
  x2 <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0)
  X <- cbind(x1, x2)
  res <- Npbcox(tt, ev, X, c = 2, lam0 = 0.1)
  cf <- survival::coxph(survival::Surv(tt, ev) ~ x1 + x2, ties = "breslow")
  # coxph stops at its own relative log-likelihood tolerance (1e-9)
  expect_equal(res$beta, unname(stats::coef(cf)), tolerance = 1e-6)
  at <- survival::coxph(survival::Surv(tt, ev) ~ x1 + x2, ties = "breslow",
                        init = res$beta, control = survival::coxph.control(iter.max = 0))
  expect_equal(res$loglik, at$loglik[2], tolerance = 1e-12)
  expect_equal(unname(res$se), sqrt(diag(at$var)), tolerance = 1e-10)
  expect_equal(res$converged, 1)
  w <- exp(as.numeric(X %*% res$beta))
  ets <- sort(unique(tt[ev == 1]))
  prev <- c(0, ets[-length(ets)])
  inc <- vapply(seq_along(ets), function(k) {
    (2 * 0.1 * (ets[k] - prev[k]) + sum(tt == ets[k] & ev == 1)) / (2 + sum(w[tt >= ets[k]]))
  }, 0)
  expect_equal(res$times, ets)
  expect_equal(res$dH, inc, tolerance = 1e-12)
  expect_equal(res$H, cumsum(inc), tolerance = 1e-12)
  expect_equal(res$S, cumprod(1 - inc), tolerance = 1e-12)
  # default prior rate 1 / mean(time)
  expect_equal(Npbcox(tt, ev, X)$lam0, 1 / mean(tt), tolerance = 1e-12)
  expect_error(Npbcox(tt, rep(0, 12), X), "no events")
  expect_error(Npbcox(tt, ev, X, c = -1), "non-negative")
  expect_error(Npbcox(tt, ev[-1], X), "different lengths")
  expect_error(Npbcox(numeric(0), numeric(0), matrix(0, 0, 1)), "empty")
})

test_that("Npbqr reports the check loss of its quantile fit", {
  x <- c(0.3, 1.2, 2.5, 3.1, 4.0, 4.4, 5.9, 6.2, 7.5, 8.1, 9.0, 9.8)
  y <- 1 + 0.5 * x + c(0.4, -0.3, 1.1, -0.8, 0.2, 0.9, -1.2, 0.5, -0.1, 1.4, -0.6, 0.3)
  res <- Npbqr(y, x, tau = 0.7, alpha = 2)
  r <- y - cbind(1, x) %*% res$beta
  loss <- sum(r * (0.7 - (r < 0)))
  expect_equal(res$check_loss, loss, tolerance = 1e-12)
  expect_equal(res$sigma, loss / 12, tolerance = 1e-12)
  expect_equal(res$e_k, sum(2 / (2 + 0:11)), tolerance = 1e-12)
  # asymptotic-series digamma helper is accurate to about 1e-11
  expect_equal(res$e_k_digamma, 2 * (digamma(14) - digamma(2)), tolerance = 1e-9)
  skip_if_not_installed("quantreg")
  rq <- quantreg::rq(y ~ x, tau = 0.7)
  rl <- sum(stats::residuals(rq) * (0.7 - (stats::residuals(rq) < 0)))
  # IRLS with an eps = 1e-3 residual floor approximates the LP optimum
  expect_true(loss >= rl - 1e-12)
  expect_lt(loss - rl, 1e-2 * rl)
  expect_error(Npbqr(y, x, tau = 1), "tau must lie")
  expect_error(Npbqr(y, x, alpha = 0), "alpha must be positive")
  expect_error(Npbqr(y, x[-1]), "different lengths")
})

test_that("Npbsr is the Hjort beta-process posterior survival curve", {
  tt <- c(4, 2, 7, 2, 9, 5, 11)
  ev <- c(1, 1, 0, 1, 1, 1, 0)
  res <- Npbsr(tt, event = ev, c = 3, lam0 = 0.2)
  u <- sort(unique(tt))
  Y <- vapply(u, function(s) sum(tt >= s), 0)
  dN <- vapply(u, function(s) sum(tt == s & ev == 1), 0)
  dH <- (3 * 0.2 * diff(c(0, u)) + dN) / (3 + Y)
  S <- cumprod(1 - dH)
  expect_equal(res$times, u)
  expect_equal(res$S_post, S, tolerance = 1e-12)
  expect_equal(res$H_post, cumsum(dH), tolerance = 1e-12)
  expect_equal(res$estimate, S[findInterval(stats::median(tt), u)], tolerance = 1e-12)
  expect_true(is.na(Npbsr(numeric(0))$estimate))
})

test_that("Nprphet is least squares on trend, changepoint, Fourier and AR columns", {
  ds <- 1:20
  y <- 2 + 0.3 * ds + sin(2 * pi * ds / 7) + c(0.1, -0.2, 0.3, 0, -0.1, 0.2, -0.3, 0.1, 0.4, -0.2,
                                                0.0, 0.1, -0.4, 0.2, 0.3, -0.1, 0.2, -0.2, 0.1, 0.0)
  res <- Nprphet(ds, y, ar_layers = 2, n_changepoints = 2, seasonality = c(7, 2))
  cps <- stats::quantile(ds, c(1, 2) / 3, type = 7, names = FALSE)
  i <- 3:20
  X <- cbind(1, ds[i], pmax(outer(ds[i], cps, "-"), 0),
             cos(2 * pi * ds[i] / 7), sin(2 * pi * ds[i] / 7),
             cos(4 * pi * ds[i] / 7), sin(4 * pi * ds[i] / 7), y[i - 1], y[i - 2])
  fit <- stats::lm.fit(X, y[i])
  expect_equal(res$coef, unname(fit$coefficients), tolerance = 1e-10)
  expect_equal(res$fitted, unname(fit$fitted.values), tolerance = 1e-12)
  expect_equal(res$rmse, sqrt(mean(fit$residuals^2)), tolerance = 1e-10)
  expect_equal(res$estimate, unname(fit$fitted.values[18]), tolerance = 1e-12)
  # no changepoints, no seasonality, no AR: a straight line
  r0 <- Nprphet(ds, y, ar_layers = 0, n_changepoints = 0, seasonality = c(7, 0))
  expect_equal(r0$coef, unname(stats::coef(stats::lm(y ~ ds))), tolerance = 1e-12)
})

test_that("Nrgmwd gives the posterior mean functional of a Dirichlet-type measure", {
  y <- c(1.2, -0.5, 2.3, 0.8)
  res <- Nrgmwd(y, alpha = 3, tau = 2, mu0 = 0.5, sigma0 = 1.5)
  m <- 3 + 4
  pm <- (3 * 0.5 + sum(y)) / m
  m2 <- 3 / m * (1.5^2 + 0.5^2) + sum(y^2) / m
  expect_equal(res$post_mean, pm, tolerance = 1e-12)
  expect_equal(res$post_base_var, m2 - pm^2, tolerance = 1e-12)
  expect_equal(res$post_var, (m2 - pm^2) / (m + 1), tolerance = 1e-12)
  expect_equal(res$prior_var, 1.5^2 / 4, tolerance = 1e-12)
  expect_equal(res$total_mass, 6)
  expect_error(Nrgmwd(y, alpha = 0), "alpha")
  expect_error(Nrgmwd(y, tau = 0), "tau")
  expect_error(Nrgmwd(y, sigma0 = -1), "sigma0")
  expect_error(Nrgmwd(numeric(0)), "empty")
})

test_that("Nrmprc integrates the inverse-Gaussian Levy mean by the midpoint rule", {
  y <- c(1, 2, 2, 5)
  res <- Nrmprc(y, alpha = 1, tau = 1.5, u_max = 10, n_grid = 4000)
  h <- 10 / 4000
  u <- (seq_len(4000) - 0.5) * h
  mid <- sum(u^-0.5 * exp(-1.5^2 * u / 2)) / sqrt(2 * pi) * h
  expect_equal(res$estimate, mid, tolerance = 1e-12)
  expect_equal(res$gap, abs(mid - 1 / 1.5), tolerance = 1e-12)
  # the truncated integral is (2 Phi(tau sqrt(U)) - 1) / tau; the midpoint
  # rule on the u^(-1/2) singularity carries an O(sqrt(h)) error
  exact <- (2 * stats::pnorm(1.5 * sqrt(10)) - 1) / 1.5
  expect_lt(abs(res$estimate - exact), 3 * sqrt(h))
  expect_equal(res$n_distinct, 3L)
  expect_error(Nrmprc(y, tau = 0), "tau")
  expect_error(Nrmprc(y, alpha = 0), "alpha")
})

test_that("Lctest computes Hansen's (1992) L_c stability statistic", {
  x <- c(0.2, 1.4, -0.3, 2.2, 0.9, -1.1, 1.8, 0.5, -0.6, 1.0, 2.5, -0.2, 0.7, 1.3)
  y <- 0.5 + 1.2 * x + c(0.3, -0.5, 0.2, 0.9, -0.1, 0.4, -0.8, 0.6, -0.2, 0.1, 1.1, -0.9, 0.0, 0.5)
  n <- length(y)
  e <- stats::residuals(stats::lm(y ~ x))
  f <- cbind(e, x * e, e^2 - mean(e^2))
  S <- apply(f, 2, cumsum)
  V <- crossprod(f)
  lc <- sum(diag(S %*% solve(V) %*% t(S))) / n
  res <- Lctest(y, x)
  expect_equal(res$statistic, lc, tolerance = 1e-10)
  expect_equal(res$df, 3L)
  expect_equal(res$individual, unname(colSums(S^2) / (n * diag(V))), tolerance = 1e-10)
  expect_equal(unname(res$critical[3]), 1.01)
  r2 <- Lctest(y, x, variance = FALSE)
  expect_equal(r2$statistic, sum(diag(S[, 1:2] %*% solve(V[1:2, 1:2]) %*% t(S[, 1:2]))) / n,
               tolerance = 1e-10)
  expect_equal(unname(r2$critical[1]), 1.07)
  expect_error(Lctest(y[1:3], x[1:3]), "more observations")
  expect_error(Lctest(y, x[-1]), "one row per element")
})

test_that("morie_nypd_all_analyses tabulates offenses, boroughs and felony rates", {
  arr <- data.frame(
    ofns_desc = c("ROBBERY", "ASSAULT", "ROBBERY", "THEFT", "ASSAULT", "ROBBERY", ""),
    arrest_boro = c("K", "M", "K", "B", "Q", "K", "M"),
    perp_race = c("A", "B", "A", "B", "A", "B", "A"),
    law_cat_cd = c("F", "M", "f ", "F", "M", "M", "F"),
    stringsAsFactors = FALSE)
  cmp <- data.frame(susp_race = c("A", "A", "B", NA, "C"), stringsAsFactors = FALSE)
  od <- file.path(tempdir(), "nypd_cov_test")
  res <- morie_nypd_all_analyses(arr, cmp, out_dir = od)
  expect_named(res, c("arrests_by_offense", "arrests_by_boro", "felony_race_disparity",
                      "complaints_by_race"))
  expect_equal(res$arrests_by_offense$payload, list(ROBBERY = 3L, ASSAULT = 2L, THEFT = 1L))
  expect_equal(res$arrests_by_boro$payload$K, 3L)
  fel <- as.integer(toupper(trimws(arr$law_cat_cd)) == "F")
  rates <- tapply(fel, arr$perp_race, mean)
  expect_equal(res$felony_race_disparity$tables$felony_rate_by_race$felony_rate,
               as.numeric(rates), tolerance = 1e-12)
  expect_equal(res$complaints_by_race$payload, list(A = 2L, B = 1L, C = 1L))
  expect_true(all(file.exists(file.path(od, sprintf("nypd_%s.json", names(res))))))
  unlink(od, recursive = TRUE)
  miss <- morie_nypd_all_analyses(data.frame(z = 1), data.frame(z = 1))
  expect_match(miss$arrests_by_offense$warnings, "ofns_desc")
  expect_match(miss$felony_race_disparity$warnings, "perp_race")
  expect_match(miss$complaints_by_race$warnings, "susp_race")
})

test_that("Nystromap forms K_nm K_mm^+ K_mn for the linear kernel", {
  X <- cbind(c(1, 0.5, -0.3, 2, 1.1, -0.7), c(0.2, 1.5, 0.8, -1, 0.4, 0.9),
             c(-0.5, 0.3, 1.2, 0.6, -1.4, 0.1))
  idx <- c(1, 3, 5)
  res <- Nystromap(X, idx)
  Knm <- X %*% t(X[idx, ]) / 3
  Kmm <- X[idx, ] %*% t(X[idx, ]) / 3
  expect_equal(res$Q, Knm %*% solve(Kmm) %*% t(Knm), tolerance = 1e-10)
  # three independent landmarks in three dimensions recover the full Gram matrix
  expect_equal(res$Q, X %*% t(X) / 3, tolerance = 1e-10)
  expect_equal(c(res$m, res$n), c(3L, 6L))
  expect_error(Nystromap(X, c(0, 2)), "1-based")
})

test_that("Objfair audits the Lipschitz individual-fairness condition", {
  h <- c(0.2, 0.9, 0.4, 0.45)
  P <- rbind(c(0, 1), c(0, 2), c(2, 3), c(1, 3))
  d <- c(0.5, 0.1, 0, 1)
  res <- Objfair(h, P, L = 1, metric = d)
  gap <- abs(h[P[, 1] + 1] - h[P[, 2] + 1])
  expect_equal(res$max_gap, max(gap), tolerance = 1e-12)
  expect_equal(c(res$max_pair_i, res$max_pair_j), c(0L, 1L))
  # pair (2,3) sits at distance 0 with a positive gap: L_required is infinite
  expect_identical(res$L_required, Inf)
  expect_equal(res$n_violations, sum(gap > d | (d == 0 & gap > 0)))
  expect_equal(res$fair, 0)
  r1 <- Objfair(h, P[-3, ], L = 5, metric = d[-3])
  expect_equal(r1$L_required, max(gap[-3] / d[-3]), tolerance = 1e-12)
  expect_equal(r1$n_violations, sum(gap[-3] > 5 * d[-3]))
  # unit metric by default
  expect_equal(Objfair(h, P)$L_required, max(gap), tolerance = 1e-12)
  expect_error(Objfair(h, rbind(c(0, 4))), "out of range")
  expect_error(Objfair(h, P, metric = d[-1]), "differ in length")
  expect_error(Objfair(h, P, L = -1), "non-negative")
  expect_error(Objfair(numeric(0), P), "empty")
})

test_that("Ocmtmd regresses standardised residuals on treatment and finds the Crump cut", {
  H <- cbind(c(0.1, 1.2, -0.4, 0.8, 1.9, -1.1, 0.3, 0.6, -0.2, 1.4, -0.8, 0.9),
             c(1.0, 0.4, 0.7, -0.3, 0.2, 1.1, -0.9, 0.5, 0.8, -0.6, 0.3, 0.0))
  A <- c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1)
  y <- 1 + A + H[, 1] - 0.5 * H[, 2] + c(0.2, -0.1, 0.4, -0.3, 0.1, 0.0, -0.2, 0.3, -0.4, 0.2, 0.1, -0.1)
  res <- Ocmtmd(y, A, H)
  e <- stats::residuals(stats::lm(y ~ A + H))
  r <- e / stats::sd(e)
  fr <- stats::lm(r ~ A)
  expect_equal(res$b1, unname(stats::coef(fr)[2]), tolerance = 1e-10)
  expect_equal(res$t_stat, summary(fr)$coefficients[2, 3], tolerance = 1e-8)
  expect_equal(res$mean_resid_treated, mean(r[A == 1]), tolerance = 1e-10)
  ps <- stats::fitted(stats::glm(A ~ H, family = stats::binomial()))
  # glm stops at a relative deviance change of 1e-8
  expect_equal(c(res$min_ps, res$max_ps), range(ps), tolerance = 1e-6)
  cand <- sort(unique(pmin(ps, 1 - ps)))
  al <- 0
  for (a in cand) {
    k <- ps[ps >= a & ps <= 1 - a]
    if (length(k) && 1 / (a * (1 - a)) <= 2 * mean(1 / (k * (1 - k)))) {
      al <- a
      break
    }
  }
  expect_equal(res$alpha_crump, al, tolerance = 1e-6)
  # supplying Q replaces the outcome model
  q <- rep(mean(y), 12)
  rq <- Ocmtmd(y, A, H, Q = q)
  expect_equal(rq$resid_sd, stats::sd(y), tolerance = 1e-12)
  expect_error(Ocmtmd(y, A + 1, H), "binary")
  expect_error(Ocmtmd(rep(1, 12), A, H, Q = rep(1, 12)), "zero spread")
  expect_error(Ocmtmd(y, A, H[-1, ]), "different lengths")
})

test_that("Outbrkdet runs the Gamma-Poisson Bayesian online changepoint recursion", {
  y <- c(2, 3, 1, 2, 9, 11, 10, 12)
  hz <- 0.05
  a <- 1.5
  b <- 0.5
  R <- 1
  cp <- numeric(8)
  rl <- integer(8)
  for (t in 1:8) {
    pr <- stats::dnbinom(y[t], size = a, prob = b / (b + 1))
    nr <- c(sum(R * pr * hz), R * pr * (1 - hz))
    R <- nr / sum(nr)
    a <- c(1.5, a + y[t])
    b <- c(0.5, b + 1)
    cp[t] <- R[2]
    rl[t] <- which.max(R) - 1L
  }
  res <- Outbrkdet(y, hazard = hz, a0 = 1.5, b0 = 0.5)
  expect_equal(res$cp_prob, cp, tolerance = 1e-12)
  expect_equal(res$run_length, rl)
  expect_equal(res$max_cp_prob, max(cp), tolerance = 1e-12)
  expect_equal(res$alarm, which(cp > 0.5) - 1L)
  expect_error(Outbrkdet(c(1, -1)), "non-negative")
})
