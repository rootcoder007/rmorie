# Coverage for sschin_native .. ssmpar_native exports. Every expectation is
# recomputed in the test body.

ss1_data <- function() {
  tm <- c(5, 8, 3, 12, 7, 2, 9, 15, 4, 11, 6, 10, 13, 1, 14)
  ev <- c(1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
  X <- cbind(c(0.5, 1.2, -0.3, 0.8, NA, 1.9, -1.1, 0.4, 0.2, NA, -0.6, 1.0, 0.7, 1.5, -0.2),
             c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, NA, 1, 0, 0))
  list(tm = tm, ev = ev, X = X)
}

test_that("morie_sschin_chained_imputation pools Cox fits by Rubin's rules", {
  skip_if_not_installed("survival")
  d <- ss1_data()
  r <- morie_sschin_chained_imputation(d$tm, d$ev, d$X, mi_iter = 3, cycles = 4)
  E <- t(vapply(r$per_imputation, function(f) f$coefficients, numeric(2)))
  V <- t(vapply(r$per_imputation, function(f) f$variance, numeric(2)))
  qbar <- colMeans(E)
  ubar <- colMeans(V)
  B <- apply(E, 2, stats::var)
  Tv <- ubar + (1 + 1 / 3) * B
  expect_equal(r$estimate, qbar, tolerance = 1e-12)
  expect_equal(r$hazard_ratio, exp(qbar), tolerance = 1e-12)
  expect_equal(r$between_variance, B, tolerance = 1e-12)
  expect_equal(r$total_variance, Tv, tolerance = 1e-12)
  riv <- (1 + 1 / 3) * B / ubar
  dfcom <- 11 - 2
  lam <- (riv + 2 / (dfcom + 3)) / (riv + 1)
  dold <- 2 / lam^2
  dobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lam)
  expect_equal(r$relative_increase_variance, riv, tolerance = 1e-12)
  expect_equal(r$fraction_missing_info, lam, tolerance = 1e-12)
  expect_equal(r$df, dold * dobs / (dold + dobs), tolerance = 1e-12)
  z <- stats::qnorm(0.975)
  cf <- z + (z^3 + z) / 4 / r$df + (5 * z^5 + 16 * z^3 + 3 * z) / 96 / r$df^2 +
    (3 * z^7 + 19 * z^5 + 17 * z^3 - 15 * z) / 384 / r$df^3
  expect_equal(r$t_quantile, cf, tolerance = 1e-12)
  expect_equal(r$ci_upper, qbar + cf * sqrt(Tv), tolerance = 1e-12)
  expect_equal(r$n_missing, 3L)
  expect_equal(r$columns_imputed, c(0, 1))
  cc <- stats::complete.cases(d$X)
  f <- survival::coxph(survival::Surv(d$tm[cc], d$ev[cc]) ~ d$X[cc, ], ties = "breslow",
                       control = survival::coxph.control(eps = 1e-11, iter.max = 100))
  # Newton steps stop at 1e-10 against coxph's 1e-12 relative tolerance
  expect_equal(r$complete_case_coefficients, unname(stats::coef(f)), tolerance = 1e-7)
  expect_equal(r$complete_case_se, unname(sqrt(diag(stats::vcov(f)))), tolerance = 1e-7)
  full <- d$X
  full[is.na(full)] <- c(0.1, 0.9, 1)
  nm <- morie_sschin_chained_imputation(d$tm, d$ev, full, mi_iter = 2, cycles = 1)
  g <- survival::coxph(survival::Surv(d$tm, d$ev) ~ full, ties = "breslow",
                       control = survival::coxph.control(eps = 1e-11, iter.max = 100))
  expect_equal(nm$between_variance, c(0, 0))
  expect_equal(nm$estimate, unname(stats::coef(g)), tolerance = 1e-7)
  expect_equal(nm$df, c(9, 9))
  expect_error(morie_sschin_chained_imputation(d$tm, d$ev, d$X, mi_iter = 1), "at least two")
  expect_error(morie_sschin_chained_imputation(d$tm, d$ev, d$X, ties = "efron"), "Breslow")
  expect_error(morie_sschin_chained_imputation(d$tm, 0 * d$ev, d$X), "no events")
  expect_error(morie_sschin_chained_imputation(d$tm[-1], d$ev, d$X), "must agree")
})

test_that("Curemod stops at a fixed point of the Sy-Taylor EM", {
  tm <- c(1, 2, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12)
  ev <- c(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0)
  z <- c(0.2, -0.5, 1.1, 0.4, -0.9, 0.3, 1.5, -0.2, 0.8, -1.2, 0.6, 1.0, -0.4, 0.1)
  r <- Curemod(tm, ev, Z = z)
  times <- sort(unique(tm[ev == 1]))
  expect_equal(r$times, times)
  w <- r$weights
  expect_equal(w[ev == 1], rep(1, sum(ev)))
  # one more M-step then E-step reproduces the weights
  fit <- suppressWarnings(stats::glm(w ~ z, family = stats::quasibinomial(),
                                     control = list(epsilon = 1e-14, maxit = 100)))
  pi_ <- stats::plogis(stats::coef(fit)[1] + stats::coef(fit)[2] * z)
  S0 <- cumprod(vapply(times, function(tt) 1 - sum(tm == tt & ev == 1) / sum(w[tm >= tt]), 0))
  s <- vapply(tm, function(t) {
    if (t >= max(times)) return(0)
    k <- sum(times <= t)
    if (k == 0) 1 else S0[k]
  }, 0)
  new <- ifelse(ev == 1, 1, pi_ * s / (1 - pi_ + pi_ * s))
  expect_equal(r$weights, unname(new), tolerance = 1e-9)
  expect_equal(r$gamma, unname(stats::coef(fit)), tolerance = 1e-8)
  expect_equal(r$cure_fraction, 1 - mean(pi_), tolerance = 1e-8)
  expect_equal(r$S0, S0, tolerance = 1e-9)
  expect_error(Curemod(tm, ev, X = matrix(1, 3, 1)), "one row per observation")
})

test_that("morie_sse4r attends over the personalised sequence", {
  I <- rbind(c(0.2, 0.5), c(-0.3, 0.8), c(0.6, -0.1))
  u <- c(0.4, -0.7)
  Tb <- rbind(c(1, 0), c(0, 1), c(0.5, 0.5), c(-1, 0.3))
  r <- morie_sse4r(I, u, Tb, top_k = 2)
  S <- cbind(I, matrix(u, 3, 2, byrow = TRUE))
  q <- S[3, ]
  sc <- (as.numeric(S[, 1:2] %*% q[1:2]) + as.numeric(S[, 1:2] %*% u)) / 2
  w <- exp(sc) / sum(exp(sc))
  ctx <- colSums(w * S)
  expect_equal(r$context, ctx, tolerance = 1e-12)
  scores <- as.numeric(Tb %*% ctx[1:2]) + sum(ctx[3:4] * u)
  expect_equal(r$scores, scores, tolerance = 1e-12)
  expect_equal(r$top_k, order(-scores)[1:2])
  other <- morie_sse4r(I, -u, Tb)
  expect_false(isTRUE(all.equal(other$context[1:2], ctx[1:2])))
  m <- morie_sse4r(I, u, Tb, attend = function(s) Reduce(`+`, s) / length(s))
  expect_equal(m$context, colMeans(S), tolerance = 1e-12)
  expect_error(morie_sse4r(I, u, Tb[, 1, drop = FALSE]), "item table is 1-wide")
  expect_error(morie_sse4r(I[0, , drop = FALSE], u, Tb), "sequence is empty")
  expect_match(sse4r_cheatsheet(), "p(1-1/n)", fixed = TRUE)
})

test_that("Ssintc is a self-consistent Turnbull NPMLE", {
  L <- c(0, 1, 2, 3, 0.5, 4, 2.5)
  R <- c(2, 3, Inf, 4, 1.5, 6, 5)
  r <- Ssintc(L, R, n_iter = 2000)
  ends <- c(unique(L), unique(R[is.finite(R)]))
  qs <- rs <- numeric(0)
  for (a in sort(unique(L))) for (b in sort(unique(R[is.finite(R)]))) {
    if (a < b && !any(ends > a & ends < b)) {
      qs <- c(qs, a)
      rs <- c(rs, b)
    }
  }
  expect_equal(r$q, qs)
  expect_equal(r$r, rs)
  A <- outer(L, qs, "<=") & outer(R, rs, ">=")
  p <- r$p
  upd <- colSums(A * outer(rep(1, 7), p) / as.numeric(A %*% p)) / 7
  expect_equal(upd, p, tolerance = 1e-9)
  expect_equal(sum(p), 1, tolerance = 1e-12)
  expect_equal(r$surv, 1 - cumsum(p), tolerance = 1e-12)
  expect_equal(r$estimate, rs[which(1 - cumsum(p) <= 0.5)[1]])
  none <- Ssintc(c(2, 3), c(1, 2))
  expect_equal(none$m, 0L)
  expect_true(is.nan(none$estimate))
})

test_that("sequential and parallel scans of affine maps agree", {
  pairs <- list(c(0.9, 0.1), c(0.5, -0.3), c(1.2, 0.4), c(0.7, 0.2), c(-0.4, 1.0))
  s <- sequential_scan(pairs, x0 = 2)
  x <- 2
  st <- numeric(5)
  for (i in 1:5) {
    x <- pairs[[i]][1] * x + pairs[[i]][2]
    st[i] <- x
  }
  expect_equal(s$states, st, tolerance = 1e-12)
  expect_equal(s$depth, 5L)
  p <- parallel_scan(pairs, x0 = 2)
  expect_equal(p$states, st, tolerance = 1e-12)
  expect_equal(p$prefix[[3]], c(0.9 * 0.5 * 1.2, (0.1 * 0.5 - 0.3) * 1.2 + 0.4), tolerance = 1e-12)
  expect_equal(sequential_scan(list())$states, numeric(0))
  expect_error(parallel_scan(list()), "empty")
  a <- check_associativity(c(0.9, 0.1), c(0.5, -0.3), c(1.2, 0.4))
  expect_equal(a$left, c(0.54, (0.1 * 0.5 - 0.3) * 1.2 + 0.4), tolerance = 1e-12)
  expect_true(a$associative)
  d <- scan_depth(1000)
  expect_equal(d$parallel_depth, ceiling(log2(1000)))
  expect_equal(d$speedup, 1000 / 10)
  expect_equal(scan_depth(1)$parallel_depth, 1)
  expect_error(scan_depth(0), "positive")
})
