# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Mirrors of the morie.fn causal cluster (Python parity batch 2).
# Closed-form estimators only; the causal-forest and TMLE tiers stay
# Python-side for now (they need the honest-splitting and targeting
# machinery, which is not mirrored here).

#' ATT inverse-probability-of-treatment weights
#'
#' Treated units keep weight 1; controls are reweighted by
#' `e / (1 - e)` so the control arm matches the treated covariate
#' distribution. Mirrors `morie.fn.causipsw`.
#'
#' @param treat Binary 0/1 treatment indicator.
#' @param ps Estimated propensity scores, strictly inside (0, 1) for
#'   controls.
#' @return List with `weights`, `ess_control`, `ess_treated`, `n`.
#' @references Hernan MA, Robins JM (2020). *Causal Inference: What
#'   If*. Chapman & Hall/CRC, Ch. 15.
#' @export
morie_att_weights <- function(treat, ps) {
  treat <- as.numeric(treat)
  ps <- as.numeric(ps)
  if (length(treat) != length(ps)) {
    stop("treat and ps must have equal length.", call. = FALSE)
  }
  if (!all(treat %in% c(0, 1))) stop("treat must be binary 0/1.", call. = FALSE)
  ctrl <- treat == 0
  if (any(ps[ctrl] <= 0 | ps[ctrl] >= 1)) {
    stop("control propensity scores must lie strictly in (0, 1).", call. = FALSE)
  }
  w <- ifelse(treat == 1, 1, ps / (1 - ps))
  ess <- function(v) if (length(v)) sum(v)^2 / sum(v^2) else 0
  list(
    weights = w, ess_control = ess(w[ctrl]), ess_treated = ess(w[!ctrl]),
    n = length(treat)
  )
}

#' Quantile treatment effect via Firpo IPW
#'
#' Weighted-quantile difference: treated quantiles weighted by `1/e`,
#' control quantiles by `1/(1 - e)`. Mirrors `morie.fn.causqte`.
#'
#' @param y Outcome.
#' @param treat Binary 0/1 treatment.
#' @param ps Propensity scores strictly in (0, 1).
#' @param tau Quantile level(s) in (0, 1).
#' @return List with `qte`, `q1`, `q0`, `tau`, `n`.
#' @references Firpo S (2007). Efficient semiparametric estimation of
#'   quantile treatment effects. *Econometrica* 75(1), 259-276.
#' @export
morie_qte_firpo <- function(y, treat, ps, tau = 0.5) {
  y <- as.numeric(y); treat <- as.numeric(treat); ps <- as.numeric(ps)
  if (length(unique(c(length(y), length(treat), length(ps)))) != 1L) {
    stop("y, treat, ps must have equal length.", call. = FALSE)
  }
  if (!all(treat %in% c(0, 1))) stop("treat must be binary 0/1.", call. = FALSE)
  if (any(ps <= 0 | ps >= 1)) {
    stop("propensity scores must lie strictly in (0, 1).", call. = FALSE)
  }
  if (any(tau <= 0 | tau >= 1)) {
    stop("tau must lie strictly in (0, 1).", call. = FALSE)
  }
  wq <- function(v, w, t) {
    o <- order(v)
    cw <- cumsum(w[o]) / sum(w)
    v[o][which(cw >= t)[1L]]
  }
  tr <- treat == 1
  q1 <- vapply(tau, function(t) wq(y[tr], 1 / ps[tr], t), numeric(1))
  q0 <- vapply(tau, function(t) wq(y[!tr], 1 / (1 - ps[!tr]), t), numeric(1))
  list(qte = q1 - q0, q1 = q1, q0 = q0, tau = tau, n = length(y))
}

#' Parametric g-formula standardised means
#'
#' `E\[Y(a)\] = E\[E\[Y | A = a, L\]\]`, with an OLS outcome model including
#' treatment-covariate interactions. Mirrors `morie.fn.causmrop`.
#'
#' @param y Outcome.
#' @param a Binary 0/1 treatment.
#' @param l Covariate matrix or vector.
#' @return List with `EY1`, `EY0`, `ate`, `n`.
#' @references Robins JM (1986). *Mathematical Modelling* 7, 1393-1512.
#' @export
morie_g_formula <- function(y, a, l) {
  y <- as.numeric(y); a <- as.numeric(a)
  L <- as.matrix(l)
  n <- length(y)
  if (length(a) != n || nrow(L) != n) {
    stop("y, a, l must share their first dimension.", call. = FALSE)
  }
  if (!all(a %in% c(0, 1))) stop("a must be binary 0/1.", call. = FALSE)
  if (sum(a) == 0 || sum(a) == n) {
    stop("need both treated and untreated units.", call. = FALSE)
  }
  design <- function(av) cbind(1, av, L, av * L)
  b <- qr.coef(qr(design(a)), y)
  b[is.na(b)] <- 0
  ey1 <- mean(design(rep(1, n)) %*% b)
  ey0 <- mean(design(rep(0, n)) %*% b)
  list(EY1 = ey1, EY0 = ey0, ate = ey1 - ey0, n = n)
}


#' Granger causality F-test
#'
#' F-test of the x-lag block in an AR model for y. Mirrors
#' `morie.fn.ggrcst`.
#'
#' @param x Candidate cause series.
#' @param y Response series.
#' @param p Lag order.
#' @return List with `statistic`, `p_value`, `df`, `n`.
#' @references Granger CWJ (1969). *Econometrica* 37(3), 424-438.
#' @export
morie_granger_test <- function(x, y, p = 1L) {
  x <- as.numeric(x); y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must have equal length.", call. = FALSE)
  p <- as.integer(p)
  if (p < 1L) stop("p must be at least 1.", call. = FALSE)
  n <- length(y)
  m <- n - p
  dof2 <- m - 2L * p - 1L
  if (dof2 < 1L) {
    stop(sprintf("need at least %d observations for p = %d.", 3L * p + 2L, p),
         call. = FALSE)
  }
  lagmat <- function(v) {
    do.call(cbind, lapply(seq_len(p), function(j) v[(p - j + 1L):(n - j)]))
  }
  target <- y[(p + 1L):n]
  Xr <- cbind(1, lagmat(y))
  Xu <- cbind(Xr, lagmat(x))
  rss <- function(X) sum(stats::lm.fit(X, target)$residuals^2)
  rss_r <- rss(Xr); rss_u <- rss(Xu)
  if (rss_u <= 0) stop("unrestricted model fits exactly.", call. = FALSE)
  f <- ((rss_r - rss_u) / p) / (rss_u / dof2)
  list(
    statistic = f, p_value = stats::pf(f, p, dof2, lower.tail = FALSE),
    df = c(p, dof2), n = n
  )
}

#' Gaussian transfer entropy / Granger conditional mutual information
#'
#' `I = 0.5 * log(RSS_restricted / RSS_unrestricted)`, which equals
#' transfer entropy exactly for Gaussian processes. Mirrors
#' `morie.fn.granci` and the Gaussian arm of `morie.fn.trnfen`.
#'
#' @param x Source series.
#' @param y Target series.
#' @param lag History length.
#' @return List with `mi`, `statistic`, `p_value`, `df`, `n`.
#' @references Barnett L, Barrett AB, Seth AK (2009). *Physical Review
#'   Letters* 103(23), 238701.
#' @export
morie_transfer_entropy_gaussian <- function(x, y, lag = 1L) {
  g <- morie_granger_test(x, y, p = lag)
  n <- length(y); p <- as.integer(lag); m <- n - p
  # recompute the two RSS values from the same design as the F-test
  lagmat <- function(v) {
    do.call(cbind, lapply(seq_len(p), function(j) v[(p - j + 1L):(n - j)]))
  }
  target <- y[(p + 1L):n]
  Xr <- cbind(1, lagmat(as.numeric(y)))
  Xu <- cbind(Xr, lagmat(as.numeric(x)))
  rss_r <- sum(stats::lm.fit(Xr, target)$residuals^2)
  rss_u <- sum(stats::lm.fit(Xu, target)$residuals^2)
  mi <- 0.5 * log(rss_r / rss_u)
  lr <- 2 * m * mi
  list(
    mi = mi, statistic = lr,
    p_value = stats::pchisq(lr, p, lower.tail = FALSE), df = p, n = n
  )
}


#' Serial (chain) mediation X -> M1 -> M2 -> Y
#'
#' Four-path decomposition: direct, through M1, through M2, and the
#' serial path. Mirrors `morie.fn.medstg`.
#'
#' @param x Treatment.
#' @param m1,m2 Ordered mediators.
#' @param y Outcome.
#' @return List with `direct`, `via_m1`, `via_m2`, `serial`,
#'   `indirect_total`, `total`, `n`.
#' @references Hayes AF (2022). *Introduction to Mediation,
#'   Moderation, and Conditional Process Analysis*, 3rd ed., Ch. 5.
#' @export
morie_serial_mediation <- function(x, m1, m2, y) {
  x <- as.numeric(x); m1 <- as.numeric(m1)
  m2 <- as.numeric(m2); y <- as.numeric(y)
  if (length(unique(c(length(x), length(m1), length(m2), length(y)))) != 1L) {
    stop("x, m1, m2, y must have equal length.", call. = FALSE)
  }
  a1 <- unname(stats::coef(stats::lm(m1 ~ x))[2L])
  cm2 <- stats::coef(stats::lm(m2 ~ x + m1))
  a2 <- unname(cm2[2L]); d <- unname(cm2[3L])
  cy <- stats::coef(stats::lm(y ~ x + m1 + m2))
  cprime <- unname(cy[2L]); b1 <- unname(cy[3L]); b2 <- unname(cy[4L])
  via1 <- a1 * b1; via2 <- a2 * b2; serial <- a1 * d * b2
  list(
    direct = cprime, via_m1 = via1, via_m2 = via2, serial = serial,
    indirect_total = via1 + via2 + serial,
    total = cprime + via1 + via2 + serial, n = length(x)
  )
}

#' Cluster-robust (CR0) treatment effect
#'
#' Sandwich variance summed over clusters with the `G/(G-1)`
#' correction and a `t(G-1)` reference. Mirrors `morie.fn.clstcr`.
#'
#' @param y Outcome.
#' @param d Binary 0/1 treatment.
#' @param cluster Cluster identifier.
#' @return List with `estimate`, `se_cluster`, `se_naive`, `p_value`,
#'   `n_clusters`, `n`.
#' @references Cameron AC, Miller DL (2015). *Journal of Human
#'   Resources* 50(2), 317-372.
#' @export
morie_cluster_robust_effect <- function(y, d, cluster) {
  y <- as.numeric(y); d <- as.numeric(d)
  g <- as.factor(cluster)
  n <- length(y)
  if (length(d) != n || length(g) != n) {
    stop("y, d, cluster must have equal length.", call. = FALSE)
  }
  if (!all(d %in% c(0, 1))) stop("d must be binary 0/1.", call. = FALSE)
  G <- nlevels(g)
  if (G < 3L) stop("need at least 3 clusters.", call. = FALSE)
  X <- cbind(1, d)
  fit <- stats::lm.fit(X, y)
  u <- fit$residuals
  bread <- solve(crossprod(X))
  meat <- matrix(0, 2, 2)
  for (lev in levels(g)) {
    sel <- g == lev
    sc <- crossprod(X[sel, , drop = FALSE], u[sel])
    meat <- meat + tcrossprod(sc)
  }
  V <- bread %*% meat %*% bread * (G / (G - 1))
  se_c <- sqrt(V[2, 2])
  s2 <- sum(u^2) / (n - 2)
  se_n <- sqrt(s2 * bread[2, 2])
  est <- unname(fit$coefficients[2L])
  list(
    estimate = est, se_cluster = se_c, se_naive = se_n,
    p_value = 2 * stats::pt(abs(est / se_c), G - 1, lower.tail = FALSE),
    n_clusters = G, n = n
  )
}

#' Kendall partial tau controlling for z
#'
#' `tau_xy.z = (t_xy - t_xz * t_yz) / sqrt((1 - t_xz^2)(1 - t_yz^2))`.
#' A shrinkage diagnostic, not a conditional-independence test: it does
#' not reach zero under exact conditional independence. Mirrors
#' `morie.fn.gb1251`.
#'
#' @param x,y,z Numeric vectors of equal length.
#' @return List with `partial_tau`, `tau_xy`, `tau_xz`, `tau_yz`, `n`.
#' @references Gibbons JD, Chakraborti S (2011). *Nonparametric
#'   Statistical Inference*, 5th ed., Sec. 12.5.
#' @export
morie_partial_tau <- function(x, y, z) {
  x <- as.numeric(x); y <- as.numeric(y); z <- as.numeric(z)
  if (length(unique(c(length(x), length(y), length(z)))) != 1L) {
    stop("x, y, z must have equal length.", call. = FALSE)
  }
  if (length(x) < 4L) stop("need at least 4 observations.", call. = FALSE)
  txy <- stats::cor(x, y, method = "kendall")
  txz <- stats::cor(x, z, method = "kendall")
  tyz <- stats::cor(y, z, method = "kendall")
  den <- sqrt((1 - txz^2) * (1 - tyz^2))
  if (den <= 0) stop("a marginal tau with z is +/-1.", call. = FALSE)
  list(
    partial_tau = (txy - txz * tyz) / den,
    tau_xy = txy, tau_xz = txz, tau_yz = tyz, n = length(x)
  )
}
