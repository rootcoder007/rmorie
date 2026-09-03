# SPDX-License-Identifier: AGPL-3.0-or-later
#
# R mirror of morie.fn.{volkupiec,volcc,volgvar,voljmp,snht,acsamp,
# pacsam,cluseff} -- slice k05, tranche 1.
#
# Every one of these Python modules previously carried a verbatim
# one-sample Kolmogorov-Smirnov test against a fitted normal, or (for
# acsamp/pacsam) spearmanr(y, y), which is identically 1. The bodies
# were deleted and rewritten from the primary sources named in each
# function's @references.
#
# Anchors, so that agreement between the arms is not the only evidence:
#   Kupiec        LR_uc reproduced by hand from the Bernoulli likelihood
#   Christoffersen a clustered breach sequence must give small LR_uc and
#                 large LR_ind -- the discrimination the test exists for
#   SNHT          T_k = n S_k^2 / (k(n-k)) is an independent algebraic
#                 identity for the same quantity
#   ACF           r_0 == 1 exactly; agrees with morie_acf, written
#                 separately; hand arithmetic on a 4-point series
#   PACF          phi_11 == r_1 exactly; AR(1) PACF cuts off after lag 1
#   ICC           zero within-cluster variance gives rho == 1 exactly;
#                 identical clusters give the estimator's known floor
#                 -1/(n0 - 1)

#' Fisher-Yates driven by the package\'s Philox stream, swapping
#'
#' downward from n-1 and consuming one uniform per step, so the Python
#' mirror reproduces it exactly.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param seed Passed to \code{.morie_random_uniform}. Defaults to \code{0}.
#' @param stream Passed to \code{.morie_random_uniform}. Defaults to \code{0}.
#' @return The value of \code{idx}, as built in the body.
#' @export
#' @examples
#' res <- .morie_k05_permutation(n = 3L)
#' res
.morie_k05_permutation <- function(n, seed = 0, stream = 0) {
  # Fisher-Yates driven by the package's Philox stream, swapping
  # downward from n-1 and consuming one uniform per step, so the
  # Python mirror reproduces it exactly.
  idx <- seq_len(n) - 1L
  if (n < 2L) return(idx)
  u <- .morie_random_uniform(n - 1L, seed = seed, stream = stream)
  pos <- 1L
  for (i in seq.int(n - 1L, 1L)) {
    j <- as.integer(u[pos] * (i + 1))
    if (j > i) j <- i
    tmp <- idx[i + 1L]
    idx[i + 1L] <- idx[j + 1L]
    idx[j + 1L] <- tmp
    pos <- pos + 1L
  }
  idx
}

#' .morie_k05_hits
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_christoffersen_cc},
#' \code{morie_kupiec_var_test}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hits Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{h}, \code{t}, \code{n}.
#' @export
.morie_k05_hits <- function(hits) {
  h <- as.numeric(hits)
  if (length(h) < 2L) stop("need at least 2 observations in the hit sequence.", call. = FALSE)
  if (any(!(h %in% c(0, 1)))) stop("hits must be a 0/1 exceedance indicator sequence.", call. = FALSE)
  list(h = h, t = length(h), n = sum(h == 1))
}

#' .morie_k05_lr_uc
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_christoffersen_cc},
#' \code{morie_kupiec_var_test}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Numeric; passed to \code{log}.
#' @param t Numeric; combined arithmetically in the body.
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_k05_lr_uc <- function(p, t, n) {
  if (!(p > 0 && p < 1)) stop("alpha must lie strictly between 0 and 1.", call. = FALSE)
  phat <- n / t
  # log form throughout, so the 0^0 = 1 corner (n = 0 or n = t)
  # contributes exactly zero instead of log(0).
  ll_null <- (t - n) * log(1 - p) + if (n > 0) n * log(p) else 0
  ll_alt <- (if (n < t) (t - n) * log(1 - phat) else 0) + (if (n > 0) n * log(phat) else 0)
  -2 * (ll_null - ll_alt)
}

#' Kupiec unconditional-coverage LR test for VaR exceedances
#'
#' Under the null the exceedance indicator is i.i.d. Bernoulli(alpha),
#' so LR_uc = -2 log\[ (1-p)^(T-N) p^N / ((1-N/T)^(T-N) (N/T)^N ) \] is
#' asymptotically chi-square on 1 df. This tests coverage ONLY: a model
#' that puts every breach in one cluster passes it. Pair it with
#' \code{morie_christoffersen_cc}.
#'
#' @param hits 0/1 exceedance indicator.
#' @param alpha the VaR tail probability the model claims.
#' @return list: statistic, pvalue, n_obs, n_exceedances,
#'   expected_exceedances, rate, df, alpha, method.
#' @references Kupiec, P. H. (1995), \emph{Journal of Derivatives}
#'   3(2), 73-84. Cross-checked against rugarch's \code{.LR.uc}.
#' @examples
#' morie_kupiec_var_test(c(rep(0, 90), rep(1, 10)), 0.05)$statistic
#' @export
morie_kupiec_var_test <- function(hits, alpha = 0.05) {
  z <- .morie_k05_hits(hits)
  stat <- .morie_k05_lr_uc(alpha, z$t, z$n)
  list(statistic = stat, pvalue = stats::pchisq(stat, 1, lower.tail = FALSE),
       n_obs = z$t, n_exceedances = z$n,
       expected_exceedances = alpha * z$t, rate = z$n / z$t,
       df = 1, alpha = alpha,
       method = "Kupiec (1995) unconditional coverage LR test")
}

#' .morie_k05_lr_ind
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_christoffersen_cc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h A vector; its length is taken and its elements indexed.
#' @return A list with \code{stat}, \code{n00}, \code{n01}, \code{n10}, \code{n11},
#' \code{pi01}, \code{pi11}, \code{pi}.
#' @export
#' @examples
#' res <- .morie_k05_lr_ind(h = 0.5)
#' res
.morie_k05_lr_ind <- function(h) {
  a <- h[-length(h)]
  b <- h[-1]
  n00 <- sum(a == 0 & b == 0)
  n01 <- sum(a == 0 & b == 1)
  n10 <- sum(a == 1 & b == 0)
  n11 <- sum(a == 1 & b == 1)
  total <- n00 + n01 + n10 + n11
  pi0 <- if ((n00 + n01) > 0) n01 / (n00 + n01) else 0
  pi1 <- if ((n10 + n11) > 0) n11 / (n10 + n11) else 0
  pp <- (n01 + n11) / total
  tt <- function(k, q) if (k > 0) k * log(q) else 0
  ll_null <- tt(n00 + n10, 1 - pp) + tt(n01 + n11, pp)
  ll_alt <- tt(n00, 1 - pi0) + tt(n01, pi0) + tt(n10, 1 - pi1) + tt(n11, pi1)
  list(stat = -2 * (ll_null - ll_alt), n00 = n00, n01 = n01,
       n10 = n10, n11 = n11, pi01 = pi0, pi11 = pi1, pi = pp)
}

#' Christoffersen conditional-coverage test for VaR exceedances
#'
#' Splits "i.i.d. Bernoulli(alpha)" into its two testable halves and
#' adds them: LR_cc = LR_uc + LR_ind, chi-square on 2 df. LR_ind
#' compares a first-order Markov chain on the indicator against an
#' i.i.d. one, so it fires when breaches CLUSTER even if their overall
#' rate is right -- the failure Kupiec's test alone cannot see.
#'
#' @param hits 0/1 exceedance indicator, in time order.
#' @param alpha the VaR tail probability the model claims.
#' @return list: statistic (LR_cc), pvalue, lr_uc, pvalue_uc, lr_ind,
#'   pvalue_ind, transition counts n00..n11, df, method.
#' @references Christoffersen, P. F. (1998), \emph{International
#'   Economic Review} 39(4), 841-862. Cross-checked against rugarch's
#'   \code{.LR.cc}.
#' @examples
#' morie_christoffersen_cc(c(rep(0, 40), 1, 1, 1, 0, 0, 1, rep(0, 54)), 0.05)$lr_ind
#' @export
morie_christoffersen_cc <- function(hits, alpha = 0.05) {
  z <- .morie_k05_hits(hits)
  uc <- .morie_k05_lr_uc(alpha, z$t, z$n)
  ind <- .morie_k05_lr_ind(z$h)
  cc <- uc + ind$stat
  list(statistic = cc, pvalue = stats::pchisq(cc, 2, lower.tail = FALSE),
       n_obs = z$t, n_exceedances = z$n,
       lr_uc = uc, pvalue_uc = stats::pchisq(uc, 1, lower.tail = FALSE),
       lr_ind = ind$stat, pvalue_ind = stats::pchisq(ind$stat, 1, lower.tail = FALSE),
       n00 = ind$n00, n01 = ind$n01, n10 = ind$n10, n11 = ind$n11,
       pi01 = ind$pi01, pi11 = ind$pi11, pi = ind$pi,
       df = 2, alpha = alpha,
       method = "Christoffersen (1998) conditional coverage LR test")
}

#' Joint VaR backtest: coverage plus independence
#'
#' Runs the decomposition once and returns all three statistics, so a
#' caller can see WHICH half of the null a model fails: a wrong tail
#' probability is a calibration problem, clustered breaches are a
#' dynamics problem, and they call for different fixes.
#'
#' @param hits 0/1 exceedance indicator, in time order.
#' @param alpha the VaR tail probability the model claims.
#' @return list as \code{morie_christoffersen_cc}, plus lr_cc and pvalue_cc.
#' @references Christoffersen, P. F. (2003), \emph{Elements of Financial
#'   Risk Management}, Academic Press, ch. 8.
#' @examples
#' morie_var_backtest(c(rep(0, 90), rep(1, 10)), 0.05)$lr_cc
#' @export
morie_var_backtest <- function(hits, alpha = 0.05) {
  r <- morie_christoffersen_cc(hits, alpha)
  r$lr_cc <- r$statistic
  r$pvalue_cc <- r$pvalue
  r$method <- "Joint VaR backtest (Kupiec UC + Christoffersen IND/CC)"
  r
}

# theta for the bipower IV estimator (Barndorff-Nielsen & Shephard 2006);
# the test's denominator carries theta - 2 = pi^2/4 + pi - 5.
.MORIE_K05_THETA_BV <- pi^2 / 4 + pi - 3
# mu_{4/3}^{-3} for tripower quarticity
.MORIE_K05_MU43I3 <- (gamma(0.5) / (2^(2 / 3) * gamma(7 / 6)))^3

#' .morie_k05_bns_one
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_bns_jump_test}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r A vector; its length is taken.
#' @return A list with \code{rv}, \code{bpv}, \code{tpq}, \code{z}.
#' @export
.morie_k05_bns_one <- function(r) {
  n <- length(r)
  a <- abs(r)
  rv <- sum(r^2)
  bpv <- (pi / 2) * sum(a[-1] * a[-n])
  tri <- (a[seq.int(3, n)] * a[seq.int(2, n - 1)] * a[seq.int(1, n - 2)])^(4 / 3)
  tpq <- n * (n / (n - 2)) * .MORIE_K05_MU43I3 * sum(tri)
  denom <- sqrt((.MORIE_K05_THETA_BV - 2) * tpq / n)
  z <- if (denom > 0) (rv - bpv) / denom else NaN
  list(rv = rv, bpv = bpv, tpq = tpq, z = z)
}

#' Barndorff-Nielsen & Shephard jump test
#'
#' Realised variance converges to integrated variance PLUS jump
#' variation; bipower variation converges to integrated variance alone.
#' Their difference is therefore a jump detector, and the linear
#' statistic z = (RV - BPV) / sqrt((theta - 2) TP / N), with
#' theta = pi^2/4 + pi - 3, is asymptotically standard normal under
#' "no jumps". TP is realised tripower quarticity, itself jump-robust:
#' plain realised quarticity would let the jump being tested for
#' inflate the standard error and destroy the power. One-sided -- only
#' a positive z is evidence of jumps.
#'
#' @param r_intraday intraday log-returns; at least 4.
#' @param block_index optional day/block label per return; statistics
#'   never straddle a boundary, the overnight return is not diffusive.
#' @return list: statistic (z), pvalue, rv, bpv, tpq, jump_component,
#'   n_returns, days, method.
#' @references Barndorff-Nielsen, O. E. & Shephard, N. (2006),
#'   \emph{Journal of Financial Econometrics} 4(1), 1-30. Cross-checked
#'   against \code{BNSjumpTest} in the highfrequency package.
#' @examples
#' morie_bns_jump_test(c(0.01, -0.02, 0.015, -0.005, 0.02))$statistic
#' @export
morie_bns_jump_test <- function(r_intraday, block_index = NULL) {
  r <- as.numeric(r_intraday)
  if (length(r) < 4L) stop("need at least 4 intraday returns for tripower quarticity.", call. = FALSE)
  if (is.null(block_index)) {
    o <- .morie_k05_bns_one(r)
    return(list(statistic = o$z, pvalue = stats::pnorm(o$z, lower.tail = FALSE),
                rv = o$rv, bpv = o$bpv, tpq = o$tpq,
                jump_component = o$rv - o$bpv, n_returns = length(r), days = NULL,
                method = "BNS (2006) linear jump test, bipower vs realised variance"))
  }
  d <- as.vector(block_index)
  if (length(d) != length(r)) stop("block_index must have one entry per return.", call. = FALSE)
  days <- unique(d)
  out <- lapply(days, function(day) {
    v <- r[d == day]
    if (length(v) < 4L) stop("each block needs at least 4 returns.", call. = FALSE)
    .morie_k05_bns_one(v)
  })
  zs <- vapply(out, function(o) o$z, numeric(1))
  list(statistic = max(zs), pvalue = stats::pnorm(max(zs), lower.tail = FALSE),
       days = days, z = zs,
       pvalue_by_block = stats::pnorm(zs, lower.tail = FALSE),
       rv = vapply(out, function(o) o$rv, numeric(1)),
       bpv = vapply(out, function(o) o$bpv, numeric(1)),
       tpq = vapply(out, function(o) o$tpq, numeric(1)),
       jump_component = vapply(out, function(o) o$rv - o$bpv, numeric(1)),
       n_returns = length(r),
       method = "BNS (2006) linear jump test per block; statistic is the max z")
}

#' .morie_k05_tk
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_snht}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; indexed elementwise.
#' @param n Numeric; combined arithmetically in the body.
#' @param xbar Numeric; combined arithmetically in the body.
#' @param sigma Numeric; combined arithmetically in the body.
#' @return A list with \code{tk}, \code{s}.
#' @export
.morie_k05_tk <- function(x, n, xbar, sigma) {
  s <- cumsum((x[seq_len(n - 1L)] - xbar) / sigma)
  k <- seq_len(n - 1L)
  # the two halves sum to zero by construction, so z2 = -s/(n-k)
  list(tk = k * (s / k)^2 + (n - k) * (s / (n - k))^2, s = s)
}

#' Standard normal homogeneity test (Alexandersson 1986)
#'
#' Tests a single mean shift in a normal series by maximising
#' T_k = k zbar1^2 + (n-k) zbar2^2 over every split point. Because the
#' series is standardised by its OWN mean and sd, T is pivotal under
#' the null -- its law depends on n alone -- so the p-value comes from
#' simulating normal series of the same length. There is no usable
#' asymptotic form: T is a maximum over n-1 dependent statistics.
#'
#' @param x the series, complete observations only.
#' @param n_mc Monte Carlo replicates; 0 skips the p-value. Draws come
#'   from morie's Philox stream, so the Python mirror reproduces it.
#' @param seed seed for that stream.
#' @return list: statistic (T), pvalue, change_point, tk, n, n_mc, seed, method.
#' @references Alexandersson, H. (1986), \emph{Journal of Climatology}
#'   6, 661-675. Cross-checked against \code{snh.test} in the trend package.
#' @examples
#' morie_snht(c(0.1, -0.1, 0.05, -0.05, 10.1, 9.9, 10.05, 9.95), n_mc = 0)$change_point
#' @export
morie_snht <- function(x, n_mc = 1999, seed = 0) {
  v <- as.numeric(x)
  n <- length(v)
  if (n < 3L) stop("need at least 3 observations.", call. = FALSE)
  xbar <- mean(v)
  sigma <- stats::sd(v)
  if (!(sigma > 0)) stop("series is constant; no change point is identifiable.", call. = FALSE)
  tk <- .morie_k05_tk(v, n, xbar, sigma)$tk
  stat <- max(tk)
  kstar <- which.max(tk)
  pval <- NA_real_
  n_mc <- as.integer(n_mc)
  if (n_mc > 0L) {
    ge <- 0L
    for (j in seq_len(n_mc)) {
      z <- .morie_normal_quantile(.morie_random_uniform(n, seed = seed, stream = j))
      m <- mean(z)
      s <- stats::sd(z)
      if (!(s > 0)) next
      if (max(.morie_k05_tk(z, n, m, s)$tk) >= stat) ge <- ge + 1L
    }
    pval <- (ge + 1) / (n_mc + 1)
  }
  list(statistic = stat, pvalue = pval, change_point = kstar, tk = tk,
       n = n, n_mc = n_mc, seed = seed,
       method = "Alexandersson (1986) SNHT, Monte Carlo p-value")
}

#' .morie_k05_acvf
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_sample_acf},
#' \code{morie_sample_pacf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{mean}.
#' @param n Numeric; combined arithmetically in the body.
#' @param max_lag Passed to \code{:}.
#' @return A vector, from \code{vapply}.
#' @export
.morie_k05_acvf <- function(v, n, max_lag) {
  d <- v - mean(v)
  vapply(0:max_lag, function(k) sum(d[seq.int(k + 1L, n)] * d[seq_len(n - k)]) / n, numeric(1))
}

#' Sample autocorrelation function
#'
#' r_k = sum (y_t - ybar)(y_\{t-k\} - ybar) / sum (y_t - ybar)^2. Both
#' numerator and denominator divide by n, not n-k: that is the
#' biased-but-positive-semidefinite convention, the one R's
#' \code{acf} uses and the one \code{morie_acf} already uses. Chosen
#' deliberately -- dividing by n-k can yield an autocovariance
#' sequence that is not a valid covariance function, which breaks
#' Durbin-Levinson and every Yule-Walker fit downstream.
#'
#' @param y the series.
#' @param max_lag highest lag; clipped to n-1.
#' @return list: acf (index 1 is lag 0, acf\[1\] == 1), acvf, lags, n,
#'   max_lag, ci_bound, method.
#' @references Box, G. E. P. & Jenkins, G. M. (1976), \emph{Time Series
#'   Analysis: Forecasting and Control}, rev. ed., Holden-Day, sec. 2.1.
#' @examples
#' morie_sample_acf(c(1, 2, 3, 4), max_lag = 2)$acf[2]
#' @export
morie_sample_acf <- function(y, max_lag = 20) {
  v <- as.numeric(y)
  n <- length(v)
  if (n < 3L) stop("need at least 3 observations.", call. = FALSE)
  max_lag <- min(as.integer(max_lag), n - 1L)
  if (max_lag < 1L) stop("max_lag must be at least 1.", call. = FALSE)
  cc <- .morie_k05_acvf(v, n, max_lag)
  if (!(cc[1] > 0)) stop("series is constant; the autocorrelation is undefined.", call. = FALSE)
  list(acf = cc / cc[1], acvf = cc, lags = 0:max_lag, n = n, max_lag = max_lag,
       ci_bound = 1.96 / sqrt(n),
       method = "Sample autocorrelation function (divide-by-n convention)")
}

#' R is indexed from 1 for lag 0, so r\[k + 1\] is lag k
#'
#' A step of the k05_tranche1 implementation. Called by \code{morie_sample_pacf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r A vector; indexed elementwise.
#' @param max_lag A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{phi}, as built in the body.
#' @export
.morie_k05_durbin_levinson <- function(r, max_lag) {
  # r is indexed from 1 for lag 0, so r[k + 1] is lag k.
  phi <- numeric(max_lag)
  prev <- numeric(0)
  for (k in seq_len(max_lag)) {
    num <- r[k + 1L]
    den <- 1
    if (k > 1L) {
      num <- num - sum(prev * r[seq.int(k, 2L)])
      den <- den - sum(prev * r[seq.int(2L, k)])
    }
    if (den == 0) stop("singular Durbin-Levinson step; series is degenerate.", call. = FALSE)
    kk <- num / den
    cur <- if (k > 1L) c(prev - kk * rev(prev), kk) else kk
    phi[k] <- kk
    prev <- cur
  }
  phi
}

#' Sample partial autocorrelation function
#'
#' phi_kk is the correlation between y_t and y_\{t-k\} once the
#' intervening lags are projected out, obtained from the sample
#' autocorrelations by the Durbin-Levinson recursion, which solves the
#' Yule-Walker system in O(k^2) rather than inverting a Toeplitz matrix
#' at every order. By construction phi_11 == r_1 exactly, and for an
#' AR(p) process the PACF cuts off after lag p.
#'
#' @param y the series.
#' @param max_lag highest lag; clipped to n-1.
#' @return list: pacf (index 1 is lag 1), lags, acf, n, max_lag,
#'   ci_bound, method.
#' @references Box, G. E. P. & Jenkins, G. M. (1976), sec. 3.2.6;
#'   Durbin, J. (1960), \emph{Revue de l'Institut International de
#'   Statistique} 28, 233-244.
#' @examples
#' morie_sample_pacf(c(1, 3, 2, 7, 6, 8, 5, 9), max_lag = 3)$pacf[1]
#' @export
morie_sample_pacf <- function(y, max_lag = 20) {
  v <- as.numeric(y)
  n <- length(v)
  if (n < 3L) stop("need at least 3 observations.", call. = FALSE)
  max_lag <- min(as.integer(max_lag), n - 1L)
  if (max_lag < 1L) stop("max_lag must be at least 1.", call. = FALSE)
  cc <- .morie_k05_acvf(v, n, max_lag)
  if (!(cc[1] > 0)) stop("series is constant; the autocorrelation is undefined.", call. = FALSE)
  r <- cc / cc[1]
  list(pacf = .morie_k05_durbin_levinson(r, max_lag), lags = seq_len(max_lag),
       acf = r, n = n, max_lag = max_lag, ci_bound = 1.96 / sqrt(n),
       method = "Sample PACF via the Durbin-Levinson recursion")
}

#' Intracluster correlation and Kish's design effect
#'
#' One-way random-effects decomposition: with the unequal-size
#' correction n0 = (N - sum n_i^2 / N) / (a - 1), the between-cluster
#' variance is (MSB - MSW)/n0 and rho = var_a / (var_a + MSW). Kish's
#' design effect follows as DEFF = 1 + (nbar - 1) rho. rho can come out
#' negative when within-cluster spread exceeds between; that is
#' reported as computed rather than clipped, because clipping hides a
#' badly specified clustering.
#'
#' @param y response, one value per unit.
#' @param cluster cluster label per unit.
#' @return list: rho, deff, msb, msw, n0, var_between, n_clusters,
#'   n_obs, cluster_sizes, mean_cluster_size, effective_n, method.
#' @references Kish, L. (1965), \emph{Survey Sampling}, Wiley, sec. 5.4.
#'   Cross-checked against \code{ICCbare} in the ICC package.
#' @examples
#' morie_icc_rho(c(1, 1, 1, 5, 5, 5, 9, 9, 9), c(0, 0, 0, 1, 1, 1, 2, 2, 2))$rho
#' @export
morie_icc_rho <- function(y, cluster) {
  v <- as.numeric(y)
  g <- as.vector(cluster)
  if (length(v) != length(g)) stop("y and cluster must have the same length.", call. = FALSE)
  labs <- unique(g)
  a <- length(labs)
  n_tot <- length(v)
  if (a < 2L) stop("need at least 2 clusters.", call. = FALSE)
  if (n_tot <= a) stop("need more observations than clusters.", call. = FALSE)
  grand <- mean(v)
  sizes <- vapply(labs, function(l) sum(g == l), numeric(1))
  means <- vapply(labs, function(l) mean(v[g == l]), numeric(1))
  ssb <- sum(sizes * (means - grand)^2)
  ssw <- sum(vapply(seq_along(labs), function(i) sum((v[g == labs[i]] - means[i])^2), numeric(1)))
  msb <- ssb / (a - 1)
  msw <- ssw / (n_tot - a)
  n0 <- (n_tot - sum(sizes^2) / n_tot) / (a - 1)
  var_a <- (msb - msw) / n0
  denom <- var_a + msw
  if (denom == 0) stop("zero total variance; rho is undefined.", call. = FALSE)
  rho <- var_a / denom
  nbar <- n_tot / a
  deff <- 1 + (nbar - 1) * rho
  list(rho = rho, deff = deff, msb = msb, msw = msw, n0 = n0,
       var_between = var_a, n_clusters = a, n_obs = n_tot,
       cluster_sizes = sizes, mean_cluster_size = nbar,
       effective_n = if (deff > 0) n_tot / deff else NaN,
       method = "One-way ANOVA intracluster correlation; Kish design effect")
}
