# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Bootstrap/jackknife resampling and range volatility.
#
# R mirror of morie.fn.{btiid,btvb,btbias,btjkn,bt632,btoob,
# btciratio,volpark,volgkr,volharm,volnois}.
#
# The oracles are closed forms wherever one exists: the jackknife
# variance of the mean is s^2/n EXACTLY, the MLE variance's bias is
# exactly -s^2/n and both resamplers must find its SIGN, the
# out-of-bag fraction is 1 - 0.632, Parkinson's 1/(4 log 2) makes the
# range estimator unbiased for driftless Brownian motion, and the ZMA
# decomposition E[RV] = IV + 2n eps^2 is what the noise estimator
# reads off.

.btv_boot_reps <- function(x, stat, B, seed) {
  n <- if (is.matrix(x)) nrow(x) else length(x)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  B <- as.integer(B)
  if (is.na(B) || B < 2L) stop("need at least 2 replicates.", call. = FALSE)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  vapply(seq_len(B), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    as.numeric(stat(if (is.matrix(x)) x[idx, , drop = FALSE] else x[idx]))
  }, numeric(1))
}


#' Nonparametric IID bootstrap of a statistic
#'
#' Efron (1979): B resamples with replacement, the statistic on each.
#' The replicates estimate the statistic's law under the EMPIRICAL
#' distribution, not under F -- and where that fails it fails without
#' warning, the sample maximum being the canonical case (its
#' bootstrap law puts mass 0.632 on the sample max itself). The
#' caveat is in the output.
#'
#' @param x sample.
#' @param stat the statistic, applied to a resample.
#' @param B replicates.
#' @param seed resampling seed.
#' @return list: replicates, estimate, se, bias, ci_percentile, B,
#'   n, consistency_caveat, method.
#' @references Efron (1979), *Annals of Statistics* 7:1-26.
#' @examples
#' morie_bt_iid(stats::rnorm(50), mean, B = 100)$se
#' @export
morie_bt_iid <- function(x, stat, B = 1000L, seed = 0) {
  d <- as.numeric(x)
  reps <- .btv_boot_reps(d, stat, B, seed)
  est <- as.numeric(stat(d))
  ci <- unname(stats::quantile(reps, c(0.025, 0.975), type = 7L))
  list(
    replicates = reps, estimate = est, se = stats::sd(reps),
    bias = mean(reps) - est, ci_percentile = ci,
    B = length(reps), n = length(d),
    consistency_caveat = paste(
      "the bootstrap estimates the statistic's",
      "distribution under the EMPIRICAL law;",
      "for statistics it is inconsistent for --",
      "the sample maximum above all -- it fails",
      "without warning"
    ),
    method = "Efron (1979) nonparametric IID bootstrap"
  )
}


#' Bootstrap variance from replicates
#'
#' `var(replicates)` with the B - 1 denominator (Efron-Tibshirani
#' 1993, Eq. 6.5) -- the replicates are centred at their own mean.
#' Takes REPLICATES, not data: the second half of a bootstrap someone
#' has already run, so expensive statistics resample once.
#'
#' @param theta_b bootstrap replicates.
#' @return list: value, se, mean_replicate, B, denominator, method.
#' @references Efron and Tibshirani (1993), *An Introduction to the
#'   Bootstrap*, Ch. 6; Efron (1979).
#' @examples
#' morie_bt_var(stats::rnorm(200))$se
#' @export
morie_bt_var <- function(theta_b) {
  r <- as.numeric(theta_b)
  if (length(r) < 2L) stop("need at least 2 replicates.", call. = FALSE)
  if (any(!is.finite(r))) {
    stop(paste(
      "every replicate must be finite; a failed refit should be",
      "dropped before this point, and dropping it changes the",
      "estimand."
    ), call. = FALSE)
  }
  v <- stats::var(r)
  list(
    value = v, se = sqrt(v), mean_replicate = mean(r),
    B = length(r), denominator = "B - 1",
    denominator_note = paste(
      "B - 1, not B: the replicates are centred",
      "at their own mean (Efron-Tibshirani 6.5)"
    ),
    method = "Bootstrap variance from replicates, Efron-Tibshirani (6.5)"
  )
}


#' Bootstrap bias estimate and correction
#'
#' `bias = mean(replicates) - estimate`, corrected value
#' `2 estimate - mean(replicates)` -- on the OPPOSITE side of the
#' estimate from the replicate mean; using the replicate mean itself
#' doubles the bias instead of removing it, and the direction is
#' tested on the MLE variance, whose bias is exactly -s^2/n.
#' Efron-Tibshirani's Ch. 10 warning travels in the output: the
#' correction adds variance and can raise the MSE.
#'
#' @param theta_hat the statistic on the original data.
#' @param theta_b bootstrap replicates.
#' @return list: bias, corrected, estimate, mean_replicate,
#'   relative_bias, B, correction_warning, method.
#' @references Efron (1979); Efron and Tibshirani (1993), Ch. 10.
#' @examples
#' morie_bt_bias(1, c(1.1, 1.2, 0.9))$corrected
#' @export
morie_bt_bias <- function(theta_hat, theta_b) {
  th <- as.numeric(theta_hat)
  r <- as.numeric(theta_b)
  if (length(r) < 2L) stop("need at least 2 replicates.", call. = FALSE)
  bias <- mean(r) - th
  list(
    bias = bias, corrected = th - bias, estimate = th,
    mean_replicate = mean(r),
    relative_bias = if (th != 0) bias / th else Inf,
    B = length(r),
    direction_note = paste(
      "the corrected value is 2 theta_hat -",
      "mean(reps), on the OPPOSITE side of theta_hat",
      "from the replicate mean"
    ),
    correction_warning = paste(
      "correction adds variance and can raise",
      "the MSE (Efron-Tibshirani Ch. 10); report",
      "the bias, correct only when it dominates"
    ),
    method = "Bootstrap bias = mean(replicates) - estimate"
  )
}


#' Leave-one-out jackknife
#'
#' Quenouille (1949), Tukey (1958): the n leave-one-out values, bias
#' `(n-1)(mean(loo) - estimate)` and variance
#' `(n-1)/n * sum((loo - mean(loo))^2)`. The (n-1) factors ARE the
#' estimator -- the leave-one-out values huddle n-1 times closer than
#' independent replicates, and the inflation undoes exactly that; for
#' the mean the jackknife variance equals s^2/n EXACTLY. Inconsistent
#' for non-smooth statistics (the median is the canonical failure,
#' Efron 1979 Sec. 3), and the output says so.
#'
#' @param x sample.
#' @param stat the statistic; must be smooth for the variance to
#'   mean anything.
#' @return list: leave_one_out, estimate, bias, corrected, variance,
#'   se, pseudovalues, n, smoothness_caveat, method.
#' @references Quenouille (1949), *JRSS-B* 11:68-84; Tukey (1958);
#'   Efron (1979), Sec. 3.
#' @examples
#' morie_bt_jackknife(stats::rnorm(30), mean)$variance
#' @export
morie_bt_jackknife <- function(x, stat) {
  d <- as.numeric(x)
  n <- length(d)
  if (n < 3L) {
    stop(sprintf("need at least 3 observations, got %d.", n),
      call. = FALSE
    )
  }
  th <- as.numeric(stat(d))
  loo <- vapply(seq_len(n), function(i) as.numeric(stat(d[-i])), numeric(1))
  m <- mean(loo)
  bias <- (n - 1) * (m - th)
  v <- (n - 1) / n * sum((loo - m)^2)
  list(
    leave_one_out = loo, estimate = th, bias = bias,
    corrected = th - bias, variance = v, se = sqrt(v),
    pseudovalues = n * th - (n - 1) * loo,
    inflation_note = paste(
      "both (n-1) factors undo the leave-one-out",
      "values' huddling; dropping either understates",
      "by a factor of order n"
    ),
    smoothness_caveat = paste(
      "inconsistent for non-smooth statistics --",
      "the median is the canonical failure (Efron",
      "1979 Sec. 3); use the bootstrap there"
    ),
    n = n,
    method = "Leave-one-out jackknife (Quenouille 1949; Tukey 1958)"
  )
}


#' .632 error estimator -- alias entry point
#'
#' One .632 implementation across the library: the computation is
#' [morie_esl_oob_632()], which carries the full derivation and the
#' reason the second argument must be the OUT-OF-BAG error.
#'
#' @param err_app apparent (resubstitution) error.
#' @param err_oob out-of-bag / leave-one-out bootstrap error.
#' @param gamma optional no-information rate for .632+.
#' @return the [morie_esl_oob_632()] list plus alias_of.
#' @references Efron and Tibshirani (1997), *JASA* 92:548-560.
#' @examples
#' morie_bt_632(0, 0.5, gamma = 0.5)$err_632_plus
#' @export
morie_bt_632 <- function(err_app, err_oob, gamma = NULL) {
  out <- morie_esl_oob_632(err_app, err_oob, gamma = gamma)
  out$alias_of <- "morie_esl_oob_632"
  out
}


#' Out-of-bag error for a bootstrap-aggregated predictor
#'
#' Efron-Tibshirani (1997), Breiman (1996): score each observation
#' ONLY with the fits whose bootstrap sample excluded it. Honesty is
#' structural -- no point is ever scored by a fit that saw it -- and
#' each point is out of bag for about 36.8% of replicates, the 0.632
#' complement, which the tests assert. Points in every bag are
#' dropped and counted; a large count means B is too small.
#'
#' @param x,y predictors and response.
#' @param fit_fn `function(x_boot, y_boot)` returning a fitted object.
#' @param predict_fn `function(fitted, x_new)` returning predictions.
#' @param B replicates.
#' @param loss elementwise loss; squared error when `NULL`.
#' @param seed resampling seed.
#' @return list: err_oob, err_apparent, per_observation, n_dropped,
#'   oob_fraction, B, n, method.
#' @references Efron and Tibshirani (1997), *JASA* 92:548-560;
#'   Breiman (1996), "Out-of-bag estimation", UC Berkeley.
#' @examples
#' x <- matrix(stats::rnorm(60), 30)
#' y <- x[, 1] + stats::rnorm(30)
#' fit <- function(Xa, ya) qr.coef(qr(cbind(1, Xa)), ya)
#' prd <- function(b, Xn) as.numeric(cbind(1, Xn) %*% b)
#' morie_bt_oob(x, y, fit, prd, B = 20)$err_oob
#' @export
morie_bt_oob <- function(x, y, fit_fn, predict_fn, B = 100L, loss = NULL,
                         seed = 0) {
  A <- as.matrix(x)
  storage.mode(A) <- "double"
  yv <- as.numeric(y)
  if (nrow(A) != length(yv)) A <- t(A)
  n <- length(yv)
  if (n < 4L) {
    stop(sprintf("need at least 4 observations, got %d.", n),
      call. = FALSE
    )
  }
  Bn <- as.integer(B)
  if (is.na(Bn) || Bn < 1L) {
    stop("need at least one replicate.",
      call. = FALSE
    )
  }
  L <- if (is.null(loss)) function(a, b) (a - b)^2 else loss
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  loss_sum <- numeric(n)
  oob_cnt <- numeric(n)
  for (b in seq_len(Bn)) {
    idx <- sample.int(n, n, replace = TRUE)
    fitted <- fit_fn(A[idx, , drop = FALSE], yv[idx])
    out <- setdiff(seq_len(n), idx)
    if (length(out)) {
      pred <- as.numeric(predict_fn(fitted, A[out, , drop = FALSE]))
      loss_sum[out] <- loss_sum[out] + as.numeric(L(yv[out], pred))
      oob_cnt[out] <- oob_cnt[out] + 1
    }
  }
  keep <- oob_cnt > 0
  per_i <- rep(NA_real_, n)
  per_i[keep] <- loss_sum[keep] / oob_cnt[keep]
  full <- fit_fn(A, yv)
  list(
    err_oob = if (any(keep)) mean(per_i[keep]) else NA_real_,
    err_apparent = mean(as.numeric(L(yv, as.numeric(
      predict_fn(full, A)
    )))),
    per_observation = per_i, n_dropped = sum(!keep),
    oob_fraction = mean(oob_cnt) / Bn,
    honesty_note = paste(
      "no observation is ever scored by a fit that",
      "saw it; each point is out of bag for about",
      "36.8% of replicates"
    ),
    B = Bn, n = n,
    method = "Out-of-bag error (Efron-Tibshirani 1997; Breiman 1996)"
  )
}


#' Percentile bootstrap CI for a ratio
#'
#' Davison-Hinkley (1997): read the interval off the resampled
#' ratios. Ratios are where the delta method quietly fails (skewness,
#' small denominators, Fieller half-lines); the percentile interval
#' needs none of that but inherits the caveat that a denominator
#' resampling near zero produces wild replicates -- the
#' small-denominator fraction is reported. `paired` is a MODELLING
#' statement: paired data resample as pairs to keep the dependence,
#' independent samples separately, and getting it wrong biases the
#' width in whichever direction the dependence points.
#'
#' @param x,y the two samples.
#' @param stat_x,stat_y numerator and denominator statistics; the
#'   mean when `NULL`.
#' @param B replicates, at least 100.
#' @param alpha miss probability.
#' @param seed resampling seed.
#' @param paired resample pairs rather than independently.
#' @return list: ratio, ci, replicates, se,
#'   small_denominator_fraction, paired, B, alpha, n_x, n_y, method.
#' @references Davison and Hinkley (1997), *Bootstrap Methods and
#'   their Application*, Chs. 2-3, 5; Fieller (1954).
#' @examples
#' morie_bt_ci_ratio(stats::rnorm(50, 2), stats::rnorm(50, 1),
#'   B = 200
#' )$ci
#' @export
morie_bt_ci_ratio <- function(x, y, stat_x = NULL, stat_y = NULL,
                              B = 2000L, alpha = 0.05, seed = 0,
                              paired = FALSE) {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  sx <- if (is.null(stat_x)) mean else stat_x
  sy <- if (is.null(stat_y)) mean else stat_y
  a <- as.numeric(alpha)
  if (a <= 0 || a >= 1) {
    stop(sprintf("alpha must lie in (0, 1), got %g.", a),
      call. = FALSE
    )
  }
  Bn <- as.integer(B)
  if (is.na(Bn) || Bn < 100L) {
    stop(sprintf(
      "need at least 100 replicates for quantiles, got %s.",
      format(B)
    ), call. = FALSE)
  }
  if (isTRUE(paired) && length(xv) != length(yv)) {
    stop("paired resampling needs equal-length samples.", call. = FALSE)
  }
  den0 <- as.numeric(sy(yv))
  if (den0 == 0) {
    stop("the denominator statistic is zero on the data.",
      call. = FALSE
    )
  }
  ratio <- as.numeric(sx(xv)) / den0
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  reps <- numeric(Bn)
  small <- 0L
  scale <- abs(den0)
  for (b in seq_len(Bn)) {
    if (isTRUE(paired)) {
      idx <- sample.int(length(xv), length(xv), replace = TRUE)
      num <- as.numeric(sx(xv[idx]))
      den <- as.numeric(sy(yv[idx]))
    } else {
      num <- as.numeric(sx(xv[sample.int(length(xv), length(xv),
        replace = TRUE
      )]))
      den <- as.numeric(sy(yv[sample.int(length(yv), length(yv),
        replace = TRUE
      )]))
    }
    if (abs(den) < 1e-3 * scale) small <- small + 1L
    reps[b] <- if (den != 0) num / den else NA_real_
  }
  good <- reps[is.finite(reps)]
  ci <- unname(stats::quantile(good, c(a / 2, 1 - a / 2), type = 7L))
  list(
    ratio = ratio, ci = ci, replicates = reps, se = stats::sd(good),
    small_denominator_fraction = small / Bn,
    why_bootstrap = paste(
      "a ratio's distribution is skewed and the",
      "delta method breaks down for small",
      "denominators; the percentile interval reads",
      "the quantiles directly"
    ),
    paired = isTRUE(paired),
    pairing_note = paste(
      "paired data must be resampled as PAIRS to keep",
      "the dependence; this is a modelling statement,",
      "not a convenience flag"
    ),
    B = Bn, alpha = a, n_x = length(xv), n_y = length(yv),
    method = "Percentile bootstrap CI for a ratio (Davison-Hinkley 1997)"
  )
}


#' Parkinson range volatility estimator
#'
#' Parkinson (1980), Eq. (4): `sigma^2 = mean((log(H/L))^2)/(4 log 2)`.
#' The constant is not a fudge: E\[(log range)^2\] = 4 log2 sigma^2 for
#' driftless Brownian motion, and dividing by it makes the estimator
#' unbiased. The range buys about a 4.9-fold variance reduction over
#' close-to-close -- measured in the tests, not quoted. Drift
#' inflates the range (the derivation assumes none) and discrete
#' sampling understates the true extremes; both biases are named.
#'
#' @param high,low per-bar highs and lows.
#' @param periods_per_year optional annualisation factor.
#' @return list: variance, sigma, sigma_annualised, constant,
#'   efficiency_vs_close, n, method.
#' @references Parkinson (1980), *Journal of Business* 53:61-65.
#' @examples
#' morie_vol_parkinson(c(101, 102), c(99, 100))$sigma
#' @export
morie_vol_parkinson <- function(high, low, periods_per_year = NULL) {
  H <- as.numeric(high)
  L <- as.numeric(low)
  if (length(H) != length(L)) {
    stop(sprintf(
      "high has %d entries and low has %d.", length(H),
      length(L)
    ), call. = FALSE)
  }
  n <- length(H)
  if (n < 2L) {
    stop(sprintf("need at least 2 bars, got %d.", n),
      call. = FALSE
    )
  }
  if (any(L <= 0)) stop("prices must be positive.", call. = FALSE)
  if (any(H < L)) {
    stop("high must be at least low in every bar.",
      call. = FALSE
    )
  }
  const <- 1 / (4 * log(2))
  v <- const * mean(log(H / L)^2)
  s <- sqrt(v)
  list(
    variance = v, sigma = s,
    sigma_annualised = if (is.null(periods_per_year)) {
      NULL
    } else {
      s * sqrt(as.numeric(periods_per_year))
    },
    constant = const,
    constant_note = paste(
      "1/(4 log 2): E[(log range)^2] = 4 log2",
      "sigma^2 for driftless Brownian motion"
    ),
    efficiency_vs_close = 4.9,
    drift_bias = "drift inflates the range, so trending periods read high",
    n = n,
    method = "Parkinson (1980) range estimator, 1/(4 log 2) mean squared log-range"
  )
}


#' Garman-Klass OHLC volatility estimator
#'
#' Garman-Klass (1980), Eq. (20):
#' `0.5 (log H/L)^2 - (2 log2 - 1)(log C/O)^2` per bar, averaged.
#' The open-close term enters NEGATIVELY: given the range, a large
#' open-to-close move signals trend rather than volatility, and the
#' minimum-variance combination partials it out (efficiency ~7.4 vs
#' close-to-close). A single bar can go negative; if the AVERAGE
#' does, the driftless-diffusion model does not describe these bars,
#' and that is an error rather than a clipped zero.
#'
#' @param open_,high,low,close per-bar OHLC.
#' @param periods_per_year optional annualisation factor.
#' @return list: variance, sigma, sigma_annualised, range_term,
#'   openclose_term, efficiency_vs_close, negative_bar_fraction, n,
#'   method.
#' @references Garman and Klass (1980), *Journal of Business*
#'   53:67-78, Eq. (20).
#' @examples
#' morie_vol_garman_klass(
#'   c(100, 101), c(102, 103), c(99, 100),
#'   c(101, 102)
#' )$sigma
#' @export
morie_vol_garman_klass <- function(open_, high, low, close,
                                   periods_per_year = NULL) {
  O <- as.numeric(open_)
  H <- as.numeric(high)
  L <- as.numeric(low)
  C <- as.numeric(close)
  n <- length(O)
  if (length(H) != n || length(L) != n || length(C) != n) {
    stop("open, high, low and close must share a length.", call. = FALSE)
  }
  if (n < 2L) {
    stop(sprintf("need at least 2 bars, got %d.", n),
      call. = FALSE
    )
  }
  if (any(L <= 0)) stop("prices must be positive.", call. = FALSE)
  if (any(H < L | O > H | O < L | C > H | C < L)) {
    stop("each bar needs low <= open, close <= high.", call. = FALSE)
  }
  hl <- log(H / L)^2
  co <- log(C / O)^2
  per_bar <- 0.5 * hl - (2 * log(2) - 1) * co
  v <- mean(per_bar)
  if (v <= 0) {
    stop(paste(
      "the average Garman-Klass variance is not positive: the",
      "driftless-diffusion model this estimator assumes does not",
      "describe these bars."
    ), call. = FALSE)
  }
  s <- sqrt(v)
  list(
    variance = v, sigma = s,
    sigma_annualised = if (is.null(periods_per_year)) {
      NULL
    } else {
      s * sqrt(as.numeric(periods_per_year))
    },
    range_term = mean(0.5 * hl),
    openclose_term = mean((2 * log(2) - 1) * co),
    negative_sign_note = paste(
      "the open-close term enters NEGATIVELY:",
      "given the range, a large open-to-close",
      "move signals trend, not volatility"
    ),
    efficiency_vs_close = 7.4,
    negative_bar_fraction = mean(per_bar < 0),
    n = n,
    method = "Garman-Klass (1980) Eq. (20)"
  )
}


#' Harmonic-mean aggregation of per-period volatilities
#'
#' `n / sum(1/sigma)`, reported with the geometric, arithmetic and
#' rms aggregates and the AM-GM-HM inequality asserted. Which to use
#' is the substance: integrated variance aggregates ARITHMETICALLY on
#' variances (the rms here; Andersen-Bollerslev-Diebold-Labys 2003),
#' while the harmonic mean is right when the quantity enters through
#' its RECIPROCAL (precision weights, rates) -- and it is dominated
#' by the SMALLEST values, robust to spuriously large sigmas and
#' worst-case for spuriously small ones.
#'
#' @param sigma positive per-period volatilities.
#' @return list: harmonic, geometric, arithmetic, rms,
#'   inequality_holds, which_to_use, n, method.
#' @references Andersen, Bollerslev, Diebold and Labys (2003),
#'   *Econometrica* 71:579-625.
#' @examples
#' morie_vol_harmonic(c(0.1, 0.2, 0.4))$harmonic
#' @export
morie_vol_harmonic <- function(sigma) {
  s <- as.numeric(sigma)
  n <- length(s)
  if (n < 1L) stop("need at least one volatility.", call. = FALSE)
  if (any(s <= 0)) {
    stop(paste(
      "volatilities must be positive; a zero makes the harmonic",
      "mean zero regardless of everything else."
    ), call. = FALSE)
  }
  hm <- n / sum(1 / s)
  gm <- exp(mean(log(s)))
  am <- mean(s)
  rms <- sqrt(mean(s^2))
  list(
    harmonic = hm, geometric = gm, arithmetic = am, rms = rms,
    inequality_holds = hm <= gm + 1e-12 && gm <= am + 1e-12,
    which_to_use = paste(
      "arithmetic on VARIANCES (the rms here) for",
      "aggregating sub-period volatility into a",
      "total; harmonic when the quantity enters",
      "through its reciprocal"
    ),
    contamination_asymmetry = paste(
      "the harmonic mean is dominated by",
      "the SMALLEST values: robust to",
      "spuriously large sigmas, worst-case",
      "for spuriously small ones"
    ),
    n = n,
    method = "Harmonic / geometric / arithmetic / rms volatility aggregates"
  )
}


#' Microstructure noise variance from high-frequency returns
#'
#' Zhang-Mykland-Ait-Sahalia (2005), Ait-Sahalia-Mykland-Zhang
#' (2005): with observed price = efficient price + iid noise,
#' `E\[RV_all\] = IV + 2n E\[eps^2\]`, so at the finest grid the noise
#' dominates and `E\[eps^2\] = RV_all/(2n)`. That divergence is the
#' volatility signature plot -- naive RV gets WORSE as data get
#' finer. The two-scale estimator
#' `IV_TS = RV_subsampled - (nbar/n) RV_all` recovers the integrated
#' variance, and both are returned so the size of the correction is
#' visible.
#'
#' @param r_intraday intraday log-returns at the finest sampling.
#' @param K subsampling factor; `round(n^(2/3))` (the ZMA rate) when
#'   `NULL`.
#' @return list: noise_variance, noise_sd, rv_all, rv_subsampled,
#'   iv_two_scale, K, noise_share_of_rv, n, method.
#' @references Zhang, Mykland and Ait-Sahalia (2005), *JASA*
#'   100:1394-1411; Ait-Sahalia, Mykland and Zhang (2005), *RFS*
#'   18:351-416.
#' @examples
#' morie_vol_noise(stats::rnorm(100, sd = 1e-3), K = 5)$noise_variance
#' @export
morie_vol_noise <- function(r_intraday, K = NULL) {
  r <- as.numeric(r_intraday)
  n <- length(r)
  if (n < 30L) {
    stop(sprintf("need at least 30 intraday returns, got %d.", n),
      call. = FALSE
    )
  }
  rv_all <- sum(r^2)
  noise_var <- rv_all / (2 * n)
  KK <- if (is.null(K)) {
    max(2L, as.integer(round(n^(2 / 3))))
  } else {
    as.integer(K)
  }
  if (KK < 2L || KK > n %/% 2L) {
    stop(sprintf("K must lie in 2..%d, got %d.", n %/% 2L, KK),
      call. = FALSE
    )
  }
  p <- c(0, cumsum(r))
  rvs <- counts <- numeric(0)
  for (off in seq_len(KK)) {
    sub <- p[seq(off, length(p), by = KK)]
    if (length(sub) >= 2L) {
      rvs <- c(rvs, sum(diff(sub)^2))
      counts <- c(counts, length(sub) - 1L)
    }
  }
  rv_avg <- mean(rvs)
  nbar <- mean(counts)
  list(
    noise_variance = noise_var, noise_sd = sqrt(noise_var),
    rv_all = rv_all, rv_subsampled = rv_avg,
    iv_two_scale = rv_avg - (nbar / n) * rv_all, K = KK,
    noise_share_of_rv = 2 * n * noise_var / rv_all,
    signature_note = paste(
      "E[RV_all] = IV + 2n E[eps^2]: at the finest",
      "grid the noise term dominates, which is why",
      "naive RV gets WORSE as sampling gets finer"
    ),
    n = n,
    method = "Noise variance RV_all/(2n) and two-scale IV (ZMA 2005)"
  )
}
