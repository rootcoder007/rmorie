# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Extreme-value estimation.
#
# R mirror of morie.fn.{evhill,hillEst,evpick,evdedh,evgevlm,evgevp2,
# evgpdpw,evextidx,evextint,evextsl,evmadog} and the _evt helper.
#
# Two recurring objects. The EXTREME-VALUE INDEX xi: the GEV shape
# for block maxima, the GPD shape for excesses, 1/alpha for a
# regularly varying tail -- one parameter, three guises. And the
# EXTREMAL INDEX theta in (0, 1]: the reciprocal mean cluster size of
# exceedances in a stationary series. Sign convention, stated because
# it burns people: Hosking's k is MINUS xi, so a heavy (Frechet) tail
# has k < 0 and xi > 0.

# Unbiased probability-weighted moment b_r (Greenwood et al. 1979;
# Hosking-Wallis-Wood 1985 Eq. 4) -- not the plotting-position
# approximation, whose small-sample bias is exactly what these
# methods exist to avoid.
.evt_pwm <- function(x, r) {
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < r + 1) {
    stop(sprintf("b_%d needs at least %d observations.", r, r + 1),
         call. = FALSE)
  }
  j <- seq_len(n)
  w <- rep(1, n)
  for (k in seq_len(r)) w <- w * (j - k) / (n - k)
  mean(w * xs)
}

.evt_lmom <- function(x) {
  b0 <- .evt_pwm(x, 0L)
  b1 <- .evt_pwm(x, 1L)
  b2 <- .evt_pwm(x, 2L)
  l2 <- 2 * b1 - b0
  if (l2 == 0) stop("the second L-moment is zero; the data are constant.",
                    call. = FALSE)
  l3 <- 6 * b2 - 6 * b1 + b0
  list(l1 = b0, l2 = l2, l3 = l3, t3 = l3 / l2)
}

.evt_top <- function(x, k) {
  xs <- sort(as.numeric(x), decreasing = TRUE)
  k <- as.integer(k)
  if (k < 1L || k >= length(xs)) {
    stop(sprintf("k must lie in 1..%d, got %d.", length(xs) - 1L, k),
         call. = FALSE)
  }
  xs[seq_len(k + 1L)]
}


#' Hill estimator of the tail index
#'
#' Hill (1975): the mean log-excess of the top k order statistics
#' over the (k+1)-st. Consistent ONLY for xi > 0 (Frechet-type,
#' regularly varying tails); non-positive data at the threshold are
#' an error, not a silent log of a negative number. The choice of k
#' is THE problem -- variance falls and bias grows with k -- so when
#' k is omitted the whole Hill plot is returned alongside the
#' default, because a single number hides exactly the instability a
#' user needs to see. The asymptotic SE xi/sqrt(k) is honest only in
#' the bias-free regime and says so.
#'
#' @param x sample; the top order statistics must be positive.
#' @param k number of top order statistics; `sqrt(n)` when `NULL`,
#'   with the full plot returned.
#' @return list: xi, tail_alpha, se, k, threshold, and when k was
#'   omitted hill_plot_k / hill_plot_xi; n, method.
#' @references Hill (1975), *Annals of Statistics* 3:1163-1174.
#' @examples
#' morie_evt_hill((1 - stats::runif(500))^(-1 / 4), k = 50)$xi
#' @export
morie_evt_hill <- function(x, k = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 10L) {
    stop(sprintf("need at least 10 observations, got %d.", n), call. = FALSE)
  }
  auto <- is.null(k)
  kk <- if (auto) as.integer(sqrt(n)) else as.integer(k)
  top <- .evt_top(xv, kk)
  if (top[kk + 1L] <= 0) {
    stop(paste("the threshold order statistic is not positive; the Hill",
               "estimator is only defined for positive heavy-tailed data",
               "(xi > 0)."), call. = FALSE)
  }
  logs <- log(top)
  xi <- mean(logs[seq_len(kk)]) - logs[kk + 1L]
  out <- list(xi = xi, tail_alpha = if (xi > 0) 1 / xi else Inf,
              se = xi / sqrt(kk),
              se_caveat = paste("xi/sqrt(k) is the bias-free asymptotic SE;",
                                "in the biased regime it understates the",
                                "error"),
              k = kk, threshold = top[kk + 1L],
              valid_for = paste("xi > 0 only -- Frechet-type tails; for xi",
                                "of any sign use morie_evt_pickands or",
                                "morie_evt_dedh"),
              n = n,
              method = "Hill (1975): mean log-excess of the top k order statistics")
  if (auto) {
    ks <- 2:min(n %/% 2L, 500L)
    xs_sorted <- sort(xv, decreasing = TRUE)
    lx <- log(pmax(xs_sorted, 1e-300))
    cums <- cumsum(lx)
    out$hill_plot_k <- ks
    out$hill_plot_xi <- cums[ks] / ks - lx[ks + 1L]
    out$k_choice_note <- paste("variance falls and bias grows with k; the",
                               "plot is returned because a single number",
                               "hides the instability")
  }
  out
}


#' Hill tail-index estimator -- alias entry point
#'
#' One estimator, two catalogue entries; the computation is
#' [morie_evt_hill()]. See it for the xi > 0 restriction and the
#' role of k.
#'
#' @param x,k as in [morie_evt_hill()].
#' @return the [morie_evt_hill()] list plus `alias_of`.
#' @references Hill (1975), *Annals of Statistics* 3:1163-1174.
#' @examples
#' morie_evt_hill_alias((1 - stats::runif(200))^(-1 / 3), k = 30)$alias_of
#' @export
morie_evt_hill_alias <- function(x, k = NULL) {
  out <- morie_evt_hill(x, k = k)
  out$alias_of <- "morie_evt_hill"
  out
}


#' Pickands estimator of the extreme-value index
#'
#' Pickands (1975): the log spacing ratio at k, 2k, 4k over log 2.
#' Consistent for EVERY real xi -- heavy, light and bounded tails
#' alike -- because it uses spacings rather than logs of levels. The
#' price is efficiency: its asymptotic variance (de Haan-Ferreira
#' Thm. 3.3.5; 3/(4 log^2 2) at xi = 0) is far above Hill's xi^2
#' where both are valid, so Hill wins when xi > 0 is known and
#' Pickands is the tool when the SIGN of xi is in question.
#'
#' @param x sample.
#' @param k spacing parameter, `4k <= n`; `n %/% 8` when `NULL`.
#' @return list: xi, se, k, order_stats_used, valid_for, versus_hill,
#'   n, method.
#' @references Pickands (1975), *Annals of Statistics* 3:119-131;
#'   de Haan and Ferreira (2006), Thm. 3.3.5.
#' @examples
#' morie_evt_pickands(stats::rnorm(400))$xi
#' @export
morie_evt_pickands <- function(x, k = NULL) {
  xv <- sort(as.numeric(x))
  n <- length(xv)
  if (n < 8L) {
    stop(sprintf("need at least 8 observations, got %d.", n), call. = FALSE)
  }
  kk <- if (is.null(k)) n %/% 8L else as.integer(k)
  if (kk < 1L || 4L * kk > n) {
    stop(sprintf("need 4k <= n; got k = %d, n = %d.", kk, n), call. = FALSE)
  }
  a <- xv[n - kk + 1L]
  b <- xv[n - 2L * kk + 1L]
  cc <- xv[n - 4L * kk + 1L]
  if (!(a > b && b > cc)) {
    stop("the three order statistics are tied; the spacing ratio is undefined.",
         call. = FALSE)
  }
  xi <- log((a - b) / (b - cc)) / log(2)
  avar <- if (abs(xi) < 1e-8) 3 / (4 * log(2)^2) else {
    xi^2 * (2^(2 * xi + 1) + 1) / (2 * (2^xi - 1) * log(2))^2
  }
  list(xi = xi, se = sqrt(avar / kk), k = kk,
       order_stats_used = c(a, b, cc),
       valid_for = "every real xi -- heavy, light and bounded tails alike",
       versus_hill = paste("far less efficient than Hill where Hill is",
                           "valid (xi > 0); the tool for when the sign of",
                           "xi is itself in question"),
       n = n,
       method = "Pickands (1975): log spacing ratio at k, 2k, 4k over log 2")
}


#' Dekkers-Einmahl-de Haan moment estimator
#'
#' DEdH (1989) Eq. (1.7): the Hill estimator plus a second-moment
#' correction, extending validity to ALL real xi. For genuinely heavy
#' tails the correction converges to zero and DEdH agrees with Hill
#' -- structure, and a test. The logs still need positive data, a
#' location restriction rather than a tail one.
#'
#' @param x sample; the top k+1 order statistics must be positive.
#' @param k top order statistics; `sqrt(n)` when `NULL`.
#' @return list: xi, hill_part, correction, M1, M2, se, k, threshold,
#'   n, method.
#' @references Dekkers, Einmahl and de Haan (1989), *Annals of
#'   Statistics* 17:1833-1855, Eq. (1.7).
#' @examples
#' morie_evt_dedh((1 - stats::runif(500))^(-1 / 4), k = 50)$xi
#' @export
morie_evt_dedh <- function(x, k = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 10L) {
    stop(sprintf("need at least 10 observations, got %d.", n), call. = FALSE)
  }
  kk <- if (is.null(k)) as.integer(sqrt(n)) else as.integer(k)
  top <- .evt_top(xv, kk)
  if (top[kk + 1L] <= 0) {
    stop(paste("the threshold order statistic is not positive; the moment",
               "estimator takes logs, so shift the data above zero first."),
         call. = FALSE)
  }
  d <- log(top[seq_len(kk)]) - log(top[kk + 1L])
  M1 <- mean(d)
  M2 <- mean(d^2)
  if (M2 <= 0) {
    stop("the top order statistics are tied at the threshold.", call. = FALSE)
  }
  corr <- 1 - 0.5 / (1 - M1^2 / M2)
  xi <- M1 + corr
  avar <- if (xi >= 0) xi^2 + 1 else {
    omx <- 1 - xi
    omx^2 * (1 - 2 * xi) * (4 - 8 * (1 - 2 * xi) / (1 - 3 * xi) +
                              (5 - 11 * xi) * (1 - 2 * xi) /
                              ((1 - 3 * xi) * (1 - 4 * xi)))
  }
  list(xi = xi, hill_part = M1, correction = corr, M1 = M1, M2 = M2,
       se = sqrt(max(avar, 0) / kk), k = kk, threshold = top[kk + 1L],
       agrees_with_hill_when = paste("xi > 0: the correction converges to",
                                     "zero and the first term IS the Hill",
                                     "estimator"),
       valid_for = paste("every real xi; the log still needs positive data,",
                         "a location restriction rather than a tail one"),
       n = n,
       method = "Dekkers-Einmahl-de Haan (1989) moment estimator, Eq. (1.7)")
}


#' L-moment estimator of the GEV parameters
#'
#' Hosking (1990): unbiased PWMs, the shape from
#' `k ~ 7.8590 c + 2.9554 c^2` with `c = 2/(3 + t3) - log2/log3`,
#' then scale and location exactly. Hosking's k is MINUS xi -- a
#' heavy (Frechet) tail has k < 0 and xi > 0, and conflating them
#' turns every Frechet into a Weibull, so both are returned, named.
#' L-moments over ML because the GEV likelihood is non-regular for
#' xi < -0.5 and loses in the small samples block maxima produce.
#'
#' @param block_maxima one maximum per block.
#' @return list: mu, sigma, k_hosking, xi, l1, l2, t3, tail_type,
#'   return_level_fn, n_blocks, method.
#' @references Hosking (1990), *JRSS-B* 52:105-124; Hosking, Wallis
#'   and Wood (1985), *Technometrics* 27:251-261.
#' @examples
#' morie_evt_gev_lmoments(5 + 2 * (-log(stats::runif(100)))^(-0.2))$xi
#' @export
morie_evt_gev_lmoments <- function(block_maxima) {
  xv <- as.numeric(block_maxima)
  n <- length(xv)
  if (n < 10L) {
    stop(sprintf("need at least 10 block maxima, got %d.", n), call. = FALSE)
  }
  lm <- .evt_lmom(xv)
  cc <- 2 / (3 + lm$t3) - log(2) / log(3)
  k <- 7.8590 * cc + 2.9554 * cc^2
  if (abs(k) < 1e-9) {
    alpha <- lm$l2 / log(2)
    mu <- lm$l1 - 0.5772156649015329 * alpha
    k <- 0
  } else {
    alpha <- lm$l2 * k / ((1 - 2^(-k)) * gamma(1 + k))
    mu <- lm$l1 - alpha / k * (1 - gamma(1 + k))
  }
  xi <- -k
  tail <- if (xi > 0.01) "Frechet (heavy, xi > 0)" else
    if (xi < -0.01) "Weibull (bounded, xi < 0)" else "Gumbel (light, xi ~ 0)"
  rl <- local({
    mu0 <- mu
    a0 <- alpha
    k0 <- k
    function(T) {
      y <- -log(1 - 1 / T)
      if (abs(k0) < 1e-9) mu0 - a0 * log(y) else mu0 + a0 / k0 * (1 - y^k0)
    }
  })
  list(mu = mu, sigma = alpha, k_hosking = k, xi = xi,
       l1 = lm$l1, l2 = lm$l2, t3 = lm$t3, tail_type = tail,
       sign_convention = "Hosking's k = -xi: heavy tail means k < 0, xi > 0",
       return_level_fn = rl,
       why_not_ml = paste("GEV maximum likelihood is non-regular for",
                          "xi < -0.5 and loses to L-moments in the small",
                          "samples block maxima produce"),
       n_blocks = n,
       method = "GEV by L-moments (Hosking 1990), unbiased PWMs")
}


#' PWM estimator for the GEV -- alias
#'
#' Hosking, Wallis and Wood (1985). L-moments are exactly linear
#' combinations of the PWMs, so the PWM fit and the L-moment fit are
#' THE SAME ESTIMATOR; one implementation serves both entries, and
#' the raw PWMs are added for readers of the 1985 paper.
#'
#' @param block_maxima one maximum per block.
#' @return the [morie_evt_gev_lmoments()] list plus b0, b1, b2,
#'   alias_of.
#' @references Hosking, Wallis and Wood (1985), *Technometrics*
#'   27:251-261.
#' @examples
#' morie_evt_gev_pwm(5 + (-log(stats::runif(60)))^(-0.1))$alias_of
#' @export
morie_evt_gev_pwm <- function(block_maxima) {
  out <- morie_evt_gev_lmoments(block_maxima)
  out$b0 <- .evt_pwm(block_maxima, 0L)
  out$b1 <- .evt_pwm(block_maxima, 1L)
  out$b2 <- .evt_pwm(block_maxima, 2L)
  out$alias_of <- "morie_evt_gev_lmoments"
  out$same_estimator_because <- paste("L-moments are linear combinations of",
                                      "the PWMs, so the two fits coincide",
                                      "exactly")
  out
}


#' PWM estimator of the generalised Pareto distribution
#'
#' Hosking and Wallis (1987): `k = l1/l2 - 2`, `sigma = l1 (1 + k)`
#' on the excesses' L-moments; Hosking's k is minus the GPD shape xi.
#' PWM over ML because it has lower bias for -0.5 < k < 0.5 and none
#' of ML's convergence failures at exceedance sample sizes (their
#' Sec. 4). Reliable only for k > -0.5 (xi < 0.5, finite variance);
#' outside that the output carries a warning, not a silently
#' untrustworthy number.
#'
#' @param x raw sample, or excesses when `threshold` is `NULL`.
#' @param threshold when given, excesses are formed here.
#' @return list: sigma, k_hosking, xi, n_excesses, threshold,
#'   mean_excess, reliable, return_level_fn, method.
#' @references Hosking and Wallis (1987), *Technometrics* 29:339-349.
#' @examples
#' morie_evt_gpd_pwm(stats::rexp(200))$xi
#' @export
morie_evt_gpd_pwm <- function(x, threshold = NULL) {
  xv <- as.numeric(x)
  if (!is.null(threshold)) {
    u <- as.numeric(threshold)
    exc <- xv[xv > u] - u
  } else {
    u <- NULL
    exc <- xv
    if (any(exc < 0)) {
      stop("excesses must be non-negative; pass the threshold to have them formed here.",
           call. = FALSE)
    }
  }
  n <- length(exc)
  if (n < 10L) {
    stop(sprintf("need at least 10 excesses, got %d.", n), call. = FALSE)
  }
  b0 <- .evt_pwm(exc, 0L)
  b1 <- .evt_pwm(exc, 1L)
  l2 <- 2 * b1 - b0
  if (l2 <= 0) {
    stop("the excesses' second L-moment must be positive.", call. = FALSE)
  }
  k <- b0 / l2 - 2
  sigma <- b0 * (1 + k)
  reliable <- k > -0.5
  rl <- local({
    s0 <- sigma
    k0 <- k
    base <- if (is.null(u)) 0 else u
    function(m) {
      if (abs(k0) < 1e-9) base + s0 * log(m) else
        base + s0 / k0 * (1 - m^(-k0))
    }
  })
  list(sigma = sigma, k_hosking = k, xi = -k,
       n_excesses = n, threshold = u, mean_excess = mean(exc),
       reliable = reliable,
       reliability_note = if (reliable) NULL else
         paste("k <= -0.5 (xi >= 0.5): infinite variance territory, where",
               "the PWM estimator's own theory stops"),
       why_pwm = paste("lower bias than ML for -0.5 < k < 0.5 and none of",
                       "ML's convergence failures at exceedance sample",
                       "sizes (Hosking-Wallis Sec. 4)"),
       sign_convention = "Hosking's k = -xi",
       return_level_fn = rl,
       method = "GPD by probability-weighted moments (Hosking-Wallis 1987)")
}


#' Runs estimator of the extremal index
#'
#' Smith and Weissman (1994): clusters over exceedances, a new
#' cluster starting when an exceedance is separated from the previous
#' by more than `run_length` non-exceedances. theta is the reciprocal
#' mean cluster size -- the effective number of independent extremes
#' is theta * n, and ignoring it overstates every return level. The
#' run length is a genuine tuning parameter (too short splits
#' clusters, too long merges them); the intervals estimator
#' ([morie_evt_extremal_intervals()]) has none and is the usual
#' cross-check.
#'
#' @param x stationary series.
#' @param threshold exceedance threshold.
#' @param run_length separation starting a new cluster.
#' @return list: theta, n_exceedances, n_clusters, mean_cluster_size,
#'   run_length, threshold, n, method.
#' @references Smith and Weissman (1994), *JRSS-B* 56:515-528.
#' @examples
#' x <- stats::rnorm(500)
#' morie_evt_extremal_runs(x, stats::quantile(x, 0.95))$theta
#' @export
morie_evt_extremal_runs <- function(x, threshold, run_length = 1L) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 20L) {
    stop(sprintf("need at least 20 observations, got %d.", n), call. = FALSE)
  }
  u <- as.numeric(threshold)
  r <- as.integer(run_length)
  if (r < 1L) {
    stop(sprintf("run_length must be at least 1, got %d.", r), call. = FALSE)
  }
  exc <- which(xv > u)
  ne <- length(exc)
  if (ne < 2L) {
    stop(sprintf("only %d exceedance(s) of %g; lower the threshold.", ne, u),
         call. = FALSE)
  }
  gaps <- diff(exc)
  nc <- 1L + sum(gaps > r)
  list(theta = nc / ne, n_exceedances = ne, n_clusters = nc,
       mean_cluster_size = ne / nc, run_length = r, threshold = u,
       interpretation = paste("theta is the reciprocal mean cluster size:",
                              "the effective number of independent extremes",
                              "is theta * n"),
       sensitivity_note = paste("run_length too short splits genuine",
                                "clusters, too long merges distinct ones;",
                                "the intervals estimator has no such tuning",
                                "parameter"),
       n = n,
       method = "Runs estimator of the extremal index (Smith-Weissman 1994)")
}


#' Ferro-Segers intervals estimator of the extremal index
#'
#' Ferro and Segers (2003), Eqs. (4) and (34): a moment ratio of the
#' interexceedance times, which converge to a mixture of
#' within-cluster short gaps and exponential between-cluster long
#' ones. NO tuning parameter -- the paper's selling point over the
#' runs estimator. The case split (uncorrected when every gap is at
#' most 2, corrected otherwise) is the authors' rule and is followed
#' rather than one form being picked.
#'
#' @param x stationary series.
#' @param threshold exceedance threshold.
#' @return list: theta, n_exceedances, form_used,
#'   mean_interexceedance, max_interexceedance,
#'   implied_mean_cluster_size, threshold, n, method.
#' @references Ferro and Segers (2003), *JRSS-B* 65:545-556.
#' @examples
#' x <- stats::rnorm(500)
#' morie_evt_extremal_intervals(x, stats::quantile(x, 0.95))$theta
#' @export
morie_evt_extremal_intervals <- function(x, threshold) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 20L) {
    stop(sprintf("need at least 20 observations, got %d.", n), call. = FALSE)
  }
  u <- as.numeric(threshold)
  exc <- which(xv > u)
  N <- length(exc)
  if (N < 3L) {
    stop(sprintf("only %d exceedance(s) of %g; lower the threshold.", N, u),
         call. = FALSE)
  }
  T <- as.numeric(diff(exc))
  if (max(T) <= 2) {
    theta <- min(1, 2 * sum(T)^2 / ((N - 1) * sum(T^2)))
    form <- "Eq. (4): max gap <= 2, uncorrected moments"
  } else {
    theta <- min(1, 2 * sum(T - 1)^2 / ((N - 1) * sum((T - 1) * (T - 2))))
    form <- "Eq. (34): gaps beyond 2 present, corrected moments"
  }
  list(theta = theta, n_exceedances = N, form_used = form,
       mean_interexceedance = mean(T), max_interexceedance = max(T),
       implied_mean_cluster_size = if (theta > 0) 1 / theta else Inf,
       no_tuning_note = paste("unlike the runs estimator there is no",
                              "run-length parameter: the",
                              "interexceedance-time mixture identifies",
                              "theta by a moment ratio"),
       threshold = u, n = n,
       method = "Intervals estimator of the extremal index (Ferro-Segers 2003)")
}


#' Sliding-blocks estimator of the extremal index
#'
#' Northrop (2015): for a series with extremal index theta,
#' `-b log F_hat(block max)` is approximately Exp(theta), so
#' `theta = 1/mean`. SLIDING blocks use each observation in b windows
#' instead of one, and Northrop shows the sliding version has
#' strictly smaller asymptotic variance than the disjoint one -- the
#' entire point of preferring it, and both are returned so the
#' comparison is visible. `threshold` is accepted only for signature
#' parity with the other extremal-index estimators and is ignored.
#'
#' @param x stationary series.
#' @param threshold ignored; present for signature parity.
#' @param block_length block size b; `floor(sqrt(n))` when `NULL`.
#' @return list: theta, theta_disjoint, block_length,
#'   n_sliding_blocks, n_disjoint_blocks, n, method.
#' @references Northrop (2015), *Extremes* 18:585-603.
#' @examples
#' morie_evt_extremal_sliding(stats::rnorm(400), block_length = 20)$theta
#' @export
morie_evt_extremal_sliding <- function(x, threshold = NULL,
                                       block_length = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 40L) {
    stop(sprintf("need at least 40 observations, got %d.", n), call. = FALSE)
  }
  b <- if (is.null(block_length)) as.integer(sqrt(n)) else
    as.integer(block_length)
  if (b < 2L || b > n %/% 2L) {
    stop(sprintf("block_length must lie in 2..%d, got %d.", n %/% 2L, b),
         call. = FALSE)
  }
  Fhat <- rank(xv, ties.method = "first") / (n + 1)
  theta_from <- function(maxF, bb) {
    Y <- -bb * log(maxF)
    m <- mean(Y)
    if (m <= 0) stop("degenerate block maxima.", call. = FALSE)
    min(1, 1 / m)
  }
  # sliding max via a simple deque-free rolling pass
  slide_max <- vapply(seq_len(n - b + 1L), function(i) {
    max(Fhat[i:(i + b - 1L)])
  }, numeric(1))
  th_slide <- theta_from(slide_max, b)
  nd <- n %/% b
  disj_max <- vapply(seq_len(nd), function(j) {
    max(Fhat[((j - 1L) * b + 1L):(j * b)])
  }, numeric(1))
  th_disj <- theta_from(disj_max, b)
  list(theta = th_slide, theta_disjoint = th_disj, block_length = b,
       n_sliding_blocks = length(slide_max), n_disjoint_blocks = nd,
       sliding_beats_disjoint_because = paste(
         "every observation participates in b windows instead of one;",
         "Northrop (2015) shows the sliding estimator's asymptotic variance",
         "is strictly smaller"),
       threshold_note = paste("this estimator uses block maxima, not a",
                              "threshold; the argument is accepted only for",
                              "signature parity and is ignored"),
       n = n,
       method = "Northrop (2015) sliding-blocks semiparametric maxima estimator")
}


#' Madogram estimator of the Pickands dependence function
#'
#' Naveau, Guillou, Cooley and Diebolt (2009), Prop. 3: the
#' lambda-madogram `nu(t) = E|F(X)^(1/t) - G(Y)^(1/(1-t))|/2` on rank
#' margins inverts to `A(t) = (nu + c)/(1 - nu - c)` with
#' `c = t/(2(1+t)) + (1-t)/(2(2-t))`. `A == 1` is asymptotic
#' independence, `max(t, 1-t)` complete dependence, and every valid A
#' is convex between those envelopes. The estimate is clipped into
#' the envelope as the authors recommend, and the clipping is
#' REPORTED -- heavy clipping means the extreme-value model itself
#' fits badly.
#'
#' @param x,y paired observations.
#' @param t evaluation points in (0, 1); a grid when `NULL`.
#' @return list: t, A, A_raw, clipped_fraction, dependence_summary,
#'   envelope, n, method.
#' @references Naveau, Guillou, Cooley and Diebolt (2009),
#'   *Biometrika* 96:1-17, Prop. 3; Pickands (1981).
#' @examples
#' morie_evt_madogram(stats::rnorm(100), stats::rnorm(100),
#'                    t = c(0.5))$A
#' @export
morie_evt_madogram <- function(x, y, t = NULL) {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  if (length(xv) != length(yv)) {
    stop(sprintf("x has %d entries and y has %d.", length(xv), length(yv)),
         call. = FALSE)
  }
  n <- length(xv)
  if (n < 20L) {
    stop(sprintf("need at least 20 pairs, got %d.", n), call. = FALSE)
  }
  tg <- if (is.null(t)) seq(0.05, 0.95, length.out = 19L) else as.numeric(t)
  if (any(tg <= 0 | tg >= 1)) {
    stop("t must lie strictly in (0, 1).", call. = FALSE)
  }
  U <- rank(xv, ties.method = "first") / (n + 1)
  V <- rank(yv, ties.method = "first") / (n + 1)
  A_raw <- vapply(tg, function(tt) {
    nu <- 0.5 * mean(abs(U^(1 / tt) - V^(1 / (1 - tt))))
    cc <- tt / (2 * (1 + tt)) + (1 - tt) / (2 * (2 - tt))
    (nu + cc) / (1 - nu - cc)
  }, numeric(1))
  lower <- pmax(tg, 1 - tg)
  A <- pmin(pmax(A_raw, lower), 1)
  nu_h <- 0.5 * mean(abs(U^2 - V^2))
  c_h <- 1 / 3
  A_half <- min(max((nu_h + c_h) / (1 - nu_h - c_h), 0.5), 1)
  list(t = tg, A = A, A_raw = A_raw,
       clipped_fraction = mean(abs(A - A_raw) > 1e-12),
       dependence_summary = 2 * (1 - A_half),
       envelope = paste("max(t, 1-t) <= A <= 1; A = 1 is asymptotic",
                        "independence, the lower envelope complete",
                        "dependence"),
       clipping_note = paste("heavy clipping means the extreme-value model",
                             "itself fits badly, not that the estimator",
                             "misfired"),
       n = n,
       method = "Lambda-madogram estimate of the Pickands dependence function (Naveau et al. 2009)")
}
