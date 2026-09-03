# SPDX-License-Identifier: AGPL-3.0-or-later
#
# MCMC diagnostics and dependent-data bootstrap. R mirrors of the
# morie.fn modules acfP, acpra, bayess, essbk, esstl and btnpb over the
# shared autocovariance / ESS core, as _mcmc.py does.
#
# The three ESS variants are separate on purpose and are not
# interchangeable: bulk ESS governs central summaries, tail ESS governs
# credible intervals, and a chain can have a perfectly good bulk ESS
# alongside a tail ESS that makes its interval meaningless.

# Autocovariance by FFT, matching _mcmc.autocov: zero-padded to a power
# of two at least 2n, divided by n (biased, which is what keeps the
# sequence positive semi-definite).
#' Autocovariance by FFT, matching _mcmc.autocov: zero-padded to a power
#'
#' of two at least 2n, divided by n (biased, which is what keeps the
#' sequence positive semi-definite).
#'
#' @param x A vector; its length is taken.
#' @param max_lag Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return The value of \code{[}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .morie_mcmc_autocov(x = x)
#' res
.morie_mcmc_autocov <- function(x, max_lag = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(max_lag)) max_lag <- n - 1L
  xc <- x - mean(x)
  size <- 1L
  while (size < 2L * n) size <- size * 2L
  f <- stats::fft(c(xc, rep(0, size - n)))
  acov <- Re(stats::fft(f * Conj(f), inverse = TRUE)) / size / n
  acov[seq_len(max_lag + 1L)]
}

#' .morie_ess_from_chains
#'
#' A step of the mcmc_native implementation. Called by
#' \code{morie_effective_sample_size_bayes}, \code{morie_effective_sample_size_bulk},
#' \code{morie_effective_sample_size_tail}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param chains A count; the body uses it as \code{matrix(...)}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_ess_from_chains <- function(chains) {
  C <- if (is.matrix(chains)) chains else matrix(chains, nrow = 1L)
  m <- nrow(C)
  n <- ncol(C)
  if (n < 4L) return(NA_real_)
  acovs <- t(vapply(seq_len(m), function(j) .morie_mcmc_autocov(C[j, ], n - 1L),
                    numeric(n)))
  chain_var <- acovs[, 1L] * n / max(n - 1L, 1L)
  W <- mean(chain_var)
  var_hat <- if (m > 1L) {
    B <- n * stats::var(rowMeans(C))
    ((n - 1) * W + B) / n
  } else {
    W
  }
  if (var_hat <= 0) return(NA_real_)
  rho <- 1 - (W - colMeans(acovs[, -1L, drop = FALSE])) / var_hat
  # Geyer's initial positive sequence: stop at the first PAIR of
  # autocorrelations that sums negative. Truncating on a single negative
  # value instead systematically overstates ESS.
  t <- 0L
  total <- 0
  while (t + 1L < length(rho)) {
    pair <- rho[t + 1L] + rho[t + 2L]
    if (pair < 0) break
    total <- total + pair
    t <- t + 2L
  }
  tau <- -1 + 2 * total
  if (tau > 0) m * n / max(tau, 1e-12) else m * n
}

#' .morie_rank_normalize
#'
#' A step of the mcmc_native implementation. Called by \code{.morie_split_rhat},
#' \code{morie_effective_sample_size_bulk}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param C A matrix; passed to \code{nrow}.
#' @return A matrix, from \code{matrix}.
#' @export
.morie_rank_normalize <- function(C) {
  flat <- as.vector(C)
  ranks <- order(order(flat))          # stable both sides
  z <- stats::qnorm((ranks - 0.375) / (length(flat) + 0.25))
  matrix(z, nrow = nrow(C), ncol = ncol(C))
}

#' .morie_split_rhat
#'
#' A step of the mcmc_native implementation. Called by \code{morie_effective_sample_size_bayes}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param chains A count; the body uses it as \code{matrix(...)}.
#' @param rank_normalized A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A numeric value.
#' @export
.morie_split_rhat <- function(chains, rank_normalized = TRUE) {
  C <- if (is.matrix(chains)) chains else matrix(chains, nrow = 1L)
  n <- ncol(C)
  if (n < 4L) return(NA_real_)
  half <- n %/% 2L
  S <- rbind(C[, seq_len(half), drop = FALSE],
             C[, (n - half + 1L):n, drop = FALSE])
  if (rank_normalized) S <- .morie_rank_normalize(S)
  m2 <- nrow(S)
  n2 <- ncol(S)
  W <- mean(apply(S, 1L, stats::var))
  B <- if (m2 > 1L) n2 * stats::var(rowMeans(S)) else 0
  if (W <= 0) return(NA_real_)
  sqrt((((n2 - 1) * W + B) / n2) / W)
}


#' Sample autocorrelation function with Ljung-Box
#'
#' The ACF at each lag with the same denominator throughout, which is
#' what keeps the estimated sequence positive semi-definite -- dividing
#' by the number of terms actually summed at each lag does not, and
#' produces "autocorrelations" no stationary process could have.
#'
#' The white-noise bands are a per-lag test, so about one spike in 20
#' will cross them by chance. Read the Ljung-Box statistic, which tests
#' all lags jointly. After fitting a model, subtract the degrees of
#' freedom it consumed before interpreting either.
#'
#' @param y series.
#' @param lag_max largest lag; defaults to \code{10 log10(n)}.
#' @param ci confidence level for the bands.
#' @return list with \code{acf} (lag 0 first), \code{lags},
#'   \code{ci_bound}, \code{significant}, \code{ljung_box},
#'   \code{ljung_box_p}.
#' @references Ljung, G. M. and Box, G. E. P. (1978). On a measure of
#'   lack of fit in time series models. \emph{Biometrika}, 65(2),
#'   297-303.
#' @examples
#' set.seed(1)
#' round(morie_autocorrelation(arima.sim(list(ar = 0.6), 200))$acf[2], 2)
#' @export
morie_autocorrelation <- function(y, lag_max = NULL, ci = 0.95) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 3L) stop("need at least 3 observations", call. = FALSE)
  if (is.null(lag_max)) lag_max <- as.integer(min(10 * log10(n), n - 1))
  lag_max <- as.integer(lag_max)
  if (lag_max < 1L || lag_max > n - 1L) {
    stop(sprintf("lag_max must be between 1 and %d", n - 1L), call. = FALSE)
  }
  yc <- y - mean(y)
  denom <- sum(yc^2)
  acf <- c(1, vapply(seq_len(lag_max),
                     function(k) sum(yc[(k + 1L):n] * yc[seq_len(n - k)]) /
                       denom, numeric(1)))
  bound <- stats::qnorm(0.5 + ci / 2) / sqrt(n)
  lb <- n * (n + 2) * sum(acf[-1L]^2 / (n - seq_len(lag_max)))
  list(acf = acf, lags = 0:lag_max, ci_bound = bound,
       significant = abs(acf[-1L]) > bound,
       n_significant = as.integer(sum(abs(acf[-1L]) > bound)),
       ljung_box = lb,
       ljung_box_p = stats::pchisq(lb, lag_max, lower.tail = FALSE),
       n = n,
       warnings = paste("the bands assume white noise; after fitting a model,",
                        "subtract the degrees of freedom it consumed before",
                        "reading them"),
       method = "autocorrelation")
}


#' MCMC acceptance-rate diagnostic
#'
#' Compares the observed acceptance rate against the optimal rate for
#' the sampler: 0.234 for random-walk Metropolis, 0.574 for MALA, about
#' 0.8 for HMC. These are not interchangeable, and judging an HMC run by
#' the Metropolis target will send you in the wrong direction.
#'
#' Acceptance rate alone says nothing about mixing. A sampler taking
#' tiny steps accepts almost everything and explores almost nothing --
#' read this with ESS, never on its own.
#'
#' @param chains matrix of draws, one row per chain.
#' @param target optional target rate; defaults by \code{kind}.
#' @param kind \code{"metropolis"}, \code{"mala"} or \code{"hmc"}.
#' @return list with \code{acceptance_rate}, \code{target},
#'   \code{deviation}, \code{recommendation}, \code{per_chain}.
#' @references Roberts, G. O., Gelman, A. and Gilks, W. R. (1997). Weak
#'   convergence and optimal scaling of random walk Metropolis
#'   algorithms. \emph{Annals of Applied Probability}, 7(1), 110-120.
#' @examples
#' set.seed(1)
#' ch <- matrix(cumsum(rnorm(400) * rbinom(400, 1, 0.3)), nrow = 2)
#' round(morie_acceptance_rate_diagnostic(ch)$acceptance_rate, 2)
#' @export
morie_acceptance_rate_diagnostic <- function(chains, target = NULL,
                                             kind = c("metropolis", "mala",
                                                      "hmc")) {
  kind <- match.arg(kind)
  targets <- c(metropolis = 0.234, mala = 0.574, hmc = 0.8)
  tgt <- if (is.null(target)) targets[[kind]] else as.numeric(target)
  C <- if (is.matrix(chains)) chains else matrix(chains, nrow = 1L)
  if (ncol(C) < 2L) stop("need at least 2 draws per chain", call. = FALSE)
  per <- vapply(seq_len(nrow(C)), function(j) mean(diff(C[j, ]) != 0),
                numeric(1))
  rate <- mean(per)
  rec <- if (rate < tgt * 0.5) {
    "acceptance is far below target: shrink the proposal step"
  } else if (rate > min(tgt * 2, 0.98)) {
    paste("acceptance is far above target: the steps are too small to",
          "explore; enlarge them")
  } else {
    "acceptance is in a reasonable range for this sampler"
  }
  list(acceptance_rate = rate, target = tgt, deviation = rate - tgt,
       recommendation = rec, per_chain = per, kind = kind,
       warnings = paste("acceptance rate alone says nothing about mixing; a",
                        "sampler with tiny steps accepts almost everything and",
                        "explores almost nothing -- read it with ESS"),
       method = "acceptance_rate_diagnostic")
}


#' Effective sample size of an MCMC chain
#'
#' \eqn{ESS = mn / (1 + 2\sum_k \rho_k)} with Geyer's initial positive
#' sequence truncation.
#'
#' Monte Carlo standard error follows the ESS, not the number of draws:
#' a million draws with an ESS of 40 carries the precision of 40
#' independent ones. ESS above the draw count is legitimate and means
#' the chain is antithetic; ESS below 100 means the summaries are
#' dominated by Monte Carlo error.
#'
#' @param chain matrix of draws (rows are chains) or a single vector.
#' @return list with \code{ess}, \code{efficiency}, \code{mcse},
#'   \code{rhat}, \code{autocorr_time}.
#' @references Geyer, C. J. (1992). Practical Markov chain Monte Carlo.
#'   \emph{Statistical Science}, 7(4), 473-483. Vehtari, A. et al.
#'   (2021). Rank-normalization, folding, and localization.
#'   \emph{Bayesian Analysis}, 16(2), 667-718.
#' @examples
#' set.seed(1)
#' round(morie_effective_sample_size_bayes(matrix(rnorm(2000),
#'                                                nrow = 4))$efficiency, 2)
#' @export
morie_effective_sample_size_bayes <- function(chain) {
  C <- if (is.matrix(chain)) chain else matrix(chain, nrow = 1L)
  m <- nrow(C)
  n <- ncol(C)
  if (n < 4L) stop("need at least 4 draws per chain", call. = FALSE)
  ess <- .morie_ess_from_chains(C)
  total <- m * n
  sd <- stats::sd(as.vector(C))
  list(ess = ess, n_draws = total, efficiency = ess / total,
       mcse = sd / sqrt(max(ess, 1e-12)),
       rhat = if (m > 1L) .morie_split_rhat(C) else NA_real_,
       autocorr_time = total / max(ess, 1e-12), n_chains = m,
       warnings = if (is.finite(ess) && ess < 100) {
         paste("ESS is below 100; the posterior summaries are dominated by",
               "Monte Carlo error")
       } else {
         character(0)
       },
       method = "effective_sample_size_bayes")
}


#' Bulk effective sample size
#'
#' ESS computed on rank-normalised draws, which is what makes it survive
#' heavy tails and infinite-variance targets where the raw ESS is not
#' even defined.
#'
#' It governs CENTRAL summaries only -- the posterior mean and median.
#' A good bulk ESS says nothing about the tails; that is
#' \code{\link{morie_effective_sample_size_tail}}.
#'
#' @param chains matrix of draws, one row per chain.
#' @return list with \code{ess_bulk}, \code{efficiency},
#'   \code{sufficient} (ESS >= 100 per chain).
#' @references Vehtari, A. et al. (2021). Rank-normalization, folding,
#'   and localization: an improved R-hat for assessing convergence of
#'   MCMC. \emph{Bayesian Analysis}, 16(2), 667-718.
#' @examples
#' set.seed(1)
#' morie_effective_sample_size_bulk(matrix(rt(2000, 2), nrow = 4))$sufficient
#' @export
morie_effective_sample_size_bulk <- function(chains) {
  C <- if (is.matrix(chains)) chains else matrix(chains, nrow = 1L)
  m <- nrow(C)
  n <- ncol(C)
  if (n < 4L) stop("need at least 4 draws per chain", call. = FALSE)
  ess <- .morie_ess_from_chains(.morie_rank_normalize(C))
  total <- m * n
  list(ess_bulk = ess, n_draws = total, efficiency = ess / total,
       sufficient = isTRUE(ess >= 100 * m), n_chains = m,
       warnings = if (isTRUE(ess < 100 * m)) {
         sprintf(paste("bulk ESS below 100 per chain (%.0f for %d chains);",
                       "central summaries are unreliable"), ess, m)
       } else {
         character(0)
       },
       method = "effective_sample_size_bulk")
}


#' Tail effective sample size
#'
#' ESS of the tail-membership indicators at the \code{prob} and
#' \code{1 - prob} quantiles, reported as the smaller of the two.
#'
#' This is the quantity that governs CREDIBLE INTERVALS, and it is
#' routinely the binding one: a chain can have an excellent bulk ESS and
#' a tail ESS in the dozens, which means a trustworthy posterior mean
#' reported alongside an interval that is mostly Monte Carlo noise.
#'
#' The indicators are deliberately NOT rank-normalised. They are already
#' 0/1, and rank-normalising a binary vector destroys the very
#' information the statistic is built on.
#'
#' @param chains matrix of draws, one row per chain.
#' @param prob tail probability in (0, 0.5).
#' @return list with \code{ess_tail}, \code{ess_lower}, \code{ess_upper},
#'   \code{sufficient}.
#' @references Vehtari, A. et al. (2021). Rank-normalization, folding,
#'   and localization: an improved R-hat for assessing convergence of
#'   MCMC. \emph{Bayesian Analysis}, 16(2), 667-718.
#' @examples
#' set.seed(1)
#' morie_effective_sample_size_tail(matrix(rnorm(4000),
#'                                         nrow = 4))$ess_tail > 100
#' @export
morie_effective_sample_size_tail <- function(chains, prob = 0.05) {
  if (prob <= 0 || prob >= 0.5) {
    stop("prob must be in (0, 0.5)", call. = FALSE)
  }
  C <- if (is.matrix(chains)) chains else matrix(chains, nrow = 1L)
  m <- nrow(C)
  n <- ncol(C)
  if (n < 4L) stop("need at least 4 draws per chain", call. = FALSE)
  flat <- as.vector(C)
  qs <- stats::quantile(flat, c(prob, 1 - prob), names = FALSE, type = 7L)
  lo <- .morie_ess_from_chains(matrix(as.numeric(C < qs[1L]), m, n))
  hi <- .morie_ess_from_chains(matrix(as.numeric(C > qs[2L]), m, n))
  ess <- min(lo, hi)
  total <- m * n
  list(ess_tail = ess, ess_lower = lo, ess_upper = hi, n_draws = total,
       efficiency = ess / total, sufficient = isTRUE(ess >= 100 * m),
       prob = prob, n_chains = m,
       warnings = if (isTRUE(ess < 100 * m)) {
         sprintf(paste("tail ESS below 100 per chain (%.0f); credible",
                       "intervals are unreliable even if the mean is fine"),
                 ess)
       } else {
         character(0)
       },
       method = "effective_sample_size_tail")
}


#' Non-overlapping block bootstrap
#'
#' Resamples contiguous, disjoint blocks so that dependence WITHIN a
#' block survives the resampling.
#'
#' The iid bootstrap destroys serial dependence entirely and therefore
#' understates the standard error of anything computed on a dependent
#' series -- often by a large factor. Block length is the whole design
#' choice: too short and the dependence is broken anyway, too long and
#' there are too few blocks to resample. Non-overlapping blocks give
#' only \code{n / block_len} resampling units; the moving-block variant
#' is more efficient but its blocks are correlated.
#'
#' @param x series.
#' @param block_len block length; defaults to \code{n^(1/3)}.
#' @param stat function of a numeric vector; defaults to the mean.
#' @param B replicates.
#' @param seed integer seed. R's generator is not numpy's, so replicate
#'   values differ across languages; the estimate and the block geometry
#'   do not.
#' @return list with \code{estimate}, \code{se}, \code{ci},
#'   \code{replicates}, \code{block_len}, \code{n_blocks}.
#' @references Carlstein, E. (1986). The use of subseries values for
#'   estimating the variance of a general statistic from a stationary
#'   sequence. \emph{Annals of Statistics}, 14(3), 1171-1179.
#' @examples
#' set.seed(1)
#' morie_boot_nonoverlap_block(arima.sim(list(ar = 0.7), 300))$n_blocks
#' @export
morie_boot_nonoverlap_block <- function(x, block_len = NULL, stat = NULL,
                                        B = 500, seed = 0) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least 2 observations", call. = FALSE)
  if (is.null(block_len)) block_len <- max(as.integer(n^(1 / 3)), 1L)
  block_len <- as.integer(block_len)
  if (block_len < 1L || block_len > n) {
    stop(sprintf("block_len must be between 1 and %d", n), call. = FALSE)
  }
  if (is.null(stat)) stat <- mean
  n_blocks <- n %/% block_len
  if (n_blocks < 2L) {
    stop(sprintf(paste("block_len=%d leaves only %d blocks; too few to",
                       "resample"), block_len, n_blocks), call. = FALSE)
  }
  blocks <- matrix(x[seq_len(n_blocks * block_len)], nrow = n_blocks,
                   ncol = block_len, byrow = TRUE)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  B <- as.integer(B)
  reps <- vapply(seq_len(B), function(b) {
    pick <- sample.int(n_blocks, n_blocks, replace = TRUE)
    as.numeric(stat(as.vector(t(blocks[pick, , drop = FALSE]))))
  }, numeric(1))
  list(estimate = as.numeric(stat(x)), se = stats::sd(reps),
       ci = stats::quantile(reps, c(0.025, 0.975), names = FALSE, type = 7L),
       replicates = reps, block_len = block_len, n_blocks = n_blocks, B = B,
       warnings = paste("non-overlapping blocks give only n/block_len",
                        "resampling units; the moving-block variant is more",
                        "efficient but its blocks are correlated"),
       method = "boot_nonoverlap_block")
}
