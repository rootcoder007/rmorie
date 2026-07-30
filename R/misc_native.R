# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Remaining shelf: privacy accounting (compdp, cdpamp, acwhe), wavelet
# and autoencoder anomaly detection (dwtA, aE_an), off-policy RL
# correction (impala), denoising score matching (diffsm) and the zonal
# energy-balance model (ebmZD).

#' Basic and advanced privacy composition
#'
#' Basic composition adds the epsilons; advanced composition gives
#' \eqn{\sqrt{2k\log(1/\delta')}\,\epsilon + k\epsilon(e^\epsilon - 1)}
#' at the cost of a delta.
#'
#' Advanced composition is not uniformly better and reaching for it by
#' reflex is a common error. Its \eqn{\sqrt k} term only overtakes the
#' linear one when k is large AND epsilon is small; for a handful of
#' queries, or for loose per-query budgets, the second term dominates
#' and basic composition wins. This function computes both and says
#' which.
#'
#' @param epsilons per-mechanism epsilons.
#' @param delta_prime the delta advanced composition spends; NULL to
#'   report basic composition only.
#' @param deltas optional per-mechanism deltas.
#' @return list with \code{basic_epsilon}, \code{advanced_epsilon},
#'   \code{total_delta}, \code{recommended}, \code{epsilon}.
#' @references Dwork, C., Rothblum, G. N. and Vadhan, S. (2010). Boosting
#'   and differential privacy. \emph{FOCS}, 51-60.
#' @examples
#' morie_basic_composition(rep(0.1, 50), delta_prime = 1e-6)$recommended
#' @export
morie_basic_composition <- function(epsilons, delta_prime = NULL,
                                    deltas = NULL) {
  eps <- as.numeric(epsilons)
  if (length(eps) == 0L) stop("epsilons must be non-empty", call. = FALSE)
  if (any(eps <= 0) || !all(is.finite(eps))) {
    stop("every epsilon must be positive", call. = FALSE)
  }
  k <- length(eps)
  dl <- if (is.null(deltas)) numeric(k) else as.numeric(deltas)
  if (length(dl) != k) {
    stop(sprintf("deltas has %d entries but epsilons has %d", length(dl), k),
         call. = FALSE)
  }
  basic <- sum(eps)
  adv <- NA_real_
  if (!is.null(delta_prime)) {
    if (delta_prime <= 0 || delta_prime >= 1) {
      stop("delta_prime must be in (0, 1)", call. = FALSE)
    }
    e <- max(eps)
    adv <- sqrt(2 * k * log(1 / delta_prime)) * e + k * e * (exp(e) - 1)
  }
  total_delta <- sum(dl) + (if (is.null(delta_prime)) 0 else delta_prime)
  rec <- if (is.na(adv) || basic <= adv) "basic" else "advanced"
  list(basic_epsilon = basic, advanced_epsilon = adv,
       total_delta = total_delta, recommended = rec,
       epsilon = if (identical(rec, "basic")) basic else adv, k = k,
       delta_prime = delta_prime, method = "basic_composition")
}


#' Zero-concentrated DP composition
#'
#' rho adds exactly across mechanisms, and converts as
#' \eqn{\epsilon = \rho + 2\sqrt{\rho\log(1/\delta)}}.
#'
#' The \eqn{\sqrt k} growth that advanced composition has to buy with a
#' union bound comes FREE here, because rho is additive and the square
#' root appears only once in the conversion. That is the reason to
#' account in zCDP.
#'
#' What zCDP cannot do is represent pure epsilon-DP: the Laplace
#' mechanism has unbounded Renyi divergence at large orders and no
#' finite rho exists for it.
#'
#' @param rho per-mechanism rho values, or a single rho.
#' @param k_compositions repetitions when \code{rho} is scalar.
#' @param delta target delta for the conversion.
#' @return list with \code{rho_total}, \code{epsilon},
#'   \code{equivalent_sigma}.
#' @references Bun, M. and Steinke, T. (2016). Concentrated differential
#'   privacy: simplifications, extensions, and lower bounds. \emph{TCC},
#'   635-658.
#' @examples
#' round(morie_cdp_subgaussian_amplification(0.01, 100)$epsilon, 4)
#' @export
morie_cdp_subgaussian_amplification <- function(rho, k_compositions = 1,
                                                delta = 1e-5) {
  r <- as.numeric(rho)
  if (any(r < 0)) stop("rho must be non-negative", call. = FALSE)
  if (delta <= 0 || delta >= 1) {
    stop("delta must be in (0, 1)", call. = FALSE)
  }
  k <- as.integer(k_compositions)
  if (k < 1L) stop("k_compositions must be at least 1", call. = FALSE)
  total <- if (length(r) == 1L) sum(r) * k else sum(r)
  list(rho_total = total, epsilon = total + 2 * sqrt(total * log(1 / delta)),
       delta = as.numeric(delta), sqrt_k_growth = TRUE,
       equivalent_sigma = if (total > 0) sqrt(1 / (2 * total)) else Inf,
       k = k,
       warnings = paste("zCDP cannot represent pure epsilon-DP: the Laplace",
                        "mechanism has unbounded Renyi divergence and no",
                        "finite rho"),
       method = "cdp_subgaussian_amplification")
}


#' Privacy-accuracy trade-off
#'
#' The noise a budget implies, and the sample size at which that noise
#' stops mattering.
#'
#' The number worth reporting is \code{noise_to_signal_n}, which is
#' \eqn{2/\epsilon^2}: past that sample size the privacy noise is
#' smaller than the sampling error already in the estimate, so privacy
#' costs nothing anyone can measure. Quoting an epsilon alone tells a
#' reader nothing about whether that is the case here.
#'
#' @param sensitivity query sensitivity.
#' @param epsilon privacy budget.
#' @param n sample size the query averages over.
#' @param confidence coverage for the half-width.
#' @return list with \code{noise_scale}, \code{noise_sd},
#'   \code{half_width}, \code{relative_error}, \code{noise_to_signal_n}.
#' @references Dwork, C. and Roth, A. (2014). \emph{FnTTCS}, 9(3-4).
#' @examples
#' morie_private_accuracy_tradeoff(1, 0.5, n = 1000)$noise_to_signal_n
#' @export
morie_private_accuracy_tradeoff <- function(sensitivity = 1, epsilon = 1,
                                            n = 100, confidence = 0.95) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  sensitivity <- as.numeric(sensitivity)[1L]
  if (sensitivity <= 0) stop("sensitivity must be positive", call. = FALSE)
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1", call. = FALSE)
  if (confidence <= 0 || confidence >= 1) {
    stop("confidence must be in (0, 1)", call. = FALSE)
  }
  b <- sensitivity / (n * eps)
  sd <- sqrt(2) * b
  list(noise_scale = b, noise_sd = sd, half_width = b * log(1 / (1 - confidence)),
       relative_error = sd / max(abs(sensitivity), 1e-300),
       noise_to_signal_n = 2 / eps^2, epsilon = eps, n = n,
       sensitivity = sensitivity, confidence = confidence,
       method = "private_accuracy_tradeoff")
}


#' Haar wavelet anomaly detection
#'
#' Universal thresholding of Haar detail coefficients at
#' \eqn{\sigma\sqrt{2\log n}}, with sigma from the median absolute
#' deviation of the finest level.
#'
#' The LEVEL at which a coefficient fires is the diagnostic, not just
#' the fact that one did: a single-point spike fires at level 1 only,
#' while a level shift fires at every coarse scale. \code{level_fired}
#' records it, and \code{max_span} caps how wide a region a coarse
#' coefficient is allowed to mark, so one level-6 coefficient cannot
#' paint 64 points as anomalous.
#'
#' The Haar basis localises abrupt changes and is blind to slow drift,
#' which produces no large coefficient at any scale.
#'
#' The \code{max_span} cap has a consequence worth knowing: a sustained
#' LEVEL SHIFT fires large coefficients at coarse scales, but those are
#' not allowed to mark their whole span, so the shift appears in
#' \code{per_level_count} without appearing in \code{anomaly}. The cap
#' buys sharp localisation of spikes at that price; raise
#' \code{max_span} to mark shifts as well.
#'
#' @param x series.
#' @param threshold explicit threshold; universal when NULL.
#' @param levels decomposition levels.
#' @param max_span widest region a firing coefficient may mark.
#' @return list with \code{anomaly}, \code{score}, \code{sigma},
#'   \code{threshold}, \code{level_fired}, \code{per_level_count}.
#' @references Donoho, D. L. and Johnstone, I. M. (1994). Ideal spatial
#'   adaptation by wavelet shrinkage. \emph{Biometrika}, 81(3), 425-455.
#' @examples
#' set.seed(1)
#' y <- rnorm(128); y[40] <- 9
#' which(morie_discrete_wavelet_anomaly(y)$anomaly)
#' @export
morie_discrete_wavelet_anomaly <- function(x, threshold = NULL, levels = NULL,
                                           max_span = 8) {
  x <- as.numeric(x)
  n0 <- length(x)
  if (n0 < 4L) stop("need at least 4 observations", call. = FALSE)
  n <- bitwShiftL(1L, as.integer(ceiling(log2(n0))))
  pad <- if (n > n0) c(x, rev(x))[seq_len(n)] else x
  max_lev <- as.integer(log2(n))
  levels <- if (is.null(levels)) max_lev - 1L else min(as.integer(levels),
                                                       max_lev)
  approx <- pad
  details <- vector("list", levels)
  for (l in seq_len(levels)) {
    even <- approx[seq(1L, length(approx), by = 2L)]
    odd <- approx[seq(2L, length(approx), by = 2L)]
    approx <- (even + odd) / sqrt(2)
    details[[l]] <- (even - odd) / sqrt(2)
  }
  d1 <- details[[1L]]
  sigma <- stats::median(abs(d1 - stats::median(d1))) / 0.6745
  lam <- if (is.null(threshold)) sigma * sqrt(2 * log(n)) else as.numeric(threshold)
  score <- numeric(n)
  fired <- integer(n)
  per_level <- integer(levels)
  for (lv in seq_len(levels)) {
    d <- details[[lv]]
    span <- 2^lv
    big <- abs(d) > lam
    per_level[lv] <- sum(big)
    if (span > max_span) next
    for (j in which(big)) {
      lo <- (j - 1L) * span + 1L
      hi <- min(j * span, n)
      idx <- lo:hi
      fresh <- idx[fired[idx] == 0L]
      if (length(fresh)) {
        score[fresh] <- abs(d[j])
        fired[fresh] <- lv
      }
    }
  }
  score <- score[seq_len(n0)]
  fired <- fired[seq_len(n0)]
  anom <- score > lam
  list(anomaly = anom, score = score, sigma = sigma, threshold = lam,
       level_fired = fired, per_level_count = per_level,
       n_anomalies = as.integer(sum(anom)), levels = as.integer(levels),
       warnings = paste("the Haar basis localises abrupt changes well but is",
                        "blind to slow drift, which produces no large",
                        "coefficient at any scale"),
       method = "discrete_wavelet_anomaly")
}


#' Linear-autoencoder reconstruction-error anomaly score
#'
#' Fits a rank-k linear bottleneck by gradient descent with
#' re-orthonormalisation, and scores each row by its squared
#' reconstruction error.
#'
#' In the linear case this IS PCA reconstruction error -- the optimum
#' spans the leading k principal directions -- which is worth knowing
#' before reaching for a deep autoencoder that may be doing the same
#' thing more slowly.
#'
#' The anomalies are in the TRAINING data, so more bottleneck capacity
#' makes detection worse rather than better: at k = d the reconstruction
#' is exact and every error is zero. Keep k well below the intrinsic
#' dimension.
#'
#' @param X data matrix.
#' @param k bottleneck rank.
#' @param n_iter,lr gradient-descent controls.
#' @param seed integer seed.
#' @param contamination expected anomaly fraction, used for the
#'   threshold.
#' @return list with \code{score}, \code{rank} (0-based),
#'   \code{anomaly}, \code{reconstruction}, \code{threshold},
#'   \code{explained_fraction}.
#' @references Hawkins, S. et al. (2002). Outlier detection using
#'   replicator neural networks. \emph{DaWaK}, 170-180.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3); X[7, ] <- c(9, -9, 9)
#' which.max(morie_autoencoder_anomaly(X, k = 1)$score)
#' @export
morie_autoencoder_anomaly <- function(X, k = 2, n_iter = 300, lr = 0.05,
                                      seed = 0, contamination = 0.05) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
  k <- as.integer(k)
  if (k < 1L || k > d) {
    stop(sprintf("k must be between 1 and %d", d), call. = FALSE)
  }
  mu <- colMeans(X)
  Z <- sweep(X, 2L, mu, "-")
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  W <- matrix(stats::rnorm(d * k, 0, 0.1), d, k)
  for (it in seq_len(as.integer(n_iter))) {
    rec <- Z %*% W %*% t(W)
    E <- rec - Z
    G <- 2 * (crossprod(E, Z %*% W) + crossprod(Z, E %*% W)) / n
    W <- W - lr * G
    W <- qr.Q(qr(W))
  }
  rec <- Z %*% W %*% t(W)
  err <- rowSums((Z - rec)^2)
  rank <- integer(n)
  rank[order(-err)] <- seq_len(n) - 1L
  cut <- stats::quantile(err, 1 - contamination, names = FALSE, type = 7L)
  total <- sum(Z^2)
  list(score = err, rank = rank, anomaly = err > cut,
       reconstruction = sweep(rec, 2L, mu, "+"), threshold = cut,
       explained_fraction = 1 - sum(err) / max(total, 1e-300), W = W, k = k,
       warnings = paste("the anomalies are in the training data, so more",
                        "bottleneck capacity makes detection WORSE; keep k",
                        "well below the intrinsic dimension"),
       method = "autoencoder_anomaly")
}


#' IMPALA V-trace off-policy targets
#'
#' Truncated importance weights \eqn{\rho = \min(\bar\rho, \pi/\mu)} and
#' \eqn{c = \min(\bar c, \pi/\mu)} applied to an n-step return.
#'
#' The two clips are not interchangeable and confusing them is the
#' standard mistake. \code{rho_bar} determines WHAT is learned -- the
#' fixed point lies somewhere between the behaviour and target policies,
#' at the behaviour policy when \code{rho_bar} is small and the target
#' policy as it grows. \code{c_bar} determines HOW FAST the trace decays
#' and does not move the fixed point at all.
#'
#' @param rewards,values per-step rewards and value estimates.
#' @param behavior_logp,target_logp log-probabilities under each policy.
#' @param gamma discount.
#' @param rho_bar,c_bar truncation levels.
#' @param bootstrap_value value past the end of the segment.
#' @return list with \code{vs}, \code{advantage}, \code{rho}, \code{c},
#'   \code{delta}, \code{ratio}, \code{n_truncated_rho}.
#' @references Espeholt, L. et al. (2018). IMPALA: scalable distributed
#'   deep-RL with importance weighted actor-learner architectures.
#'   \emph{ICML}, 1407-1416.
#' @examples
#' morie_impala_vtrace(c(1, 0, 1), c(0.5, 0.4, 0.3), c(-1, -1, -1),
#'                     c(-0.9, -1.2, -1))$vs
#' @export
morie_impala_vtrace <- function(rewards, values, behavior_logp, target_logp,
                                gamma = 0.99, rho_bar = 1, c_bar = 1,
                                bootstrap_value = 0) {
  rw <- as.numeric(rewards)
  V <- as.numeric(values)
  blp <- as.numeric(behavior_logp)
  tlp <- as.numeric(target_logp)
  T <- length(rw)
  if (!(length(V) == T && length(blp) == T && length(tlp) == T)) {
    stop("rewards, values and both log-probability arrays must agree",
         call. = FALSE)
  }
  if (gamma < 0 || gamma > 1) {
    stop("gamma must be in [0, 1]", call. = FALSE)
  }
  ratio <- exp(pmax(pmin(tlp - blp, 50), -50))
  rho <- pmin(rho_bar, ratio)
  c_ <- pmin(c_bar, ratio)
  V_next <- c(V[-1L], bootstrap_value)
  delta <- rho * (rw + gamma * V_next - V)
  vs_minus <- numeric(T)
  acc <- 0
  for (t in seq(T, 1L)) {
    acc <- delta[t] + gamma * c_[t] * acc
    vs_minus[t] <- acc
  }
  vs <- V + vs_minus
  vs_next <- c(vs[-1L], bootstrap_value)
  list(vs = vs, advantage = rho * (rw + gamma * vs_next - V), rho = rho,
       c = c_, delta = delta, ratio = ratio,
       n_truncated_rho = as.integer(sum(ratio > rho_bar)),
       n_truncated_c = as.integer(sum(ratio > c_bar)),
       warnings = paste("rho_bar sets WHAT is learned (the fixed point lies",
                        "between behaviour and target policy); c_bar sets HOW",
                        "FAST, and does not move the fixed point"),
       method = "impala_vtrace")
}


#' Denoising score-matching objective
#'
#' Averages \eqn{\lVert s_\theta(\tilde x) + (\tilde x - x)/\sigma^2
#' \rVert^2} over Gaussian perturbations.
#'
#' Vincent's identity is what makes this tractable: the explicit
#' score-matching objective contains the trace of the score Jacobian,
#' which costs a backward pass per dimension, and the denoising form
#' removes it entirely by giving each noisy sample a known target.
#'
#' The objective targets the NOISED distribution \eqn{p_\sigma}, not p.
#' A small sigma reduces that bias but leaves the score unconstrained
#' wherever data is sparse, which is why practical use anneals sigma
#' rather than picking one.
#'
#' @param x data matrix.
#' @param score function mapping a matrix to the score at those points.
#' @param sigma noise scale.
#' @param n_noise perturbations averaged per point.
#' @param seed integer seed.
#' @return list with \code{objective}, \code{per_sample},
#'   \code{target_norm}.
#' @references Vincent, P. (2011). A connection between score matching
#'   and denoising autoencoders. \emph{Neural Computation}, 23(7),
#'   1661-1674.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), ncol = 2)
#' r <- morie_diffusion_score_matching(X, function(z) -z, sigma = 0.5)
#' r$objective > 0
#' @export
morie_diffusion_score_matching <- function(x, score, sigma = 0.1,
                                           n_noise = 16, seed = 0) {
  sigma <- as.numeric(sigma)
  if (sigma <= 0) stop("sigma must be positive", call. = FALSE)
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  per <- numeric(n)
  for (it in seq_len(as.integer(n_noise))) {
    eps <- matrix(stats::rnorm(n * d), n, d)
    xt <- X + sigma * eps
    s <- as.matrix(score(xt))
    if (!identical(dim(s), dim(X))) {
      stop(sprintf("score returned (%d, %d), expected (%d, %d)",
                   nrow(s), ncol(s), n, d), call. = FALSE)
    }
    target <- -(xt - X) / sigma^2
    per <- per + rowSums((s - target)^2)
  }
  per <- per / as.integer(n_noise)
  list(objective = mean(per), sigma = sigma, per_sample = per,
       target_norm = 1 / sigma^2, n_noise = as.integer(n_noise),
       warnings = paste("the objective targets the NOISED distribution",
                        "p_sigma, not p; small sigma reduces the bias but",
                        "leaves the score unconstrained where data is sparse"),
       method = "diffusion_score_matching")
}


#' Zonal energy-balance model (Budyko-Sellers)
#'
#' Each equal-area latitude band balances absorbed shortwave against
#' linearised outgoing longwave \eqn{A + BT} and meridional transport
#' proportional to the departure from the global mean.
#'
#' The model exists to demonstrate BISTABILITY. Albedo depends on
#' temperature -- ice forms below a threshold and reflects far more --
#' so cooling begets cooling. Started warm it settles on a temperate
#' climate; started cold, under identical forcing, on a globally
#' ice-covered one. Both are stable, and \code{start} decides which you
#' find.
#'
#' Bands are equal-area (uniform in sin latitude) rather than uniform in
#' degrees, because the latter over-weights the poles and drags the
#' global mean tens of degrees cold, destroying the very bistability the
#' model is for.
#'
#' @param S solar distribution factor per zone, or a scalar multiplier.
#' @param albedo ice-free albedo.
#' @param A,B outgoing-longwave intercept and slope.
#' @param k meridional transport coefficient.
#' @param n_zones number of bands.
#' @param max_iter,tol iteration controls.
#' @param ice_albedo albedo of ice-covered zones.
#' @param ice_threshold temperature (C) below which a zone ices over.
#' @param start initial temperature (C).
#' @return list with \code{temperature}, \code{global_mean},
#'   \code{ice_fraction}, \code{albedo}, \code{latitude},
#'   \code{snowball}.
#' @references Budyko, M. I. (1969). The effect of solar radiation
#'   variations on the climate of the Earth. \emph{Tellus}, 21(5),
#'   611-619.
#'   Sellers, W. D. (1969). A global climatic model based on the
#'   energy balance of the earth-atmosphere system.
#'   \emph{Journal of Applied Meteorology}, 8(3),
#'   392-400.
#' @examples
#' morie_zonal_ebm(1, start = 20)$snowball
#' morie_zonal_ebm(1, start = -40)$snowball
#' @export
morie_zonal_ebm <- function(S, albedo = 0.3, A = 203.3, B = 2.09, k = 3.8,
                            n_zones = 9, max_iter = 500, tol = 1e-8,
                            ice_albedo = 0.62, ice_threshold = -10,
                            start = 15) {
  n <- as.integer(n_zones)
  if (n < 2L) stop("n_zones must be at least 2", call. = FALSE)
  edges <- seq(-1, 1, length.out = n + 1L)
  xm <- 0.5 * (edges[-length(edges)] + edges[-1L])
  lat <- asin(xm) * 180 / pi
  prof <- 1 - 0.482 * (1.5 * xm^2 - 0.5)
  Sarr <- if (length(S) > 1L) as.numeric(S) else as.numeric(S) * prof
  if (length(Sarr) != n) {
    stop(sprintf("S must be a scalar or have %d entries", n), call. = FALSE)
  }
  Q <- 1361 / 4
  T_ <- rep(as.numeric(start), n)
  conv <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    al <- ifelse(T_ < ice_threshold, ice_albedo, albedo)
    absorbed <- Q * Sarr * (1 - al)
    Tbar <- mean(T_)
    T_new <- (absorbed - A + k * Tbar) / (B + k)
    if (max(abs(T_new - T_)) < tol) {
      T_ <- T_new
      conv <- TRUE
      break
    }
    T_ <- 0.5 * T_ + 0.5 * T_new
  }
  al <- ifelse(T_ < ice_threshold, ice_albedo, albedo)
  ice <- mean(T_ < ice_threshold)
  list(temperature = T_, global_mean = mean(T_), ice_fraction = ice,
       albedo = al, latitude = lat, converged = conv,
       snowball = ice > 0.95,
       warnings = paste("the linearised A + BT outgoing longwave is valid",
                        "over a narrow temperature range and is not reliable",
                        "for the snowball state the model predicts"),
       method = "zonal_ebm")
}
