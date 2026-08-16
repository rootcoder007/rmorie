# SPDX-License-Identifier: AGPL-3.0-or-later

# ---------------------------------------------------------------------
# Covariate balance, space-time interaction, Ripley's K, SIBTEST and
# panel cointegration. Mirrors morie.fn.causbalt / jacqkn / mrkcsr /
# difsbs / pdcoin.
# ---------------------------------------------------------------------

# Internal: weighted mean and the weighted variance used for the SMD.
# The sw - sum(w^2)/sw denominator reduces to n - 1 at equal weights, so
# the weighted and unweighted diagnostics agree there.
#' Internal: weighted mean and the weighted variance used for the SMD
#'
#' The sw - sum(w^2)/sw denominator reduces to n - 1 at equal weights,
#' so the weighted and unweighted diagnostics agree there.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param w Numeric; passed to \code{sum}.
#' @return A vector, from \code{c}.
#' @export
.smd_moments <- function(x, w) {
  sw <- sum(w)
  m <- sum(w * x) / sw
  denom <- sw - sum(w^2) / sw
  v <- if (denom > 0) sum(w * (x - m)^2) / denom else NA_real_
  c(mean = m, var = v)
}

#' Standardised mean difference for covariate balance
#'
#' For each column of \code{x} the standardised mean difference is
#' \eqn{(\bar\mu_t - \bar\mu_c) / \sqrt{(s_t^2 + s_c^2)/2}}, the
#' difference in group means in units of the pooled within-group spread.
#' The pooling is a plain average of the two variances rather than the
#' sample-size-weighted one, so the denominator does not move when
#' weighting changes the effective group sizes.
#'
#' No p-value is reported. Austin's argument is that a hypothesis test of
#' balance confounds imbalance with sample size, so the convention is to
#' compare \eqn{|SMD|} against a fixed threshold, 0.1 by default.
#'
#' Mirrors \code{morie.fn.causbalt} on the Python side.
#'
#' @param x Numeric matrix of covariates (n x p); a vector is one column.
#' @param treat Binary treatment indicator, length n.
#' @param weights Optional non-negative observation weights.
#' @param threshold Imbalance cutoff for \eqn{|SMD|}. Default 0.1.
#' @return Named list with \code{smd}, \code{max_smd}, \code{imbalanced},
#'   \code{n_imbalanced}, \code{balanced}, \code{threshold},
#'   \code{n_treated}, \code{n_control}, \code{method}.
#' @references Austin PC (2009). Balance diagnostics for comparing the
#'   distribution of baseline covariates between treatment groups in
#'   propensity-score matched samples. \emph{Statistics in Medicine},
#'   28(25), 3083-3107.
#' @examples
#' set.seed(1)
#' morie_covariate_balance(matrix(rnorm(300), 100, 3), rbinom(100, 1, 0.5))$max_smd
#' @export
morie_covariate_balance <- function(x, treat, weights = NULL, threshold = 0.1) {
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  n <- nrow(X)
  p <- ncol(X)
  if (length(treat) != n) {
    stop("treat must have one entry per row of x; got ", length(treat),
         " and ", n, ".", call. = FALSE)
  }
  lev <- sort(unique(treat))
  if (length(lev) != 2L) {
    stop("treat must be binary; got ", length(lev), " distinct values.", call. = FALSE)
  }
  if (!all(is.finite(X))) stop("x must be finite.", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n) {
    stop("weights must have one entry per row of x; got ", length(w),
         " and ", n, ".", call. = FALSE)
  }
  if (any(w < 0) || !all(is.finite(w))) {
    stop("weights must be finite and non-negative.", call. = FALSE)
  }

  is_t <- treat == lev[2L]
  if (sum(w[is_t]) <= 0 || sum(w[!is_t]) <= 0) {
    stop("Both treatment groups need positive total weight.", call. = FALSE)
  }

  smd <- vapply(seq_len(p), function(j) {
    a <- .smd_moments(X[is_t, j], w[is_t])
    b <- .smd_moments(X[!is_t, j], w[!is_t])
    pooled <- (a[["var"]] + b[["var"]]) / 2
    if (is.finite(pooled) && pooled > 0) (a[["mean"]] - b[["mean"]]) / sqrt(pooled) else 0
  }, numeric(1))

  over <- which(abs(smd) > threshold)
  list(
    smd = smd,
    max_smd = max(abs(smd)),
    imbalanced = over,
    n_imbalanced = length(over),
    balanced = length(over) == 0L,
    threshold = threshold,
    n_treated = sum(is_t),
    n_control = sum(!is_t),
    method = paste0("Standardised mean difference (Austin 2009)",
                    if (is.null(weights)) "" else ", weighted")
  )
}

# Internal: a_ij = 1 when j is among i's k nearest, excluding i itself.
#' Internal: a_ij = 1 when j is among i\'s k nearest, excluding i itself
#'
#' A step of the panelspatial implementation. Called by \code{morie_jacquez_knn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A matrix; indexed by row and column.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{A}, as built in the body.
#' @export
.knn_indicator <- function(D, k) {
  diag(D) <- Inf
  n <- nrow(D)
  A <- matrix(FALSE, n, n)
  for (i in seq_len(n)) A[i, order(D[i, ])[seq_len(k)]] <- TRUE
  A
}

#' Jacquez k-nearest-neighbour test for space-time interaction
#'
#' Counts the pairs near in space \emph{and} near in time,
#' \eqn{J_k = \sum_i \sum_j a_{ij} b_{ij}}, where \eqn{a_{ij}} marks
#' \eqn{j} among \eqn{i}'s \eqn{k} nearest neighbours in space and
#' \eqn{b_{ij}} the same in time. Neither indicator is symmetric, so the
#' sum runs over ordered pairs.
#'
#' The statistic is a count, so it does not depend on the units of the
#' coordinates or the clock and stays valid for irregular regions. That
#' is also why the null is simulated: under no interaction the time
#' labels are exchangeable, so permuting them and ranking the observed
#' count gives \eqn{p = (1 + \#\{J^{(b)} \ge J^{obs}\}) / (1 + B)}.
#'
#' Mirrors \code{morie.fn.jacqkn} on the Python side.
#'
#' @param coords Numeric matrix of case locations (n x d).
#' @param time Numeric vector of case times, length n.
#' @param k Number of nearest neighbours, in space and time alike.
#' @param B Number of time-label permutations. Default 999.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{expected}, \code{k}, \code{n}, \code{B},
#'   \code{null_statistics}, \code{method}.
#' @references Jacquez GM (1996). A k nearest neighbour test for
#'   space-time interaction. \emph{Statistics in Medicine}, 15(18),
#'   1935-1949.
#' @examples
#' set.seed(1)
#' morie_jacquez_knn(matrix(runif(60), 30, 2), runif(30), k = 3, B = 99)$p_value
#' @export
morie_jacquez_knn <- function(coords, time, k = 3L, B = 999L) {
  P <- if (is.null(dim(coords))) matrix(as.numeric(coords), ncol = 1L) else as.matrix(coords)
  n <- nrow(P)
  tt <- as.numeric(time)
  if (length(tt) != n) {
    stop("time must have one entry per case; got ", length(tt), " and ", n, ".",
         call. = FALSE)
  }
  if (!all(is.finite(P)) || !all(is.finite(tt))) {
    stop("coords and time must be finite.", call. = FALSE)
  }
  k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1, got ", k, ".", call. = FALSE)
  if (k >= n) {
    stop("k must be smaller than the number of cases; got k=", k, ", n=", n, ".",
         call. = FALSE)
  }

  A <- .knn_indicator(as.matrix(stats::dist(P)), k)
  stat <- function(times) {
    sum(A & .knn_indicator(as.matrix(stats::dist(matrix(times, ncol = 1L))), k))
  }
  observed <- stat(tt)

  B <- as.integer(B)
  if (B < 1L) stop("B must be at least 1, got ", B, ".", call. = FALSE)
  null <- vapply(seq_len(B), function(i) stat(sample(tt)), numeric(1))

  list(
    statistic = observed,
    p_value = (1 + sum(null >= observed)) / (1 + B),
    expected = mean(null),
    k = k, n = n, B = B,
    null_statistics = null,
    method = "Jacquez (1996) k-NN space-time interaction test"
  )
}

# Internal: Ripley's K without edge correction. The bias is shared with
# the simulated patterns, drawn in the same window, so it cancels.
#' Internal: Ripley\'s K without edge correction. The bias is shared
#' with
#'
#' the simulated patterns, drawn in the same window, so it cancels.
#'
#' @param P A matrix; passed to \code{nrow}.
#' @param radii See Usage.
#' @param area Numeric; combined arithmetically in the body.
#' @return A vector, from \code{vapply}.
#' @export
.ripley_k <- function(P, radii, area) {
  n <- nrow(P)
  d <- as.matrix(stats::dist(P))
  diag(d) <- Inf
  vapply(radii, function(r) area * sum(d <= r) / (n * n), numeric(1))
}

#' CSR test via Monte Carlo envelopes on Ripley's K
#'
#' Compares the observed \eqn{\hat K(r)} with envelopes from patterns
#' simulated under complete spatial randomness in the same window. Under
#' CSR in the plane \eqn{K(r) = \pi r^2}, but that holds only on the
#' infinite plane; inside a bounded window the uncorrected estimator is
#' biased downward, so the envelopes are the right reference.
#'
#' The p-value is one maximum deviation over the whole radius range,
#' ranked among the simulations. Reading significance off the radius
#' where the curve leaves a pointwise envelope would be multiple testing.
#'
#' This is the second-order counterpart of
#' \code{\link{morie_csr_nn_test}}, which tests the same null through
#' first-order nearest-neighbour distances.
#'
#' Mirrors \code{morie.fn.mrkcsr} on the Python side.
#'
#' @param coords Numeric matrix of event locations (n x d).
#' @param window Observation region as \code{d} (min, max) pairs.
#'   Defaults to the bounding box of \code{coords}.
#' @param nsim Number of CSR patterns simulated. Default 99.
#' @param radii Radii at which K is evaluated. Defaults to 19 points up
#'   to a quarter of the smallest window side.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{radii}, \code{k_observed}, \code{k_mean}, \code{k_lower},
#'   \code{k_upper}, \code{n}, \code{nsim}, \code{area}, \code{method}.
#' @references Ripley BD (1977). Modelling spatial patterns.
#'   \emph{Journal of the Royal Statistical Society, Series B}, 39(2),
#'   172-212.
#' @examples
#' set.seed(1)
#' morie_ripley_csr_test(matrix(runif(200), 100, 2), nsim = 49)$p_value
#' @export
morie_ripley_csr_test <- function(coords, window = NULL, nsim = 99L,
                                  radii = NULL) {
  P <- if (is.null(dim(coords))) matrix(as.numeric(coords), ncol = 1L) else as.matrix(coords)
  n <- nrow(P)
  if (n < 3L) stop("Need at least 3 events, got ", n, ".", call. = FALSE)
  if (!all(is.finite(P))) stop("coords must be finite.", call. = FALSE)

  bounds <- .csrnn_window(window, P)
  side <- bounds[, 2L] - bounds[, 1L]
  area <- prod(side)

  if (is.null(radii)) radii <- seq(0, min(side) / 4, length.out = 20L)[-1L]
  r <- as.numeric(radii)
  if (length(r) == 0L || any(r <= 0)) {
    stop("radii must be positive and non-empty.", call. = FALSE)
  }

  k_obs <- .ripley_k(P, r, area)

  nsim <- as.integer(nsim)
  if (nsim < 1L) stop("nsim must be at least 1, got ", nsim, ".", call. = FALSE)
  d <- ncol(P)
  sims <- matrix(NA_real_, nsim, length(r))
  for (i in seq_len(nsim)) {
    Q <- matrix(NA_real_, n, d)
    for (j in seq_len(d)) Q[, j] <- stats::runif(n, bounds[j, 1L], bounds[j, 2L])
    sims[i, ] <- .ripley_k(Q, r, area)
  }

  k_mean <- colMeans(sims)
  u_obs <- max(abs(k_obs - k_mean))
  u_sim <- vapply(seq_len(nsim), function(i) {
    others <- if (nsim > 1L) (k_mean * nsim - sims[i, ]) / (nsim - 1L) else k_mean
    max(abs(sims[i, ] - others))
  }, numeric(1))

  list(
    statistic = u_obs,
    p_value = (1 + sum(u_sim >= u_obs)) / (1 + nsim),
    radii = r,
    k_observed = k_obs,
    k_mean = k_mean,
    k_lower = apply(sims, 2L, min),
    k_upper = apply(sims, 2L, max),
    n = n, nsim = nsim, area = area,
    method = "Monte Carlo envelope on Ripley's K (Ripley 1977)"
  )
}
