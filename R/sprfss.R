# SPDX-License-Identifier: AGPL-3.0-or-later
#' Which stationarity assumption the data can support
#'
#' The hierarchy of Sec 2.2. STRICT stationarity means the spatial
#' distribution is invariant under translation, so the field repeats
#' itself throughout the domain. SECOND-ORDER (weak) requires only the
#' first two moments: E\[Z(s)\] = mu constant and Cov\[Z(s), Z(s+h)\] = C(h)
#' depending on the lag alone. INTRINSIC is weaker still -- only the
#' INCREMENTS need be stationary, E\[Z(s+h) - Z(s)\] = 0 with
#' Var\[Z(s+h) - Z(s)\] = 2 gamma(h) -- so a process can be intrinsically
#' stationary with no finite variance and no covariance function at all.
#'
#' Second-order does NOT imply strict in general, but it does in a
#' Gaussian random field where the first two moments fix the
#' distribution. That implication is reported rather than assumed.
#'
#' The increment test orients every pair into the same half-space first.
#' Binning on lag DISTANCE alone averages the +x and -x pairs together,
#' so a linear trend cancels itself and passes; the condition is about
#' the lag VECTOR.
#'
#' @param coords Coordinates (n by d).
#' @param z Numeric vector of length n.
#' @param n_blocks Blocks per axis for the drift check; at least 2.
#' @param n_bins Lag bins for the increment check.
#' @param max_dist Largest lag retained.
#' @param tol Relative drift above which a condition is judged violated.
#' @return Named list: mean_stationary, variance_stationary,
#'   second_order_plausible, intrinsic_plausible, strict_if_gaussian,
#'   mean_drift, variance_drift, increment_bias, increment_means,
#'   block_means, block_vars, n_blocks_used, tol.
#' @references Schabenberger & Gotway (2005), Sec 2.2, pp. 42-43; the
#'   Gaussian implication p. 48; the intrinsic hypothesis p. 51.
#' @examples
#' co <- matrix(runif(400), 200, 2) * 10
#' sprfss(co, rnorm(200))$second_order_plausible
#' @export
sprfss <- function(coords, z, n_blocks = 4, n_bins = 10, max_dist = NULL,
                   tol = 0.25) {
  coords <- as.matrix(coords); z <- as.numeric(z)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows")
  }
  if (n_blocks < 2) stop("`n_blocks` must be >= 2")
  lo <- apply(coords, 2, min); hi <- apply(coords, 2, max)
  span <- ifelse(hi > lo, hi - lo, 1)
  idx <- pmin(pmax(floor(sweep(sweep(coords, 2, lo), 2, span, "/") * n_blocks),
                   0), n_blocks - 1)
  key <- if (ncol(coords) == 1) idx[, 1] else idx[, 1] * n_blocks + idx[, 2]
  means <- c(); vars_ <- c()
  for (k in unique(key)) {
    m <- key == k
    if (sum(m) >= 3) { means <- c(means, mean(z[m])); vars_ <- c(vars_, stats::var(z[m])) }
  }
  if (length(means) < 2) stop("too few populated blocks; reduce `n_blocks`")

  n <- length(z)
  ij <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  i2 <- ij[, 1]; j2 <- ij[, 2]
  lagvec <- coords[j2, , drop = FALSE] - coords[i2, , drop = FALSE]
  dv <- z[j2] - z[i2]
  flip <- lagvec[, 1] < 0
  if (ncol(lagvec) >= 2) {
    onaxis <- lagvec[, 1] == 0
    flip <- ifelse(onaxis, lagvec[, 2] < 0, flip)
  }
  dv <- ifelse(flip, -dv, dv)
  dd <- sqrt(rowSums(lagvec^2))
  md <- if (is.null(max_dist)) (if (length(dd)) max(dd) / 2 else 1) else max_dist
  ke <- pmin(pmax(findInterval(dd, seq(0, md, length.out = n_bins + 1),
                               rightmost.closed = TRUE), 1), n_bins)
  inc_means <- vapply(seq_len(n_bins),
                      function(b) if (any(ke == b)) mean(dv[ke == b]) else NA_real_,
                      numeric(1))
  # ddof=1 to match `overall_sd` below -- this is a scale normaliser for the
  # increments, and the two spreads in one result must be the same estimator.
  inc_sd <- stats::sd(dv); if (!is.finite(inc_sd) || inc_sd == 0) inc_sd <- 1
  inc_bias <- max(abs(inc_means), na.rm = TRUE) / inc_sd

  overall_sd <- stats::sd(z)
  mean_drift <- if (overall_sd > 0) (max(means) - min(means)) / overall_sd else 0
  vbar <- mean(vars_)
  var_drift <- if (vbar > 0) (max(vars_) - min(vars_)) / vbar else 0
  mean_ok <- mean_drift <= tol * 4
  var_ok <- var_drift <= tol * 4
  second_order <- isTRUE(mean_ok && var_ok)
  list(mean_stationary = mean_ok, variance_stationary = var_ok,
       second_order_plausible = second_order,
       intrinsic_plausible = isTRUE(inc_bias <= tol),
       strict_if_gaussian = second_order, mean_drift = mean_drift,
       variance_drift = var_drift, increment_bias = inc_bias,
       increment_means = inc_means, block_means = means, block_vars = vars_,
       n_blocks_used = length(means), tol = tol)
}
