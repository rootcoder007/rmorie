# SPDX-License-Identifier: AGPL-3.0-or-later
#' Isotropy check by comparing directional semivariograms.
#'
#' A second-order stationary field is ISOTROPIC when the covariance
#' depends only on the LENGTH of the lag, C(h) = C(||h||), so no
#' direction is special. Under geometric anisotropy the iso-correlation
#' contours are ellipses rather than circles and the fix is the affine
#' correction of Sec 4.3.7.
#'
#' Pairs are split into `n_dir` angular sectors on the HALF circle -- a
#' lag and its negation are the same direction -- a semivariogram is
#' fitted in each, and the spread across directions relative to the
#' overall level is the statistic.
#'
#' @param coords Coordinates (n by 2).
#' @param z Numeric vector of length n.
#' @param n_dir Angular sectors on the half circle; at least 2.
#' @param n_bins Lag bins within each sector.
#' @param max_dist Largest lag retained.
#' @param tol Relative spread above which isotropy is rejected.
#' @return Named list: is_isotropic, relative_spread, directional_gamma,
#'   angles, omnidirectional, tol.
#' @references Schabenberger & Gotway (2005), Sec 2.2; correction Sec 4.3.7.
#' @examples
#' co <- matrix(runif(400), 200, 2) * 10
#' spiso(co, sin(co[, 1]) + cos(co[, 2]))$relative_spread
#' @export
spiso <- function(coords, z, n_dir = 4, n_bins = 10, max_dist = NULL,
                  tol = 0.25) {
  coords <- as.matrix(coords); z <- as.numeric(z)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows")
  }
  if (ncol(coords) != 2) stop("directional analysis needs 2-D `coords`")
  if (n_dir < 2) stop("`n_dir` must be >= 2")
  n <- length(z)
  ij <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  i <- ij[, 1]; j <- ij[, 2]
  d <- coords[j, , drop = FALSE] - coords[i, , drop = FALSE]
  dist <- sqrt(rowSums(d^2))
  # A lag and its negation are the same direction, so the angle folds onto
  # [0, pi). Orient every lag into one half-space BEFORE atan2 rather than
  # folding after: for a pair enumerated in the opposite order atan2 returns
  # the supplement, and folding that back mod pi lands one ulp away from the
  # direct value. That is invisible except for lags sitting exactly on a
  # sector boundary -- on a regular lattice thousands of them do -- where it
  # silently reassigns the pair to the neighbouring direction and makes the
  # result depend on the order the points were listed in.
  flip <- d[, 1] < 0 | (d[, 1] == 0 & d[, 2] < 0)
  d[flip, ] <- -d[flip, , drop = FALSE]
  ang <- atan2(d[, 2], d[, 1]) %% pi
  sq <- (z[i] - z[j])^2
  if (is.null(max_dist)) max_dist <- if (length(dist)) max(dist) / 2 else 1
  edges <- seq(0, pi, length.out = n_dir + 1)
  # A lag whose true direction lies exactly on a sector boundary lands a few
  # ulp either side of the edge: atan2 is not correctly rounded and differs
  # between platforms' libm, and the lattice offsets themselves are not
  # exact in binary. On a regular grid that is not a rare event -- thousands
  # of pairs sit on the diagonals -- so letting the last bit decide the
  # sector makes the answer platform-dependent. Snap to the edge at the
  # resolution of the angle computation and let the half-open
  # [edge_a, edge_a+1) convention place them.
  ang_tol <- 8 * .Machine$double.eps * pi
  for (e in edges) ang[abs(ang - e) < ang_tol] <- e
  ang[abs(ang - pi) < ang_tol] <- 0
  lagedges <- seq(0, max_dist, length.out = n_bins + 1)
  gam <- matrix(NA_real_, n_dir, n_bins)
  for (a in seq_len(n_dir)) {
    sel <- ang >= edges[a] & ang < edges[a + 1] & dist <= max_dist
    if (!any(sel)) next
    b <- pmin(pmax(findInterval(dist[sel], lagedges), 1),
              n_bins)
    for (k in seq_len(n_bins)) {
      m <- b == k
      if (any(m)) gam[a, k] <- sum(sq[sel][m]) / (2 * sum(m))
    }
  }
  omni <- .sp_empirical_variogram(coords, z, n_bins, max_dist)$gamma
  spread <- apply(gam, 2, function(col) {
    if (all(is.na(col))) NA_real_ else max(col, na.rm = TRUE) - min(col, na.rm = TRUE) })
  level <- colMeans(gam, na.rm = TRUE)
  rel <- mean(spread / ifelse(level > 0, level, NA_real_), na.rm = TRUE)
  list(is_isotropic = isTRUE(is.finite(rel) && rel <= tol),
       relative_spread = rel, directional_gamma = gam,
       angles = (edges[-length(edges)] + edges[-1]) / 2,
       omnidirectional = omni, tol = tol)
}
