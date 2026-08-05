# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ordinary kriging with a spherical variogram
#'
#' Solves the ordinary-kriging system
#' \eqn{\[\[Gamma, 1\], \[1 prime, 0\]\] \[lambda; mu\] = \[gamma_0; 1\]} at each
#' prediction location, where \code{Gamma_ij} is the semivariance between
#' observed locations and \code{gamma_0} the semivariance between the
#' observed locations and the target.  The unbiasedness constraint
#' \code{sum(lambda) = 1} is the last row of the system.
#'
#' Formula: gamma(h) = 0 for h = 0; nugget + (sill - nugget) *
#'   (1.5 h/a - 0.5 (h/a)^3) for 0 < h <= a; sill for h > a.
#'
#' @param known_coords Matrix of observed locations, one row per point.
#' @param known_values Observed values, one per row of \code{known_coords}.
#' @param predict_coords Matrix of prediction locations.
#' @param nugget Variogram nugget. Default 0.
#' @param sill Variogram sill. Default 1.
#' @param range_ Variogram range. Default 1.
#' @return List with \code{estimate} (predictions), \code{predictions},
#'   \code{variances}, \code{nugget}, \code{sill}, \code{range},
#'   \code{n_known}, \code{n_predict}, \code{method}.
#' @references Matheron (1963), Principles of geostatistics, Economic
#'   Geology 58(8):1246-1266, \doi{10.2113/gsecongeo.58.8.1246};
#'   Cressie (1993), Statistics for Spatial Data, rev. ed., Wiley.
#' @export
Krig <- function(known_coords, known_values, predict_coords,
                 nugget = 0, sill = 1, range_ = 1) {
  K <- .s03mat(known_coords)
  P <- .s03mat(predict_coords)
  z <- .s03vec(known_values)
  n <- length(z)
  if (n == 0L) stop("ordinary_kriging: known_values is empty")
  if (nrow(K) != n) stop("known_coords and known_values must have same length.")
  if (ncol(P) != ncol(K)) stop("ordinary_kriging: coordinate dimensions differ")
  m <- nrow(P)
  sv <- function(h) {
    ifelse(h == 0, 0,
      ifelse(h <= range_,
             nugget + (sill - nugget) * (1.5 * h / range_ - 0.5 * (h / range_)^3),
             sill))
  }
  dmat <- function(A, B) {
    out <- matrix(0, nrow(A), nrow(B))
    for (i in seq_len(nrow(A))) {
      for (j in seq_len(nrow(B))) out[i, j] <- sqrt(sum((A[i, ] - B[j, ])^2))
    }
    out
  }
  A <- matrix(0, n + 1L, n + 1L)
  A[seq_len(n), seq_len(n)] <- sv(dmat(K, K))
  A[seq_len(n), n + 1L] <- 1
  A[n + 1L, seq_len(n)] <- 1
  dp <- dmat(K, P)
  pred <- numeric(m); vars <- numeric(m)
  for (j in seq_len(m)) {
    g0 <- sv(dp[, j])
    b <- c(g0, 1)
    lam <- tryCatch(solve(A, b), error = function(e) .s03lstsq(A, b))
    pred[j] <- sum(lam[seq_len(n)] * z)
    vars[j] <- sum(lam[seq_len(n)] * g0) + lam[n + 1L]
  }
  .t1_result(estimate = pred, predictions = pred, variances = vars,
             nugget = nugget, sill = sill, range = range_,
             n_known = n, n_predict = m,
             method = "Ordinary kriging, spherical variogram (Matheron 1963)")
}
