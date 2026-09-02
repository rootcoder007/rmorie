# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal: valid interval for a spatial autoregressive parameter
#'
#' A SAR/CAR/Durbin likelihood exists only where the implied precision
#' (or |I - rho W|) is positive definite. The bound is an eigenvalue
#' condition, rho in (1/theta_min, 1/theta_max), stated at eq (6.48).
#'
#' Hardcoding (-0.99, 0.99) instead is only safe when W is
#' row-standardised AND the true bound is wider. For a raw adjacency it
#' is not: on a 24-node chain the valid range is (-0.504, 0.504), so most
#' of a (-0.99, 0.99) search lies where the likelihood is undefined.
#'
#' Note that a row-standardised W is ASYMMETRIC, and `eigen(symmetric =
#' TRUE)` would read one triangle only; the matrix is symmetrised first.
#'
#' @param W Weights matrix (n by n).
#' @param form "identity" for I - rho W, "weighted" for D - rho W.
#' @return Numeric length-2 vector (lo, hi), an open interval.
#' @references Schabenberger & Gotway (2005), eq (6.48), p. 340.
#' @noRd
.sp_rho_bounds <- function(W, form = "identity") {
  W <- as.matrix(W)
  M <- if (identical(form, "weighted")) {
    d <- rowSums(W)
    s <- ifelse(d > 0, 1 / sqrt(ifelse(d > 0, d, 1)), 0)
    (W * s) * rep(s, each = nrow(W))
  } else if (identical(form, "identity")) {
    W
  } else {
    stop("`form` must be \"identity\" or \"weighted\"")
  }
  ev <- eigen((M + t(M)) / 2, symmetric = TRUE, only.values = TRUE)$values
  c(
    if (min(ev) < 0) 1 / min(ev) else -Inf,
    if (max(ev) > 0) 1 / max(ev) else Inf
  )
}

#' Internal: `.sp_rho_bounds` shrunk to a closed interval for optimisers
#' @param W Weights matrix.
#' @param form "identity" or "weighted".
#' @param pad Fraction of the width to shrink by at each end.
#' @return Numeric length-2 vector.
#' @noRd
.sp_rho_interval <- function(W, form = "identity", pad = 1e-6) {
  b <- .sp_rho_bounds(W, form)
  lo <- if (is.finite(b[1])) max(b[1], -1e6) else -1e6
  hi <- if (is.finite(b[2])) min(b[2], 1e6) else 1e6
  eps <- pad * max(hi - lo, 1e-12)
  # Snap the endpoints inward onto a 1e-8 lattice. The bound comes from an
  # eigen-decomposition, and the Python arms Jacobi solver and Rs LAPACK
  # agree only to about 1e-12 relative -- enough for an optimiser (or a
  # deterministic grid) started from these endpoints to land on a visibly
  # different point when the likelihood is flat. Snapping makes the
  # interval bit-identical across the two arms.
  c(ceiling((lo + eps) * 1e8) / 1e8, floor((hi - eps) * 1e8) / 1e8)
}
