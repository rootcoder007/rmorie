# SPDX-License-Identifier: AGPL-3.0-or-later
#' Doubly robust DiD with a spatial lag of treatment
#'
#' With a non-negative neighbour matrix \code{W} the row-standardised
#' spatial lag \code{(W z)_i = sum_j w_ij z_j / sum_j w_ij} is the
#' neighbourhood average of \code{z}.  The lag of the treatment is
#' appended to the covariate block of the doubly robust moment of
#' Sant'Anna and Zhao (2020), equation (2.6), so that both the
#' propensity score and the outcome regression condition on how treated
#' a unit's neighbourhood is.  A \code{W} with no off-diagonal mass
#' makes the lag constant and the two estimates coincide.
#'
#' @param y Outcome change, one entry per unit.
#' @param D Binary treatment indicator.
#' @param X Optional baseline covariates.
#' @param W_neighbors Optional non-negative n x n neighbour weights.
#' @return List with \code{estimate}, \code{tau_nospatial},
#'   \code{spatial_shift}, \code{se}, \code{wd_mean}, \code{wd_sd},
#'   \code{n}.
#' @references Anselin, L. (2003). Spatial externalities, spatial
#'   multipliers, and spatial econometrics. International Regional
#'   Science Review 26(2), 153-166.  Sant'Anna, P. H. C. and Zhao, J.
#'   (2020). Journal of Econometrics 219(1), 101-122.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Drspa(V, V)
Drspa <- function(y, D, X = NULL, W_neighbors = NULL) {
  yv <- .s03vec(y); dv <- .s03vec(D); n <- length(yv)
  if (n == 0L) stop("Drspa: empty input, y has no observations")
  if (length(dv) != n) stop("Drspa: y and D must have the same length")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  if (!is.null(Xr) && nrow(Xr) != n) stop("Drspa: X must have one row per unit")
  if (is.null(W_neighbors)) {
    wd <- rep(0, n)
  } else {
    W <- .s03mat(W_neighbors)
    if (nrow(W) != n || ncol(W) != n) stop("Drspa: W_neighbors must be n x n")
    if (any(W < 0)) stop("Drspa: W_neighbors must be non-negative")
    rs <- rowSums(W)
    wd <- vapply(seq_len(n), function(i)
      if (rs[i] > 0) sum(W[i, ] * dv) / rs[i] else 0, 0)
  }
  base <- .s03drdid(yv, dv, Xr)
  cols <- if (is.null(Xr)) matrix(wd, n, 1L) else cbind(Xr, wd)
  m <- .s03mean(wd); sdv <- if (n > 1L) .s03sd(wd) else 0
  fit <- if (sdv <= 1e-12) base else .s03drdid(yv, dv, cols)
  .t1_result(estimate = fit$tau, tau_nospatial = base$tau,
             spatial_shift = fit$tau - base$tau, se = fit$se,
             wd_mean = m, wd_sd = sdv, n = n,
             method = "Spatial DR-DiD with neighbor effects")
}
