# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pairs bootstrap confidence intervals for quantile regression
#'
#' Koenker, R. (2005), Quantile Regression, Econometric Society Monograph 38,
#' Cambridge University Press.  Section 3.9 covers resampling for quantile
#' regression; the (x, y)-pair bootstrap is the default there because the
#' asymptotic covariance of the quantile regression estimator involves the
#' conditional density of the response at the quantile, a nuisance nobody
#' wants to estimate, and pair resampling sidesteps it entirely while
#' remaining valid under a stochastic design and heteroskedasticity.
#'
#' The fit minimises Koenker and Bassett's check function
#' sum_i rho_tau(y_i - x_i' beta) with rho_tau(u) = u (tau - 1{u < 0}), by
#' iteratively reweighted least squares with the Schlossmacher weights
#' w_i = tau / max(r_i, eps) for r_i > 0 and (1 - tau) / max(-r_i, eps)
#' otherwise, the standard smooth surrogate for the linear program.  The eps
#' floor keeps a residual landing exactly on the fitted plane from producing
#' an infinite weight; it makes the solution an approximation to the LP
#' vertex, accurate well beyond the resampling noise but not exact, and that
#' is stated here rather than hidden.
#'
#' Anchors: with an intercept-only design the check-function minimiser is the
#' tau-th sample quantile of y, computed by sorting rather than through the
#' fitter; and the fitted objective must not exceed the objective at the OLS
#' coefficients, since the OLS point is feasible for the same minimisation.
#'
#' @param X the n x p design.
#' @param y the n responses.
#' @param tau quantile level, strictly between 0 and 1.
#' @param B replicates.
#' @param alpha two-sided error rate.
#' @param seed seed for the shared deterministic stream.
#' @return list: beta_b, beta_hat, se, lo, hi, loss, tau, n, p, B, estimate,
#'   method.
#' @keywords internal
#' @examples
#' X <- cbind(1, 1:20); y <- 1 + 0.4 * (1:20) + c(rep(0, 10), rep(1, 10))
#' Btnpqr(X, y, 0.5, B = 20)$beta_hat
#' @export
Btnpqr <- function(X, y, tau = 0.5, B = 200, alpha = 0.05, seed = 1) {
  Xm <- .s03mat(X); yy <- .s03vec(y)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(yy)) stop("boot_quantile_regression: X and y have different lengths")
  if (n <= p) stop("boot_quantile_regression: need more rows than columns")
  t <- as.numeric(tau)
  if (!(t > 0 && t < 1)) stop("boot_quantile_regression: tau must lie strictly between 0 and 1")
  if (as.integer(B) < 2L) stop("boot_quantile_regression: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_quantile_regression: alpha must lie strictly between 0 and 1")
  bh <- .btnpqr_fit(Xm, yy, t)
  g <- .t1_lcg(seed)
  reps <- vector("list", as.integer(B))
  for (b in seq_len(as.integer(B))) {
    idx <- integer(n)
    for (i in seq_len(n)) {
      j <- as.integer(g$unif() * n)
      if (j >= n) j <- n - 1L
      idx[i] <- j + 1L
    }
    reps[[b]] <- .btnpqr_fit(Xm[idx, , drop = FALSE], yy[idx], t)
  }
  se <- numeric(p); lo <- numeric(p); hi <- numeric(p)
  for (j in seq_len(p)) {
    col <- vapply(reps, function(r) r[j], 0)
    se[j] <- .s03sd(col, 1L)
    lo[j] <- .s03quantile7(col, a / 2)
    hi[j] <- .s03quantile7(col, 1 - a / 2)
  }
  list(beta_b = reps, beta_hat = bh, se = se, lo = lo, hi = hi,
       loss = .btnpqr_loss(Xm, yy, bh, t), tau = t, n = n, p = p,
       B = as.integer(B), estimate = bh[1],
       method = "Koenker (2005) Quantile Regression, CUP, sec. 3.9 (xy-pair bootstrap)")
}

#' @noRd
.btnpqr_fit <- function(Xm, yy, tau, maxit = 200L, tol = 1e-10, eps = 1e-6) {
  n <- nrow(Xm); p <- ncol(Xm)
  b <- .s03lstsq(Xm, yy)
  for (k in seq_len(maxit)) {
    r <- yy - as.numeric(Xm %*% b)
    w <- ifelse(r > 0, tau / pmax(r, eps), (1 - tau) / pmax(-r, eps))
    sw <- sqrt(w)
    nb <- .s03lstsq(Xm * sw, yy * sw)
    d <- max(abs(nb - b))
    b <- nb
    if (d < tol) break
  }
  b
}

#' @noRd
.btnpqr_loss <- function(Xm, yy, b, tau) {
  u <- yy - as.numeric(Xm %*% b)
  sum(u * (tau - as.numeric(u < 0)))
}
