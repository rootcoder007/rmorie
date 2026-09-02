# SPDX-License-Identifier: AGPL-3.0-or-later
#' Residual bootstrap for OLS coefficients
#'
#' Freedman, D. A. (1981), "Bootstrapping Regression Models", The Annals of
#' Statistics 9(6), 1218-1228, doi:10.1214/aos/1176345638 (verified against
#' Crossref).
#'
#' The design is held fixed and only the errors are resampled:
#' y* = X beta_hat + r*, with r*_i drawn with replacement from the centred
#' OLS residuals, then OLS refit on (X, y*).  This conditions on X, which is
#' right when X is genuinely fixed, but it buys that by assuming the errors
#' are iid -- under heteroskedasticity it is inconsistent and the pairs or
#' wild bootstrap is required instead.
#'
#' Because the design never changes, the conditional moments are available in
#' closed form and are this module's anchor: E*\[beta*\] = beta_hat and
#' Var*(beta*) = sigma_tilde^2 (X'X)^-1 with sigma_tilde^2 = sum r_i^2 / n,
#' the resampling distribution's own variance (divisor n, not n - p).
#' var_closed reports the diagonal.  That is the homoskedastic formula with a
#' DOWNWARD-biased scale: the residual bootstrap inherits the OLS residuals'
#' shrinkage, which is exactly why Freedman's rescaled variant divides the
#' residuals by sqrt(1 - h_ii); rescale = TRUE selects it.
#'
#' @param X the n x p design.
#' @param y the n responses.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @param rescale divide residuals by sqrt(1 - h_ii) before resampling.
#' @return list: beta_b, beta_hat, resid, se, lo, hi, var_closed,
#'   sigma2_tilde, n, p, B, estimate, method.
#' @keywords internal
#' @examples
#' X <- cbind(1, 1:12); y <- 2 + 0.5 * (1:12)
#' Btres(X, y, B = 20)$sigma2_tilde
#' @export
Btres <- function(X, y, B = 200, seed = 1, alpha = 0.05, rescale = FALSE) {
  Xm <- .s03mat(X); yy <- .s03vec(y)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(yy)) stop("boot_residual_regression: X and y have different lengths")
  if (n <= p) stop("boot_residual_regression: need more rows than columns")
  if (as.integer(B) < 2L) stop("boot_residual_regression: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_residual_regression: alpha must lie strictly between 0 and 1")
  bh <- .s03lstsq(Xm, yy)
  fit <- as.numeric(Xm %*% bh)
  res <- yy - fit
  XtXinv <- .btres_xtxinv(Xm, p)
  if (isTRUE(rescale)) {
    h <- rowSums((Xm %*% XtXinv) * Xm)
    res <- res / sqrt(pmax(1 - h, 1e-12))
  }
  res <- res - .s03mean(res)
  s2 <- sum(res^2) / n
  g <- .t1_lcg(seed)
  reps <- vector("list", as.integer(B))
  for (b in seq_len(as.integer(B))) {
    ys <- numeric(n)
    for (i in seq_len(n)) {
      j <- as.integer(g$unif() * n)
      if (j >= n) j <- n - 1L
      ys[i] <- fit[i] + res[j + 1L]
    }
    reps[[b]] <- .s03lstsq(Xm, ys)
  }
  se <- numeric(p); lo <- numeric(p); hi <- numeric(p); vc <- numeric(p)
  for (j in seq_len(p)) {
    col <- vapply(reps, function(r) r[j], 0)
    se[j] <- .s03sd(col, 1L)
    lo[j] <- .s03quantile7(col, a / 2)
    hi[j] <- .s03quantile7(col, 1 - a / 2)
    vc[j] <- s2 * XtXinv[j, j]
  }
  list(beta_b = reps, beta_hat = bh, resid = res, se = se, lo = lo, hi = hi,
       var_closed = vc, sigma2_tilde = s2, n = n, p = p, B = as.integer(B),
       estimate = bh[1],
       method = "Freedman (1981) Ann. Statist. 9(6):1218-1228, residual resampling")
}

#' @noRd
.btres_xtxinv <- function(Xm, p) {
  A <- .s03crossprod(Xm)
  out <- matrix(0, p, p)
  for (j in seq_len(p)) {
    e <- numeric(p); e[j] <- 1
    out[, j] <- .s03ridgesolve(A, e)
  }
  out
}
