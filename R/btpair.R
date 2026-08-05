# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pairs (case) bootstrap for OLS: resample (x_i, y_i) jointly
#'
#' Freedman, D. A. (1981), "Bootstrapping Regression Models", The Annals of
#' Statistics 9(6), 1218-1228, doi:10.1214/aos/1176345638 (verified against
#' Crossref).
#'
#' Freedman's paper contains both regression bootstraps and is explicit about
#' when each is legitimate.  Resampling residuals conditions on X and
#' therefore assumes the errors are iid; resampling whole cases treats the
#' rows as an iid draw from a joint distribution and so survives
#' heteroskedasticity and a stochastic design, at the price of not
#' conditioning on X.  This is the case version: draw i*_1, ..., i*_n
#' uniformly with replacement from 1..n, refit OLS on (X[i*], y[i*]), collect
#' beta*.
#'
#' Because the design is redrawn a resample can be rank deficient; the fit is
#' the package's ridge-stabilised normal-equation solve, so such a resample
#' yields a shrunken rather than an undefined coefficient, and the count of
#' near-singular resamples is reported in n_illcond.
#'
#' Anchor: on a noiseless fixture y = X beta the OLS fit of every full-rank
#' resample recovers beta exactly, so the replicate spread is zero.
#'
#' @param X the n x p design, intercept column included if wanted.
#' @param y the n responses.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate for the percentile interval.
#' @return list: beta_b, beta_hat, se, lo, hi, n_illcond, n, p, B, estimate,
#'   method.
#' @keywords internal
#' @examples
#' X <- cbind(1, 1:12); y <- 2 + 0.5 * (1:12)
#' Btpair(X, y, B = 20)$se
#' @export
Btpair <- function(X, y, B = 200, seed = 1, alpha = 0.05) {
  Xm <- .s03mat(X); yy <- .s03vec(y)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(yy)) stop("boot_pairs_regression: X and y have different lengths")
  if (n <= p) stop("boot_pairs_regression: need more rows than columns")
  if (as.integer(B) < 2L) stop("boot_pairs_regression: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_pairs_regression: alpha must lie strictly between 0 and 1")
  bh <- .s03lstsq(Xm, yy)
  g <- .t1_lcg(seed)
  reps <- vector("list", as.integer(B)); ill <- 0L
  for (b in seq_len(as.integer(B))) {
    idx <- integer(n)
    for (i in seq_len(n)) {
      j <- as.integer(g$unif() * n)
      if (j >= n) j <- n - 1L
      idx[i] <- j + 1L
    }
    if (length(unique(idx)) < p) ill <- ill + 1L
    reps[[b]] <- .s03lstsq(Xm[idx, , drop = FALSE], yy[idx])
  }
  se <- numeric(p); lo <- numeric(p); hi <- numeric(p)
  for (j in seq_len(p)) {
    col <- vapply(reps, function(r) r[j], 0)
    se[j] <- .s03sd(col, 1L)
    lo[j] <- .s03quantile7(col, a / 2)
    hi[j] <- .s03quantile7(col, 1 - a / 2)
  }
  list(beta_b = reps, beta_hat = bh, se = se, lo = lo, hi = hi,
       n_illcond = ill, n = n, p = p, B = as.integer(B), estimate = bh[1],
       method = "Freedman (1981) Ann. Statist. 9(6):1218-1228, case resampling")
}
