# SPDX-License-Identifier: AGPL-3.0-or-later
#' Split-sample (cross-fitted) doubly robust DiD
#'
#' Sant'Anna and Zhao (2020), Journal of Econometrics 219(1), 101-122
#' (arXiv:1812.01723 -- FETCHED), equations (2.6)-(2.7) for the estimand;
#' Chernozhukov et al. (2018), Double/debiased machine learning, The
#' Econometrics Journal 21(1), C1-C68 (arXiv:1608.00060), definition 3.1
#' for the cross-fitting device: pi and mu_0 are estimated on the
#' complement of each fold and evaluated on the fold, so the estimator
#' stays Neyman-orthogonal without a Donsker condition.
#'
#' Determinism: observation i goes to fold i mod K.  No permutation, no
#' seed; the split is a function of the data ordering alone.
#'
#' @param y outcome change, or period-1 outcome when y0 is given.
#' @param D treatment indicator.
#' @param X covariates.
#' @param K number of folds.
#' @param y0 period-0 outcome.
#' @return list: estimate, se, fold_tau, fold_n, full_tau, n, K, method.
#' @keywords internal
#' @examples
#' Drdidsplit(c(1, 2, 0, 3, 1, 2), c(1, 0, 1, 0, 1, 0), NULL, 2)$estimate
#' @export
Drdidsplit <- function(y, D, X = NULL, K = 5, y0 = NULL) {
  dy <- .s03vec(y)
  if (!is.null(y0)) dy <- dy - .s03vec(y0)
  d <- .s03vec(D); n <- length(dy); KK <- as.integer(K)
  Z <- .s03design(X, n)
  fold <- (seq_len(n) - 1L) %% KK
  inf <- numeric(n); ftau <- numeric(KK); fn <- integer(KK); num <- 0
  for (f in seq_len(KK) - 1L) {
    tr_i <- which(fold != f); te_i <- which(fold == f)
    if (length(te_i) == 0L || length(tr_i) == 0L) {
      ftau[f + 1L] <- NaN; fn[f + 1L] <- length(te_i); next
    }
    gam <- .s03logit(Z[tr_i, , drop = FALSE], d[tr_i], 60L)
    keep0 <- tr_i[d[tr_i] < 0.5]
    b0 <- if (length(keep0)) .s03lstsq(Z[keep0, , drop = FALSE], dy[keep0]) else numeric(ncol(Z))
    s1 <- 0; s0 <- 0
    pis <- numeric(n); mus <- numeric(n)
    for (i in te_i) {
      e <- 0; m <- 0
      for (j in seq_along(gam)) { e <- e + Z[i, j] * gam[j]; m <- m + Z[i, j] * b0[j] }
      p <- .s03sigmoid(e)
      pis[i] <- p; mus[i] <- m
      s1 <- s1 + d[i]
      s0 <- s0 + p * (1 - d[i]) / (1 - p)
    }
    t <- 0
    for (i in te_i) {
      w1 <- if (s1 > 0) d[i] / s1 else 0
      w0 <- if (s0 > 0) pis[i] * (1 - d[i]) / (1 - pis[i]) / s0 else 0
      cc <- (w1 - w0) * (dy[i] - mus[i])
      t <- t + cc
      inf[i] <- length(te_i) * cc
    }
    ftau[f + 1L] <- t; fn[f + 1L] <- length(te_i)
    num <- num + length(te_i) * t
  }
  est <- if (n) num / n else NaN
  inf <- inf - est
  v <- 0
  for (x in inf) v <- v + x * x
  full <- .s03drdid(dy, d, X)
  list(estimate = est, se = if (n) sqrt(v / (n * n)) else NaN,
       fold_tau = ftau, fold_n = fn, full_tau = full$tau, n = n, K = KK,
       method = "Cross-fitted DR-DiD (Sant'Anna and Zhao 2020; Chernozhukov et al. 2018 def. 3.1)")
}
