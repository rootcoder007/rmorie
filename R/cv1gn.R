# SPDX-License-Identifier: AGPL-3.0-or-later
#' CV1 genomic cross-validation: train on observed, predict unobserved lines
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer, read
#' as rendered pages.  Volume \[Pages 109-139\], Chapter 4, Section 4.3.6,
#' p. 118, defines the scheme: the lines are partitioned into g groups, "the
#' information of g-1 groups are used as the training set while all
#' individuals of the remaining group are used as the testing set. ...
#' Jarquin et al. (2017) denotes this type of CV strategy as CV1".  Section
#' 4.5.1, equation (4.2), p. 129, gives the predictive ability as Pearson's
#' correlation between the testing observations and their predictions, and
#' equation (4.1) gives the testing mean squared error.
#'
#' Volume \[Pages 141-170\], Chapter 5, Section 5.3, equation (5.3), supplies
#' the predictor: the GBLUP mean 1_n mu + Z_L b with b ~ N(0, sigma_g^2 G).
#' Written on the marker scale that is the ridge predictor used here, with
#' lam standing for sigma^2 / sigma_beta^2.
#'
#' DETERMINISM.  The folds are not drawn.  Line i goes to fold i mod K, the
#' in-order complementary partition, so that K = n is exactly leave-one-out and
#' both arms partition identically.
#'
#' @param y the n phenotypes.
#' @param markers n-by-p marker matrix.
#' @param n_folds number of complementary groups, 2 <= K <= n.
#' @param lam ridge penalty standing for sigma^2/sigma_beta^2.
#' @return list: estimate, pa, mse, y_hat, pa_fold, fold, n, method.
#' @keywords internal
#' @examples
#' Cv1gn(c(1, 2, 3, 4), matrix(c(1, 0, 0, 1, 1, 1, 0, 0), 4, 2), 2)$pa
#' @export
Cv1gn <- function(y, markers, n_folds, lam = 1) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n < 2L) stop("cv1_genomic: need at least two lines")
  X <- .s03mat(markers)
  if (nrow(X) != n) stop("cv1_genomic: markers has a different number of rows than y")
  p <- ncol(X)
  K <- as.integer(n_folds)
  if (K < 2L || K > n) {
    stop("cv1_genomic: n_folds must lie between 2 and the number of lines")
  }
  lam <- as.numeric(lam)
  if (lam < 0) stop("cv1_genomic: lam must be non-negative")
  fold <- (seq_len(n) - 1L) %% K
  yhat <- numeric(n)
  for (f in seq_len(K) - 1L) {
    tr <- which(fold != f)
    te <- which(fold == f)
    if (length(tr) == 0L || length(te) == 0L) {
      stop("cv1_genomic: a fold left no training or no testing lines")
    }
    mu <- sum(yy[tr]) / length(tr)
    A <- matrix(0, p, p)
    r <- numeric(p)
    for (i in tr) {
      d <- yy[i] - mu
      for (a in seq_len(p)) {
        r[a] <- r[a] + X[i, a] * d
        for (cc in seq_len(p)) A[a, cc] <- A[a, cc] + X[i, a] * X[i, cc]
      }
    }
    for (a in seq_len(p)) A[a, a] <- A[a, a] + lam
    beta <- .s03ridgesolve(A, r, 1e-12)
    for (i in te) {
      s <- mu
      for (a in seq_len(p)) s <- s + X[i, a] * beta[a]
      yhat[i] <- s
    }
  }
  pa_fold <- numeric(K)
  for (f in seq_len(K) - 1L) {
    te <- which(fold == f)
    pa_fold[f + 1L] <- if (length(te) > 1L) .s03corr(yy[te], yhat[te]) else NaN
  }
  s <- sum((yy - yhat)^2)
  list(estimate = .s03corr(yy, yhat), pa = .s03corr(yy, yhat), mse = s / n,
       y_hat = yhat, pa_fold = pa_fold, fold = fold, n = n,
       method = paste0("CV1 of Chapter 4 Sect. 4.3.6 scored by eqs. ",
                       "(4.1)-(4.2), GBLUP predictor of eq. (5.3)"))
}
