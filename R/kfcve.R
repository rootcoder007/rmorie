# SPDX-License-Identifier: AGPL-3.0-or-later
#' K-fold cross-validation prediction error
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 109-139], Chapter 4, Section 4.3.2 and equation (4.1),
#' p. 129, read as a rendered page.  The data are split into K complementary
#' folds, the model is fitted K times holding one fold out, the testing mean
#' square error of a fold is MSE = (1/T) sum (y_i - fhat(x_i))^2 (4.1), and
#' "the arithmetic mean of the k folds is obtained and reported as the
#' prediction performance", CV_K = (1/K) sum_k MSE_k.  Section 4.3.3 notes
#' that leave-one-out is the K = n case of the same construction.
#'
#' @param y the n observed responses.
#' @param y_hat_folds list of K prediction vectors, one per fold.
#' @param folds optional list of K 0-based index vectors; defaults to the
#'   contiguous complementary partition into K near-equal blocks.
#' @return list: estimate, cv_error, mse_fold, rmse, n, method.
#' @keywords internal
#' @examples
#' Kfcve(c(1, 2, 3, 4), list(c(1, 2), c(3, 4)))$cv_error
#' @export
Kfcve <- function(y, y_hat_folds, folds = NULL) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n == 0L) stop("k_fold_cv_error: y is empty")
  yh <- lapply(y_hat_folds, .s03vec)
  K <- length(yh)
  if (K == 0L) stop("k_fold_cv_error: no folds supplied")
  if (!is.null(folds)) {
    idx <- lapply(folds, function(f) as.integer(f))
  } else {
    idx <- vector("list", K)
    start <- 0L
    for (j in seq_len(K)) {
      m <- n %/% K + if (j <= n %% K) 1L else 0L
      idx[[j]] <- if (m > 0L) seq.int(start, start + m - 1L) else integer(0)
      start <- start + m
    }
  }
  if (length(idx) != K) stop("k_fold_cv_error: folds and y_hat_folds have different lengths")
  mse <- numeric(K)
  for (j in seq_len(K)) {
    if (length(idx[[j]]) != length(yh[[j]])) {
      stop(sprintf("k_fold_cv_error: fold %d has a prediction count that does not match it", j - 1L))
    }
    if (length(idx[[j]]) == 0L) stop(sprintf("k_fold_cv_error: fold %d is empty", j - 1L))
    s <- 0
    for (a in seq_along(idx[[j]])) {
      i <- idx[[j]][a]
      if (i < 0L || i >= n) stop("k_fold_cv_error: fold index out of range")
      d <- yy[i + 1L] - yh[[j]][a]
      s <- s + d * d
    }
    mse[j] <- s / length(idx[[j]])
  }
  cv <- 0
  for (v in mse) cv <- cv + v
  cv <- cv / K
  list(estimate = cv, cv_error = cv, mse_fold = mse, rmse = sqrt(cv), n = n,
       method = "CV_K = (1/K) sum_k MSE_k, Chapter 4 Sect. 4.3.2 with MSE from eq. (4.1)")
}
