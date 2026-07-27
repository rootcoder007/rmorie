# SPDX-License-Identifier: AGPL-3.0-or-later

#' Kernel SVM (RBF / poly / sigmoid) -- R parity
#'
#' Native C-SVC via SMO (LIBSVM's formulation); no SVM package is used.
#' Multi-class uses one-against-one voting over all pairs.
#'
#' @param x Numeric predictor matrix.
#' @param y Binary response.
#' @param kernel One of "rbf" (radial), "poly", "sigmoid", "linear".
#' @param C Cost parameter.
#' @param gamma Kernel coefficient ("scale" -> 1/(ncol(x)*var(x)), "auto" -> 1/p, or numeric).
#' @param degree Polynomial degree.
#' @param seed RNG seed.
#' @return Named list: estimate, train_accuracy, n_support, kernel, C,
#'   gamma, degree, n, method.
#' @importFrom stats predict
#' @examples
#' morie_svm_kernel_trick(x = rnorm(50), y = rnorm(50))
#' @export
morie_svm_kernel_trick <- function(x, y, kernel = "rbf", C = 1.0,
                             gamma = "scale", degree = 3L, seed = 0L) {
  x <- .morie_ensure_design_matrix(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  y <- as.factor(y)
  lev <- levels(y)
  if (length(lev) < 2L) stop("morie_svm_kernel_trick needs at least two classes.")
  ktype <- .svm_kernel_code(kernel)
  g <- .svm_gamma(gamma, x)
  set.seed(seed)

  if (length(lev) == 2L) {
    ypm <- ifelse(y == lev[2L], 1, -1)
    fit <- .svm_fit_binary(x, ypm, C, ktype, g, 0, degree)
    dv <- .svm_decide(fit, x, ktype, g, 0, degree)
    preds <- factor(ifelse(dv > 0, lev[2L], lev[1L]), levels = lev)
    n_sv <- fit$n_sv
  } else {
    # One-against-one: every pair votes, as LIBSVM does for multi-class.
    votes <- matrix(0L, nrow(x), length(lev))
    n_sv <- 0L
    for (a in seq_len(length(lev) - 1L)) {
      for (b in seq(a + 1L, length(lev))) {
        idx <- which(y == lev[a] | y == lev[b])
        ypm <- ifelse(y[idx] == lev[b], 1, -1)
        f <- .svm_fit_binary(x[idx, , drop = FALSE], ypm, C, ktype, g, 0, degree)
        n_sv <- n_sv + f$n_sv
        d <- .svm_decide(f, x, ktype, g, 0, degree)
        win <- ifelse(d > 0, b, a)
        votes[cbind(seq_len(nrow(x)), win)] <- votes[cbind(seq_len(nrow(x)), win)] + 1L
      }
    }
    preds <- factor(lev[max.col(votes, ties.method = "first")], levels = lev)
  }
  acc <- mean(preds == y)
  list(
    estimate       = as.numeric(acc),
    train_accuracy = as.numeric(acc),
    n_support      = as.integer(n_sv),
    kernel         = kernel,
    C              = as.numeric(C),
    gamma          = as.character(gamma),
    degree         = as.integer(degree),
    n              = nrow(x),
    method         = sprintf("Kernel SVM (%s)", kernel)
  )
}
