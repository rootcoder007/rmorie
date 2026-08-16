# SPDX-License-Identifier: AGPL-3.0-or-later
#' Grid search for DNN hyperparameter tuning with cross-validation
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer, read
#' as rendered pages.  Volume [Pages 427-476], Chapter 11, Section 11.4,
#' pp. 438-441: the tuning is a "full Cartesian grid search (with sample = 1)"
#' over the declared flags, run inside an inner cross-validation, and the
#' combination with the best inner score is kept.  Volume [Pages 109-139],
#' Chapter 4, Section 4.4.2, p. 127, states the same rule in general terms --
#' train "with every permutation of hyperparameter choices using the training
#' set.  Then, the combination of hyperparameters with the best prediction
#' performance on the validation set is chosen" -- and Section 4.3.2 with
#' equation (4.1) supplies the score, CV_K, which is the objective
#' argmin_H CV_K(H).
#'
#' DETERMINISM.  Folds are the in-order complementary partition, i mod K, so
#' K = n is exactly leave-one-out; the Cartesian product is enumerated in a
#' fixed order, last key varying fastest, and ties go to the first point, so
#' both arms select the same combination.
#'
#' @param param_grid named list, name -> candidate values.
#' @param cv_data list(X, y); X is n-by-p, y has length n.
#' @param fit_cv optional function (X, y, K, params) -> CV score.  Defaults to
#'   a ridge regression whose penalty is the grid key "lam".
#' @param k number of inner folds; k = n is leave-one-out.
#' @return list: estimate, best_params, cv_score, scores, grid, keys, n, method.
#' @keywords internal
#' @examples
#' Htprd(list(lam = c(0.1, 1)), list(cbind(1, c(-1, 0, 1, 2)), c(1, 2, 2, 5)), k = 2)$cv_score
#' @export
Htprd <- function(param_grid, cv_data, fit_cv = NULL, k = 5L) {
  if (length(param_grid) == 0L) stop("hyperparameter_tuning_grid: the grid is empty")
  X <- .s03mat(cv_data[[1L]])
  y <- .s03vec(cv_data[[2L]])
  n <- length(y)
  if (n < 2L || nrow(X) != n) {
    stop("hyperparameter_tuning_grid: cv_data must be an n-by-p X and an n-vector y")
  }
  K <- as.integer(k)
  if (K < 2L || K > n) stop("hyperparameter_tuning_grid: k must lie between 2 and n")
  fn <- if (is.null(fit_cv)) .htprd_ridge_cv else fit_cv
  keys <- names(param_grid)
  pts <- list(list())
  for (kn in keys) {
    vals <- param_grid[[kn]]
    if (length(vals) == 0L) {
      stop(sprintf("hyperparameter_tuning_grid: %s has no candidate values", kn))
    }
    nxt <- list()
    for (d in pts) {
      for (v in vals) {
        e <- d
        e[[kn]] <- v
        nxt[[length(nxt) + 1L]] <- e
      }
    }
    pts <- nxt
  }
  scores <- vapply(pts, function(pt) as.numeric(fn(X, y, K, pt)), 0)
  best <- 1L
  if (length(scores) > 1L) {
    for (i in seq(2L, length(scores))) if (scores[i] < scores[best]) best <- i
  }
  list(estimate = scores[best], best_params = pts[[best]],
       cv_score = scores[best], scores = scores, grid = pts, keys = keys,
       n = length(pts),
       method = paste0("argmin_H CV_K(H) over the full Cartesian grid, ",
                       "Chapter 11 Sect. 11.4 with CV_K of eq. (4.1)"))
}

#' .htprd_ridge_cv
#'
#' Part of the htprd implementation; see the file header for the source
#' it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param K See Usage.
#' @param params See Usage.
#' @return A numeric value.
#' @export
.htprd_ridge_cv <- function(X, y, K, params) {
  n <- length(y)
  lam <- if (is.null(params$lam)) 1 else as.numeric(params$lam)
  p <- ncol(X)
  mse <- numeric(K)
  for (f in seq_len(K) - 1L) {
    tr <- which((seq_len(n) - 1L) %% K != f)
    te <- which((seq_len(n) - 1L) %% K == f)
    if (length(tr) == 0L || length(te) == 0L) {
      stop("hyperparameter_tuning_grid: a fold left no training or no testing rows")
    }
    A <- matrix(0, p, p)
    b <- numeric(p)
    for (i in tr) {
      for (a in seq_len(p)) {
        b[a] <- b[a] + X[i, a] * y[i]
        for (cc in seq_len(p)) A[a, cc] <- A[a, cc] + X[i, a] * X[i, cc]
      }
    }
    for (a in seq_len(p)) A[a, a] <- A[a, a] + lam
    beta <- .s03ridgesolve(A, b, 1e-12)
    s <- 0
    for (i in te) {
      e <- y[i]
      for (a in seq_len(p)) e <- e - X[i, a] * beta[a]
      s <- s + e * e
    }
    mse[f + 1L] <- s / length(te)
  }
  sum(mse) / K
}
