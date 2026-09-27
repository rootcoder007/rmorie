# SPDX-License-Identifier: AGPL-3.0-or-later
#' Supervised principal components
#'
#' Standardized univariate coefficients s_j = x_j' y / |x_j| on centred data,
#' features with |s_j| > threshold, their first m principal components and a
#' least-squares regression of y on them (ESL Alg. 18.1, eqs 18.32-18.33).
#'
#' @param X Features, N by p.
#' @param y Outcome.
#' @param threshold theta.
#' @param m Number of components.
#' @param newdata Points to predict.
#' @return Named list: scores_univariate, selected (1-based), loadings,
#'   coefficients, fitted, prediction.
#' @references Bair, E., Hastie, T., Paul, D. & Tibshirani, R. (2006). JASA 101,
#'   119-137.
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(i), sin(2 * i), cos(3 * i))
#' morie_esl_supervised_pc(X, X[, 1] + X[, 2] + 0.2 * cos(7 * i), 1)$selected
#' @export
morie_esl_supervised_pc <- function(X, y, threshold, m = 1, newdata = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (length(y) != nrow(X) || threshold < 0 || m < 1) stop("need matching X and y, threshold >= 0 and m >= 1", call. = FALSE)
  xm <- colMeans(X)
  Xc <- sweep(X, 2, xm)
  yc <- y - mean(y)
  s <- drop(crossprod(Xc, yc)) / sqrt(colSums(Xc^2))
  sel <- which(abs(s) > threshold)
  if (length(sel) < m) stop(sprintf("only %d features pass the threshold; need at least m = %d", length(sel), m), call. = FALSE)
  V <- svd(Xc[, sel, drop = FALSE])$v[, seq_len(m), drop = FALSE]
  for (k in seq_len(m)) if (V[which.max(abs(V[, k])), k] < 0) V[, k] <- -V[, k]
  Z <- Xc[, sel, drop = FALSE] %*% V
  beta <- drop(solve(crossprod(Z), crossprod(Z, yc)))
  pred <- if (is.null(newdata)) NULL else drop(sweep(rbind(newdata), 2, xm)[, sel, drop = FALSE] %*% V %*% beta) + mean(y)
  list(scores_univariate = s, selected = sel, loadings = V, coefficients = c(mean(y), beta),
       fitted = drop(Z %*% beta) + mean(y), prediction = pred)
}
