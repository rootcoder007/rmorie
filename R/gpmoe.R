# SPDX-License-Identifier: AGPL-3.0-or-later
#' Predict from K Gaussian-process experts under a softmax gate
#'
#' One GP over the whole input is both slow and wrong when different
#' regions want different smoothness. Tresp fits several GPs and lets a
#' gate decide per test point. The predictive variance picks up both the
#' experts own uncertainty and their disagreement.
#'
#' Determinism: experts partition the training set into K contiguous
#' blocks along the sorted first coordinate and the gate is a softmax of
#' negative squared distance to each block centroid. No EM, no random
#' initialisation.
#'
#' Formula: \code{mu(x) = sum_k pi_k(x) mu_k(x)},
#' \code{var(x) = sum_k pi_k(x) \[s_k^2(x) + mu_k(x)^2\] - mu(x)^2}.
#'
#' @param X Training inputs.
#' @param y Training targets.
#' @param X_test Test inputs.
#' @param K Number of experts.
#' @param ell Squared-exponential length scale.
#' @param noise Observation noise on each expert diagonal.
#' @return List with \code{estimate}, \code{mean}, \code{var}, \code{gate}, \code{n}, \code{K}.
#' @references Tresp, V. (2001). Mixtures of Gaussian processes. NIPS
#'   13, 654-660.
#' @export
#' @examples
#' Gpmoe(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), X_test = c(1, 2, 3, 4, 5, 6, 7, 8), K = 5L)
Gpmoe <- function(X, y, X_test, K, ell = 1, noise = 1e-6) {
  Xm <- as.matrix(X)
  Xt <- as.matrix(X_test)
  yv <- as.numeric(y)
  n <- nrow(Xm)
  m <- nrow(Xt)
  K <- as.integer(K)
  o <- order(Xm[, 1], seq_len(n))
  blocks <- vector("list", K)
  for (pos in seq_len(n)) {
    k <- min(((pos - 1L) * K) %/% n, K - 1L) + 1L
    blocks[[k]] <- c(blocks[[k]], o[pos])
  }
  cent <- matrix(0, K, ncol(Xm))
  mu_k <- matrix(0, K, m)
  var_k <- matrix(0, K, m)
  for (k in seq_len(K)) {
    b <- blocks[[k]]
    if (length(b) == 0L) b <- o[1L]
    cent[k, ] <- colSums(Xm[b, , drop = FALSE]) / length(b)
    Xb <- Xm[b, , drop = FALSE]
    yb <- yv[b]
    g <- .s4_gppost(.s4_rbf(Xb, Xb, ell), .s4_rbf(Xb, Xt, ell), rep(1, m), yb, noise)
    mu_k[k, ] <- g$mean
    var_k[k, ] <- g$var
  }
  gate <- matrix(0, m, K)
  mean_v <- numeric(m)
  var_v <- numeric(m)
  for (j in seq_len(m)) {
    d <- -rowSums((cent - matrix(Xt[j, ], K, ncol(Xm), byrow = TRUE))^2)
    pi_j <- .s4_softmax(d)
    gate[j, ] <- pi_j
    mj <- sum(pi_j * mu_k[, j])
    mean_v[j] <- mj
    var_v[j] <- sum(pi_j * (var_k[, j] + mu_k[, j]^2)) - mj * mj
  }
  .t1_result(estimate = sum(mean_v) / m, mean = mean_v, var = var_v, gate = gate,
             n = n, K = K, method = "Mixture of GP experts (Tresp)")
}
