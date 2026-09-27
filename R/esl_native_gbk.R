# SPDX-License-Identifier: AGPL-3.0-or-later
#' K-class gradient tree boosting
#'
#' Each round fits, for every class, a least-squares tree to y_ik - p_k(x_i)
#' and sets each leaf to leaf_scale sum r / sum |r| (1 - |r|) (ESL Alg. 10.4, eq
#' 10.57, leaf_scale = (K - 1) / K; gbm's multinomial uses 1), shrunk by nu;
#' p_k is the softmax of the K functions (eq 10.21).
#'
#' @param X Predictors.
#' @param g Class labels.
#' @param M Rounds.
#' @param nu Shrinkage.
#' @param max_depth,min_leaf Tree controls.
#' @param query Points to classify (default X).
#' @param leaf_scale Newton step factor (default (K - 1) / K).
#' @return Named list: prob, prediction, deviance_path, classes, trees.
#' @references Friedman, J. (2001). Annals of Statistics 29, 1189-1232.
#' @examples
#' i <- 1:60
#' X <- cbind(sin(i) + ((i %% 3) == 0) * 1.5, cos(2 * i))
#' morie_esl_gbm_multiclass(X, i %% 3, M = 5)$deviance_path
#' @export
morie_esl_gbm_multiclass <- function(X, g, M = 100, nu = 0.1, max_depth = 1, min_leaf = 1, query = NULL,
                                     leaf_scale = NULL) {
  X <- as.matrix(X)
  cl <- sort(unique(g))
  N <- nrow(X)
  K <- length(cl)
  if (length(g) != N || K < 2 || !(nu > 0 && nu <= 1)) stop("need matching X and g, at least two classes and 0 < nu <= 1", call. = FALSE)
  sc <- if (is.null(leaf_scale)) (K - 1) / K else leaf_scale
  Y <- outer(g, cl, "==") * 1
  Fm <- matrix(0, N, K)
  soft <- function(F) {
    e <- exp(F - apply(F, 1, max))
    e / rowSums(e)
  }
  grow <- function(idx, r, depth) {
    if (depth >= max_depth || length(idx) < 2 * min_leaf || diff(range(r[idx])) == 0) return(list(leaf = TRUE, idx = idx))
    b <- .esl10_best_split(X[idx, , drop = FALSE], r[idx], min_leaf)
    if (is.null(b) || b$gain <= 0) return(list(leaf = TRUE, idx = idx))
    m <- X[idx, b$j] <= b$thr
    list(leaf = FALSE, feature = b$j, threshold = b$thr, left = grow(idx[m], r, depth + 1), right = grow(idx[!m], r, depth + 1))
  }
  leafify <- function(nd, r) {
    if (nd$leaf) {
      den <- sum(abs(r[nd$idx]) * (1 - abs(r[nd$idx])))
      return(list(leaf = TRUE, value = if (den > 0) sc * sum(r[nd$idx]) / den else 0))
    }
    nd$left <- leafify(nd$left, r)
    nd$right <- leafify(nd$right, r)
    nd
  }
  trees <- list()
  dev <- numeric(0)
  for (m in seq_len(M)) {
    P <- soft(Fm)
    rt <- lapply(seq_len(K), function(k) {
      r <- Y[, k] - P[, k]
      leafify(grow(seq_len(N), r, 0), r)
    })
    for (k in seq_len(K)) Fm[, k] <- Fm[, k] + nu * morie_esl_tree_predict(rt[[k]], X)
    trees[[m]] <- rt
    dev <- c(dev, -2 * sum(log(soft(Fm)[cbind(seq_len(N), match(g, cl))])))
  }
  Q <- if (is.null(query)) X else rbind(query)
  Fq <- matrix(0, nrow(Q), K)
  for (rt in trees) for (k in seq_len(K)) Fq[, k] <- Fq[, k] + nu * morie_esl_tree_predict(rt[[k]], Q)
  P <- soft(Fq)
  list(prob = P, prediction = cl[max.col(P, ties.method = "first")], deviance_path = dev, classes = cl, trees = trees)
}
