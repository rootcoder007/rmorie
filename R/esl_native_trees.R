# SPDX-License-Identifier: AGPL-3.0-or-later
.esl10_best_split <- function(X, y, min_leaf) {
  n <- nrow(X)
  best <- NULL
  parent <- sum((y - mean(y))^2)
  for (j in seq_len(ncol(X))) {
    o <- order(X[, j], method = "radix")
    xs <- X[o, j]
    ys <- y[o]
    if (n - min_leaf < min_leaf) next
    for (i in min_leaf:(n - min_leaf)) {
      if (i < 1 || i >= n || xs[i] == xs[i + 1]) next
      L <- ys[1:i]
      R <- ys[(i + 1):n]
      sse <- sum((L - mean(L))^2) + sum((R - mean(R))^2)
      if (is.null(best) || sse < best$sse - 1e-15) {
        best <- list(sse = sse, j = j, thr = 0.5 * (xs[i] + xs[i + 1]), gain = parent - sse)
      }
    }
  }
  best
}

#' CART regression tree
#'
#' Greedy binary splits minimising the within-node sum of squares (ESL eqs
#' 9.10-9.13), midpoint thresholds, stopped by depth and minimum leaf size (not
#' pruned); matches rpart's anova splits with cp = 0.
#'
#' @param X Predictors, n by p.
#' @param y Response.
#' @param max_depth,min_leaf,min_impurity_decrease Stopping rules.
#' @return Named list: estimate (training RSS), tree (nested nodes with
#'   1-based feature), n_leaves, depth, fitted, n, p.
#' @references Breiman, L., Friedman, J., Olshen, R. & Stone, C. (1984).
#'   Classification and Regression Trees.
#' @examples
#' i <- 1:60
#' X <- cbind(((7 * i) %% 59) / 59 * 3, ((11 * i) %% 61) / 61 * 2)
#' morie_esl_decision_tree(X, sin(2 * X[, 1]) + (X[, 2] > 1.1), max_depth = 2)$n_leaves
#' @export
morie_esl_decision_tree <- function(X, y, max_depth = 3, min_leaf = 1, min_impurity_decrease = 0) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (length(y) != nrow(X)) stop(sprintf("X has %d rows but y has %d entries.", nrow(X), length(y)), call. = FALSE)
  if (max_depth < 1 || min_leaf < 1) stop("max_depth and min_leaf must be >= 1", call. = FALSE)
  grow <- function(idx, depth) {
    yy <- y[idx]
    if (depth >= max_depth || length(idx) < 2 * min_leaf || diff(range(yy)) == 0) {
      return(list(leaf = TRUE, value = mean(yy), n = length(idx)))
    }
    b <- .esl10_best_split(X[idx, , drop = FALSE], yy, min_leaf)
    if (is.null(b) || b$gain <= min_impurity_decrease) return(list(leaf = TRUE, value = mean(yy), n = length(idx)))
    m <- X[idx, b$j] <= b$thr
    list(leaf = FALSE, feature = b$j, threshold = b$thr, n = length(idx), impurity_decrease = b$gain,
         left = grow(idx[m], depth + 1), right = grow(idx[!m], depth + 1))
  }
  tree <- grow(seq_len(nrow(X)), 0)
  fit <- morie_esl_tree_predict(tree, X)
  count <- function(nd) if (nd$leaf) 1 else count(nd$left) + count(nd$right)
  deep <- function(nd) if (nd$leaf) 0 else 1 + max(deep(nd$left), deep(nd$right))
  list(estimate = sum((y - fit)^2), tree = tree, n_leaves = count(tree), depth = deep(tree), fitted = fit,
       n = nrow(X), p = ncol(X))
}

#' Predict with a regression tree
#'
#' @param tree A tree node list, or the result of morie_esl_decision_tree.
#' @param X Rows to predict.
#' @return Numeric predictions.
#' @examples
#' t <- morie_esl_decision_tree(cbind(c(0, 1, 9)), c(0, 0, 4), max_depth = 1)
#' morie_esl_tree_predict(t, cbind(c(0, 100)))
#' @export
morie_esl_tree_predict <- function(tree, X) {
  if (!is.null(tree$tree) && is.null(tree$leaf)) tree <- tree$tree
  X <- as.matrix(X)
  vapply(seq_len(nrow(X)), function(r) {
    nd <- tree
    while (!nd$leaf) nd <- if (X[r, nd$feature] <= nd$threshold) nd$left else nd$right
    nd$value
  }, numeric(1))
}

#' Gradient boosting with squared-error loss
#'
#' f_m = f_(m-1) + nu T_m where each regression tree T_m is fitted to the
#' residuals, the negative gradient (ESL Alg. 10.3, eqs 10.29-10.41); starts
#' at mean(y). With stumps this equals gbm's gaussian boosting without
#' subsampling.
#'
#' @param X Predictors.
#' @param y Response.
#' @param M Number of trees.
#' @param nu Shrinkage in (0, 1].
#' @param max_depth,min_leaf Tree controls.
#' @return Named list: estimate (training RSS), f0, trees, nu, M,
#'   train_rss_path, fitted, n, p.
#' @references Friedman, J. (2001). Annals of Statistics 29, 1189-1232.
#' @examples
#' i <- 1:60
#' X <- cbind(((7 * i) %% 59) / 59 * 3, ((11 * i) %% 61) / 61 * 2)
#' morie_esl_gbm(X, sin(2 * X[, 1]), M = 10, max_depth = 1)$estimate
#' @export
morie_esl_gbm <- function(X, y, M = 100, nu = 0.1, max_depth = 2, min_leaf = 1) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (!(nu > 0 && nu <= 1)) stop(sprintf("the learning rate must lie in (0, 1]; got %s.", format(nu)), call. = FALSE)
  f0 <- mean(y)
  Fv <- rep(f0, length(y))
  trees <- list()
  path <- numeric(0)
  for (m in seq_len(M)) {
    r <- y - Fv
    if (diff(range(r)) == 0) break
    t <- morie_esl_decision_tree(X, r, max_depth = max_depth, min_leaf = min_leaf)$tree
    trees[[length(trees) + 1]] <- t
    Fv <- Fv + nu * morie_esl_tree_predict(t, X)
    path <- c(path, sum((y - Fv)^2))
  }
  list(estimate = sum((y - Fv)^2), f0 = f0, trees = trees, nu = nu, M = length(trees), train_rss_path = path,
       fitted = Fv, n = nrow(X), p = ncol(X))
}

#' Predict with a gradient-boosting model
#'
#' @param model Result of morie_esl_gbm.
#' @param X Rows to predict.
#' @return Numeric predictions.
#' @examples
#' i <- 1:60
#' X <- cbind(((7 * i) %% 59) / 59 * 3, ((11 * i) %% 61) / 61 * 2)
#' m <- morie_esl_gbm(X, sin(2 * X[, 1]), M = 10, max_depth = 1)
#' morie_esl_gbm_predict(m, rbind(c(0.5, 0.3)))
#' @export
morie_esl_gbm_predict <- function(model, X) {
  X <- as.matrix(X)
  Fv <- rep(model$f0, nrow(X))
  for (t in model$trees) Fv <- Fv + model$nu * morie_esl_tree_predict(t, X)
  Fv
}

.esl10_weighted_stump <- function(X, y, w) {
  best <- list(err = Inf, j = 1, thr = 0, sg = 1)
  for (j in seq_len(ncol(X))) {
    for (thr in sort(unique(X[, j]))) {
      for (sg in c(1, -1)) {
        pred <- ifelse(X[, j] <= thr, sg, -sg)
        err <- sum(w[pred != y])
        if (err < best$err - 1e-15) best <- list(err = err, j = j, thr = thr, sg = sg)
      }
    }
  }
  best
}

#' AdaBoost.M1 with decision stumps
#'
#' Weighted-error stumps G_m with alpha_m = log((1 - err_m) / err_m) and weights
#' multiplied by exp(alpha_m) on the misclassified points (ESL Alg. 10.1,
#' forward stagewise fitting of the exponential loss, eq 10.8).
#'
#' @param X Features.
#' @param y Labels in -1, +1.
#' @param M Maximum rounds.
#' @return Named list: estimate (training error), stumps (1-based feature),
#'   alphas, rounds_used, prediction, margin, n, p.
#' @references Freund, Y. & Schapire, R. (1997). JCSS 55, 119-139.
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(3 * i))
#' morie_esl_adaboost(X, ifelse(X[, 1] + X[, 2] > 0, 1, -1), M = 5)$alphas
#' @export
morie_esl_adaboost <- function(X, y, M = 50) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (length(y) != nrow(X)) stop(sprintf("X has %d rows but y has %d labels.", nrow(X), length(y)), call. = FALSE)
  if (any(!y %in% c(-1, 1))) stop("labels must lie in {-1, +1}.", call. = FALSE)
  n <- nrow(X)
  w <- rep(1 / n, n)
  Fv <- numeric(n)
  stumps <- list()
  alphas <- numeric(0)
  for (m in seq_len(M)) {
    b <- .esl10_weighted_stump(X, y, w)
    if (b$err >= 0.5 - 1e-12) break
    perfect <- b$err <= 1e-15
    a <- if (perfect) 10 else log((1 - b$err) / b$err)
    pred <- ifelse(X[, b$j] <= b$thr, b$sg, -b$sg)
    stumps[[length(stumps) + 1]] <- list(feature = b$j, threshold = b$thr, sign = b$sg)
    alphas <- c(alphas, a)
    Fv <- Fv + a * pred
    if (perfect) break
    w <- w * exp(a * (pred != y))
    w <- w / sum(w)
  }
  cm <- ifelse(Fv >= 0, 1, -1)
  list(estimate = mean(cm != y), stumps = stumps, alphas = alphas, rounds_used = length(stumps), prediction = cm,
       margin = y * Fv, n = n, p = ncol(X))
}

#' Classify with an AdaBoost committee
#'
#' @param model Result of morie_esl_adaboost.
#' @param X Rows to classify.
#' @return Labels in -1, +1 (sign of the weighted vote, ties to +1).
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(3 * i))
#' m <- morie_esl_adaboost(X, ifelse(X[, 1] + X[, 2] > 0, 1, -1), M = 5)
#' morie_esl_adaboost_predict(m, rbind(c(0.5, 0.5)))
#' @export
morie_esl_adaboost_predict <- function(model, X) {
  X <- as.matrix(X)
  Fv <- numeric(nrow(X))
  for (k in seq_along(model$stumps)) {
    s <- model$stumps[[k]]
    Fv <- Fv + model$alphas[k] * ifelse(X[, s$feature] <= s$threshold, s$sign, -s$sign)
  }
  ifelse(Fv >= 0, 1, -1)
}
