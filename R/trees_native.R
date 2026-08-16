# SPDX-License-Identifier: AGPL-3.0-or-later

# ---------------------------------------------------------------------
# Native decision trees, bagging and gradient boosting.
#
# Implements, without any tree-ensemble package:
#
#   * CART-style binary trees, split by variance reduction (regression /
#     second-order boosting) or Gini impurity (classification).
#     Breiman, Friedman, Olshen & Stone (1984), Classification and
#     Regression Trees.
#
#   * Random forests -- Algorithm 15.1 of Hastie, Tibshirani & Friedman
#     (2009), The Elements of Statistical Learning, 2nd edn, p. 588:
#     bootstrap a sample of size N, and at each node select m of the p
#     variables at random before picking the best split among those m.
#     Regression averages the trees; classification takes a majority vote.
#     Breiman (2001), Machine Learning 45(1), 5-32.
#
#   * Gradient tree boosting -- Algorithm 10.3, ESL p. 361, with the
#     shrinkage of eq. (10.41). Gradients for the squared-error and
#     binomial-deviance losses are ESL Table 10.2, p. 360.
#     Friedman (2001), Annals of Statistics 29(5), 1189-1232.
#
#   * The regularised second-order objective used by XGBoost, whose
#     leaf weight and split gain are, per the project's own docs
#     (https://xgboost.readthedocs.io/en/stable/tutorials/model.html):
#         w_j*  = -G_j / (H_j + lambda)
#         Gain  = 0.5 [ G_L^2/(H_L+lambda) + G_R^2/(H_R+lambda)
#                       - (G_L+G_R)^2/(H_L+H_R+lambda) ] - gamma
#     Chen & Guestrin (2016), KDD '16, 785-794.
#
# Splits are found with prefix sums over each feature's sort order, so a
# node costs O(n log n) per candidate feature rather than O(n^2).
# ---------------------------------------------------------------------

# Soft-thresholding operator for the optional L1 term on leaf weights.
#' Soft-thresholding operator for the optional L1 term on leaf weights
#'
#' Part of the trees_native implementation; see the file header for the
#' source it follows.
#'
#' @param g See Usage.
#' @param alpha See Usage.
#' @return A numeric value.
#' @export
.tree_soft_threshold <- function(g, alpha) {
  if (alpha <= 0) return(g)
  if (g > alpha) return(g - alpha)
  if (g < -alpha) return(g + alpha)
  0
}

#' Internal helper: optimal leaf weight under the regularised objective
#' @noRd
.tree_leaf_weight <- function(G, H, lambda, alpha) {
  -.tree_soft_threshold(G, alpha) / (H + lambda)
}

#' Internal helper: best split of one node under the second-order objective
#'
#' Returns NULL when no split improves the objective. `feats` is the set of
#' columns this node is allowed to consider, which is how mtry enters.
#' @noRd
.tree_best_split <- function(X, g, h, idx, feats, min_node, lambda, gamma_pen) {
  G <- sum(g[idx]); H <- sum(h[idx])
  parent <- G * G / (H + lambda)
  best <- list(gain = 0, j = NA_integer_, thr = NA_real_)
  for (j in feats) {
    v <- X[idx, j]
    o <- order(v)
    vs <- v[o]
    gs <- cumsum(g[idx][o])
    hs <- cumsum(h[idx][o])
    n <- length(vs)
    if (n < 2L) next
    # Only positions where the value actually changes are valid splits.
    cut <- which(vs[-n] < vs[-1L])
    cut <- cut[cut >= min_node & (n - cut) >= min_node]
    if (!length(cut)) next
    GL <- gs[cut]; HL <- hs[cut]
    GR <- G - GL;  HR <- H - HL
    gain <- 0.5 * (GL * GL / (HL + lambda) + GR * GR / (HR + lambda) - parent) -
      gamma_pen
    k <- which.max(gain)
    if (gain[k] > best$gain) {
      best <- list(
        gain = gain[k], j = j,
        thr = (vs[cut[k]] + vs[cut[k] + 1L]) / 2
      )
    }
  }
  if (is.na(best$j)) NULL else best
}

#' Internal helper: grow one regression / second-order tree
#'
#' `g` and `h` are the first and second derivatives of the loss at the
#' current predictions. Squared-error loss gives g = f - y and h = 1, in
#' which case variance reduction and the second-order gain coincide.
#' @noRd
.tree_grow <- function(X, g, h, idx, depth, max_depth, min_node, mtry,
                       lambda, alpha, gamma_pen, importance) {
  G <- sum(g[idx]); H <- sum(h[idx])
  leaf <- list(leaf = TRUE, w = .tree_leaf_weight(G, H, lambda, alpha))
  if (depth >= max_depth || length(idx) < 2L * min_node) return(leaf)
  p <- ncol(X)
  feats <- if (mtry >= p) seq_len(p) else sample.int(p, mtry)
  s <- .tree_best_split(X, g, h, idx, feats, min_node, lambda, gamma_pen)
  if (is.null(s)) return(leaf)
  # Accumulate total gain per feature; this is the importance measure.
  importance$val[s$j] <- importance$val[s$j] + s$gain
  go_left <- X[idx, s$j] <= s$thr
  l <- idx[go_left]; r <- idx[!go_left]
  if (!length(l) || !length(r)) return(leaf)
  list(
    leaf = FALSE, j = s$j, thr = s$thr,
    left = .tree_grow(X, g, h, l, depth + 1L, max_depth, min_node, mtry,
                      lambda, alpha, gamma_pen, importance),
    right = .tree_grow(X, g, h, r, depth + 1L, max_depth, min_node, mtry,
                       lambda, alpha, gamma_pen, importance)
  )
}

#' Internal helper: predict from one grown tree
#'
#' Accepts either the flat node table produced by the compiled kernel or the
#' nested list produced by the pure-R reference builder.
#' @noRd
.tree_predict <- function(node, X) {
  X <- as.matrix(X)
  if (!is.null(node$leaf) && length(node$leaf) > 1L) {
    return(morie_tree_predict_cpp(node, X))
  }
  out <- numeric(nrow(X))
  walk <- function(node, rows) {
    if (!length(rows)) return(invisible(NULL))
    if (isTRUE(node$leaf)) {
      out[rows] <<- node$w
      return(invisible(NULL))
    }
    go_left <- X[rows, node$j] <= node$thr
    walk(node$left, rows[go_left])
    walk(node$right, rows[!go_left])
  }
  walk(node, seq_len(nrow(X)))
  out
}

#' Internal helper: fit one tree, returning the tree and its gain vector
#'
#' Delegates to the compiled kernel in src/morie_trees.cpp. The pure-R
#' `.tree_grow` above is the reference implementation and is kept as the
#' fallback for builds without the compiled backend; the two agree exactly,
#' since the kernel applies the same split rule and leaf weight.
#' @noRd
.morie_tree_fit <- function(X, g, h, max_depth = 6L, min_node = 1L,
                            mtry = ncol(X), lambda = 0, alpha = 0,
                            gamma_pen = 0) {
  X <- as.matrix(X)
  if (exists("morie_tree_fit_cpp", mode = "function")) {
    return(morie_tree_fit_cpp(X, as.numeric(g), as.numeric(h),
                              as.integer(max_depth), as.integer(min_node),
                              as.integer(min(mtry, ncol(X))),
                              lambda, alpha, gamma_pen))
  }
  imp <- new.env(parent = emptyenv())
  imp$val <- numeric(ncol(X))
  tree <- .tree_grow(X, g, h, seq_len(nrow(X)), 0L, max_depth, min_node,
                     mtry, lambda, alpha, gamma_pen, imp)
  list(tree = tree, importance = imp$val)
}

# ---------------------------------------------------------------------
# Gini-split trees for multi-class random forests
# ---------------------------------------------------------------------

#' Internal helper: best Gini split of one node
#' @noRd
.tree_best_split_gini <- function(X, Y, idx, feats, min_node) {
  n <- length(idx)
  tot <- colSums(Y[idx, , drop = FALSE])
  gini <- function(cnt, m) if (m <= 0) 0 else 1 - sum((cnt / m)^2)
  parent <- gini(tot, n)
  best <- list(gain = 0, j = NA_integer_, thr = NA_real_)
  for (j in feats) {
    v <- X[idx, j]
    o <- order(v)
    vs <- v[o]
    cs <- apply(Y[idx, , drop = FALSE][o, , drop = FALSE], 2L, cumsum)
    if (is.null(dim(cs))) cs <- matrix(cs, ncol = ncol(Y))
    cut <- which(vs[-n] < vs[-1L])
    cut <- cut[cut >= min_node & (n - cut) >= min_node]
    if (!length(cut)) next
    for (k in cut) {
      cl <- cs[k, ]; cr <- tot - cl
      w <- (k * gini(cl, k) + (n - k) * gini(cr, n - k)) / n
      if (parent - w > best$gain) {
        best <- list(gain = parent - w, j = j,
                     thr = (vs[k] + vs[k + 1L]) / 2)
      }
    }
  }
  if (is.na(best$j)) NULL else best
}

#' Internal helper: grow one Gini classification tree
#' @noRd
.tree_grow_gini <- function(X, Y, idx, depth, max_depth, min_node, mtry,
                            importance) {
  props <- colSums(Y[idx, , drop = FALSE]) / length(idx)
  leaf <- list(leaf = TRUE, p = props)
  if (depth >= max_depth || length(idx) < 2L * min_node) return(leaf)
  if (max(props) == 1) return(leaf)                       # already pure
  p <- ncol(X)
  feats <- if (mtry >= p) seq_len(p) else sample.int(p, mtry)
  s <- .tree_best_split_gini(X, Y, idx, feats, min_node)
  if (is.null(s)) return(leaf)
  importance$val[s$j] <- importance$val[s$j] + s$gain * length(idx)
  go_left <- X[idx, s$j] <= s$thr
  l <- idx[go_left]; r <- idx[!go_left]
  if (!length(l) || !length(r)) return(leaf)
  list(
    leaf = FALSE, j = s$j, thr = s$thr,
    left = .tree_grow_gini(X, Y, l, depth + 1L, max_depth, min_node, mtry,
                           importance),
    right = .tree_grow_gini(X, Y, r, depth + 1L, max_depth, min_node, mtry,
                            importance)
  )
}

#' Internal helper: class-probability prediction from a Gini tree
#' @noRd
.tree_predict_gini <- function(node, X, K) {
  out <- matrix(0, nrow(X), K)
  walk <- function(node, rows) {
    if (!length(rows)) return(invisible(NULL))
    if (isTRUE(node$leaf)) {
      out[rows, ] <<- matrix(node$p, length(rows), K, byrow = TRUE)
      return(invisible(NULL))
    }
    go_left <- X[rows, node$j] <= node$thr
    walk(node$left, rows[go_left])
    walk(node$right, rows[!go_left])
  }
  walk(node, seq_len(nrow(X)))
  out
}

# ---------------------------------------------------------------------
# Random forest -- ESL Algorithm 15.1
# ---------------------------------------------------------------------

#' Internal helper: native random forest
#'
#' Returns fitted values, out-of-bag predictions (ESL p. 592 -- each
#' observation is predicted only by the trees that did not see it), and
#' normalised feature importances.
#' @noRd
.morie_rf_fit <- function(X, y, task = c("regression", "classification"),
                          n_estimators = 100L, mtry = NULL,
                          max_depth = 30L, min_node = NULL) {
  task <- match.arg(task)
  # randomForest's nodesize defaults: 5 for regression, 1 for classification.
  # Growing regression trees to single observations costs several times the
  # runtime for a slightly worse out-of-bag error.
  if (is.null(min_node)) min_node <- if (task == "regression") 5L else 1L
  X <- as.matrix(X)
  n <- nrow(X); p <- ncol(X)
  if (is.null(mtry)) {
    # ESL p. 592: p/3 for regression, sqrt(p) for classification.
    mtry <- if (task == "regression") max(1L, floor(p / 3)) else
      max(1L, floor(sqrt(p)))
  }
  mtry <- min(as.integer(mtry), p)
  imp <- numeric(p)

  forest <- vector("list", n_estimators)

  if (task == "regression") {
    yv <- as.numeric(y)
    fitted <- numeric(n)
    oob_sum <- numeric(n); oob_cnt <- integer(n)
    for (b in seq_len(n_estimators)) {
      boot <- sample.int(n, n, replace = TRUE)
      Xb <- X[boot, , drop = FALSE]; yb <- yv[boot]
      # Squared-error loss: g = -y (so the leaf weight -G/H is the mean),
      # h = 1. With lambda = 0 the second-order gain is exactly the
      # variance reduction CART maximises.
      f <- .morie_tree_fit(Xb, -yb, rep(1, length(yb)),
                           max_depth = max_depth, min_node = min_node,
                           mtry = mtry)
      forest[[b]] <- f$tree
      imp <- imp + f$importance
      fitted <- fitted + .tree_predict(f$tree, X)
      oob <- setdiff(seq_len(n), unique(boot))
      if (length(oob)) {
        oob_sum[oob] <- oob_sum[oob] + .tree_predict(f$tree, X[oob, , drop = FALSE])
        oob_cnt[oob] <- oob_cnt[oob] + 1L
      }
    }
    fitted <- fitted / n_estimators
    oob_pred <- ifelse(oob_cnt > 0, oob_sum / pmax(oob_cnt, 1L), mean(yv))
    return(list(fitted = fitted, oob = oob_pred, importance = .imp_norm(imp),
                forest = forest, mtry = mtry,
                n_estimators = n_estimators, task = task))
  }

  yf <- as.factor(y)
  lev <- levels(yf)
  K <- length(lev)
  Y <- matrix(0, n, K)
  Y[cbind(seq_len(n), as.integer(yf))] <- 1
  votes <- matrix(0, n, K)
  oob_votes <- matrix(0, n, K); oob_cnt <- integer(n)
  for (b in seq_len(n_estimators)) {
    boot <- sample.int(n, n, replace = TRUE)
    e <- new.env(parent = emptyenv()); e$val <- numeric(p)
    tr <- .tree_grow_gini(X[boot, , drop = FALSE], Y[boot, , drop = FALSE],
                          seq_along(boot), 0L, max_depth, min_node, mtry, e)
    forest[[b]] <- tr
    imp <- imp + e$val
    votes <- votes + .tree_predict_gini(tr, X, K)
    oob <- setdiff(seq_len(n), unique(boot))
    if (length(oob)) {
      oob_votes[oob, ] <- oob_votes[oob, ] +
        .tree_predict_gini(tr, X[oob, , drop = FALSE], K)
      oob_cnt[oob] <- oob_cnt[oob] + 1L
    }
  }
  pick <- function(M) factor(lev[max.col(M, ties.method = "first")], levels = lev)
  oob_votes[oob_cnt == 0L, ] <- votes[oob_cnt == 0L, ]
  list(fitted = pick(votes), oob = pick(oob_votes),
       importance = .imp_norm(imp), levels = lev, forest = forest,
       mtry = mtry, n_estimators = n_estimators, task = task)
}

#' Internal helper: predict from a native random forest on new data
#' @noRd
.morie_rf_predict <- function(fit, X) {
  X <- as.matrix(X)
  if (identical(fit$task, "regression")) {
    acc <- numeric(nrow(X))
    for (tr in fit$forest) acc <- acc + .tree_predict(tr, X)
    return(acc / length(fit$forest))
  }
  K <- length(fit$levels)
  votes <- matrix(0, nrow(X), K)
  for (tr in fit$forest) votes <- votes + .tree_predict_gini(tr, X, K)
  factor(fit$levels[max.col(votes, ties.method = "first")],
         levels = fit$levels)
}

#' Internal helper: normalise an importance vector to sum to 1
#' @noRd
.imp_norm <- function(v) {
  s <- sum(v, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) rep(0, length(v)) else v / s
}

# ---------------------------------------------------------------------
# Gradient tree boosting -- ESL Algorithm 10.3 with shrinkage (10.41)
# ---------------------------------------------------------------------

#' Internal helper: native gradient boosting
#'
#' `task = "regression"` uses squared-error loss (ESL Table 10.2: the
#' negative gradient is the ordinary residual). `task = "classification"`
#' uses the binomial deviance on a logit scale, for which the negative
#' gradient is y - p and the second derivative p(1 - p); the Newton step
#' -G/(H + lambda) is then the exact line search of Algorithm 10.3 step
#' 2(c) for that loss.
#' @noRd
.morie_gb_fit <- function(X, y, task = c("regression", "classification"),
                          n_estimators = 100L, learning_rate = 0.1,
                          max_depth = 3L, min_node = 1L,
                          lambda = 0, alpha = 0, gamma_pen = 0,
                          subsample = 1.0) {
  task <- match.arg(task)
  X <- as.matrix(X)
  n <- nrow(X); p <- ncol(X)
  imp <- numeric(p)
  trees <- vector("list", n_estimators)
  if (task == "regression") {
    yv <- as.numeric(y)
    f0 <- mean(yv)                       # argmin_gamma sum (y - gamma)^2
  } else {
    yv <- as.numeric(as.factor(y)) - 1
    pbar <- min(max(mean(yv), 1e-6), 1 - 1e-6)
    f0 <- log(pbar / (1 - pbar))         # argmin over the deviance
  }
  f <- rep(f0, n)
  for (m in seq_len(n_estimators)) {
    if (task == "regression") {
      g <- f - yv                        # dL/df for 0.5 (y - f)^2
      h <- rep(1, n)
    } else {
      pr <- 1 / (1 + exp(-f))
      g <- pr - yv
      h <- pmax(pr * (1 - pr), 1e-6)
    }
    rows <- if (subsample >= 1) seq_len(n) else
      sort(sample.int(n, max(2L, floor(subsample * n))))
    fit <- .morie_tree_fit(X[rows, , drop = FALSE], g[rows], h[rows],
                           max_depth = max_depth, min_node = min_node,
                           mtry = p, lambda = lambda, alpha = alpha,
                           gamma_pen = gamma_pen)
    imp <- imp + fit$importance
    trees[[m]] <- fit$tree
    f <- f + learning_rate * .tree_predict(fit$tree, X)
  }
  list(f0 = f0, trees = trees, learning_rate = learning_rate,
       raw = f, importance = .imp_norm(imp), task = task,
       fitted = if (task == "regression") f else 1 / (1 + exp(-f)))
}

#' Internal helper: predict from a native gradient-boosting fit
#' @noRd
.morie_gb_predict <- function(fit, X) {
  X <- as.matrix(X)
  f <- rep(fit$f0, nrow(X))
  for (tr in fit$trees) f <- f + fit$learning_rate * .tree_predict(tr, X)
  if (identical(fit$task, "regression")) f else 1 / (1 + exp(-f))
}

#' Internal helper: training loss after each boosting round
#'
#' Replays the stored trees so callers can see the loss path without the
#' fit having to carry it. Squared error for regression, mean binomial
#' deviance for classification.
#' @noRd
.morie_gb_loss_path <- function(fit, X, y) {
  X <- as.matrix(X)
  f <- rep(fit$f0, nrow(X))
  out <- numeric(length(fit$trees))
  reg <- identical(fit$task, "regression")
  yv <- if (reg) as.numeric(y) else as.numeric(as.factor(y)) - 1
  for (m in seq_along(fit$trees)) {
    f <- f + fit$learning_rate * .tree_predict(fit$trees[[m]], X)
    out[m] <- if (reg) {
      mean((yv - f)^2)
    } else {
      p <- 1 / (1 + exp(-f))
      -mean(yv * log(pmax(p, 1e-12)) + (1 - yv) * log(pmax(1 - p, 1e-12)))
    }
  }
  out
}
