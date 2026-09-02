# Variable importance for a CATE forest.
# Sources: Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized
# Random Forests", The Annals of Statistics 47(2), 1148-1178,
# doi:10.1214/18-AOS1709, arXiv:1610.01271, equation (3) and the
# split-frequency measure of Sec. 6; Wager, S. & Athey, S. (2018)
# "Estimation and Inference of Heterogeneous Treatment Effects using
# Random Forests", Journal of the American Statistical Association
# 113(523), 1228-1242, doi:10.1080/01621459.2017.1319839, for
# Definition 3 and the pi/d floor; Breiman, L. (2001) "Random
# Forests", Machine Learning 45(1), 5-32, for permutation importance;
# Strobl, C., Boulesteix, A.-L., Zeileis, A. & Hothorn, T. (2007)
# "Bias in random forest variable importance measures", BMC
# Bioinformatics 8, 25, for the cardinality bias of raw split counts.
#
# Native implementation mirroring Python morie.fn.crfsel exactly: the
# depth-weighted split frequency, the cross-fitted local centering,
# the treatment-residualised pseudo-outcome, and the permutation
# importance measured on the SAME forest. Both arms draw their
# shuffling permutations from the shared generator so the same seed
# reproduces the same ordering.

# Cross-fitted local centering
#
# Cross-fitted m(X) and e(X), as in the partial-linear forest. The
# Python arm leaves the helper as private; exposing it here so the
# centering step is auditable.
#
# @param y Numeric outcome vector.
# @param W Numeric treatment vector.
# @param X Numeric covariate matrix.
# @param n_folds Integer number of folds.
# @param n_trees Integer number of trees per forest.
# @param min_leaf Integer minimum leaf size.
# @param seed Integer seed for the shared generator.
# @return A list with \code{m_hat} and \code{e_hat}.
# @keywords internal
# @noRd

# Split counts by (depth, feature), depth 1 at the root. Mirrors
# morie.fn.crfsel._depth_counts; element [[depth]] holds the counts at
# that depth, so the caller indexes by depth directly.
#' Split counts by (depth, feature), depth 1 at the root. Mirrors
#'
#' morie.fn.crfsel._depth_counts; element [\[depth\]] holds the counts at
#' that depth, so the caller indexes by depth directly.
#'
#' @param tree Passed to \code{walk}.
#' @param max_depth A count; the body uses it as \code{seq_len(...)}.
#' @param d A count; the body uses it as \code{numeric(...)}.
#' @return The value of \code{counts}, as built in the body.
#' @export
.depth_counts <- function(tree, max_depth, d) {
  counts <- lapply(seq_len(max_depth), function(i) numeric(d))
  walk <- function(nd, depth) {
    if (isTRUE(nd$leaf) || depth > max_depth) return(invisible(NULL))
    counts[[depth]][nd$feature] <<- counts[[depth]][nd$feature] + 1
    walk(nd$left, depth + 1L)
    walk(nd$right, depth + 1L)
  }
  walk(tree, 1L)
  counts
}

#' Cross-fitted local centering
#'
#' Cross-fitted m(X) and e(X), as in the partial-linear forest. The
#' Python arm leaves the helper as private; exposing it here so the
#' centering step is auditable.
#'
#' @param y Numeric outcome vector.
#' @param W Numeric treatment vector.
#' @param X Numeric covariate matrix.
#' @param n_folds Integer number of folds.
#' @param n_trees Integer number of trees per forest.
#' @param min_leaf Integer minimum leaf size.
#' @param seed Integer seed for the shared generator.
#' @return A list with \code{m_hat} and \code{e_hat}.
#' @keywords internal
#' @noRd
.center_cate <- function(y, W, X, n_folds, n_trees, min_leaf, seed) {
  n <- length(y)
  if (n_folds < 2L) n_folds <- 2L
  if (n_folds > n) n_folds <- n
  folds <- vector("list", n_folds)
  for (v in seq_len(n_folds) - 1L) folds[[v + 1L]] <- (seq_len(n) - 1L) %% n_folds == v
  mh <- numeric(n); eh <- numeric(n)
  for (v in seq_along(folds)) {
    val <- which(folds[[v]])
    tr <- setdiff(seq_len(n), val)
    if (length(tr) == 0L) next
    Xt <- X[tr, , drop = FALSE]
    # outcome forest
    trees_y <- grow_forest(Xt, y[tr], n_trees = n_trees,
                           min_leaf = min_leaf, seed = seed + 0L)$trees
    mh[val] <- vapply(val, function(i) {
      w <- forest_weights(trees_y, Xt, X[i, ])
      sum(w * y[tr])
    }, numeric(1))
    # treatment forest
    trees_w <- grow_forest(Xt, W[tr], n_trees = n_trees,
                           min_leaf = min_leaf, seed = seed + 1L)$trees
    eh[val] <- vapply(val, function(i) {
      w <- forest_weights(trees_w, Xt, X[i, ])
      sum(w * W[tr])
    }, numeric(1))
  }
  list(m_hat = mh, e_hat = eh)
}

#' Depth-weighted split frequency
#'
#' Mirrors \code{split_frequency_importance}: counts the splits at each
#' depth WITHIN that depth, then weights by \code{1 / depth^decay}, and
#' normalises the result to sum to one.
#'
#' @param trees A list of tree objects.
#' @param d Integer number of covariates.
#' @param max_depth Integer, depths above this are dropped.
#' @param decay Numeric, depth decay.
#' @return Numeric vector of length d summing to one.
#' @keywords internal
#' @noRd
.split_frequency_importance <- function(trees, d, max_depth, decay) {
  if (max_depth < 1L)
    stop(sprintf("crfsel: max_depth must be at least 1, got %d", max_depth))
  if (decay < 0)
    stop(sprintf("crfsel: decay must be non-negative, got %s",
                 format(decay)))
  total <- numeric(d)
  for (depth in seq_len(max_depth)) {
    at_depth <- numeric(d)
    for (tree in trees) {
      c <- .depth_counts(tree, max_depth, d)
      at_depth <- at_depth + c[[depth]]
    }
    tot <- sum(at_depth)
    if (tot <= 0) next
    w <- 1 / (depth ^ decay)
    total <- total + w * at_depth / tot
  }
  s <- sum(total)
  if (s > 0) total / s else rep(1 / d, d)
}

#' Permutation importance on the SAME forest
#'
#' The rise in the weighted prediction error when a covariate is
#' shuffled. Uses the shared generator so the same seed reproduces the
#' same ordering as the Python arm.
#'
#' @param trees A list of trees.
#' @param X Numeric covariate matrix.
#' @param y Numeric pseudo-outcome.
#' @param features Optional integer vector of feature indices.
#' @param seed Integer seed.
#' @param n_repeats Integer number of shuffles per feature.
#' @return A list with \code{importance} and \code{baseline_error}.
#' @keywords internal
#' @noRd
.permutation_importance <- function(trees, X, y, features, seed,
                                     n_repeats) {
  n <- length(y)
  d <- ncol(X)
  feats <- if (is.null(features)) seq_len(d) else as.integer(features)
  err_fn <- function(Xa) {
    tot <- 0
    for (i in seq_len(n)) {
      w <- forest_weights(trees, X, Xa[i, ])
      pred <- sum(w * y)
      tot <- tot + (y[i] - pred) ^ 2
    }
    tot / n
  }
  base <- err_fn(X)
  out <- numeric(d)
  e <- .ghc_rng(seed)
  for (j in feats) {
    acc <- 0
    for (r in seq_len(as.integer(n_repeats))) {
      u <- .ghc_unif(e, n)
      ord <- order(u)
      Xp <- X
      Xp[, j] <- X[ord, j]
      acc <- acc + err_fn(Xp) - base
    }
    out[j] <- acc / n_repeats
  }
  list(importance = out, baseline_error = base)
}

#' Rank covariates by how the CATE forest uses them
#'
#' Grows a forest on the treatment-residualised pseudo-outcome, scores
#' each covariate by depth-weighted split frequency and (optionally)
#' by permutation error rise, and returns the ranking with the same
#' field names as the Python arm.
#'
#' @param y Numeric outcome vector.
#' @param W Numeric treatment vector.
#' @param X Numeric covariate matrix.
#' @param n_trees Integer number of trees.
#' @param min_leaf Integer minimum leaf size.
#' @param max_depth Integer, depths to consider.
#' @param decay Numeric, depth decay.
#' @param seed Integer seed.
#' @param names Optional character vector of covariate names.
#' @param permutation Logical, also compute permutation importance.
#' @return A list mirroring the Python arm's payload.
#' @references Athey, S., Tibshirani, J. & Wager, S. (2019).
#'   Generalized Random Forests. Annals of Statistics, 47(2),
#'   1148-1178.
#' @export
morie_crfsel <- function(y, W, X, n_trees = 200L, min_leaf = 5L,
                         max_depth = 4L, decay = 2.0, seed = 0L,
                         names = NULL, permutation = FALSE) {
  yv <- as.numeric(y); Wv <- as.numeric(W)
  n <- length(yv)
  if (length(Wv) != n)
    stop(sprintf("crfsel: %d outcomes but %d treatments",
                 n, length(Wv)))
  Xm <- as.matrix(X)
  if (nrow(Xm) != n)
    stop(sprintf("crfsel: %d covariate rows for %d outcomes",
                 nrow(Xm), n))
  d <- ncol(Xm)
  if (d == 0L) stop("crfsel: no covariates")
  nm <- if (is.null(names)) paste0("X", seq_len(d)) else as.character(names)
  if (length(nm) != d)
    stop(sprintf("crfsel: %d names for %d covariates", length(nm), d))
  if (n < 60L)
    stop(sprintf("crfsel: need at least 60 observations, got %d", n))

  ce <- .center_cate(yv, Wv, Xm, n_folds = 5L,
                     n_trees = max(50L, as.integer(n_trees) %/% 2L),
                     min_leaf = min_leaf, seed = as.integer(seed))
  yr <- yv - ce$m_hat
  wr <- Wv - ce$e_hat
  denom <- mean(wr ^ 2)
  if (denom < 1e-12)
    stop("crfsel: the treatment does not vary")
  pseudo <- wr * yr / denom
  trees <- grow_forest(Xm, pseudo, n_trees = n_trees,
                       min_leaf = min_leaf, seed = as.integer(seed))$trees
  freq <- .split_frequency_importance(trees, d, max_depth, decay)
  if (isTRUE(permutation)) {
    perm <- .permutation_importance(trees, Xm, pseudo, NULL,
                                    as.integer(seed), 3L)
    permv <- perm$importance; base <- perm$baseline_error
  } else {
    permv <- NULL; base <- NULL
  }
  ord <- order(-freq)
  ranking <- lapply(seq_along(ord), function(r) {
    j <- ord[r]
    list(variable = nm[j], index = j, importance = freq[j], rank = r,
         permutation = if (is.null(permv)) NULL else permv[j])
  })
  list(estimate = freq, importance = freq,
       importance_by_name = setNames(as.list(freq), nm),
       ranking = ranking, top = nm[ord[1L]], permutation = permv,
       baseline_error = base, n = n, d = d,
       n_trees = as.integer(n_trees), max_depth = as.integer(max_depth),
       decay = as.numeric(decay),
       method = paste("depth-weighted split-frequency variable",
                      "importance for a CATE forest,",
                      "Athey, Tibshirani & Wager (2019)"))
}
