# Honest random forests, with valid pointwise inference.
#
# Sources:
#   Wager, S. & Athey, S. (2018) "Estimation and Inference of
#   Heterogeneous Treatment Effects using Random Forests", JASA
#   113(523), 1228-1242, arXiv:1510.04342.
#   Athey, S. & Imbens, G. (2016) "Recursive partitioning for
#   heterogeneous causal effects", PNAS 113(27), 7353-7360.
#   Wager, S., Hastie, T. & Efron, B. (2014) "Confidence Intervals for
#   Random Forests: The Jackknife and the Infinitesimal Jackknife",
#   JMLR 15(1), 1625-1651.
#   Efron, B. (2014) "Estimation and Accuracy after Model Selection",
#   JASA 109(507), 991-1007.
#   Breiman, L. (2001) "Random Forests", Machine Learning 45(1), 5-32.

._KINDS <- c("double-sample", "propensity", "adaptive")
._EPS <- 1e-12

._mean <- function(v) if (length(v) == 0L) 0.0 else mean(v)

._best_split <- function(X, y, rows, feats, min_leaf, alpha) {
  n <- length(rows)
  if (n < 2L * min_leaf) return(NULL)
  # rows are 0-based everywhere in this file, as they are in the
  # Python arm; R subscripts are 1-based
  ridx <- rows + 1L
  base <- ._mean(y[ridx])
  tot <- sum((y[ridx] - base)^2)
  best <- NULL
  floor_size <- max(min_leaf, as.integer(ceiling(alpha * n)))
  sum_y <- sum(y[ridx])
  sum_y2 <- sum(y[ridx]^2)
  for (f in feats) {
    order_idx <- order(X[ridx, f])
    o_rows <- ridx[order_idx]
    vals <- X[o_rows, f]
    ys <- y[o_rows]
    csum <- 0.0
    csq <- 0.0
    for (t in 1:(n - 1L)) {
      csum <- csum + ys[t]
      csq <- csq + ys[t] * ys[t]
      left <- t
      right <- n - t
      if (left < floor_size || right < floor_size) next
      if (vals[t] == vals[t + 1L]) next
      rsum <- sum_y - csum
      sse <- csq - csum * csum / left
      sse <- sse + (sum_y2 - csq - rsum * rsum / right)
      gain <- tot - sse
      if (is.null(best) || gain > best[[1L]]) {
        best <- list(gain, f, 0.5 * (vals[t] + vals[t + 1L]))
      }
    }
  }
  best
}

honest_tree <- function(X, y, W = NULL, kind = "double-sample",
                        min_leaf = 5L, alpha = 0.05, pi = 0.5,
                        max_depth = 12L, seed = 0L,
                        subsample = NULL) {
  if (!(kind %in% ._KINDS)) {
    stop(sprintf("hntfst: kind must be one of %s, got %r",
                 paste(._KINDS, collapse = ", "), kind))
  }
  n <- length(y)
  d <- if (n > 0L) ncol(X) else 0L
  if (d == 0L) stop("hntfst: no features")
  if (!(0.0 < alpha && alpha < 0.5)) {
    stop(sprintf("hntfst: alpha must be in (0, 0.5), got %r", alpha))
  }
  if (!(0.0 < pi && pi <= 1.0)) {
    stop(sprintf("hntfst: pi must be in (0, 1], got %r", pi))
  }
  e <- .ghc_rng(seed)
  sub <- if (is.null(subsample)) seq_len(n) - 1L else subsample
  s <- length(sub)
  if (s < 4L * min_leaf) {
    stop(sprintf("hntfst: subsample of %d is too small for a minimum leaf of %d", s, min_leaf))
  }

  if (kind == "double-sample") {
    perm <- sub[order(.ghc_unif(e, s))]
    half <- s %/% 2L
    I_idx <- perm[seq_len(half)]
    J_idx <- perm[(half + 1L):s]
  } else if (kind == "propensity") {
    I_idx <- J_idx <- sub
  } else {
    I_idx <- J_idx <- sub
  }

  if (kind == "propensity") {
    if (is.null(W)) stop("hntfst: a propensity tree needs W")
    split_target <- as.numeric(W)
  } else {
    split_target <- as.numeric(y)
  }

  grow <- function(rows_J, rows_I, depth) {
    node <- list(leaf = TRUE, I = as.integer(rows_I),
                 J = as.integer(rows_J),
                 value = if (length(rows_I) > 0L) ._mean(y[rows_I + 1L])
                 else 0.0, n_I = length(rows_I))
    if (depth >= max_depth || length(rows_I) < 2L * min_leaf) {
      return(node)
    }
    feats <- which(.ghc_unif(e, d) < max(pi, 1.0 / d))
    if (length(feats) == 0L) {
      feats <- as.integer(.ghc_unif(e, 1L) * d) %% d + 1L
    }
    sp <- ._best_split(X, split_target, rows_J, feats, min_leaf, alpha)
    if (is.null(sp)) return(node)
    f <- sp[[2L]]
    thr <- sp[[3L]]
    JL <- rows_J[X[rows_J + 1L, f] <= thr]
    JR <- rows_J[X[rows_J + 1L, f] > thr]
    IL <- rows_I[X[rows_I + 1L, f] <= thr]
    IR <- rows_I[X[rows_I + 1L, f] > thr]
    if (length(IL) < min_leaf || length(IR) < min_leaf) return(node)
    if (length(JL) == 0L || length(JR) == 0L) return(node)
    list(leaf = FALSE, feature = f, threshold = thr,
         left = grow(JL, IL, depth + 1L),
         right = grow(JR, IR, depth + 1L))
  }

  tree <- grow(J_idx, I_idx, 0L)
  list(tree = tree, info = list(I = I_idx, J = J_idx,
                                 kind = kind, subsample = sub))
}

leaf_of <- function(tree, x) {
  node <- tree
  path <- list()
  while (!isTRUE(node$leaf)) {
    path[[length(path) + 1L]] <- list(feature = node$feature,
                                      threshold = node$threshold)
    node <- if (x[node$feature] <= node$threshold) node$left else
      node$right
  }
  list(node = node, path = path)
}

.hntfst_tree_predict <- function(tree, x) {
  leaf_of(tree, x)$node$value
}

infinitesimal_jackknife <- function(preds, in_bag, n, s,
                                    correction = TRUE) {
  B <- length(preds)
  if (B < 2L) {
    stop(sprintf("hntfst: the IJ variance needs at least 2 trees, got %d",
                 B))
  }
  if (n <= s) {
    stop(sprintf("hntfst: need n > s for the IJ correction, got n=%d s=%d", n, s))
  }
  pbar <- ._mean(preds)
  total <- 0.0
  in_bag_mat <- if (is.matrix(in_bag)) in_bag else do.call(rbind, in_bag)
  for (i in 1:n) {
    col <- in_bag_mat[, i]
    nbar <- mean(col)
    cov_b <- (preds - pbar) * (col - nbar)
    cov <- sum(cov_b) / B
    total <- total + cov * cov
  }
  if (correction) {
    total <- total * (n - 1.0) / n * (n / (n - s))^2
  }
  total
}

honest_forest <- function(X, y, W = NULL, kind = "double-sample",
                          n_trees = 200L, subsample_frac = 0.5,
                          min_leaf = 5L, alpha = 0.05, pi = 0.5,
                          max_depth = 12L, seed = 0L, at = NULL,
                          level = 0.95, correction = TRUE) {
  yv <- as.numeric(y)
  n <- length(yv)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != n) {
    stop(sprintf("hntfst: %d feature rows for %d responses",
                 nrow(Xm), n))
  }
  if (n < 16L) {
    stop(sprintf("hntfst: need at least 16 observations, got %d", n))
  }
  if (!(0.0 < subsample_frac && subsample_frac < 1.0)) {
    stop(sprintf("hntfst: subsample_frac must be in (0, 1), got %r",
                 subsample_frac))
  }
  s <- max(4L * min_leaf, as.integer(subsample_frac * n))
  if (s >= n) stop("hntfst: the subsample must be smaller than n")
  Q <- if (is.null(at)) Xm else as.matrix(at)
  B <- as.integer(n_trees)
  if (B < 2L) stop(sprintf("hntfst: need at least 2 trees, got %d", B))

  e <- .ghc_rng(seed)
  preds <- matrix(0.0, B, nrow(Q))
  in_bag <- matrix(FALSE, B, n)
  splits_on <- integer(ncol(Xm))
  depths <- integer(0)

  for (b in 1:B) {
    sub <- order(.ghc_unif(e, n))[1:s] - 1L
    in_bag[b, sub + 1L] <- TRUE
    res <- honest_tree(Xm, yv, W = W, kind = kind, min_leaf = min_leaf,
                       alpha = alpha, pi = pi, max_depth = max_depth,
                       # b is 1-based in R and 0-based in the
                       # Python arm, so b-1 keeps tree t on the
                       # same RNG stream in both
                       seed = as.integer(seed) * 7919L + b - 1L,
                       subsample = sub)
    tree <- res$tree
    for (q in seq_len(nrow(Q))) {
      preds[b, q] <- .hntfst_tree_predict(tree, Q[q, ])
    }
    walk <- function(nd, dep) {
      if (isTRUE(nd$leaf)) {
        depths <<- c(depths, dep)
        return(invisible(NULL))
      }
      splits_on[nd$feature] <<- splits_on[nd$feature] + 1L
      walk(nd$left, dep + 1L)
      walk(nd$right, dep + 1L)
    }
    walk(tree, 0L)
  }

  fitted <- colMeans(preds)
  var <- numeric(nrow(Q))
  for (q in seq_len(nrow(Q))) {
    var[q] <- infinitesimal_jackknife(preds[, q], in_bag, n, s, correction)
  }
  se <- sqrt(pmax(var, 0.0))
  z <- qnorm(0.5 + 0.5 * as.numeric(level))
  tot_splits <- sum(splits_on)
  if (tot_splits == 0L) tot_splits <- 1L
  list(estimate = fitted, fitted = fitted, se = se,
       ci = lapply(seq_len(nrow(Q)), function(q)
         c(fitted[q] - z * se[q], fitted[q] + z * se[q])),
       variance = var, n = n, s = s, n_trees = B,
       split_counts = splits_on,
       split_share = as.numeric(splits_on) / tot_splits,
       mean_depth = ._mean(depths), kind = kind,
       honest = kind != "adaptive", correction = isTRUE(correction),
       level = as.numeric(level),
       method = paste("honest random forest, Wager & Athey (2018) Procedures 1-2, Definitions 1-5, eq. (8)"))
}

grow_forest <- function(X, y, W = NULL, kind = "double-sample",
                        n_trees = 200L, subsample_frac = 0.5,
                        min_leaf = 5L, alpha = 0.05, pi = 0.5,
                        max_depth = 12L, seed = 0L, clusters = NULL) {
  n <- length(y)
  s <- max(4L * min_leaf, as.integer(subsample_frac * n))
  if (s >= n) stop("hntfst: the subsample must be smaller than n")
  e <- .ghc_rng(seed)
  if (!is.null(clusters)) {
    lab <- as.character(clusters)
    if (length(lab) != n) {
      stop(sprintf("hntfst: %d cluster labels for %d rows",
                   length(lab), n))
    }
    groups <- split(seq_len(n) - 1L, lab)
    keys <- sort(names(groups))
    n_keep <- max(2L, as.integer(subsample_frac * length(keys)))
  }
  trees <- list()
  bags <- list()
  for (b in 1:as.integer(n_trees)) {
    if (!is.null(clusters)) {
      pick <- keys[order(.ghc_unif(e, length(keys)))[1:n_keep]]
      sub <- unlist(groups[pick], use.names = FALSE)
    } else {
      sub <- order(.ghc_unif(e, n))[1:s] - 1L
    }
    res <- honest_tree(X, y, W = W, kind = kind, min_leaf = min_leaf,
                       alpha = alpha, pi = pi, max_depth = max_depth,
                       # b is 1-based in R and 0-based in the
                       # Python arm, so b-1 keeps tree t on the
                       # same RNG stream in both
                       seed = as.integer(seed) * 7919L + b - 1L,
                       subsample = sub)
    trees[[b]] <- res$tree
    bag <- logical(n)
    bag[sub + 1L] <- TRUE
    bags[[b]] <- bag
  }
  list(trees = trees, bags = bags, s = s)
}

forest_weights <- function(trees, X, x) {
  n <- nrow(as.matrix(X))
  w <- numeric(n)
  B <- length(trees)
  if (B == 0L) stop("hntfst: no trees")
  used <- 0L
  Xm <- as.matrix(X)
  for (tree in trees) {
    res <- leaf_of(tree, x)
    rows <- res$node$I
    if (length(rows) == 0L) next
    used <- used + 1L
    share <- 1.0 / length(rows)
    for (i in rows) {
      w[i + 1L] <- w[i + 1L] + share
    }
  }
  if (used == 0L) stop("hntfst: every leaf is empty")
  w / used
}

honestforest <- honest_forest
honest_random_forest <- honest_forest

morie_hntfst <- function(X, y, W = NULL, kind = "double-sample",
                         n_trees = 200L, subsample_frac = 0.5,
                         min_leaf = 5L, alpha = 0.05, pi = 0.5,
                         max_depth = 12L, seed = 0L, at = NULL,
                         level = 0.95, correction = TRUE) {
  honest_forest(X, y, W, kind, n_trees, subsample_frac, min_leaf, alpha,
                pi, max_depth, seed, at, level, correction)
}

.hntfst_cheatsheet <- function() {
  paste("hntfst: honest forest. Procedure 1 splits the subsample into I and J, places splits with J's responses and I's features but NEVER I's responses, and estimates leaves from I alone (Def. 2). Procedure 2 splits on W instead of Y. Def. 3: each feature has prob >= pi/d of being split on. Variance is the IJ, eq. (8), with the n(n-1)/(n-s)^2 correction for subsampling without replacement.")
}
