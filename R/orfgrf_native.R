# morie.fn -- function file (rootcoder007/morie)
# Orthogonal Random Forest: local residualization, then a local fit.
#
# Sources: Oprescu, M., Syrgkanis, V. & Wu, Z. S. (2019)
# "Orthogonal Random Forest for Causal Inference", ICML 2019, PMLR
# 97, 4932-4941, arXiv:1806.03467. Eq. (2) (the conditional moment),
# Sec. 1 (Neyman orthogonality and why local residualization
# replaces global local centering), Sec. 6 (the
# heterogeneous-treatment-effect application implemented here).
#
# Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
# C., Newey, W. & Robins, J. (2018) "Double/debiased machine
# learning for treatment and structural parameters", The
# Econometrics Journal 21(1), C1-C68, doi:10.1111/ectj.12097. The
# residual-on-residual construction and Neyman orthogonality that
# ORF localises.
#
# Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized random
# forests", The Annals of Statistics 47(2), 1148-1178,
# doi:10.1214/18-AOS1709. The forest weights alpha_i(x) and the
# global "local centering" benchmark ORF is compared against.
#
# Robinson, P. M. (1988) "Root-N-consistent semiparametric
# regression", Econometrica 56(4), 931-954, doi:10.2307/1912705.
# The partially linear model whose residual-on-residual estimator
# this generalises.

# forest_weights and grow_forest are normally supplied by hntfst.
# They are inlined here so the file is self-contained, with the same
# signatures the Python arm imports.

.orfgrf_EPS <- 1e-10
.orfgrf_ROUTES <- c("local", "global")

# Minimal honest CART builder. Each tree gets half the observations
# chosen uniformly without replacement; honesty is enforced by
# splitting each tree's bag into a structure half and a leaf half.
.grow_one_tree <- function(X, y, indices, min_leaf, alpha,
                           max_depth, e) {
  n <- length(indices)
  # shuffle indices in place using the shared generator
  perm <- indices[order(.ghc_unif(e, n))]
  half <- floor(n / 2)
  if (half < 2L) half <- 2L
  struct <- perm[seq_len(half)]
  leaf <- perm[(half + 1L):n]
  build <- function(idxs, depth) {
    if (length(idxs) <= min_leaf || depth >= max_depth) {
      return(list(leaf = TRUE, value = mean(y[leaf[leaf %in% idxs]])))
    }
    p <- ncol(X)
    best <- list(gain = -Inf)
    for (j in seq_len(p)) {
      vals <- sort(unique(X[idxs, j]))
      if (length(vals) < 2L) next
      cuts <- (vals[-length(vals)] + vals[-1L]) / 2
      for (cut in cuts) {
        left <- idxs[X[idxs, j] <= cut]
        right <- idxs[X[idxs, j] > cut]
        if (length(left) < min_leaf || length(right) < min_leaf) next
        # honesty: mean over leaf-sample residuals
        ml <- mean(y[leaf[leaf %in% left]])
        mr <- mean(y[leaf[leaf %in% right]])
        parent <- mean(y[leaf[leaf %in% idxs]])
        gain <- parent - 0.5 * (ml + mr)
        if (gain > best$gain) {
          best <- list(gain = gain, j = j, cut = cut)
        }
      }
    }
    if (is.null(best$j))
      return(list(leaf = TRUE, value = mean(y[leaf[leaf %in% idxs]])))
    left <- idxs[X[idxs, best$j] <= best$cut]
    right <- idxs[X[idxs, best$j] > best$cut]
    list(leaf = FALSE, j = best$j, cut = best$cut,
         left = build(left, depth + 1L),
         right = build(right, depth + 1L))
  }
  build(struct, 0L)
}

.orfgrf_grow_forest <- function(X, y, W = NULL, kind = "double-sample",
                        n_trees = 100, min_leaf = 5, alpha = 0.05,
                        max_depth = 12, seed = 0) {
  n <- length(y)
  e <- .ghc_rng(as.numeric(seed))
  trees <- list()
  bags <- list()
  half <- floor(n / 2)
  for (k in seq_len(as.integer(n_trees))) {
    if (identical(kind, "double-sample")) {
      u <- .ghc_unif(e, n)
      sel <- order(u)[seq_len(max(half, 1L))]
    } else {
      sel <- seq_len(n)
    }
    trees[[length(trees) + 1L]] <- .grow_one_tree(
      X, y, sel, as.integer(min_leaf), as.numeric(alpha),
      as.integer(max_depth), e)
    bags[[length(bags) + 1L]] <- sel
  }
  list(trees = trees, bags = bags, s = NULL)
}

# Recursive descent through one tree to the leaf containing x.
.predict_tree <- function(tree, x) {
  if (isTRUE(tree$leaf)) return(tree$value)
  if (x[tree$j] <= tree$cut) return(.predict_tree(tree$left, x))
  .predict_tree(tree$right, x)
}

# Forest kernel weights alpha_i(x) for a single evaluation point.
# Each unit that lands in the same leaf as x in tree t contributes
# 1 / (#units from tree t's leaf-sample in that leaf).
.orfgrf_forest_weights <- function(trees, X, x) {
  n <- nrow(X)
  w <- rep(0, n)
  for (t in trees$trees) {
    v <- .predict_tree(t, x)
    # unit that ends up at the predicted value contributes 1 to its
    # own weight. The honest construction uses the leaf-sample
    # count; here we approximate with the tree's full training set
    # since the honest split is internal to the tree.
    # Identify the leaf's training indices by descending.
    idx <- seq_len(n)
    node <- t
    while (!isTRUE(node$leaf)) {
      if (x[node$j] <= node$cut) {
        idx <- idx[X[idx, node$j] <= node$cut]
        node <- node$left
      } else {
        idx <- idx[X[idx, node$j] > node$cut]
        node <- node$right
      }
    }
    sz <- max(length(idx), 1L)
    for (i in idx) w[i] <- w[i] + 1 / sz
  }
  w / length(trees$trees)
}

# Weighted linear fit of `target` on W near a point. weights are the
# forest kernel weights alpha_i(x), so the fit is local by
# construction. exclude drops one index for the leave-one-out
# residual.
local_nuisance <- function(target, W, weights, exclude = NULL,
                          ridge = 1e-8) {
  Wm <- as.matrix(W)
  w <- as.numeric(weights)
  n <- length(target)
  if (!is.null(exclude))
    w[as.integer(exclude) + 1L] <- 0
  sw <- sum(w)
  if (sw <= .orfgrf_EPS)
    stop("orfgrf: the local neighbourhood is empty after weighting")
  # Weighted normal equations: (X' W X + ridge I) beta = X' W y
  # with X = [1 W] (intercept included by hand).
  Xaug <- cbind(1, Wm)
  WX <- sweep(Xaug, 1, w, "*")
  A <- crossprod(WX, Xaug)
  d <- ncol(A)
  A[seq_len(d) + (seq_len(d) - 1L) * d] <- A[seq_len(d) +
    (seq_len(d) - 1L) * d] + ridge
  b <- crossprod(WX, target)
  coef <- solve(A, b)
  fit <- as.numeric(Xaug %*% coef)
  list(fit = fit, coef = as.numeric(coef))
}

# Solve sum_i alpha_i T~(Y~ - theta T~) = 0. Refuses when the
# residualized treatment has no weighted variation left.
orthogonal_moment <- function(y_res, t_res, weights) {
  n <- length(y_res)
  if (length(t_res) != n || length(weights) != n)
    stop("orfgrf: residuals and weights must agree in length")
  den <- sum(weights * t_res * t_res)
  num <- sum(weights * t_res * y_res)
  scale <- sum(weights)
  if (scale <= .orfgrf_EPS || den <= .orfgrf_EPS * max(scale, 1.0))
    stop("orfgrf: no residual treatment variation near this point ",
         "(weighted sum of T~^2 is ", den, ") -- the effect is not ",
         "identified here")
  list(theta = num / den, den = den)
}

# One point estimate theta(x). residualize="local" fits the
# nuisances under the same kernel weights used for the second
# stage -- the ORF proposal. "global" fits them once on the whole
# sample, the "local centering" benchmark.
orf_estimate <- function(Y, T, X, W, x, trees,
                         residualize = "local", ridge = 1e-8,
                         leave_one_out = TRUE) {
  if (!(residualize %in% .orfgrf_ROUTES))
    stop("orfgrf: residualize must be local or global, got ",
         residualize)
  n <- length(Y)
  w <- .orfgrf_forest_weights(trees, X, x)
  if (identical(residualize, "global")) {
    flat <- rep(1 / n, n)
    qh <- local_nuisance(Y, W, flat, ridge = ridge)$fit
    gh <- local_nuisance(T, W, flat, ridge = ridge)$fit
    yr <- Y - qh
    tr <- T - gh
  } else if (isTRUE(leave_one_out)) {
    yr <- numeric(n); tr <- numeric(n)
    for (i in seq_len(n)) {
      if (w[i] <= 0) { yr[i] <- 0; tr[i] <- 0; next }
      qh <- local_nuisance(Y, W, w, exclude = i - 1L, ridge = ridge)$fit
      gh <- local_nuisance(T, W, w, exclude = i - 1L, ridge = ridge)$fit
      yr[i] <- Y[i] - qh[i]
      tr[i] <- T[i] - gh[i]
    }
  } else {
    qh <- local_nuisance(Y, W, w, ridge = ridge)$fit
    gh <- local_nuisance(T, W, w, ridge = ridge)$fit
    yr <- Y - qh
    tr <- T - gh
  }
  om <- orthogonal_moment(yr, tr, w)
  list(theta = om$theta, den = om$den, w = w)
}

# ORF for the heterogeneous treatment effect theta_0(x).
orthogonal_random_forest <- function(Y, T, X, W, x_eval = NULL,
                                     n_trees = 100, min_leaf = 5,
                                     alpha = 0.05, max_depth = 12,
                                     seed = 0, residualize = "local",
                                     ridge = 1e-8,
                                     kind = "double-sample",
                                     leave_one_out = TRUE) {
  y <- as.numeric(Y); t <- as.numeric(T)
  n <- length(y)
  if (length(t) != n)
    stop("orfgrf: ", length(t), " treatments for ", n, " outcomes")
  Xm <- as.matrix(X); Wm <- as.matrix(W)
  if (nrow(Xm) != n || nrow(Wm) != n)
    stop("orfgrf: feature/control rows must equal n")
  if (n < 8L) stop("orfgrf: need at least 8 observations, got ", n)
  trees <- .orfgrf_grow_forest(Xm, y, W = t, kind = kind,
                       n_trees = as.integer(n_trees),
                       min_leaf = as.integer(min_leaf),
                       alpha = as.numeric(alpha),
                       max_depth = as.integer(max_depth),
                       seed = as.numeric(seed))
  pts <- if (is.null(x_eval)) Xm else as.matrix(x_eval)
  thetas <- numeric(nrow(pts)); dens <- numeric(nrow(pts))
  for (k in seq_len(nrow(pts))) {
    out <- orf_estimate(y, t, Xm, Wm, pts[k, ], trees,
                        residualize = residualize, ridge = ridge,
                        leave_one_out = leave_one_out)
    thetas[k] <- out$theta; dens[k] <- out$den
  }
  list(estimate = sum(thetas) / length(thetas),
       theta = thetas, denominator = dens,
       n = n, n_trees = as.integer(n_trees),
       residualize = residualize,
       n_controls = ncol(Wm), n_features = ncol(Xm),
       orthogonal = TRUE,
       method = paste("Orthogonal Random Forest, Oprescu, Syrgkanis ",
                      "& Wu (2019), eq. (2) with ", residualize,
                      " residualization", sep = ""))
}

.orfgrf_cheatsheet <- function() {
  paste("orfgrf: ORF. Moment E[Y - theta(x) T - f(x,W) | X=x] = 0. ",
        "Residualize BOTH Y and T on the controls W, then ",
        "theta(x) = sum a_i T~ Y~ / sum a_i T~^2 under forest ",
        "weights. Neyman orthogonality means first-stage error ",
        "enters only at second order. The ORF twist vs GRF local ",
        "centering: residualize LOCALLY around x, not globally -- ",
        "identical only when the nuisances do not vary with x.",
        sep = "")
}

morie_orfgrf <- orthogonal_random_forest
