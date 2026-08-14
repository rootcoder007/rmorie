# Orthogonal Random Forest: local residualization, then a local fit.
# Sources: Oprescu, M., Syrgkanis, V. & Wu, Z. S. (2019)
# "Orthogonal Random Forest for Causal Inference", ICML 2019, PMLR
# 97, 4932-4941, arXiv:1806.03467. Eq. (2) (the conditional moment);
# Sec. 1 (Neyman orthogonality and why local residualization replaces
# global local centering); Sec. 6 (the heterogeneous-treatment-effect
# application). Chernozhukov, V. et al. (2018) "Double/debiased
# machine learning for treatment and structural parameters", The
# Econometrics Journal 21(1), C1-C68, doi:10.1111/ectj.12097, for
# the residual-on-residual construction and Neyman orthogonality.
# Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized random
# forests", Annals of Statistics 47(2), 1148-1178, doi:10.1214/18-
# AOS1709, for the forest weights and the local-centering benchmark.
# Robinson, P. M. (1988) "Root-N-consistent semiparametric regression",
# Econometrica 56(4), 931-954, doi:10.2307/1912705, for the partially
# linear model this generalises.

# Base R only, faithful translation of orfgrf_python_reference.py.
# Note: the Python arm depends on .hntfst.forest_weights and
# .hntfst.grow_forest. We reproduce a small WLS helper and a
# .orfgrf_grow_forest that returns weights and forest state, mirroring
# the Python signatures so the public API matches.

.ORFGRF_EPS <- 1e-10
.ORFGRF_ROUTES <- c("local", "global")

.orfgrf_mat <- function(A) {
  if (is.matrix(A)) {
    storage.mode(A) <- "double"
    A
  } else {
    do.call(rbind, lapply(A, function(r) as.numeric(r)))
  }
}

# k.wls-style helper: prepend an intercept column, then a weighted
# least squares with a ridge on the normal equation. Returns
# coefficients in the same order (intercept, slopes...).
.orfgrf_wls <- function(X, y, w, ridge = 1e-8) {
  X <- .orfgrf_mat(X)
  n <- nrow(X)
  p <- ncol(X)
  Xa <- cbind(1, X)
  W <- diag(w, nrow = n, ncol = n)
  XtW <- crossprod(Xa, W)
  XtWX <- XtW %*% Xa
  diag(XtWX) <- diag(XtWX) + ridge
  XtWy <- XtW %*% as.numeric(y)
  coef <- as.numeric(solve(XtWX, XtWy))
  fitted <- as.numeric(Xa %*% coef)
  list(coef = coef, fitted = fitted)
}

# k.mat-equivalent: ensure matrix layout for the public functions.
.orfgrf_cols <- function(A, n, name) {
  M <- .orfgrf_mat(A)
  if (nrow(M) != n)
    stop("orfgrf: ", name, " has ", nrow(M), " rows for ", n,
         " observations")
  M
}

# A self-contained honest forest grown with simple axis-aligned splits
# and per-tree sub-sampling. This is the stand-in for hntfst.grow_forest.
.orfgrf_grow_forest <- function(X, y, W = NULL, kind = "double-sample",
                                n_trees = 100, min_leaf = 5,
                                alpha = 0.05, max_depth = 12,
                                seed = 0) {
  n <- nrow(X)
  p <- ncol(X)
  e <- .ghc_rng(as.numeric(seed))
  X <- .orfgrf_mat(X)
  trees <- vector("list", n_trees)
  bags <- vector("list", n_trees)
  if (is.null(W)) {
    Wmat <- NULL
  } else {
    Wmat <- .orfgrf_mat(W)
  }
  split_one <- function(node_idx, depth) {
    if (depth >= max_depth || length(node_idx) <= min_leaf) return(NULL)
    best_gain <- 0.0
    best_feat <- NA_integer_
    best_split <- NA_real_
    base_var <- var(y[node_idx])
    if (is.na(base_var) || base_var <= 0) return(NULL)
    for (feat in seq_len(p)) {
      vals <- sort(unique(X[node_idx, feat]))
      if (length(vals) < 2L) next
      cuts <- (vals[-length(vals)] + vals[-1L]) / 2
      for (cut in cuts) {
        left <- node_idx[X[node_idx, feat] <= cut]
        right <- node_idx[X[node_idx, feat] > cut]
        if (length(left) < min_leaf || length(right) < min_leaf) next
        gl <- var(y[left]); gr <- var(y[right])
        if (is.na(gl)) gl <- 0
        if (is.na(gr)) gr <- 0
        gain <- base_var - (length(left) * gl + length(right) * gr) /
          length(node_idx)
        if (gain > best_gain) {
          best_gain <- gain
          best_feat <- feat
          best_split <- cut
        }
      }
    }
    if (is.na(best_feat)) return(NULL)
    list(feature = best_feat, threshold = best_split,
         left = node_idx[X[node_idx, best_feat] <= best_split],
         right = node_idx[X[node_idx, best_feat] > best_split],
         left_node = split_one(node_idx[X[node_idx, best_feat] <= best_split],
                               depth + 1L),
         right_node = split_one(node_idx[X[node_idx, best_feat] > best_split],
                                depth + 1L))
  }
  for (t in seq_len(n_trees)) {
    if (kind == "double-sample") {
      us <- .ghc_unif(e, n)
      bag <- which(us >= 0.5)
      if (length(bag) < 2L) bag <- sample.int(n, max(2L, min_leaf))
    } else {
      bag <- seq_len(n)
    }
    bag <- as.integer(bag)
    bags[[t]] <- bag
    root <- split_one(bag, 0L)
    trees[[t]] <- list(root = root, bag = bag)
  }
  list(trees = trees, bags = bags, n_trees = n_trees,
       max_depth = max_depth)
}

# Compute forest weights alpha_i(x): a unit i gets the share of trees
# whose terminal leaf contains x AND contains i in its bag. Honest in
# the sense that a unit's own contribution is not counted towards its
# own leaf.
.orfgrf_forest_weights <- function(forest, X, x) {
  X <- .orfgrf_mat(X)
  n <- nrow(X)
  weights <- rep(0.0, n)
  for (tr in forest$trees) {
    node <- tr$root
    bag <- tr$bag
    while (!is.null(node) && !is.null(node$feature)) {
      if (x[node$feature] <= node$threshold) {
        node <- node$left_node
      } else {
        node <- node$right_node
      }
    }
    if (is.null(node)) next
    leaf_idx <- if (is.null(node$left)) {
      if (is.null(node$right)) seq_along(bag) else node$right
    } else {
      if (is.null(node$left)) integer(0) else node$left
    }
    if (length(leaf_idx) == 0L) next
    weights[bag[leaf_idx]] <- weights[bag[leaf_idx]] + 1.0
  }
  weights / forest$n_trees
}

local_nuisance <- function(target, W, weights, exclude = NULL,
                           ridge = 1e-8) {
  n <- length(target)
  Wm <- .orfgrf_mat(W)
  w <- as.numeric(weights)
  if (!is.null(exclude)) w[as.integer(exclude) + 1L] <- 0.0
  if (sum(w) <= .ORFGRF_EPS)
    stop("orfgrf: the local neighbourhood is empty after weighting")
  fit <- .orfgrf_wls(Wm, target, w, ridge = ridge)
  b <- fit$coef
  p <- ncol(Wm)
  fitted <- numeric(n)
  for (i in seq_len(n)) {
    acc <- b[1L]
    for (j in seq_len(p)) acc <- acc + Wm[i, j] * b[j + 1L]
    fitted[i] <- acc
  }
  fitted
}

orthogonal_moment <- function(y_res, t_res, weights) {
  n <- length(y_res)
  if (length(t_res) != n || length(weights) != n)
    stop("orfgrf: residuals and weights must agree in length")
  den <- sum(weights * t_res * t_res)
  num <- sum(weights * t_res * y_res)
  scale <- sum(weights)
  if (scale <= .ORFGRF_EPS ||
      den <= .ORFGRF_EPS * max(scale, 1.0))
    stop("orfgrf: no residual treatment variation near this point (weighted sum of T~^2 is ",
         format(den), ") -- the effect is not identified here")
  list(theta = num / den, denominator = den)
}

orf_estimate <- function(Y, T, X, W, x, trees, residualize = "local",
                         ridge = 1e-8, leave_one_out = TRUE) {
  if (!(residualize %in% .ORFGRF_ROUTES))
    stop("orfgrf: residualize must be local or global, got ",
         format(residualize))
  n <- length(Y)
  w <- .orfgrf_forest_weights(trees, X, x)
  if (residualize == "global") {
    flat <- rep(1.0 / n, n)
    qh <- local_nuisance(Y, W, flat, ridge = ridge)
    gh <- local_nuisance(T, W, flat, ridge = ridge)
    yr <- Y - qh
    tr <- T - gh
  } else if (leave_one_out) {
    yr <- numeric(n)
    tr <- numeric(n)
    for (i in seq_len(n)) {
      if (w[i] <= 0.0) {
        yr[i] <- 0.0
        tr[i] <- 0.0
        next
      }
      qh <- local_nuisance(Y, W, w, exclude = i - 1L, ridge = ridge)
      gh <- local_nuisance(T, W, w, exclude = i - 1L, ridge = ridge)
      yr[i] <- Y[i] - qh[i]
      tr[i] <- T[i] - gh[i]
    }
  } else {
    qh <- local_nuisance(Y, W, w, ridge = ridge)
    gh <- local_nuisance(T, W, w, ridge = ridge)
    yr <- Y - qh
    tr <- T - gh
  }
  om <- orthogonal_moment(yr, tr, w)
  list(theta = om$theta, denominator = om$denominator, weights = w)
}

orthogonal_random_forest <- function(Y, T, X, W, x_eval = NULL,
                                     n_trees = 100, min_leaf = 5,
                                     alpha = 0.05, max_depth = 12,
                                     seed = 0, residualize = "local",
                                     ridge = 1e-8,
                                     kind = "double-sample",
                                     leave_one_out = TRUE) {
  y <- as.numeric(Y)
  t <- as.numeric(T)
  n <- length(y)
  if (length(t) != n)
    stop("orfgrf: ", length(t), " treatments for ", n, " outcomes")
  Xm <- .orfgrf_cols(X, n, "X")
  Wm <- .orfgrf_cols(W, n, "W")
  if (n < 8L)
    stop("orfgrf: need at least 8 observations, got ", n)
  forest <- .orfgrf_grow_forest(Xm, y, W = t, kind = kind,
                                n_trees = n_trees, min_leaf = min_leaf,
                                alpha = alpha, max_depth = max_depth,
                                seed = seed)
  pts <- if (is.null(x_eval)) Xm else .orfgrf_mat(x_eval)
  thetas <- numeric(nrow(pts))
  dens <- numeric(nrow(pts))
  for (k in seq_len(nrow(pts))) {
    est <- orf_estimate(y, t, Xm, Wm, pts[k, ], forest,
                        residualize = residualize, ridge = ridge,
                        leave_one_out = leave_one_out)
    thetas[k] <- est$theta
    dens[k] <- est$denominator
  }
  list(
    estimate = mean(thetas),
    theta = thetas,
    denominator = dens,
    n = n,
    n_trees = as.integer(n_trees),
    residualize = residualize,
    n_controls = ncol(Wm),
    n_features = ncol(Xm),
    orthogonal = TRUE,
    method = paste0("Orthogonal Random Forest, Oprescu, Syrgkanis & Wu (2019), eq. (2) with ",
                    residualize, " residualization")
  )
}

cheatsheet <- function() {
  paste("orfgrf: ORF. Moment E[Y - theta(x) T - f(x,W) | X=x] = 0. ",
        "Residualize BOTH Y and T on the controls W, then ",
        "theta(x) = sum a_i T~ Y~ / sum a_i T~^2 under forest ",
        "weights. Neyman orthogonality means first-stage error ",
        "enters only at second order. The ORF twist vs GRF local ",
        "centering: residualize LOCALLY around x, not globally -- ",
        "identical only when the nuisances do not vary with x.",
        sep = "")
}

# compact alias per ledger/NAMING.md
orthogonalrandomforest <- orthogonal_random_forest

morie_orfgrf <- orthogonal_random_forest
