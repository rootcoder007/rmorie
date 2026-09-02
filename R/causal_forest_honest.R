# SPDX-License-Identifier: AGPL-3.0-or-later
#
# R mirror of the morie.fn causal-forest tier (crfath, crfboot, crfhte,
# csfgrf, survcfg, csurv2, qbcfgr, htgcrf, drlnr, ipsiMed) and its
# shared honest-forest core (_cforest.py).
#
# Distinct from R/causal_forest_native.R, which holds the R-learner
# causal forest with the OpenMP kernel. This file is the honest-split
# (Athey-Imbens) construction the Python tier uses.
#
# Shared helpers live in R/causal_shared_native.R.

#' .morie_cf_tau
#'
#' A step of the causal_forest_honest implementation. Called by \code{.morie_cf_grow}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param y A vector; indexed elementwise.
#' @param d Passed to \code{==}.
#' @return A numeric value.
#' @export
.morie_cf_tau <- function(y, d) {
  tr <- d == 1
  co <- d == 0
  if (!any(tr) || !any(co)) {
    return(NA_real_)
  }
  mean(y[tr]) - mean(y[co])
}

# Grow one honest tree. `split_rows` chooses the splits; `est_rows`
# (never seen by the splitter) fills the leaf values.
#' Grow one honest tree. `split_rows` chooses the splits; `est_rows`
#'
#' (never seen by the splitter) fills the leaf values.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; indexed elementwise.
#' @param d A vector; indexed elementwise.
#' @param split_rows A vector; its length is taken and its elements indexed.
#' @param est_rows A vector; its length is taken and its elements indexed.
#' @param depth Numeric; combined arithmetically in the body.
#' @param max_depth Passed to \code{.morie_cf_grow}.
#' @param min_leaf Numeric; combined arithmetically in the body.
#' @param mtry Numeric; passed to \code{min}.
#' @param imbalance_penalty A flag; the body branches on it. Defaults to \code{0}.
#' @return The value of \code{node}, as built in the body.
#' @export
.morie_cf_grow <- function(X, y, d, split_rows, est_rows, depth, max_depth,
                           min_leaf, mtry, imbalance_penalty = 0) {
  node <- list(
    feature = NA_integer_, threshold = NA_real_,
    left = NULL, right = NULL,
    tau = .morie_cf_tau(y[est_rows], d[est_rows]),
    n = length(est_rows)
  )
  if (is.na(node$tau)) node$tau <- .morie_cf_tau(y[split_rows], d[split_rows])
  if (is.na(node$tau)) node$tau <- 0

  if (depth >= max_depth || length(split_rows) < 4L * min_leaf) {
    return(node)
  }

  p <- ncol(X)
  feats <- sample.int(p, min(mtry, p))
  best_score <- 0
  best_f <- NA_integer_
  best_thr <- NA_real_
  for (f in feats) {
    cuts <- unique(stats::quantile(X[split_rows, f], seq(0.1, 0.9, by = 0.1),
      names = FALSE, type = 7
    ))
    for (thr in cuts) {
      lm_ <- X[split_rows, f] <= thr
      lsp <- split_rows[lm_]
      rsp <- split_rows[!lm_]
      if (length(lsp) < 2L * min_leaf || length(rsp) < 2L * min_leaf) next
      tl <- .morie_cf_tau(y[lsp], d[lsp])
      tr_ <- .morie_cf_tau(y[rsp], d[rsp])
      if (is.na(tl) || is.na(tr_)) next
      # Athey-Imbens criterion: reward heterogeneity between children
      score <- length(lsp) * length(rsp) / length(split_rows) * (tl - tr_)^2
      if (imbalance_penalty) {
        # GRF's regularizer: the raw criterion is happiest carving off a
        # tiny extreme leaf whose tau is mostly estimation noise, and the
        # noise itself is what earns the split. This prices that in.
        score <- score - imbalance_penalty * (1 / length(lsp) + 1 / length(rsp))
      }
      if (score > best_score) {
        best_score <- score
        best_f <- f
        best_thr <- thr
      }
    }
  }
  if (is.na(best_f)) {
    return(node)
  }

  node$feature <- best_f
  node$threshold <- best_thr
  node$left <- .morie_cf_grow(
    X, y, d, split_rows[X[split_rows, best_f] <= best_thr],
    est_rows[X[est_rows, best_f] <= best_thr],
    depth + 1L, max_depth, min_leaf, mtry, imbalance_penalty
  )
  node$right <- .morie_cf_grow(
    X, y, d, split_rows[X[split_rows, best_f] > best_thr],
    est_rows[X[est_rows, best_f] > best_thr],
    depth + 1L, max_depth, min_leaf, mtry, imbalance_penalty
  )
  node
}

#' .morie_cf_walk
#'
#' A step of the causal_forest_honest implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param node A list; the body reads \code{$feature}, \code{$left}, \code{$right}, \code{$tau}, \code{$threshold} from it.
#' @param xrow A vector; indexed elementwise.
#' @return The value of \code{$}.
#' @export
.morie_cf_walk <- function(node, xrow) {
  while (!is.na(node$feature)) {
    node <- if (xrow[node$feature] <= node$threshold) node$left else node$right
  }
  node$tau
}

#' Honest causal forest for heterogeneous treatment effects
#'
#' Each tree splits on one half of a subsample by maximising the
#' squared difference in child treatment effects, then re-estimates its
#' leaf values on the untouched half. That honesty split is what makes
#' the leaf effects approximately unbiased. Out-of-bag predictions
#' average only the trees that never saw the row and are the ones to
#' feed any downstream heterogeneity test.
#'
#' Mirrors `morie.fn.crfath.causal_forest_wager_athey` and the shared
#' `morie.fn._cforest.CausalForest` core.
#'
#' @param y Outcome.
#' @param d Binary 0/1 treatment.
#' @param x Covariate matrix (or vector).
#' @param n_trees Number of trees.
#' @param min_leaf Minimum units of each arm per leaf.
#' @param max_depth Maximum tree depth.
#' @param mtry Features tried per split; default `ceiling(sqrt(p))`.
#' @param subsample Fraction of rows drawn per tree.
#' @param imbalance_penalty GRF's split-imbalance regularizer, subtracted
#'   as `penalty * (1/n_L + 1/n_R)`. Zero recovers the plain
#'   Athey-Imbens criterion. It carries the units of the criterion
#'   itself, so it is not scale-free.
#' @param seed RNG seed.
#' @return List with `cate`, `cate_oob`, `ate`, `cate_sd`, `n_trees`,
#'   `n`, and `forest` (usable with [morie_causal_forest_predict()]).
#' @references Wager S, Athey S (2018). Estimation and inference of
#'   heterogeneous treatment effects using random forests. \emph{JASA}
#'   113(523), 1228-1242.
#'
#'   Athey S, Imbens G (2016). Recursive partitioning for heterogeneous
#'   causal effects. \emph{PNAS} 113(27), 7353-7360.
#' @export
#' @examples
#' set.seed(1)
#' n <- 120
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + d * (0.5 + X[, 2]) + rnorm(n, 0, 0.3)
#' fit <- morie_causal_forest(y, d, X, n_trees = 30L, min_leaf = 10L)
#' mean(fit$cate)
morie_causal_forest <- function(y, d, x, n_trees = 200L, min_leaf = 10L,
                                max_depth = 6L, mtry = NULL,
                                subsample = 0.5, imbalance_penalty = 0,
                                seed = 0L) {
  y <- as.numeric(y)
  d <- as.numeric(d)
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  n <- length(y)
  if (nrow(X) != n || length(d) != n) {
    stop("y, d and x must share their first dimension.", call. = FALSE)
  }
  if (!all(d %in% c(0, 1))) stop("d must be binary 0/1.", call. = FALSE)
  if (n < 8L * min_leaf) {
    stop(sprintf("need at least %d observations, got %d.", 8L * min_leaf, n),
      call. = FALSE
    )
  }
  if (subsample <= 0 || subsample > 1) {
    stop("subsample must lie in (0, 1].", call. = FALSE)
  }
  if (is.null(mtry)) mtry <- max(1L, ceiling(sqrt(ncol(X))))

  set.seed(seed)
  m <- max(4L * min_leaf, as.integer(subsample * n))
  trees <- vector("list", n_trees)
  in_bag <- matrix(FALSE, nrow = n_trees, ncol = n)
  for (b in seq_len(n_trees)) {
    idx <- sample.int(n, min(m, n))
    half <- length(idx) %/% 2L
    trees[[b]] <- .morie_cf_grow(
      X, y, d, idx[seq_len(half)],
      idx[(half + 1L):length(idx)],
      0L, max_depth, min_leaf, mtry,
      imbalance_penalty
    )
    in_bag[b, idx] <- TRUE
  }

  cate <- vapply(seq_len(n), function(i) {
    mean(vapply(trees, .morie_cf_walk, numeric(1), xrow = X[i, ]))
  }, numeric(1))
  oob <- vapply(seq_len(n), function(i) {
    keep <- !in_bag[, i]
    if (!any(keep)) {
      return(NA_real_)
    }
    mean(vapply(trees[keep], .morie_cf_walk, numeric(1), xrow = X[i, ]))
  }, numeric(1))

  list(
    cate = cate, cate_oob = oob, ate = mean(cate, na.rm = TRUE),
    cate_sd = stats::sd(cate, na.rm = TRUE), n_trees = n_trees, n = n,
    forest = list(trees = trees, X = X)
  )
}

#' Predict CATEs at new covariate values from a fitted causal forest
#'
#' @param forest The `forest` element of a [morie_causal_forest()] fit.
#' @param newx Covariate matrix with the same columns as the training X.
#' @return Numeric vector of predicted conditional treatment effects.
#' @references Wager S, Athey S (2018). Estimation and inference of
#'   heterogeneous treatment effects using random forests.
#'   \emph{JASA} 113(523), 1228-1242.
#' @export
#' @examples
#' set.seed(1)
#' n <- 120
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + d * (0.5 + X[, 2]) + rnorm(n, 0, 0.3)
#' fit <- morie_causal_forest(y, d, X, n_trees = 30L, min_leaf = 10L)
#' morie_causal_forest_predict(fit$forest, X[1:5, , drop = FALSE])
morie_causal_forest_predict <- function(forest, newx) {
  Xq <- as.matrix(newx)
  storage.mode(Xq) <- "double"
  if (ncol(Xq) != ncol(forest$X)) {
    stop("newx must have the same number of columns as the training X.",
      call. = FALSE
    )
  }
  vapply(seq_len(nrow(Xq)), function(i) {
    mean(vapply(forest$trees, .morie_cf_walk, numeric(1), xrow = Xq[i, ]))
  }, numeric(1))
}

#' Bootstrap confidence intervals for the causal-forest CATE
#'
#' Refits an honest forest on each of `B` bootstrap resamples and takes
#' percentile intervals across replicates. Intervals from a single
#' forest's tree spread understate the uncertainty because the trees
#' share the training data. Mirrors `morie.fn.crfboot`.
#'
#' @param y,d,x As in [morie_causal_forest()].
#' @param B Bootstrap replicates.
#' @param n_trees,min_leaf,seed Per-forest hyperparameters.
#' @param alpha Two-sided interval level.
#' @return List with `cate`, `ci_low`, `ci_high`, `se`, `ate`,
#'   `ate_ci`, `B`, `n`.
#' @references Wager S, Athey S (2018). Estimation and inference of
#'   heterogeneous treatment effects using random forests.
#'   \emph{JASA} 113(523), 1228-1242.
#' @export
#' @examples
#' set.seed(2)
#' n <- 100
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + d * 0.8 + rnorm(n, 0, 0.3)
#' r <- morie_causal_forest_bootstrap(y, d, X, B = 8L, n_trees = 15L)
#' str(r, max.level = 1)
morie_causal_forest_bootstrap <- function(y, d, x, B = 40L, n_trees = 60L,
                                          min_leaf = 10L, seed = 0L,
                                          alpha = 0.05) {
  y <- as.numeric(y)
  d <- as.numeric(d)
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  n <- length(y)
  if (B < 2L) stop("B must be at least 2.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must lie in (0, 1).", call. = FALSE)
  }
  draws <- matrix(NA_real_, nrow = B, ncol = n)
  for (b in seq_len(B)) {
    set.seed(seed + b)
    idx <- sample.int(n, n, replace = TRUE)
    fit <- morie_causal_forest(y[idx], d[idx], X[idx, , drop = FALSE],
      n_trees = n_trees, min_leaf = min_leaf,
      seed = seed + b
    )
    draws[b, ] <- morie_causal_forest_predict(fit$forest, X)
  }
  probs <- c(alpha / 2, 1 - alpha / 2)
  ci <- apply(draws, 2, stats::quantile, probs = probs, names = FALSE)
  ate_draws <- rowMeans(draws)
  list(
    cate = colMeans(draws), ci_low = ci[1, ], ci_high = ci[2, ],
    se = apply(draws, 2, stats::sd), ate = mean(ate_draws),
    ate_ci = stats::quantile(ate_draws, probs, names = FALSE),
    B = B, n = n
  )
}

#' Best-linear-predictor test for treatment-effect heterogeneity
#'
#' Regress the demeaned outcome on the residualised treatment and its
#' interaction with the centred CATE predictions. `alpha` near 1 says
#' the forest's average effect is calibrated; `beta` is the
#' heterogeneity coefficient, and `beta = 0` means the predicted
#' variation carries no signal. Pass out-of-bag predictions -- in-bag
#' ones inflate `beta` mechanically. Mirrors `morie.fn.crfhte`.
#'
#' @param y Outcome.
#' @param d Binary 0/1 treatment.
#' @param cate_predictions Out-of-bag CATE estimates.
#' @param propensity Treatment probabilities; default `mean(d)`.
#' @return List with `alpha`, `beta`, `se_beta`, `p_value`,
#'   `heterogeneous`, `n`.
#' @references Chernozhukov V, Demirer M, Duflo E, Fernandez-Val I
#'   (2018). Generic machine learning inference on heterogenous
#'   treatment effects in randomized experiments. arXiv:1712.04802.
#' @export
#' @examples
#' set.seed(2)
#' n <- 100
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + d * (0.5 + X[, 2]) + rnorm(n, 0, 0.3)
#' tau <- 0.5 + X[, 2] + rnorm(n, 0, 0.2)
#' morie_hte_blp_test(y, d, tau)
morie_hte_blp_test <- function(y, d, cate_predictions, propensity = NULL) {
  y <- as.numeric(y)
  d <- as.numeric(d)
  tau <- as.numeric(cate_predictions)
  n <- length(y)
  if (length(d) != n || length(tau) != n) {
    stop("y, d and cate_predictions must have equal length.", call. = FALSE)
  }
  if (!all(d %in% c(0, 1))) stop("d must be binary 0/1.", call. = FALSE)
  ok <- is.finite(tau)
  if (sum(ok) < 10L) {
    stop("need at least 10 finite CATE predictions.", call. = FALSE)
  }
  y <- y[ok]
  d <- d[ok]
  tau <- tau[ok]
  m <- length(y)
  e <- if (is.null(propensity)) rep(mean(d), m) else as.numeric(propensity)[ok]
  rd <- d - e
  tc <- tau - mean(tau)
  fit <- stats::lm(I(y - mean(y)) ~ rd + I(rd * tc))
  co <- stats::coef(summary(fit))
  beta <- unname(co[3, 1])
  se <- unname(co[3, 2])
  p <- stats::pt(beta / se, df = m - 3L, lower.tail = FALSE)
  list(
    alpha = unname(co[2, 1]), beta = beta, se_beta = se,
    p_value = p, heterogeneous = p < 0.05, n = m
  )
}

#' Causal survival forest on IPCW restricted-mean-survival pseudo outcomes
#'
#' Censoring makes the raw event time unusable as a forest outcome, so
#' the honest forest runs on an inverse-probability-of-censoring-weighted
#' pseudo outcome whose expectation is the RMST at `horizon`. The
#' resulting CATE is a difference in restricted mean survival time, not
#' a hazard ratio. Mirrors `morie.fn.csfgrf` (and `morie.fn.survcfg`).
#'
#' @param time Follow-up times (positive).
#' @param event 1 = event observed, 0 = right-censored.
#' @param d Binary 0/1 treatment.
#' @param x Covariates.
#' @param horizon Restriction time; default the 90th percentile of `time`.
#' @param n_trees,min_leaf,seed Forest hyperparameters.
#' @return List with `cate`, `cate_oob`, `ate`, `horizon`,
#'   `pseudo_outcome`, `n`, `forest`.
#' @references Cui Y, Kosorok MR, Sverdrup E, Wager S, Zhu R (2023).
#'   Estimating heterogeneous treatment effects with right-censored data
#'   via causal survival forests. \emph{JRSS-B} 85(2), 179-211.
#' @export
#' @examples
#' set.seed(3)
#' n <- 150
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' tt <- rexp(n, rate = exp(-0.5 * d - 0.3 * X[, 1]))
#' ev <- as.numeric(tt < quantile(tt, 0.8))
#' tt <- pmin(tt, quantile(tt, 0.8))
#' r <- morie_causal_survival_forest(tt, ev, d, X, n_trees = 20L)
#' str(r, max.level = 1)
morie_causal_survival_forest <- function(time, event, d, x, horizon = NULL,
                                         n_trees = 200L, min_leaf = 15L,
                                         seed = 0L) {
  time <- as.numeric(time)
  event <- as.numeric(event)
  d <- as.numeric(d)
  n <- length(time)
  if (length(event) != n || length(d) != n) {
    stop("time, event and d must have equal length.", call. = FALSE)
  }
  if (any(time <= 0)) stop("time must be positive.", call. = FALSE)
  if (!all(event %in% c(0, 1))) stop("event must be binary 0/1.", call. = FALSE)
  if (!all(d %in% c(0, 1))) stop("d must be binary 0/1.", call. = FALSE)
  tau <- if (is.null(horizon)) {
    stats::quantile(time, 0.9, names = FALSE)
  } else {
    as.numeric(horizon)
  }
  if (tau <= 0) stop("horizon must be positive.", call. = FALSE)

  pseudo <- .morie_cf_rmst_pseudo(time, event, tau)
  fit <- morie_causal_forest(pseudo, d, x,
    n_trees = n_trees,
    min_leaf = min_leaf, seed = seed
  )
  c(
    fit[c("cate", "cate_oob", "ate", "n", "forest")],
    list(horizon = tau, pseudo_outcome = pseudo)
  )
}

#' Best linear predictor for the causal survival forest CATE
#'
#' Chains [morie_causal_survival_forest()] into [morie_hte_blp_test()],
#' passing the out-of-bag CATEs and the IPCW-RMST pseudo outcome.
#' Mirrors `morie.fn.csurv2`.
#'
#' @inheritParams morie_causal_survival_forest
#' @return List with `alpha`, `beta`, `se_beta`, `p_value`,
#'   `heterogeneous`, `ate`, `horizon`, `n`.
#' @references Cui Y et al. (2023). Estimating heterogeneous treatment
#'   effects with right-censored data via causal survival forests.
#'   \emph{JRSS-B} 85(2), 179-211; Chernozhukov V et al. (2018).
#'   Fisher-Schultz Lecture: generic machine learning inference on
#'   heterogeneous treatment effects in randomized experiments, with an
#'   application to immunization in India. arXiv:1712.04802.
#' @export
#' @examples
#' set.seed(3)
#' n <- 150
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' tt <- rexp(n, rate = exp(-0.5 * d - 0.3 * X[, 1]))
#' ev <- as.numeric(tt < quantile(tt, 0.8))
#' tt <- pmin(tt, quantile(tt, 0.8))
#' r <- morie_causal_survival_blp(tt, ev, d, X, n_trees = 20L)
#' str(r, max.level = 1)
morie_causal_survival_blp <- function(time, event, d, x, horizon = NULL,
                                      n_trees = 200L, min_leaf = 15L,
                                      seed = 0L) {
  f <- morie_causal_survival_forest(time, event, d, x,
    horizon = horizon,
    n_trees = n_trees, min_leaf = min_leaf,
    seed = seed
  )
  blp <- morie_hte_blp_test(f$pseudo_outcome, d, f$cate_oob)
  c(
    blp[c("alpha", "beta", "se_beta", "p_value", "heterogeneous", "n")],
    list(ate = f$ate, horizon = f$horizon)
  )
}

#' Quantile-balanced causal forest for distributional treatment effects
#'
#' A quantile contrast is a mean contrast of an indicator, so the honest
#' forest runs on `1{Y <= q_tau}`. A negative CDF contrast means
#' treatment pushes mass above the threshold, so `shift_effect` is its
#' negation. Mirrors `morie.fn.qbcfgr`.
#'
#' @param y Outcome.
#' @param d Binary 0/1 treatment.
#' @param x Covariates.
#' @param quantile Quantile level in (0, 1).
#' @param n_trees,min_leaf,seed Forest hyperparameters.
#' @return List with `cdf_effect`, `shift_effect`, `threshold`,
#'   `quantile`, `ate_cdf`, `n`, `forest`.
#' @references Athey S, Tibshirani J, Wager S (2019). Generalized random
#'   forests. \emph{The Annals of Statistics} 47(2), 1148-1178.
#' @export
#' @examples
#' set.seed(4)
#' n <- 120
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + d * 0.8 + rnorm(n, 0, 0.3)
#' r <- morie_quantile_causal_forest(y, d, X, quantile = 0.5, n_trees = 20L)
#' str(r, max.level = 1)
morie_quantile_causal_forest <- function(y, d, x, quantile = 0.5,
                                         n_trees = 200L, min_leaf = 15L,
                                         seed = 0L) {
  y <- as.numeric(y)
  q <- as.numeric(quantile)
  if (q <= 0 || q >= 1) {
    stop("quantile must lie strictly in (0, 1).", call. = FALSE)
  }
  thr <- stats::quantile(y, q, names = FALSE)
  ind <- as.numeric(y <= thr)
  fit <- morie_causal_forest(ind, d, x,
    n_trees = n_trees,
    min_leaf = min_leaf, seed = seed
  )
  list(
    cdf_effect = fit$cate, shift_effect = -fit$cate, threshold = thr,
    quantile = q, ate_cdf = fit$ate, n = fit$n, forest = fit$forest
  )
}

#' Causal forest with an isotonic monotonicity constraint on the CATE
#'
#' When theory says the effect can only move one way in a covariate, the
#' unconstrained forest's wiggles are noise. The fitted CATE is projected
#' onto the monotone functions of that covariate by pool-adjacent-
#' violators, the exact L2 isotonic projection. Mirrors `morie.fn.htgcrf`.
#'
#' @param y,d,x As in [morie_causal_forest()].
#' @param monotone_feature Column index the effect must be monotone in;
#'   `NULL` returns the unconstrained forest.
#' @param direction 1 for nondecreasing, -1 for nonincreasing.
#' @param n_trees,min_leaf,seed Forest hyperparameters.
#' @return List with `cate`, `cate_raw`, `monotone_feature`, `direction`,
#'   `violations_before`, `violations_after`, `n`.
#' @references Robertson T, Wright FT, Dykstra RL (1988). \emph{Order
#'   Restricted Statistical Inference}. Wiley.
#' @export
#' @examples
#' set.seed(4)
#' n <- 120
#' X <- matrix(rnorm(n * 2), n, 2)
#' d <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + d * (0.5 + 0.5 * X[, 1]) + rnorm(n, 0, 0.3)
#' r <- morie_monotone_causal_forest(y, d, X, monotone_feature = 1L,
#'                                   n_trees = 20L)
#' str(r, max.level = 1)
morie_monotone_causal_forest <- function(y, d, x, monotone_feature = NULL,
                                         direction = 1L, n_trees = 200L,
                                         min_leaf = 10L, seed = 0L) {
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  fit <- morie_causal_forest(y, d, X,
    n_trees = n_trees,
    min_leaf = min_leaf, seed = seed
  )
  raw <- fit$cate
  if (is.null(monotone_feature)) {
    return(list(
      cate = raw, cate_raw = raw, monotone_feature = NULL,
      direction = direction, violations_before = 0L,
      violations_after = 0L, n = fit$n
    ))
  }
  j <- as.integer(monotone_feature)
  if (j < 1L || j > ncol(X)) {
    stop(sprintf(
      "monotone_feature must index a column of x (1..%d).",
      ncol(X)
    ), call. = FALSE)
  }
  if (!direction %in% c(1L, -1L, 1, -1)) {
    stop("direction must be 1 or -1.", call. = FALSE)
  }
  o <- order(X[, j])
  v <- raw[o] * direction
  before <- sum(diff(v) < -1e-12)
  fitted <- stats::isoreg(v)$yf # PAVA
  after <- sum(diff(fitted) < -1e-9)
  cate <- numeric(length(raw))
  cate[o] <- fitted * direction
  list(
    cate = cate, cate_raw = raw, monotone_feature = j,
    direction = direction, violations_before = before,
    violations_after = after, n = fit$n
  )
}

#' DR-learner: doubly robust meta-learner for the CATE
#'
#' Stage 1 builds the cross-fitted AIPW pseudo outcome, whose conditional
#' mean is exactly the CATE; stage 2 regresses it on the covariates.
#' Because the pseudo outcome is the AIPW score, the second stage
#' inherits double robustness and Neyman orthogonality. Mirrors
#' `morie.fn.drlnr`.
#'
#' @param y Outcome.
#' @param t Binary 0/1 treatment.
#' @param x Covariates.
#' @param n_folds Cross-fitting folds.
#' @param seed RNG seed.
#' @param trunc Propensity truncation.
#' @return List with `cate`, `ate`, `se_ate`, `pseudo_outcome`,
#'   `coefficients`, `n_folds`, `n`.
#' @references Kennedy EH (2023). Towards optimal doubly robust
#'   estimation of heterogeneous causal effects. \emph{Electronic Journal
#'   of Statistics} 17(2), 3008-3049.
#' @export
#' @examples
#' set.seed(5)
#' n <- 120
#' X <- matrix(rnorm(n * 2), n, 2)
#' t <- rbinom(n, 1, 0.5)
#' y <- 1 + X[, 1] + t * (0.5 + X[, 2]) + rnorm(n, 0, 0.3)
#' r <- morie_dr_learner(y, t, X, n_folds = 4L)
#' str(r, max.level = 1)
morie_dr_learner <- function(y, t, x, n_folds = 5L, seed = 0L, trunc = 0.01) {
  y <- as.numeric(y)
  t <- as.numeric(t)
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  n <- length(y)
  if (length(t) != n || nrow(X) != n) {
    stop("y, t and x must share their first dimension.", call. = FALSE)
  }
  if (!all(t %in% c(0, 1))) stop("t must be binary 0/1.", call. = FALSE)
  k <- as.integer(n_folds)
  if (k < 2L || k > n %/% 4L) {
    stop(sprintf("n_folds must lie in [2, %d].", n %/% 4L), call. = FALSE)
  }
  if (trunc <= 0 || trunc >= 0.5) {
    stop("trunc must lie in (0, 0.5).", call. = FALSE)
  }

  set.seed(seed)
  folds <- sample(rep_len(seq_len(k), n))
  e_all <- pmin(pmax(.morie_logit_fit(X, t), trunc), 1 - trunc)
  psi <- numeric(n)
  for (f in seq_len(k)) {
    te <- folds == f
    tr1 <- !te & t == 1
    tr0 <- !te & t == 0
    if (sum(tr1) < 2L || sum(tr0) < 2L) {
      stop("a fold lacks one treatment arm; reduce n_folds.", call. = FALSE)
    }
    m1 <- as.vector(cbind(1, X[te, , drop = FALSE]) %*%
      .morie_ridge_fit(X[tr1, , drop = FALSE], y[tr1]))
    m0 <- as.vector(cbind(1, X[te, , drop = FALSE]) %*%
      .morie_ridge_fit(X[tr0, , drop = FALSE], y[tr0]))
    e <- e_all[te]
    psi[te] <- m1 - m0 + t[te] * (y[te] - m1) / e -
      (1 - t[te]) * (y[te] - m0) / (1 - e)
  }
  D <- cbind(1, X)
  b <- qr.coef(qr(D), psi)
  b[is.na(b)] <- 0
  list(
    cate = as.vector(D %*% b), ate = mean(psi),
    se_ate = stats::sd(psi) / sqrt(n), pseudo_outcome = psi,
    coefficients = b, n_folds = k, n = n
  )
}

#' Interventional (randomised-mediator) direct and indirect effects
#'
#' When a mediator-outcome confounder is itself affected by the exposure,
#' the natural effects are not identified. The interventional analogues
#' replace the unit's own counterfactual mediator with a draw from its
#' population distribution given the exposure level, which is identified
#' under weaker conditions but answers a population question rather than
#' an individual one. Mirrors `morie.fn.ipsiMed`.
#'
#' @param y Outcome.
#' @param x Binary 0/1 exposure.
#' @param m Mediator.
#' @param c Optional baseline covariates.
#' @param n_draws Monte Carlo draws of the randomised mediator.
#' @param seed RNG seed.
#' @return List with `ide`, `iie`, `overall`, `n_draws`, `n`.
#' @references VanderWeele TJ, Vansteelandt S, Robins JM (2014). Effect
#'   decomposition in the presence of an exposure-induced
#'   mediator-outcome confounder. \emph{Epidemiology} 25(2), 300-306.
#' @export
#' @examples
#' set.seed(5)
#' n <- 150
#' x <- rbinom(n, 1, 0.5)
#' m <- 0.5 * x + rnorm(n, 0, 0.5)
#' y <- 1 + 0.4 * x + 0.6 * m + rnorm(n, 0, 0.4)
#' r <- morie_interventional_effects(y, x, m, n_draws = 400L)
#' str(r, max.level = 1)
morie_interventional_effects <- function(y, x, m, c = NULL, n_draws = 2000L,
                                         seed = 0L) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  m <- as.numeric(m)
  n <- length(y)
  if (length(x) != n || length(m) != n) {
    stop("y, x and m must have equal length.", call. = FALSE)
  }
  if (!all(x %in% c(0, 1))) stop("x must be binary 0/1.", call. = FALSE)
  if (sum(x) == 0 || sum(x) == n) {
    stop("need both exposure arms.", call. = FALSE)
  }
  B <- as.integer(n_draws)
  if (B < 100L) stop("n_draws must be at least 100.", call. = FALSE)
  C <- if (is.null(c)) matrix(numeric(0), nrow = n, ncol = 0) else as.matrix(c)
  if (nrow(C) != n) {
    stop("c must have one row per observation.", call. = FALSE)
  }

  Dy <- cbind(1, x, m, x * m, C)
  by <- qr.coef(qr(Dy), y)
  by[is.na(by)] <- 0
  Dm <- cbind(1, x, C)
  bm <- qr.coef(qr(Dm), m)
  bm[is.na(bm)] <- 0
  resid <- m - as.vector(Dm %*% bm)
  cbar <- if (ncol(C)) colMeans(C) else numeric(0)

  set.seed(seed)
  draw <- function(xv) {
    mu <- bm[1] + bm[2] * xv + if (ncol(C)) sum(bm[-(1:2)] * cbar) else 0
    mu + sample(resid, B, replace = TRUE)
  }
  predict_y <- function(xv, mv) {
    Cmat <- if (ncol(C)) {
      matrix(cbar, nrow = length(mv), ncol = ncol(C), byrow = TRUE)
    } else {
      matrix(numeric(0), nrow = length(mv), ncol = 0)
    }
    as.vector(cbind(1, xv, mv, xv * mv, Cmat) %*% by)
  }
  g0 <- draw(0)
  g1 <- draw(1)
  ide <- mean(predict_y(1, g0)) - mean(predict_y(0, g0))
  iie <- mean(predict_y(1, g1)) - mean(predict_y(1, g0))
  list(ide = ide, iie = iie, overall = ide + iie, n_draws = B, n = n)
}
