# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of sdcfst -- cross-fitted doubly robust treatment effects with an
# honest regression forest. Mirrors src/morie/fn/sdcfst.py operation for
# operation, on the shared numerics in R/aaa_helpers_w3num.R and the
# generator in R/aaa_helpers_ghc_rng.R.
#
# Two ideas do the work, and they are separable, so both are switchable.
#
# CROSS-FITTING. Estimating a nuisance function and plugging it into a
# score computed on the SAME observations makes the score's error
# correlated with the nuisance error, and the resulting bias does not
# vanish at root-n. Chernozhukov et al.'s answer is to split the sample:
# fit the nuisance on the other folds, evaluate the score on this one,
# average. Nothing about the estimator changes -- only which data saw
# the nuisance fit. The per-fold estimates are reported as well as the
# average, because a fold that disagrees with the rest is information
# about the nuisance model, and averaging it away hides that.
#
# NEYMAN ORTHOGONALITY. The score must be insensitive to small errors in
# the nuisance functions -- its derivative with respect to them zero at
# the truth. That is what makes cross-fitting enough. Four scores, so
# the difference is visible rather than asserted:
#
#   "aipw"             the doubly robust score: outcome regressions plus
#                      an inverse-propensity correction on the
#                      residuals. Orthogonal, and consistent if EITHER
#                      the outcome model or the propensity model is
#                      right.
#   "partialling_out"  Robinson's partially linear score: residualise y
#                      and D on X and regress one on the other. Also
#                      orthogonal, but it estimates a partially linear
#                      coefficient, which equals the ATE only when the
#                      effect is constant.
#   "ipw"              inverse-propensity weighting alone. NOT
#                      orthogonal; the baseline whose sensitivity to the
#                      propensity model the others remove.
#   "plugin"           the outcome regressions alone. Also not
#                      orthogonal; with "ipw" it shows the two ways to
#                      be wrong that "aipw" repairs.
#
# The nuisance learner is separate again:
#
#   "forest"  an HONEST regression forest, which is where the "semi" in
#             the module name comes from. Each tree splits on one
#             subsample and fills its leaves from a DISJOINT one, so a
#             leaf's value is not fitted to the points that chose the
#             split. Without that, a forest's in-leaf averages are
#             biased towards their own training points and the
#             cross-fitted score inherits the bias it was meant to
#             remove.
#   "linear"  least squares for the outcomes and ridge-penalised
#             logistic regression for the propensity. Fast, right when
#             the confounding really is linear, and the arm that makes
#             the forest's contribution measurable.
#
# Propensities are trimmed away from 0 and 1 and the number trimmed is
# REPORTED. A propensity of 0.001 turns one observation into a thousand,
# and an estimator that silently does that is not robust, it is lucky.
#
# References
#   Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
#     C., Newey, W. and Robins, J. (2018) "Double/debiased machine
#     learning for treatment and structural parameters." The
#     Econometrics Journal 21(1), C1-C68.
#   Robinson, P.M. (1988) "Root-N-consistent semiparametric regression."
#     Econometrica 56(4), 931-954.
#   Robins, J.M., Rotnitzky, A. and Zhao, L.P. (1994) JASA 89(427),
#     846-866.
#   Wager, S. and Athey, S. (2018) "Estimation and inference of
#     heterogeneous treatment effects using random forests." JASA
#     113(523), 1228-1242.

.SDCFST_SCORES <- c("aipw", "partialling_out", "ipw", "plugin")
.SDCFST_LEARNERS <- c("forest", "linear")

# Variance-reduction split over the given rows and features. Candidate
# cuts are midpoints between consecutive DISTINCT values -- the only set
# that can produce different partitions, and putting a cut exactly on a
# data point is where the two arms' comparisons could part company.
.sdcfst_best_split <- function(X, y, rows, feats, min_leaf) {
  n <- length(rows)
  if (n < 2L * min_leaf) return(NULL)
  best <- NULL
  for (f in feats) {
    vals <- sort(unique(X[rows, f]))
    if (length(vals) < 2L) next
    ord <- rows[order(X[rows, f], rows)]
    ys <- y[ord]
    tot <- .w3_csum(ys)
    tot2 <- .w3_csum(ys * ys)
    sl <- 0; sl2 <- 0
    for (k in seq_len(n - 1L)) {
      sl <- sl + ys[k]
      sl2 <- sl2 + ys[k] * ys[k]
      nl <- k
      nr <- n - nl
      if (nl < min_leaf || nr < min_leaf) next
      if (X[ord[k], f] == X[ord[k + 1L], f]) next
      sr <- tot - sl
      sr2 <- tot2 - sl2
      sse <- (sl2 - sl * sl / nl) + (sr2 - sr * sr / nr)
      cut <- 0.5 * (X[ord[k], f] + X[ord[k + 1L], f])
      if (is.null(best) || sse < best$sse - 1e-15)
        best <- list(sse = sse, f = f, cut = cut)
    }
  }
  best
}

# One honest tree: split on struct_rows, fill leaves from leaf_rows.
.sdcfst_grow <- function(X, y, struct_rows, leaf_rows, feats_n, min_leaf,
                         max_depth, e, depth = 0L) {
  p <- ncol(X)
  if (depth >= max_depth || length(struct_rows) < 2L * min_leaf ||
      length(leaf_rows) == 0L) {
    val <- if (length(leaf_rows))
      .w3_csum(y[leaf_rows]) / length(leaf_rows)
    else .w3_csum(y[struct_rows]) / length(struct_rows)
    return(list(leaf = TRUE, value = val, n = length(leaf_rows)))
  }
  # mtry features WITHOUT replacement, so a small mtry cannot waste its
  # draws on the same column twice.
  pool <- seq_len(p)
  feats <- integer(0)
  for (i in seq_len(min(feats_n, p))) {
    j <- floor(.ghc_unif(e, 1L) * length(pool))
    if (j >= length(pool)) j <- length(pool) - 1
    feats <- c(feats, pool[j + 1L])
    pool <- pool[-(j + 1L)]
  }
  feats <- sort(feats)
  sp <- .sdcfst_best_split(X, y, struct_rows, feats, min_leaf)
  if (is.null(sp))
    return(list(leaf = TRUE,
                value = .w3_csum(y[leaf_rows]) / length(leaf_rows),
                n = length(leaf_rows)))
  sl <- struct_rows[X[struct_rows, sp$f] <= sp$cut]
  sr <- struct_rows[X[struct_rows, sp$f] > sp$cut]
  ll <- leaf_rows[X[leaf_rows, sp$f] <= sp$cut]
  lr <- leaf_rows[X[leaf_rows, sp$f] > sp$cut]
  if (length(ll) == 0L || length(lr) == 0L)
    return(list(leaf = TRUE,
                value = .w3_csum(y[leaf_rows]) / length(leaf_rows),
                n = length(leaf_rows)))
  list(leaf = FALSE, feature = sp$f, cut = sp$cut,
       left = .sdcfst_grow(X, y, sl, ll, feats_n, min_leaf, max_depth, e,
                           depth + 1L),
       right = .sdcfst_grow(X, y, sr, lr, feats_n, min_leaf, max_depth, e,
                            depth + 1L))
}

.sdcfst_tree_predict <- function(node, x) {
  while (!node$leaf)
    node <- if (x[node$feature] <= node$cut) node$left else node$right
  node$value
}

#' Grow an honest regression forest on the given rows
#'
#' Each tree draws a subsample and splits it in HALF: one half chooses
#' the splits, the other fills the leaves. That is the honesty condition
#' -- a leaf's value never sees the points that put it there.
#'
#' @param X Covariate matrix.
#' @param y Response.
#' @param rows Row indices to grow on.
#' @param n_trees Number of trees.
#' @param mtry Features tried per split; the square root of the column
#'   count by default.
#' @param min_leaf Minimum rows in a leaf.
#' @param max_depth Maximum depth.
#' @param e A generator environment from .ghc_rng.
#' @return A list of trees plus the settings used.
#' @export
morie_sdcfst_forest <- function(X, y, rows, n_trees = 20L, mtry = NULL,
                                min_leaf = 5L, max_depth = 6L, e = NULL) {
  if (is.null(e)) e <- .ghc_rng(1)
  p <- ncol(X)
  if (is.null(mtry)) mtry <- max(1L, as.integer(floor(sqrt(p) + 0.5)))
  trees <- vector("list", as.integer(n_trees))
  m <- length(rows)
  half <- m %/% 2L
  for (t in seq_len(as.integer(n_trees))) {
    # Subsample without replacement by a partial Fisher-Yates over a
    # copy, consuming exactly m - 1 uniforms whatever the data.
    pool <- rows
    if (length(pool) > 1L)
      for (k in seq(length(pool) - 1L, 1L)) {
        j <- floor(.ghc_unif(e, 1L) * (k + 1))
        if (j > k) j <- k
        tmp <- pool[k + 1L]; pool[k + 1L] <- pool[j + 1L]; pool[j + 1L] <- tmp
      }
    struct_rows <- sort(pool[seq_len(half)])
    leaf_rows <- sort(pool[(half + 1L):length(pool)])
    trees[[t]] <- .sdcfst_grow(X, y, struct_rows, leaf_rows, mtry, min_leaf,
                               max_depth, e)
  }
  list(trees = trees, mtry = mtry, min_leaf = min_leaf, max_depth = max_depth)
}

#' Average of the trees' leaf values at x
#'
#' @param forest A forest from morie_sdcfst_forest.
#' @param x A covariate row.
#' @return The prediction.
#' @export
morie_sdcfst_predict <- function(forest, x)
  .w3_csum(vapply(forest$trees, function(t) .sdcfst_tree_predict(t, x),
                  numeric(1))) / length(forest$trees)

#' Ridge-penalised logistic regression by Newton-Raphson
#'
#' The ridge term is not a modelling flourish: with a small fold and a
#' separating covariate the unpenalised likelihood has no maximum, the
#' Hessian goes singular, and the two arms would fail in different
#' places. A fixed tiny ridge makes the problem well posed in both.
#'
#' @param X Covariate matrix.
#' @param z Binary response.
#' @param rows Rows to fit on.
#' @param ridge Ridge penalty.
#' @param iters Maximum Newton steps.
#' @return The coefficient vector, intercept first.
#' @export
morie_sdcfst_logistic <- function(X, z, rows, ridge = 1e-6, iters = 50L) {
  p <- ncol(X) + 1L
  beta <- numeric(p)
  for (it in seq_len(as.integer(iters))) {
    g <- numeric(p)
    h <- matrix(0, p, p)
    for (i in rows) {
      d <- c(1, X[i, ])
      eta <- .w3_dot(d, beta)
      if (eta > 30) eta <- 30 else if (eta < -30) eta <- -30
      mu <- 1 / (1 + exp(-eta))
      wgt <- mu * (1 - mu)
      if (wgt < 1e-10) wgt <- 1e-10
      r <- z[i] - mu
      for (a in seq_len(p)) {
        g[a] <- g[a] + d[a] * r
        for (b in seq_len(p)) h[a, b] <- h[a, b] + wgt * d[a] * d[b]
      }
    }
    for (a in seq_len(p)) {
      g[a] <- g[a] - ridge * beta[a]
      h[a, a] <- h[a, a] + ridge
    }
    step <- .w3_solve_chol(.w3_chol(h), g)
    beta <- beta + step
    if (max(abs(step)) < 1e-12) break
  }
  beta
}

.sdcfst_logit_predict <- function(beta, x) {
  eta <- beta[1] + .w3_dot(beta[-1], as.numeric(x))
  if (eta > 30) eta <- 30 else if (eta < -30) eta <- -30
  1 / (1 + exp(-eta))
}

# Fold labels from a shuffle, so folds are balanced by construction.
.sdcfst_folds <- function(n, k, e) {
  idx <- seq_len(n)
  if (n > 1L)
    for (t in seq(n - 1L, 1L)) {
      j <- floor(.ghc_unif(e, 1L) * (t + 1))
      if (j > t) j <- t
      tmp <- idx[t + 1L]; idx[t + 1L] <- idx[j + 1L]; idx[j + 1L] <- tmp
    }
  lab <- integer(n)
  for (pos in seq_len(n)) lab[idx[pos]] <- (pos - 1L) %% k
  lab
}

#' Cross-fitted treatment effect with honest-forest nuisances
#'
#' @param y Outcome.
#' @param D Binary treatment, 0 or 1.
#' @param X Covariates.
#' @param K_fold Number of cross-fitting folds. K = 1 means no
#'   cross-fitting at all, offered so the bias it removes can be
#'   measured.
#' @param score "aipw", "partialling_out", "ipw" or "plugin".
#' @param learner "forest" or "linear".
#' @param n_trees Trees per forest.
#' @param mtry Features tried per split.
#' @param min_leaf Minimum rows in a leaf.
#' @param max_depth Maximum tree depth.
#' @param trim Propensities are clipped into this range from each end,
#'   and the number clipped is reported.
#' @param seed Seed for the generator shared with the Python arm.
#' @param ridge Ridge penalty for the logistic propensity.
#' @return A list with the estimate, its standard error from the
#'   influence function, the per-fold estimates, the trimming count and
#'   the fitted propensity summary.
#' @export
morie_sdcfst <- function(y, D, X, K_fold = 5L, score = "aipw",
                         learner = "forest", n_trees = 20L, mtry = NULL,
                         min_leaf = 5L, max_depth = 6L, trim = 0.02,
                         seed = 1, ridge = 1e-6) {
  if (!(score %in% .SDCFST_SCORES))
    stop("score must be one of ", paste(.SDCFST_SCORES, collapse = ", "))
  if (!(learner %in% .SDCFST_LEARNERS))
    stop("learner must be one of ", paste(.SDCFST_LEARNERS, collapse = ", "))
  yv <- as.numeric(y)
  dv <- as.numeric(D)
  Xv <- matrix(as.numeric(as.matrix(X)), nrow = length(yv))
  n <- length(yv)
  if (length(dv) != n || nrow(Xv) != n)
    stop("y, D and X must have the same length")
  if (any(dv != 0 & dv != 1)) stop("D must be binary")
  if (n < 8L) stop("need at least eight observations")
  K <- as.integer(K_fold)
  if (K < 1L || K > n) stop("K_fold must lie in 1..n")

  e <- .ghc_rng(seed)
  lab <- if (K == 1L) integer(n) else .sdcfst_folds(n, K, e)

  ps <- numeric(n); m0 <- numeric(n); m1 <- numeric(n); mall <- numeric(n)

  for (k in 0:(K - 1L)) {
    te <- which(lab == k)
    tr <- if (K > 1L) which(lab != k) else te
    tr1 <- tr[dv[tr] == 1]
    tr0 <- tr[dv[tr] == 0]
    if (!length(tr1) || !length(tr0))
      stop("a fold left one treatment arm empty; use fewer folds")
    if (learner == "forest") {
      f1 <- morie_sdcfst_forest(Xv, yv, tr1, n_trees, mtry, min_leaf,
                                max_depth, e)
      f0 <- morie_sdcfst_forest(Xv, yv, tr0, n_trees, mtry, min_leaf,
                                max_depth, e)
      fa <- morie_sdcfst_forest(Xv, yv, tr, n_trees, mtry, min_leaf,
                                max_depth, e)
      fd <- morie_sdcfst_forest(Xv, dv, tr, n_trees, mtry, min_leaf,
                                max_depth, e)
      for (i in te) {
        m1[i] <- morie_sdcfst_predict(f1, Xv[i, ])
        m0[i] <- morie_sdcfst_predict(f0, Xv[i, ])
        mall[i] <- morie_sdcfst_predict(fa, Xv[i, ])
        ps[i] <- morie_sdcfst_predict(fd, Xv[i, ])
      }
    } else {
      des <- cbind(rep(1, n), Xv)
      b1 <- .w3_ols(yv[tr1], des[tr1, , drop = FALSE])
      b0 <- .w3_ols(yv[tr0], des[tr0, , drop = FALSE])
      ba <- .w3_ols(yv[tr], des[tr, , drop = FALSE])
      bp <- morie_sdcfst_logistic(Xv, dv, tr, ridge)
      for (i in te) {
        m1[i] <- .w3_dot(des[i, ], b1$beta)
        m0[i] <- .w3_dot(des[i, ], b0$beta)
        mall[i] <- .w3_dot(des[i, ], ba$beta)
        ps[i] <- .sdcfst_logit_predict(bp, Xv[i, ])
      }
    }
  }

  trimmed <- 0L
  for (i in seq_len(n)) {
    if (ps[i] < trim) { ps[i] <- trim; trimmed <- trimmed + 1L }
    else if (ps[i] > 1 - trim) { ps[i] <- 1 - trim; trimmed <- trimmed + 1L }
  }

  if (score == "partialling_out") {
    # Robinson: residualise both sides on X, then regress. The
    # coefficient is a ratio of averages, and its influence function is
    # the residual score over the mean squared treatment residual.
    vres <- dv - ps
    ures <- yv - mall
    num <- .w3_csum(vres * ures)
    den <- .w3_csum(vres * vres)
    if (den <= 0)
      stop("no variation left in the treatment after residualising")
    est <- num / den
    psi <- vres * (ures - est * vres) / (den / n)
  } else {
    psi <- numeric(n)
    for (i in seq_len(n)) {
      psi[i] <- if (score == "aipw")
        m1[i] - m0[i] + dv[i] * (yv[i] - m1[i]) / ps[i] -
          (1 - dv[i]) * (yv[i] - m0[i]) / (1 - ps[i])
      else if (score == "ipw")
        dv[i] * yv[i] / ps[i] - (1 - dv[i]) * yv[i] / (1 - ps[i])
      else m1[i] - m0[i]
    }
    est <- .w3_csum(psi) / n
  }

  var <- if (n > 1L) .w3_csum((psi - est) * (psi - est)) / (n * (n - 1)) else NaN
  se <- if (!is.nan(var) && var >= 0) sqrt(var) else NaN

  fold_est <- numeric(0)
  for (k in 0:(K - 1L)) {
    te <- which(lab == k)
    if (length(te))
      fold_est <- c(fold_est, .w3_csum(psi[te]) / length(te))
  }

  z <- if (se > 0) est / se else NaN
  list(estimate = est, se = se, z = z,
       p = if (!is.nan(z)) 2 * (1 - .w3_ncdf(abs(z))) else NaN,
       ci_lower = est - 1.959963984540054 * se,
       ci_upper = est + 1.959963984540054 * se,
       fold_estimates = fold_est, influence = psi, propensity = ps,
       m1 = m1, m0 = m0, trimmed = trimmed,
       min_propensity = min(ps), max_propensity = max(ps),
       n = n, n_treated = as.integer(.w3_csum(dv)), K_fold = K,
       score = score, learner = learner, seed = as.integer(seed),
       method = "cross-fitted doubly robust treatment effect")
}

#' One-line summary of the sdcfst module
#'
#' @return A character scalar.
#' @export
morie_sdcfst_cheatsheet <- function()
  paste0("sdcfst: cross-fitted doubly robust treatment effects. scores ",
         paste(.SDCFST_SCORES, collapse = ", "), "; learners ",
         paste(.SDCFST_LEARNERS, collapse = ", "))
