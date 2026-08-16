# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of crsfst -- cross-fitted random survival forest for a treatment
# effect. Mirrors src/morie/fn/crsfst.py operation for operation, on the
# survival forest in R/qsfrgr_native.R and the shared numerics in
# R/aaa_helpers_w3num.R.
#
# The question is what treatment does to survival, and the honest answer
# has to survive two problems at once. Censoring, which the survival
# forest handles. And overfitting, which cross-fitting handles: a forest
# that predicted the survival of a patient it was trained on would
# report its own memory back as a finding, and averaging that over the
# sample produces a treatment effect with no valid standard error at
# all.
#
# Cross-fitting removes it by construction. Split the sample into K
# folds; for each fold, train on the OTHER K-1 and predict only the
# held-out one. Every observation is then predicted by a forest that
# never saw it. The module counts how many observations were predicted
# by a forest that had seen them, and that count is always reported: a
# cross-fitting implementation whose leakage is silent is worse than
# none.
#
# The effect is measured on RESTRICTED MEAN SURVIVAL TIME rather than as
# a hazard ratio. RMST up to a horizon tau is the area under the
# survival curve, which for a step function is an exact finite sum, not
# a quadrature. It is in units of time -- "this treatment buys four
# months over three years" -- so it is interpretable without a
# proportional-hazards assumption that survival data routinely violates.
# The horizon is a parameter and it matters: an RMST difference at one
# year and at five can have opposite signs when the curves cross, and a
# method that hid tau would hide that.
#
# Two forests per fold, one per arm, so the arms are allowed different
# covariate structure.
#
# References
#   Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
#     "Estimating heterogeneous treatment effects with right-censored
#     data via causal survival forests." Journal of the Royal
#     Statistical Society Series B 85(2), 179-211.
#     doi:10.1093/jrsssb/qkac001.
#   Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
#     C., Newey, W. and Robins, J. (2018) "Double/debiased machine
#     learning for treatment and structural parameters." The
#     Econometrics Journal 21(1), C1-C68.
#   Royston, P. and Parmar, M.K.B. (2013) "Restricted mean survival
#     time: an alternative to the hazard ratio." BMC Medical Research
#     Methodology 13, 152.
#   Uno, H. et al. (2014) "Moving beyond the hazard ratio in quantifying
#     the clinical benefit-risk of therapies." Journal of Clinical
#     Oncology 32(22), 2380-2385.

#' Restricted mean survival time: the area under a step curve
#'
#' Exact. The curve is one until its first step and constant between
#' steps, so the integral is a finite sum of rectangles and there is
#' nothing to approximate. Anything past the horizon is cut off, which
#' is what restricted means.
#'
#' @param curve A curve from the Kaplan-Meier function.
#' @param tau The horizon.
#' @return The restricted mean.
#' @export
morie_crsfst_rmst <- function(curve, tau) {
  tau <- as.numeric(tau)
  if (tau <= 0) stop("the horizon must be positive")
  area <- numeric(0)
  prev_t <- 0; prev_s <- 1
  for (k in seq_along(curve$t)) {
    if (curve$t[k] >= tau) break
    area <- c(area, prev_s * (curve$t[k] - prev_t))
    prev_t <- curve$t[k]
    prev_s <- curve$s[k]
  }
  area <- c(area, prev_s * (tau - prev_t))
  .w3_csum(area)
}

#' A deterministic partition of the sample into k folds
#'
#' Shuffled by the shared generator and dealt round-robin, so the folds
#' are balanced to within one and the assignment is reproducible.
#'
#' @param n The sample size.
#' @param k The number of folds.
#' @param seed The random stream.
#' @return A zero-based fold index per observation.
#' @export
morie_crsfst_folds <- function(n, k, seed = 0) {
  k <- as.integer(k)
  if (k < 2L) stop("cross-fitting needs at least two folds")
  if (k > n) stop("more folds than observations")
  e <- .ghc_rng(seed)
  idx <- seq_len(n)
  if (n > 1L) for (i in seq(n, 2L)) {
    j <- floor(.ghc_unif(e, 1L) * i)
    if (j > i - 1L) j <- i - 1L
    tmp <- idx[i]; idx[i] <- idx[j + 1L]; idx[j + 1L] <- tmp
  }
  fold <- integer(n)
  for (pos in seq_len(n)) fold[idx[pos]] <- (pos - 1L) %% k
  fold
}

#' Cross-fitted treatment effect on restricted mean survival time
#'
#' @param time Observed time.
#' @param event Event indicator.
#' @param D Treatment, zero or one.
#' @param X Covariates.
#' @param K Folds.
#' @param tau The horizon; defaults to the largest time observed in BOTH
#'   arms, because an RMST beyond the point where one arm has any data
#'   left is an extrapolation dressed as an estimate.
#' @param n_trees Trees per forest.
#' @param min_leaf Minimum leaf size.
#' @param max_depth Maximum depth.
#' @param honest Whether the trees are honest.
#' @param seed The random stream.
#' @param rule The survival split rule.
#' @return A list with the per-observation conditional effect, the
#'   average effect, the two arms' restricted means, the fold
#'   assignment, and the leakage count -- which must be zero.
#' @export
morie_crsfst <- function(time, event, D, X, K = 3L, tau = NULL,
                         n_trees = 8L, min_leaf = 3L, max_depth = 3L,
                         honest = TRUE, seed = 0, rule = "logrank") {
  t <- as.numeric(time)
  e <- as.integer(ifelse(as.numeric(event) != 0, 1L, 0L))
  d <- as.integer(ifelse(as.numeric(D) != 0, 1L, 0L))
  xs <- as.matrix(X); storage.mode(xs) <- "double"
  n <- length(t)
  if (length(e) != n || length(d) != n || nrow(xs) != n)
    stop("time, event, D and X must agree in length")
  if (n < 8L) stop("need at least eight observations to cross-fit")
  n1 <- sum(d)
  if (n1 == 0L || n1 == n) stop("both arms must be present")
  if (is.null(tau)) {
    # The last time either arm can still speak for. Going past it is
    # extrapolation, and picking the overall maximum would quietly do
    # exactly that whenever one arm ends earlier.
    m1 <- max(t[d == 1L]); m0 <- max(t[d == 0L])
    tau <- if (m1 < m0) m1 else m0
  }
  tau <- as.numeric(tau)

  fold <- morie_crsfst_folds(n, K, seed)
  cate <- rep(NaN, n); r1 <- rep(NaN, n); r0 <- rep(NaN, n)
  leaked <- 0L; used <- 0L
  for (f in 0:(as.integer(K) - 1L)) {
    tr <- which(fold != f); te <- which(fold == f)
    if (!length(te)) next
    a1 <- tr[d[tr] == 1L]; a0 <- tr[d[tr] == 0L]
    if (length(a1) < 4L || length(a0) < 4L) next
    for (arm in c(1L, 0L)) {
      rows <- if (arm == 1L) a1 else a0
      sx <- xs[rows, , drop = FALSE]
      st <- t[rows]; se <- e[rows]
      trees <- morie_qsfrgr_forest(sx, st, se, n_trees, NULL, min_leaf,
                                   max_depth, honest,
                                   seed + 100 * f + arm, rule)
      if (!length(trees)) next
      for (i in te) {
        if (i %in% rows) leaked <- leaked + 1L
        ww <- morie_qsfrgr_weights(trees, xs[i, ], length(rows))
        v <- morie_crsfst_rmst(morie_qsfrgr_km(st, se, ww$w), tau)
        if (arm == 1L) r1[i] <- v else r0[i] <- v
      }
    }
    for (i in te) if (!is.nan(r1[i]) && !is.nan(r0[i])) {
      cate[i] <- r1[i] - r0[i]
      used <- used + 1L
    }
  }

  got <- cate[!is.nan(cate)]
  if (!length(got))
    stop("no fold produced a comparable pair of arms; the sample is ",
         "too small or too unbalanced")
  ate <- .w3_csum(got) / length(got)
  se <- if (length(got) > 1L)
    sqrt((.w3_csum((got - ate) * (got - ate)) / (length(got) - 1)) /
           length(got)) else NaN
  list(cate = cate, rmst_treated = r1, rmst_control = r0,
       estimate = ate, se = se,
       ci_lower = if (is.nan(se)) NaN else ate - 1.959963984540054 * se,
       ci_upper = if (is.nan(se)) NaN else ate + 1.959963984540054 * se,
       fold = fold, tau = tau, n_scored = used, n_leaked = leaked,
       n = n, n_treated = n1, n_control = n - n1, n_events = sum(e),
       K = as.integer(K), honest = isTRUE(honest), rule = rule,
       method = "cross-fitted random survival forest")
}

#' One-line summary of the crsfst module
#'
#' @return A character scalar.
#' @export
morie_crsfst_cheatsheet <- function()
  paste0("crsfst: cross-fitted random survival forest. K-fold ",
         "out-of-fold prediction, effect on restricted mean survival ",
         "time up to tau")
