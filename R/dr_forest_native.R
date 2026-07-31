# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Doubly-robust overlap-weighted estimation and the loss-balanced causal
# forest. R mirrors of the morie.fn modules drovw, drpdid and egrgrf.
#
# The forest sits on the honest-split core in causal_forest_honest.R,
# which gained the GRF imbalance penalty for this module.

#' Overlap-weighted doubly-robust ATE with cross-fitting
#'
#' The augmented-IPW score, weighted by \eqn{e(1-e)} and fitted with
#' cross-fitting so that no observation contributes to the nuisance
#' models used to score it.
#'
#' "Doubly robust" means consistent if EITHER the propensity model or
#' the outcome model is correct. It does not mean the estimator survives
#' both being wrong, which is how the term is often used in practice.
#'
#' Cross-fitting is not optional here. Without it the nuisance estimates
#' are correlated with the residuals they are supposed to be orthogonal
#' to, and the standard error is understated by an amount that grows
#' with model flexibility.
#'
#' The overlap weights keep the \eqn{1/e} factor from exploding near the
#' boundary, at the cost of estimating the effect for the overlap
#' population rather than the full one -- the returned \code{estimand}
#' says so.
#'
#' @param y outcome.
#' @param D 0/1 treatment.
#' @param X covariates.
#' @param ps optional externally supplied propensity scores.
#' @param n_folds cross-fitting folds, at least 2.
#' @param seed integer seed for the fold assignment.
#' @return list with \code{ate}, \code{se}, \code{ci}, \code{estimand},
#'   \code{influence}, \code{propensity}, \code{mu1}, \code{mu0}.
#' @references Chernozhukov, V. et al. (2018). Double/debiased machine
#'   learning. \emph{The Econometrics Journal}, 21(1), C1-C68. Li, F.
#'   et al. (2018). Balancing covariates via propensity score
#'   weighting. \emph{JASA}, 113(521), 390-400.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(400), ncol = 2)
#' D <- rbinom(200, 1, plogis(X[, 1]))
#' y <- X[, 1] + 2 * D + rnorm(200)
#' round(morie_dr_overlap_weighted(y, D, X)$ate, 2)
#' @export
morie_dr_overlap_weighted <- function(y, D, X, ps = NULL, n_folds = 2,
                                      seed = 0) {
  y <- as.numeric(y)
  D <- as.numeric(D)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- length(y)
  if (!(length(D) == n && nrow(X) == n)) {
    stop("y, D and X must agree on the number of observations",
         call. = FALSE)
  }
  if (!all(D == 0 | D == 1)) stop("D must be 0/1", call. = FALSE)
  n_folds <- as.integer(n_folds)
  if (n_folds < 2L) stop("n_folds must be at least 2", call. = FALSE)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  fold <- integer(n)
  fold[sample.int(n)] <- (seq_len(n) - 1L) %% n_folds
  A <- cbind(1, X)
  e_hat <- numeric(n)
  mu1 <- numeric(n)
  mu0 <- numeric(n)
  for (f in seq_len(n_folds) - 1L) {
    te <- fold == f
    tr <- !te
    if (is.null(ps)) {
      b <- numeric(ncol(A))
      for (it in seq_len(200L)) {
        p <- 1 / (1 + exp(-pmax(pmin(as.vector(A[tr, , drop = FALSE] %*% b),
                                     500), -500)))
        b <- b - 0.5 * (as.vector(crossprod(A[tr, , drop = FALSE], p - D[tr])) /
                          max(sum(tr), 1) + 1e-4 * b)
      }
      e_hat[te] <- 1 / (1 + exp(-pmax(pmin(as.vector(A[te, , drop = FALSE] %*% b),
                                           500), -500)))
    } else {
      e_hat[te] <- as.numeric(ps)[te]
    }
    for (lvl in c(1, 0)) {
      m <- tr & D == lvl
      pred <- if (sum(m) > ncol(A)) {
        as.vector(A[te, , drop = FALSE] %*% qr.solve(A[m, , drop = FALSE], y[m]))
      } else if (any(m)) {
        rep(mean(y[m]), sum(te))
      } else {
        rep(mean(y[tr]), sum(te))
      }
      if (lvl == 1) mu1[te] <- pred else mu0[te] <- pred
    }
  }
  e_hat <- pmin(pmax(e_hat, 1e-4), 1 - 1e-4)
  h <- e_hat * (1 - e_hat)
  psi <- mu1 - mu0 + D * (y - mu1) / e_hat - (1 - D) * (y - mu0) / (1 - e_hat)
  tot <- sum(h)
  ate <- sum(h * psi) / tot
  infl <- h * (psi - ate) / (tot / n)
  se <- stats::sd(infl) / sqrt(n)
  list(ate = ate, se = se, ci = c(ate - 1.96 * se, ate + 1.96 * se),
       estimand = "ATO (overlap-weighted)", influence = infl,
       propensity = e_hat, mu1 = mu1, mu0 = mu0,
       max_weight_share = max(h) / tot, n_folds = n_folds,
       warnings = paste("doubly robust means consistent if EITHER nuisance",
                        "model is right, not that both may be wrong"),
       method = "dr_overlap_weighted")
}


#' Placebo doubly-robust difference-in-differences
#'
#' Runs \code{\link{morie_dr_overlap_weighted}} on the change between
#' two PRE-treatment periods, where the true effect is zero by
#' construction.
#'
#' The value of running the placebo through the SAME machinery as the
#' headline estimate is that any bias in the estimator itself shows up
#' here rather than being attributed to the treatment.
#'
#' Failure is informative; passing is weak. A pre-trend smaller than the
#' effect of interest is not evidence of parallel trends, only evidence
#' that the sample could not detect a larger one --
#' \code{min_detectable} says how large.
#'
#' @param y_pre1,y_pre2 outcomes in two pre-treatment periods.
#' @param D 0/1 treatment (as eventually assigned).
#' @param X covariates.
#' @param ... passed to \code{\link{morie_dr_overlap_weighted}}.
#' @return list with \code{placebo_effect}, \code{se}, \code{z},
#'   \code{p_value}, \code{passed}, \code{min_detectable}.
#' @references Roth, J. (2022). Pretest with caution: event-study
#'   estimates after testing for parallel trends. \emph{AER: Insights},
#'   4(3), 305-322.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(400), ncol = 2)
#' D <- rbinom(200, 1, plogis(X[, 1]))
#' morie_placebo_dr_did(rnorm(200), rnorm(200), D, X)$passed
#' @export
morie_placebo_dr_did <- function(y_pre1, y_pre2, D, X, ...) {
  y1 <- as.numeric(y_pre1)
  y2 <- as.numeric(y_pre2)
  if (length(y1) != length(y2)) {
    stop("the two pre-period outcomes must have the same length",
         call. = FALSE)
  }
  r <- morie_dr_overlap_weighted(y2 - y1, D, X, ...)
  est <- r$ate
  se <- r$se
  z <- if (se > 0) est / se else NA_real_
  p <- if (is.finite(z)) 2 * stats::pnorm(abs(z), lower.tail = FALSE) else NA_real_
  list(placebo_effect = est, se = se, z = z, p_value = p, ci = r$ci,
       passed = isTRUE(p > 0.05), min_detectable = 2.8 * se,
       propensity = r$propensity,
       warnings = paste("failure is informative, passing is weak: a pre-trend",
                        "smaller than the effect of interest is not evidence",
                        "of parallel trends"),
       method = "placebo_dr_did")
}


#' Loss-balanced (imbalance-regularized) causal forest
#'
#' An honest causal forest whose split criterion carries GRF's
#' imbalance penalty:
#' \eqn{\Delta - \gamma(1/n_L + 1/n_R)}.
#'
#' The unregularized heterogeneity criterion is maximised, other things
#' equal, by carving off the smallest admissible leaf -- the extreme tau
#' it reports is mostly estimation noise, and the noise is what earns
#' the split. The penalty is large exactly when one child is tiny, so
#' leaf losses come out balanced rather than concentrated.
#'
#' Gamma carries the units of the criterion itself, \eqn{n(\Delta\tau)^2},
#' so it is NOT scale-free: a value that regularizes one problem is a
#' no-op on another with larger effects or more data. Sweep it and watch
#' the leaf count.
#'
#' @param y outcome.
#' @param D 0/1 treatment.
#' @param X covariates.
#' @param n_trees,min_leaf,max_depth,subsample forest controls.
#' @param imbalance_penalty gamma above.
#' @param seed integer seed.
#' @return list with \code{cate} (out-of-bag), \code{ate}, \code{se},
#'   \code{ci}, \code{leaf_sizes}, \code{n_leaves}.
#' @references Athey, S., Tibshirani, J. and Wager, S. (2019).
#'   Generalized random forests. \emph{Annals of Statistics}, 47(2),
#'   1148-1178.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(1200), ncol = 4)
#' D <- rbinom(300, 1, 0.5)
#' y <- X[, 2] + ifelse(X[, 1] > 0, 2, -2) * D + rnorm(300, 0, 0.5)
#' r <- morie_egregious_loss_forest(y, D, X, n_trees = 40)
#' round(mean(r$cate[X[, 1] > 0], na.rm = TRUE), 2)
#' @export
morie_egregious_loss_forest <- function(y, D, X, n_trees = 200L,
                                        min_leaf = 10L, max_depth = 6L,
                                        imbalance_penalty = 100,
                                        subsample = 0.5, seed = 0L) {
  y <- as.numeric(y)
  D <- as.numeric(D)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) != length(y)) {
    stop(sprintf("X has %d rows but y has %d", nrow(X), length(y)),
         call. = FALSE)
  }
  if (imbalance_penalty < 0) {
    stop(sprintf("imbalance_penalty must be non-negative, got %g",
                 imbalance_penalty), call. = FALSE)
  }
  fit <- morie_causal_forest(y, D, X, n_trees = n_trees, min_leaf = min_leaf,
                             max_depth = max_depth, subsample = subsample,
                             imbalance_penalty = imbalance_penalty,
                             seed = seed)
  cate <- fit$cate_oob
  good <- is.finite(cate)
  k <- sum(good)
  ate <- if (k) mean(cate[good]) else NA_real_
  se <- if (k > 1L) stats::sd(cate[good]) / sqrt(k) else NA_real_
  sizes <- integer(0)
  collect <- function(nd) {
    if (is.na(nd$feature)) {
      sizes <<- c(sizes, as.integer(nd$n))
    } else {
      collect(nd$left)
      collect(nd$right)
    }
  }
  for (tr in fit$forest$trees) collect(tr)
  list(cate = cate, ate = ate, se = se,
       ci = c(ate - 1.96 * se, ate + 1.96 * se), leaf_sizes = sizes,
       n_leaves = length(sizes),
       imbalance_penalty = as.numeric(imbalance_penalty),
       estimate = ate, n = length(y),
       warnings = if (k < length(y)) {
         sprintf("%d rows had no out-of-bag tree; their CATE is NA",
                 length(y) - k)
       } else {
         character(0)
       },
       method = "egregious_loss_forest")
}
