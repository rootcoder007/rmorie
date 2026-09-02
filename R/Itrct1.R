# SPDX-License-Identifier: AGPL-3.0-or-later
#' Doubly robust DiD with a treatment by covariate interaction
#'
#' The ATT is estimated separately at each level of the effect modifier
#' V with the doubly robust panel estimator of Sant'Anna and Zhao; the
#' contrast between the extreme levels is the interaction, the DiD
#' analogue of effect modification.  The level-specific influence
#' functions are computed on disjoint subsamples, so the variance of the
#' contrast is the sum of the level variances.
#'
#' Formula: tau = E\[(w1(D) - w0(D, X; pi)) (dY - mu_0(X))\] within levels.
#'
#' @param y Outcome change dY per unit.
#' @param D Treatment indicator, 0 or 1.
#' @param V Effect modifier; its distinct values define the strata.
#' @param X Covariates for the propensity and outcome models.
#' @return List with \code{estimate}, \code{se}, \code{att},
#'   \code{att_se}, \code{levels}, \code{level_n}, \code{att_overall},
#'   \code{se_overall}, \code{n_levels}, \code{n}, \code{method}.
#' @references Sant'Anna and Zhao (2020), Doubly robust
#'   difference-in-differences estimators, Journal of Econometrics
#'   219(1):101-122, \doi{10.1016/j.jeconom.2020.06.003}; Hernan and
#'   Robins (2020), Causal Inference: What If, chapter 13.
#' @export
#' @examples
#' set.seed(1)
#' Itrct1(y = rnorm(40), D = rbinom(40, 1, 0.5), V = rbinom(40, 1, 0.5),
#'        X = matrix(rnorm(80), 40, 2))
Itrct1 <- function(y, D, V, X) {
  dy <- .s03vec(y); n <- length(dy)
  if (n == 0L) stop("interaction_did: y is empty")
  d <- .s03vec(D); v <- .s03vec(V)
  if (length(d) != n || length(v) != n) stop("interaction_did: y, D and V have different lengths")
  if (any(d != 0 & d != 1)) stop("interaction_did: D must be 0 or 1")
  rows <- if (!is.null(X)) .s03mat(X) else NULL
  if (!is.null(rows) && nrow(rows) != n) stop("interaction_did: X and y have different lengths")
  levels <- sort(unique(v))
  atts <- numeric(length(levels)); ses <- numeric(length(levels)); counts <- numeric(length(levels))
  for (L in seq_along(levels)) {
    idx <- which(v == levels[L])
    sub_d <- d[idx]
    if (sum(sub_d) == 0 || sum(sub_d) == length(idx))
      stop("interaction_did: a level of V has only one treatment arm")
    sub_x <- if (!is.null(rows)) rows[idx, , drop = FALSE] else NULL
    r <- .s03drdid(dy[idx], sub_d, sub_x)
    atts[L] <- r$tau; ses[L] <- r$se; counts[L] <- length(idx)
  }
  K <- length(levels)
  if (K >= 2L) {
    est <- atts[K] - atts[1]; se <- sqrt(ses[K]^2 + ses[1]^2)
  } else {
    est <- atts[1]; se <- ses[1]
  }
  full <- .s03drdid(dy, d, rows)
  .t1_result(estimate = est, se = se, att = atts, att_se = ses,
             levels = levels, level_n = counts, att_overall = full$tau,
             se_overall = full$se, n_levels = K, n = n,
             method = "DR-DiD (Sant'Anna & Zhao 2020 eq. 2.6) within levels of V; contrast = interaction")
}
