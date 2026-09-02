# SPDX-License-Identifier: AGPL-3.0-or-later
#' Optimal individualized treatment regime chosen by DR-DiD value
#'
#' An individualized rule is chosen by maximising its estimated value
#' over a restricted class.  The class here is the threshold rules
#' \code{1{W_j > c}}; the value of a rule is the doubly robust DiD ATT
#' among the units it targets.  Candidate thresholds are the deciles of
#' each covariate, so the search is finite and deterministic; subgroups
#' without both treatment arms, or smaller than \code{min_frac} of the
#' sample, are skipped.  No sample splitting is done, so the reported
#' maximum is optimistic; the complementary group's ATT is reported
#' beside it.
#'
#' Formula: d* = argmax_{j,c} tau_hat({i : W_ij > c}).
#'
#' @param y Outcome change dY.
#' @param D Treatment indicator, 0 or 1.
#' @param W Covariates the rule may use, one row per unit.
#' @param min_frac Smallest admissible targeted share of the sample.
#' @return List with \code{estimate}, \code{se}, \code{feature},
#'   \code{threshold}, \code{n_targeted}, \code{share_targeted},
#'   \code{att_complement}, \code{att_overall}, \code{se_overall},
#'   \code{gain}, \code{n_rules}, \code{n}, \code{method}.
#' @references Athey and Imbens (2017), The state of applied
#'   econometrics, Journal of Economic Perspectives 31(2):3-32,
#'   \doi{10.1257/jep.31.2.3}; Sant'Anna and Zhao (2020), Journal of
#'   Econometrics 219(1):101-122. \doi{10.1016/j.jeconom.2020.06.003}
#' @export
#' @examples
#' set.seed(1)
#' Itr2dd(y = rnorm(40), D = rbinom(40, 1, 0.5), W = matrix(rnorm(80), 40, 2))
Itr2dd <- function(y, D, W, min_frac = 0.25) {
  dy <- .s03vec(y); n <- length(dy)
  if (n == 0L) stop("itr_optimal_did: y is empty")
  d <- .s03vec(D)
  if (length(d) != n) stop("itr_optimal_did: y and D have different lengths")
  if (any(d != 0 & d != 1)) stop("itr_optimal_did: D must be 0 or 1")
  rows <- .s03mat(W)
  if (nrow(rows) != n) stop("itr_optimal_did: W and y have different lengths")
  p <- ncol(rows)
  if (!(min_frac > 0 && min_frac < 1)) stop("itr_optimal_did: min_frac must lie in (0, 1)")
  LEV <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  full <- .s03drdid(dy, d, rows)
  best <- NULL; tried <- 0L
  for (j in seq_len(p)) {
    col <- rows[, j]
    cand <- unique(vapply(LEV, function(q) .s03quantile7(col, q), 0))
    for (cc in cand) {
      idx <- which(col > cc)
      if (length(idx) < min_frac * n || length(idx) > (1 - min_frac) * n) next
      sub_d <- d[idx]
      if (sum(sub_d) == 0 || sum(sub_d) == length(idx)) next
      r <- .s03drdid(dy[idx], sub_d, rows[idx, , drop = FALSE])
      tried <- tried + 1L
      if (is.null(best) || r$tau > best$tau)
        best <- list(tau = r$tau, j = j, c = cc, m = length(idx), se = r$se)
    }
  }
  if (is.null(best)) stop("itr_optimal_did: no admissible threshold rule")
  comp <- which(rows[, best$j] <= best$c)
  cd <- d[comp]
  tau_c <- if (sum(cd) > 0 && sum(cd) < length(comp))
    .s03drdid(dy[comp], cd, rows[comp, , drop = FALSE])$tau else NaN
  .t1_result(estimate = best$tau, se = best$se, feature = best$j - 1L,
             threshold = best$c, n_targeted = best$m,
             share_targeted = best$m / n, att_complement = tau_c,
             att_overall = full$tau, se_overall = full$se,
             gain = best$tau - full$tau, n_rules = tried, n = n,
             method = "d*(W) = argmax_{j,c} DR-DiD ATT on {W_j > c}, Athey & Imbens (2017)")
}
