# SPDX-License-Identifier: AGPL-3.0-or-later
#' Begg and Mazumdar's rank correlation test for publication bias
#'
#' The effects are standardised so that under the null they are
#' independent of their variances: \eqn{w_i = 1/v_i},
#' \eqn{\theta = \sum w_i y_i / \sum w_i}, \eqn{v_b = 1/\sum w_i},
#' \eqn{v_i^* = v_i - v_b}, \eqn{y_i^* = (y_i - \theta)/\sqrt{v_i^*}};
#' the statistic is Kendall's tau between \eqn{y^*} and \eqn{v}, with
#' the two-sided normal-approximation p-value.  Subtracting \eqn{v_b}
#' is what makes the null hold; without it the test over-rejects.
#'
#' @param yi Observed effect sizes.
#' @param vi Their sampling variances (not standard errors).
#' @return List with \code{tau}, \code{statistic} (z), \code{p_value},
#'   \code{n}, \code{method}.
#' @references Begg and Mazumdar (1994), Biometrics 50:1088-1101.  Paywalled; the coded form was read from Viechtbauer's metafor, R/ranktest.r, the reference implementation.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Beggtest(V, V)
Beggtest <- function(yi, vi) {
  yi <- .t4_vec(yi)
  vi <- .t4_vec(vi)
  k <- length(yi)
  if (length(vi) != k) stop("yi and vi must be the same length")
  if (k < 3) stop("need at least 3 studies")
  if (any(vi <= 0)) stop("sampling variances must be positive")
  w <- 1 / vi
  theta <- sum(w * yi) / sum(w)
  vb <- 1 / sum(w)
  vstar <- vi - vb
  if (any(vstar <= 0)) stop("vi - 1/sum(1/vi) must be positive for every study")
  ystar <- (yi - theta) / sqrt(vstar)
  kt <- .t4_kendalltaub(ystar, vi)
  p <- if (is.nan(kt$z)) NaN else 2 * stats::pnorm(abs(kt$z), lower.tail = FALSE)
  .t4_result(tau = kt$tau, statistic = kt$z, p_value = p,
             n = as.integer(k), method = "Begg-Mazumdar rank correlation test")
}
