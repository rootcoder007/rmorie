# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adjust a pooled effect for suppressed small studies
#'
#' An asymmetric funnel suggests studies are missing from one side. Trim
#' and fill strips the asymmetric tail, re-estimates the centre, then
#' puts back mirror images so the funnel is symmetric. The filled studies
#' are fabrications: this is a sensitivity analysis, and the quantity of
#' interest is how far the pooled effect moves, not where it lands.
#'
#' Determinism: fixed number of trim-refit rounds, ranks tied by
#' position, no stopping rule.
#'
#' Formula: \code{L0 = (4 T_n - n(n+1))/(2n - 1)} with \code{T_n} the
#' rank sum on the suppressed-opposite side; filled values are
#' \code{2 theta - y_i}.
#'
#' @param yi Study effect sizes.
#' @param vi Study sampling variances.
#' @param side Side the missing studies are on, "left" or "right".
#' @param n_iter Trim-and-refit rounds.
#' @return List with \code{theta_adj}, \code{estimate}, \code{k_filled},
#'   \code{fill_yi}, \code{theta_raw}, \code{k}.
#' @references Duval, S. & Tweedie, R. (2000). Trim and fill. Biometrics
#'   56:455-463, equation (2) and section 3.
#' @export
Matrim <- function(yi, vi, side = "left", n_iter = 50) {
  y <- as.numeric(yi); v <- as.numeric(vi); k <- length(y)
  sgn <- if (identical(side, "left")) 1 else -1
  w <- 1 / v
  theta <- sum(w * y) / sum(w)
  theta_raw <- theta
  k0 <- 0L
  for (it in seq_len(as.integer(n_iter))) {
    cc <- sgn * (y - theta)
    r <- .s4_rank_first(abs(cc))
    Tn <- sum(r[cc > 0])
    l0 <- (4 * Tn - k * (k + 1)) / (2 * k - 1)
    k0 <- as.integer(.s4_rnd(l0))
    if (k0 < 0L) k0 <- 0L
    if (k0 > k - 1L) k0 <- k - 1L
    o <- order(sgn * y, seq_len(k))
    keep <- o[seq_len(k - k0)]
    theta <- sum(w[keep] * y[keep]) / sum(w[keep])
  }
  o <- order(sgn * y, seq_len(k))
  trimmed <- if (k0 > 0L) o[(k - k0 + 1L):k] else integer(0)
  fill <- 2 * theta - y[trimmed]
  ally <- c(y, fill)
  allv <- c(v, v[trimmed])
  allw <- 1 / allv
  theta_adj <- sum(allw * ally) / sum(allw)
  .t1_result(theta_adj = theta_adj, estimate = theta_adj, k_filled = k0,
             fill_yi = fill, theta_raw = theta_raw, k = k,
             method = "Duval-Tweedie trim and fill, L0 estimator")
}
