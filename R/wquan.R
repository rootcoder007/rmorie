# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weighted quantile, Harrell-Davis and inverse-weighted-ECDF forms
#'
#' Q_p = sum_i W_i X_(i) with W_i the increment of the regularized incomplete
#' beta function I(p(n+1), (1-p)(n+1)) across the observation knot, which for
#' equal weights is exactly equations (2)-(3) of the paper.  Source consulted:
#' Harrell and Davis (1982), A new distribution-free quantile estimator,
#' Biometrika 69(3), 635-640.
#'
#' @param y numeric observations.
#' @param weights optional non-negative observation weights.
#' @param p probability in (0, 1).
#' @return list: estimate, hd, ecdf, w, p, n, method.
#' @keywords internal
#' @examples
#' wquan(c(1, 2, 3), NULL, 0.5)
#' @export
wquan <- function(y, weights = NULL, p = 0.5) {
  yy <- as.numeric(y); n <- length(yy)
  wv <- if (is.null(weights)) rep(1, n) else as.numeric(weights)[seq_len(n)]
  ord <- order(yy)
  xs <- yy[ord]; ws <- wv[ord]
  tot <- sum(ws)
  a <- p * (n + 1); b <- (1 - p) * (n + 1)
  knots <- c(0, cumsum(ws / tot))
  knots[n + 1] <- 1
  cdfv <- stats::pbeta(knots, a, b)
  wt <- diff(cdfv)
  hd <- sum(wt * xs)
  cum <- cumsum(ws / tot)
  idx <- which(cum >= p)
  ecdf <- if (length(idx) > 0) xs[idx[1]] else xs[n]
  list(estimate = as.numeric(hd), hd = as.numeric(hd), ecdf = as.numeric(ecdf),
       w = wt, p = as.numeric(p), n = as.integer(n),
       method = "Harrell-Davis weighted quantile (Harrell & Davis 1982)")
}

# CANONICAL TEST
# r <- wquan(c(1, 2, 3), NULL, 0.5)
# stopifnot(abs(r$estimate - 2) < 1e-12, abs(sum(r$w) - 1) < 1e-12)

#' @rdname wquan
#' @keywords internal
#' @export
morie_weighted_quantile <- wquan
