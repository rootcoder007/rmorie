# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hansen-Hurwitz estimator for unequal-probability sampling with replacement
#'
#' That = mean of y_k / p_k, with the unbiased variance estimate
#' sum((y_k/p_k - That)^2) / (n (n - 1)).  Source consulted: Hansen and
#' Hurwitz (1943), On the theory of sampling from finite populations, Annals
#' of Mathematical Statistics 14(4), 333-362.
#'
#' @param y values observed on the n draws.
#' @param p per-draw selection probability of the unit drawn.
#' @param sizes optional size measure; p is then sizes / sum(sizes).
#' @return list: estimate, se, variance, zbar, z, n, method.
#' @keywords internal
#' @examples
#' ppswrs(c(1, 2, 3), c(0.1, 0.2, 0.3))
#' @export
ppswrs <- function(y, p, sizes = NULL) {
  yy <- as.numeric(y)
  pp <- if (!is.null(sizes)) as.numeric(sizes) / sum(as.numeric(sizes)) else as.numeric(p)
  n <- min(length(yy), length(pp))
  z <- yy[seq_len(n)] / pp[seq_len(n)]
  est <- sum(z) / n
  varr <- if (n > 1) sum((z - est)^2) / (n * (n - 1)) else NA_real_
  se <- if (!is.na(varr) && varr >= 0) sqrt(varr) else NA_real_
  list(estimate = as.numeric(est), se = as.numeric(se),
       variance = as.numeric(varr), zbar = as.numeric(est), z = z,
       n = as.integer(n),
       method = "Hansen-Hurwitz pps-with-replacement estimator (Hansen & Hurwitz 1943)")
}

# CANONICAL TEST
# r <- ppswrs(c(1, 2, 3), c(0.1, 0.2, 0.3))
# stopifnot(abs(r$estimate - 10) < 1e-12, abs(r$variance) < 1e-20)

#' @rdname ppswrs
#' @keywords internal
#' @export
morie_pps_with_replacement <- ppswrs
