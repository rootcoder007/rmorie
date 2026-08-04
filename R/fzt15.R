# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of the modified gamma kernel density estimator (Theorem 1.5)
#'
#' Theorem 1.5:
#' \deqn{\mathrm{Var}[\tilde f_X(x)] = 4\mathrm{Var}[A_h] + \mathrm{Var}[A_{4h}] - 4\mathrm{Cov}[A_h,A_{4h}] + o(n^{-1}h^{-1/4}).}{Var[ftilde(x)] = 4 Var[A_h] + Var[A_4h] - 4 Cov[A_h, A_4h] + o(n^-1 h^-1/4).}
#'
#' Not a new calculation -- it is the variance of the linear combination
#' `2 A_h - A_4h`, which the proof reaches by showing
#' `J_h / J_4h = 1 + O(sqrt(h))` so the nonlinear ratio in (1.14) linearises.
#' Being a linear combination, the ORDERS do not change:
#' `O(n^-1 h^-1/4)` in the interior, `O(n^-1 h^-3/4)` at the boundary.
#'
#' With Theorem 1.3 the MSE is `O(h^2) + O(n^-1 h^-1/4)` in the interior,
#' optimised at `h = O(n^(-4/9))` for a rate `O(n^(-8/9))`; at the boundary
#' `O(h^2) + O(n^-1 h^-3/4)`, optimised at `h = O(n^(-4/11))` for
#' `O(n^(-8/11))`. Both beat Chen's `O(n^(-4/5))` and `O(n^(-2/3))`. Those
#' rates come back as `hopt` and `mserate`.
#'
#' @param varh `Var[A_h(x)]`, e.g. from `Gkrawbv`.
#' @param var4h `Var[A_4h(x)]`.
#' @param cov `Cov[A_h(x), A_4h(x)]`, e.g. from `Gkcov`.
#' @param n Sample size; only used to evaluate the optimal-bandwidth rates.
#' @param boundary Logical; report the boundary rates instead of the interior.
#' @return Named list with ``variance``, ``hopt``, ``mserate``, ``region``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 1.5, Eqs. (1.18)-(1.19).
#' @examples
#' Mgkvar(varh = 0.02, var4h = 0.01, cov = 0.005, n = 100)
#' @export
Mgkvar <- function(varh, var4h, cov, n = NULL, boundary = FALSE) {
  var_t <- 4 * varh + var4h - 4 * cov
  if (isTRUE(boundary)) {
    pow_h <- -4 / 11; pow_mse <- -8 / 11; region <- "boundary"
  } else {
    pow_h <- -4 / 9; pow_mse <- -8 / 9; region <- "interior"
  }
  if (is.null(n)) {
    hopt <- NA_real_; mserate <- NA_real_
  } else {
    if (n < 1) stop("sample size must be at least 1.")
    hopt <- n^pow_h; mserate <- n^pow_mse
  }
  list(variance = var_t, hopt = hopt, mserate = mserate, region = region,
       method = "modified gamma KDE variance (Theorem 1.5)")
}

# CANONICAL TEST
# r <- Mgkvar(varh = 0.02, var4h = 0.01, cov = 0.005, n = 100)
# stopifnot(abs(r$variance - 0.07) < 1e-15)

#' @rdname Mgkvar
#' @keywords internal
#' @export
morie_fauzi_thm1_5_consistency_mgkde <- Mgkvar
