# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean-square continuity, decided by the covariance at the origin.
#'
#' For a field with constant mean and variance, MS continuity at s means
#' lim_{h->0} E[(Z(s) - Z(s+h))^2] = 0, and since
#' E[(Z(s) - Z(s+h))^2] = 2 Var[Z(s)] - 2 C(h) = 2 (C(0) - C(h)) = 2 gamma(h),
#' the field is mean-square continuous IF AND ONLY IF C is continuous at
#' the origin. The whole question reduces to the behaviour of C near zero.
#'
#' The practical consequence the book draws: a process with a NUGGET
#' EFFECT has a discontinuity at the origin and cannot be mean-square
#' continuous. The gap reported here is that nugget.
#'
#' The decision is whether the gap TENDS TO ZERO, not whether it falls
#' below a fixed number. A continuous C has gaps shrinking with h; a
#' nugget leaves them on a plateau. A fixed tolerance cannot separate
#' those: for C(h) = exp(-3h) the gap at h = 1e-8 is still 3e-8, which
#' fails a 1e-8 test despite the function being perfectly continuous.
#'
#' @param cov_func Function C(h) taking a numeric vector, returning one.
#' @param tol Gap magnitude below which a plateau is not declared.
#' @return Named list: is_continuous, c0, limit_at_zero_plus, gap,
#'   nugget, gap_ratio, gamma_limit, approach, gaps, lags.
#' @references Schabenberger & Gotway (2005), Sec 2.3, pp. 49-50.
#' @examples
#' spcont(function(h) exp(-3 * h))$is_continuous
#' spcont(function(h) ifelse(h == 0, 1.3, exp(-3 * h)))$nugget
#' @export
spcont <- function(cov_func, tol = 1e-8) {
  if (!is.function(cov_func)) stop("`cov_func` must be a function C(h)")
  c0 <- as.numeric(cov_func(0))[1]
  hs <- c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6)
  approach <- as.numeric(cov_func(hs))
  gaps <- c0 - approach
  ratio <- if (abs(gaps[1]) > 0) gaps[length(gaps)] / gaps[1] else 0
  shrinking <- abs(gaps[length(gaps)]) < abs(gaps[1]) * 0.1 || abs(gaps[1]) <= tol
  plateau <- abs(gaps[length(gaps)]) > tol && ratio > 0.5
  is_cont <- isTRUE(shrinking && !plateau)
  nugget <- if (is_cont) 0 else gaps[length(gaps)]
  list(is_continuous = is_cont, c0 = c0,
       limit_at_zero_plus = approach[length(approach)],
       gap = gaps[length(gaps)], nugget = nugget, gap_ratio = ratio,
       gamma_limit = 2 * nugget, approach = approach, gaps = gaps, lags = hs)
}
