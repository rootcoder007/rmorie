# SPDX-License-Identifier: AGPL-3.0-or-later

#' Asymptotic variance of the quantile estimator
#'
#' Eqs. (3.2)-(3.3):
#' \deqn{V[\hat Q(p)] = \frac{p(1-p)}{n f^2(F^{-1}(p))} + O(n^{-2}),\qquad \mathrm{AMSE} = \frac{[Q'(p)]^2 p(1-p)}{n},}{V[Qhat(p)] = p(1-p)/(n f^2(F^-1(p))) + O(n^-2), AMSE = Q'(p)^2 p(1-p)/n,}
#' the second form using `Q'(p) = 1 / f(Q(p))`.
#'
#' Both spellings come back, because the equality between them is the thing
#' worth checking: a quantile's variance is the binomial variance `p(1-p)`
#' transported through the inverse-cdf map, and the density at the quantile is
#' the Jacobian. Where the density is small the quantile is badly determined --
#' the entire reason tail quantiles are hard, stated as arithmetic.
#'
#' Remark 3.3 is blunt that the KERNEL quantile estimator has this same
#' first-order variance. Chapter 3's improvement is second-order only.
#'
#' Supply `qp` (the quantile-function derivative) or `density` (the density at
#' the quantile), not both.
#'
#' @param p Probability in `(0, 1)`.
#' @param n Sample size.
#' @param density `f(F^-1(p))`.
#' @param qp `Q'(p)`; equals `1 / density`.
#' @return Named list with ``variance``, ``se``, ``amse``, ``sigma2``, ``qp``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eqs. (3.2)-(3.3).
#' @examples
#' Qasyvar(p = 0.5, n = 100, density = 0.4)
#' @export
Qasyvar <- function(p, n, density = NULL, qp = NULL) {
  if (!(p > 0 && p < 1)) stop("p must lie strictly in (0, 1).")
  if (n < 1) stop("sample size must be at least 1.")
  if (is.null(density) == is.null(qp)) stop("supply exactly one of density or qp.")
  if (is.null(qp)) {
    if (density <= 0) stop("the density at the quantile must be positive.")
    qp <- 1 / density
  }
  s2 <- qp * qp * p * (1 - p)
  list(variance = s2 / n, se = sqrt(s2 / n), amse = s2 / n, sigma2 = s2,
       qp = qp, n = n,
       method = "asymptotic variance of the quantile estimator (Eqs. 3.2-3.3)")
}

# CANONICAL TEST
# r <- Qasyvar(p = 0.5, n = 100, density = 0.4)
# stopifnot(abs(r$variance - 0.25 / (100 * 0.16)) < 1e-15)

#' @rdname Qasyvar
#' @keywords internal
#' @export
morie_fauzi_quantile_asymp_var <- Qasyvar
