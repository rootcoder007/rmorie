# SPDX-License-Identifier: AGPL-3.0-or-later

#' Covariance of the second cumulative-survival estimator with the survival estimator (Eq. 4.22)
#'
#' Eq. (4.22):
#' \deqn{\mathrm{Cov}[S_{X,2}(t), \tilde S_X(t)] = \tfrac{1}{n}S_X(t)F_X(t) + o(h/n),}{Cov[S_X2(t), Stilde(t)] = (1/n) S(t) F(t) + o(h/n),}
#' the same expression as (4.16) for `S_X,1`.
#'
#' That the two covariances coincide, and Theorem 4.2's variance coincides with
#' Theorem 4.1's, is why Theorem 4.3 gives ONE variance formula covering
#' `m_X,1` and `m_X,2` together while giving them separate bias formulas. The
#' estimators differ in bias only.
#'
#' Kept as its own function rather than aliased to `Srvcov1`, because the
#' equality is a THEOREM -- a consequence of the transformation argument in
#' Sec. 4.1, not a definition -- and collapsing the two would hide that.
#'
#' @param n Sample size.
#' @param surv `S_X(t)`.
#' @param cdf `F_X(t)`; defaults to `1 - surv`.
#' @return Named list with ``covariance``, ``surv``, ``cdf``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (4.22).
#' @examples
#' Srvcov2(n = 100, surv = 0.4)
#' @export
Srvcov2 <- function(n, surv, cdf = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  f <- if (is.null(cdf)) 1 - surv else cdf
  list(covariance = surv * f / n, surv = surv, cdf = f, n = n,
       method = "Cov[S_X,2, tilde S_X] (Eq. 4.22)")
}

# CANONICAL TEST
# stopifnot(abs(Srvcov2(n = 100, surv = 0.4)$covariance - 0.0024) < 1e-15)

#' @rdname Srvcov2
#' @keywords internal
#' @export
morie_fauzi_cov_surv_est2 <- Srvcov2
