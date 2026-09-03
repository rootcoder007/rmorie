# SPDX-License-Identifier: AGPL-3.0-or-later

#' Covariance of the first cumulative-survival estimator with the survival estimator (Eq. 4.16)
#'
#' Eq. (4.16):
#' \deqn{\mathrm{Cov}\[S_{X,1}(t), \tilde S_X(t)\] = \tfrac{1}{n}S_X(t)F_X(t) +
#' o(h/n).}{Cov\[S_X1(t), Stilde(t)\] = (1/n) S(t) F(t) + o(h/n).}
#'
#' Small, but not negligible: it is exactly the term that survives when
#' Theorem 4.3 forms the ratio `m_X,1 = S_X,1 / Stilde` and linearises it.
#' Dropping it leaves the mean-residual-life variance wrong at order `1/n` --
#' its leading order.
#'
#' The leading term is the empirical-df variance `F(1-F)/n` written the other
#' way round, since `S = 1 - F`. Not a coincidence: at leading order both
#' estimators are the empirical df, with the kernel smoothing entering only at
#' `O(h/n)`.
#'
#' @param n Sample size.
#' @param surv `S_X(t)`.
#' @param cdf `F_X(t)`; defaults to `1 - surv`.
#' @return Named list with ``covariance``, ``surv``, ``cdf``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (4.16).
#' @examples
#' Srvcov1(n = 100, surv = 0.4)
#' @export
Srvcov1 <- function(n, surv, cdf = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  f <- if (is.null(cdf)) 1 - surv else cdf
  list(covariance = surv * f / n, surv = surv, cdf = f, n = n,
       method = "Cov[S_X,1, tilde S_X] (Eq. 4.16)")
}

# CANONICAL TEST
# stopifnot(abs(Srvcov1(n = 100, surv = 0.4)$covariance - 0.0024) < 1e-15)

#' @rdname Srvcov1
#' @keywords internal
#' @export
morie_fauzi_cov_surv_est1 <- Srvcov1
