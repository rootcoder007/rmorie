# SPDX-License-Identifier: AGPL-3.0-or-later

#' Dimension reduction in single-index and multiple-index models
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 1.2 (page 3), Section 2.2 (page 11) and
#' Section 2.4 (page 18).  A single-index model collapses a
#' d-dimensional covariate onto one index: beta is estimable at the
#' parametric rate n^(-1/2), and because that is faster than any
#' nonparametric rate, replacing beta by an estimate does not change
#' the asymptotics of the estimator of G, which therefore converges at
#' the ONE-dimensional rate n^(-s/(2s+1)).  With M indices the
#' nonparametric problem is M-dimensional instead, so estimation of
#' E(Y|X=x), but not of beta, still suffers the curse as M grows.
#'
#' Closed-form rate arithmetic; no estimation and no randomness.
#'
#' @param d Integer dimension of X.
#' @param n Integer sample size.
#' @param s Integer smoothness / kernel order.
#' @param M Integer number of indices; M = 1 is the single-index model.
#' @return Named list with fullexp, fullrate, indexexp, indexrate,
#'   betaexp, betarate, gain, effdim, d, M, s, n, method.
#' @keywords internal
#' @examples
#' Dimredrate(5, 10000)$indexexp   # 2/5
#' @export
Dimredrate <- function(d, n, s = 2L, M = 1L) {
  d <- as.integer(d)
  n <- as.integer(n)
  s <- as.integer(s)
  M <- as.integer(M)
  if (d < 1L || n < 1L || s < 1L || M < 1L) {
    stop("d, n, s and M must all be positive integers.", call. = FALSE)
  }
  fullexp <- s / (2 * s + d)
  idxexp <- s / (2 * s + M)
  fullrate <- n^(-fullexp)
  idxrate <- n^(-idxexp)
  list(fullexp = fullexp, fullrate = fullrate, indexexp = idxexp,
       indexrate = idxrate, betaexp = 0.5, betarate = n^(-0.5),
       gain = fullrate / idxrate, effdim = M, d = d, M = M, s = s, n = n,
       method = "Horowitz (2009) Sections 1.2, 2.2, 2.4 rate comparison")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Dimredrate
#' @keywords internal
#' @export
morie_horowitz_dimension_reduction <- Dimredrate
