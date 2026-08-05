# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalizability coefficient for the crossed p x i design
#'
#' The random-effects ANOVA of a persons-by-items score matrix gives the
#' three variance components; the generalizability coefficient is for
#' relative decisions and the index of dependability for absolute ones.
#' For this design the generalizability coefficient is algebraically
#' identical to coefficient alpha.
#'
#' Formula: var_p = (MS_p - MS_pi)/n_i, var_i = (MS_i - MS_pi)/n_p,
#'   var_pi = MS_pi; E rho^2 = var_p / (var_p + var_pi/n_i);
#'   Phi = var_p / (var_p + (var_i + var_pi)/n_i).
#'
#' @param X Persons-by-items score matrix.
#' @param facets Optional number of items to project onto.
#' @return List with \code{estimate}, \code{e_rho2}, \code{phi},
#'   \code{var_p}, \code{var_i}, \code{var_pi}, \code{ms_p}, \code{ms_i},
#'   \code{ms_pi}, \code{n_p}, \code{n_i}, \code{method}.
#' @references Cronbach, Gleser, Nanda and Rajaratnam (1972), The
#'   Dependability of Behavioral Measurements, Wiley; Brennan (2001),
#'   Generalizability Theory, Springer, ch. 2.
#' @export
Genvxt <- function(X, facets = NULL) {
  M <- .s03mat(X)
  npr <- nrow(M); ni <- ncol(M)
  if (npr < 2L) stop("generalizability_theory: need at least two persons")
  if (ni < 2L) stop("generalizability_theory: need at least two items")
  grand <- mean(M)
  pm <- rowMeans(M); im <- colMeans(M)
  msp <- ni * sum((pm - grand)^2) / (npr - 1)
  msi <- npr * sum((im - grand)^2) / (ni - 1)
  res <- 0
  for (i in seq_len(npr)) for (j in seq_len(ni))
    res <- res + (M[i, j] - pm[i] - im[j] + grand)^2
  mspi <- res / ((npr - 1) * (ni - 1))
  vp <- (msp - mspi) / ni
  vi <- (msi - mspi) / npr
  vpi <- mspi
  k <- if (is.null(facets)) ni else as.integer(facets)
  if (k < 1L) stop("generalizability_theory: facets must be at least 1")
  erho <- if ((vp + vpi / k) != 0) vp / (vp + vpi / k) else NaN
  phi <- if ((vp + (vi + vpi) / k) != 0) vp / (vp + (vi + vpi) / k) else NaN
  .t1_result(estimate = erho, e_rho2 = erho, phi = phi, var_p = vp,
             var_i = vi, var_pi = vpi, ms_p = msp, ms_i = msi, ms_pi = mspi,
             n_p = npr, n_i = k,
             method = "random-effects ANOVA components, Cronbach et al. (1972); Brennan (2001) ch. 2")
}
