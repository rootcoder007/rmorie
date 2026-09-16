# SPDX-License-Identifier: AGPL-3.0-or-later

#' Restricted maximum likelihood (REML) for semivariogram parameters
#'
#' Follows Sec. 4.5.2 and Sec. 5.5.3. The matrix of error contrasts K is
#' ELIMINATED via Searle et al. (1992, pp. 451-452),
#' \eqn{K'(K\Sigma K')^{-1}K = \Sigma^{-1} - \Sigma^{-1}X\Omega X'\Sigma^{-1}},
#' which reduces the quadratic form to \eqn{r'\Sigma^{-1}r}; a scale parameter
#' is then profiled out by eq (5.49), leaving the nugget RATIO and the range.
#' Sec. 5.5.2 names the optimiser ("Newton-Raphson, Quasi-Newton, or some
#' other suitable algorithm"); the quasi-Newton branch is used, driven by an
#' exact analytic gradient. Formerly this minimised eq (4.39) directly,
#' \eqn{\phi_R(\theta) = \ln|K \Sigma(\theta) K'| + (n-p)\ln(2\pi)
#'      + Z'K'(K\Sigma(\theta)K')^{-1}KZ}, where K is a matrix of error
#' contrasts chosen so that \eqn{E[KZ(s)] = 0}. REML maximises the likelihood
#' of \eqn{KZ(s)} rather than of \eqn{Z(s)}, which removes the mean from the
#' problem and mitigates the downward bias of the ML variance estimates
#' (Patterson and Thompson, 1971).
#'
#' K is not unique. Sec. 4.5.2 writes it explicitly for the intercept-only
#' mean and notes, citing Harville (1974), that the choice does not affect the
#' estimates; here it is an orthonormal basis for the orthogonal complement of
#' the column space of `X`, which works for any linear mean structure.
#'
#' There is no REML estimator of the mean. The text is explicit on this: the
#' mean is recovered afterwards by evaluating the generalized least squares
#' estimator (4.40) at theta_reml, which is what `mean` reports.
#'
#' @param coords Matrix of sampling locations, one row per observation.
#' @param z Numeric vector of observed values.
#' @param X Design matrix for the mean; defaults to an intercept, the
#'   \eqn{E[Z(s)] = \mu} case worked in the text.
#' @param variogram_model One of "exponential", "gaussian", "spherical".
#' @return A list with `nugget`, `partial_sill`, `sill`, `range`, `mean`,
#'   `neg2_restricted_loglik`, `converged`, `n` and `n_contrasts`.
#' @references Schabenberger Ch 4, Sec 4.5.2
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' spreml(V, V)
spreml <- function(coords, z, X = NULL, variogram_model = "exponential") {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  n <- length(z)
  if (nrow(coords) != n) {
    stop("`coords` and `z` must have the same number of rows")
  }
  if (is.null(X)) X <- matrix(1, nrow = n, ncol = 1) else X <- as.matrix(X)
  if (nrow(X) != n) stop("`X` must have one row per observation")

  fit <- .schab_fit_reml(coords, z, X, model = variogram_model)
  list(nugget = fit$nugget, partial_sill = fit$partial_sill,
       sill = fit$nugget + fit$partial_sill, range = fit$range,
       nugget_ratio = fit$nugget_ratio,
       mean = if (length(fit$beta) > 1) fit$beta else fit$beta[1],
       neg2_restricted_loglik = fit$neg2_restricted_loglik,
       converged = fit$converged, n = n, n_contrasts = n - ncol(X),
       model = variogram_model, method = "restricted maximum likelihood")
}
