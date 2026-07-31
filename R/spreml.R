# SPDX-License-Identifier: AGPL-3.0-or-later

#' Restricted maximum likelihood (REML) for semivariogram parameters
#'
#' Minimises minus twice the restricted log likelihood, eq (4.39),
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
spreml <- function(coords, z, X = NULL, variogram_model = "exponential") {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  n <- length(z)
  if (nrow(coords) != n) {
    stop("`coords` and `z` must have the same number of rows")
  }
  if (is.null(X)) X <- matrix(1, nrow = n, ncol = 1) else X <- as.matrix(X)
  if (nrow(X) != n) stop("`X` must have one row per observation")

  K <- .schab_error_contrasts(X)
  KZ <- as.numeric(K %*% z)

  neg2_reml <- function(theta) {
    nugget <- theta[1]; sill <- theta[2]; rng <- theta[3]
    if (any(!is.finite(theta)) || nugget < 0 || sill < 0 || rng <= 0 ||
        (nugget + sill) <= 0) {
      return(Inf)
    }
    sigma <- .schab_covariance_matrix(coords, nugget, sill, rng, variogram_model)
    m <- K %*% sigma %*% t(K)
    ch <- tryCatch(chol(m), error = function(e) NULL)
    if (is.null(ch)) return(Inf)   # theta outside the positive-definite region
    logdet <- 2 * sum(log(diag(ch)))
    sol <- backsolve(ch, backsolve(ch, KZ, transpose = TRUE))
    logdet + nrow(K) * log(2 * pi) + sum(KZ * sol)
  }

  ev <- .sp_empirical_variogram(coords, z)
  sb <- .schab_start_and_bounds(ev$lag, ev$gamma)
  start <- sb$start; lo <- sb$lo; hi <- sb$hi

  # Nelder-Mead rather than a quasi-Newton method: phi_R is +Inf outside the
  # positive-definite region, so a numerical gradient straddling that boundary
  # comes back non-finite and the solver quits at the starting point while
  # reporting success. A simplex never differences across the barrier.
  bounded <- function(theta) {
    if (any(!is.finite(theta)) || any(theta < lo) || any(theta > hi)) return(Inf)
    neg2_reml(theta)
  }
  best_x <- start
  best_f <- bounded(start)
  for (frac in c(0.05, 0.2, 0.5)) {
    for (rscale in c(0.5, 1.0, 2.0)) {
      x0 <- pmin(pmax(c(frac * start[2], start[2], rscale * start[3]), lo), hi)
      res <- stats::optim(x0, bounded, method = "Nelder-Mead",
                          control = list(maxit = 2000, reltol = 1e-12))
      if (is.finite(res$value) && res$value < best_f) {
        best_x <- res$par
        best_f <- res$value
      }
    }
  }
  nugget <- best_x[1]; sill <- best_x[2]; rng <- best_x[3]

  # eq (4.40): the EGLS estimator evaluated at theta_reml.
  sigma <- .schab_covariance_matrix(coords, nugget, sill, rng, variogram_model)
  sinv_x <- solve(sigma, X)
  beta <- solve(crossprod(X, sinv_x), crossprod(sinv_x, z))

  list(nugget = nugget, partial_sill = sill, sill = nugget + sill,
       range = rng, mean = if (length(beta) > 1) as.numeric(beta) else beta[1],
       neg2_restricted_loglik = best_f,
       converged = best_f < bounded(start), n = n, n_contrasts = nrow(K),
       model = variogram_model, method = "restricted maximum likelihood")
}
