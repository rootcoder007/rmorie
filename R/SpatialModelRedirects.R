#' Spatial probit, logit, Poisson, ZIP and panel models (entry points)
#'
#' Named entry points for spatial regression models that route to the
#' estimators: \code{SpatialProbit} to the GMM spatial autoregressive probit
#' (\code{SpatialProbitGmm}, method "gmm") or the Bayesian SAR probit sampler
#' (\code{SarProbitGibbs}, method "bayes"); \code{SpatialLogit} to the
#' linearized GMM spatial logit (\code{SpatialLogitGmm}); \code{SpatialPoisson}
#' and \code{SpatialZip} to the spatial-lag Poisson and zero-inflated Poisson
#' regressions (\code{SarPoisson}, \code{SarZip}); \code{SpatialPanelFe} to the
#' fixed-effects maximum likelihood spatial panel models
#' (\code{SpatialPanelMl}) and \code{SpatialPanelRe} to the random-effects
#' spatial lag panel (\code{SpatialPanelReLag}). Identical to the Python arms
#' \code{morie.fn.xrspp}, \code{xrspl}, \code{xrspc}, \code{xrspz},
#' \code{xrspn} and \code{xrspr}.
#'
#' @param y Outcome (0/1, counts, or a panel stacked by period).
#' @param X Design matrix.
#' @param W Spatial weights matrix.
#' @param method "gmm" or "bayes" for \code{SpatialProbit}.
#' @param ... Passed to the chosen probit estimator.
#' @param Z Instrument matrix (logit) or zero-model design (ZIP).
#' @param rho_bounds Search interval for rho.
#' @param n_units Number of cross-sectional units.
#' @param model "lag", "error" or "durbin".
#' @param effects "individual", "time" or "twoways".
#' @param lee_yu Apply the Lee-Yu variance correction.
#' @param interval Search interval for the spatial parameter.
#' @param tol,maxit Convergence tolerance and iteration cap.
#' @return The list returned by the underlying estimator.
#' @references Pinkse, J. and Slade, M. E. (1998). Contracting in space: an
#'   application of spatial statistics to discrete-choice models. Journal of
#'   Econometrics 85, 125-154.
#'
#'   Klier, T. and McMillen, D. P. (2008). Clustering of auto supplier plants
#'   in the United States. Journal of Business and Economic Statistics 26,
#'   460-471.
#'
#'   Lambert, D. M., Brown, J. P. and Florax, R. J. G. M. (2010). A two-step
#'   estimator for a spatial lag model of counts. Regional Science and Urban
#'   Economics 40, 241-252.
#'
#'   Elhorst, J. P. (2003). Specification and estimation of spatial panel data
#'   models. International Regional Science Review 26, 244-268.
#' @examples
#' W <- matrix(0, 10, 10)
#' W[abs(row(W) - col(W)) == 1] <- 1
#' W <- W / rowSums(W)
#' X <- cbind(1, c(2, -1, 0.1, 1.5, 0.6, -0.4, 0.9, -1.3, 0.2, 1.1))
#' y <- c(1, 0, 1, 1, 0, 0, 1, 0, 0, 1)
#' SpatialProbit(y, X, W)$rho
#' SpatialLogit(y, X, W)$rho
#' @export
SpatialProbit <- function(y, X, W, method = "gmm", ...) {
  switch(method,
    gmm = SpatialProbitGmm(y, X, W, ...),
    bayes = SarProbitGibbs(y, X, W, ...),
    stop("method must be 'gmm' or 'bayes'")
  )
}

#' @rdname SpatialProbit
#' @export
SpatialLogit <- function(y, X, W, Z = NULL) SpatialLogitGmm(y, X, W, Z = Z)

#' @rdname SpatialProbit
#' @export
SpatialZip <- function(y, X, W, Z = NULL, rho_bounds = c(-0.99, 0.99)) SarZip(y, X, W, Z = Z, rho_bounds = rho_bounds)

#' @rdname SpatialProbit
#' @export
SpatialPoisson <- function(y, X, W, rho_bounds = c(-0.99, 0.99)) SarPoisson(y, X, W, rho_bounds = rho_bounds)

#' @rdname SpatialProbit
#' @export
SpatialPanelFe <- function(y, X, W, n_units, model = "lag", effects = "individual", lee_yu = FALSE,
                           interval = c(-0.99, 0.99)) {
  if (!effects %in% c("individual", "time", "twoways")) stop("effects must be 'individual', 'time' or 'twoways'")
  SpatialPanelMl(y, X, W, n_units, model = model, effects = effects, lee_yu = lee_yu, interval = interval)
}

#' @rdname SpatialProbit
#' @export
SpatialPanelRe <- function(y, X, W, n_units, interval = c(-0.99, 0.99), tol = 1e-10, maxit = 500) {
  SpatialPanelReLag(y, X, W, n_units, interval = interval, tol = tol, maxit = maxit)
}
