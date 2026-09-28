#' Random-phase spectral simulation of an isotropic Gaussian random field
#'
#' \eqn{f(s) = \sqrt{2\,sill/M} \sum_m \cos(\omega_m \cdot s + \phi_m)} with
#' uniform phases and wave vectors from the normalised spectral density
#' (Shinozuka 1971; Shinozuka and Deodatis 1991), so the covariance is
#' exactly \eqn{sill\,\rho(h)}. Radius quantiles: gaussian
#' \eqn{(2/a)\sqrt{-\log U}}; matern (exponential = nu 1/2)
#' \eqn{\sqrt{U^{-1/\nu} - 1}/a}; uniform direction. Uniforms are Philox
#' streams 0, 1, 2 of \code{seed}, identical to the Python arm.
#'
#' @param coords Two-column matrix of locations.
#' @param cov_model \code{"exponential"}, \code{"gaussian"} or
#'   \code{"matern"}.
#' @param sill Variance.
#' @param range_ Range a.
#' @param nu Matern smoothness.
#' @param n_waves Number of waves M.
#' @param seed Philox seed.
#' @return List with \code{field}, \code{omega} (M x 2), \code{phase}.
#' @references Shinozuka, M. (1971). Simulation of multivariate and
#'   multidimensional random processes. Journal of the Acoustical Society of
#'   America 49, 357-367.
#'
#'   Shinozuka, M. and Deodatis, G. (1991). Simulation of stochastic
#'   processes by spectral representation. Applied Mechanics Reviews 44,
#'   191-204.
#' @examples
#' RandomPhaseField(rbind(c(0, 0), c(1, .5)), "gaussian", n_waves = 4, seed = 2)$field
#' @export
RandomPhaseField <- function(coords, cov_model = "exponential", sill = 1, range_ = 1, nu = 0.5,
                             n_waves = 500L, seed = 1L) {
  if (!cov_model %in% c("exponential", "gaussian", "matern")) stop("cov_model must be exponential, gaussian or matern")
  if (range_ <= 0 || sill < 0 || nu <= 0 || n_waves < 1) stop("need range_ > 0, sill >= 0, nu > 0, n_waves >= 1")
  P <- as.matrix(coords)
  M <- as.integer(n_waves)
  ur <- .morie_random_uniform(M, seed = seed, stream = 0)
  ut <- .morie_random_uniform(M, seed = seed, stream = 1)
  up <- .morie_random_uniform(M, seed = seed, stream = 2)
  v <- if (cov_model == "exponential") 0.5 else nu
  rad <- if (cov_model == "gaussian") 2 / range_ * sqrt(-log(ur)) else sqrt(ur^(-1 / v) - 1) / range_
  om <- cbind(rad * cos(2 * pi * ut), rad * sin(2 * pi * ut))
  ph <- 2 * pi * up
  field <- sqrt(2 * sill / M) * rowSums(cos(P %*% t(om) + matrix(ph, nrow(P), M, byrow = TRUE)))
  list(field = field, omega = om, phase = ph)
}
