# R arm of morie/fn/gauvar.py -- gaussian semivariogram model.
#
# The Python body was a placeholder: it averaged `h` and never used c0, c
# or a. There was no R arm at all.
#
#   gamma(h) = c0 + c (1 - exp(-(h/a)^2)),  h > 0;   gamma(0) = 0
#
# The smoothest of the standard family: infinitely differentiable at the
# origin, which is why Schabenberger & Gotway (p. 144) call the processes
# it describes "truly artificial" and warn that the name has nothing to do
# with the Gaussian distribution. It approaches its sill parabolically
# rather than linearly.
#
# Range conventions again: a here is the range PARAMETER. Schabenberger &
# Gotway eq. (4.10) uses R(h) = exp{-3 (h/alpha)^2} on the PRACTICAL range
# alpha, so a = alpha/sqrt(3) and alpha = a sqrt(3). Note that the
# exponential conversion is alpha/3, NOT alpha/sqrt(3) -- an easy way to
# misfit a range by 70%.
#
# Cressie (1993), Statistics for Spatial Data, rev. edn., sec. 2.3.1.

#' @noRd
morie_gaussian_variogram_model <- function(h, c0 = 0, c = 1, a = 1) {
  hs <- .expvar_lags(h)
  c0 <- as.numeric(c0)[1L]
  c <- as.numeric(c)[1L]
  a <- as.numeric(a)[1L]
  if (c0 < 0) stop("c0 (nugget) must be >= 0", call. = FALSE)
  if (c < 0) stop("c (partial sill) must be >= 0", call. = FALSE)
  if (!(a > 0)) stop("a (range parameter) must be > 0", call. = FALSE)

  gamma <- ifelse(hs == 0, 0, c0 + c * (1 - exp(-(hs / a)^2)))
  cov <- ifelse(hs == 0, c0 + c, c * exp(-(hs / a)^2))

  list(gamma = gamma, covariance = cov, h = hs, c0 = c0, c = c, a = a,
       sill = c0 + c, practical_range = sqrt(3) * a, n = length(hs),
       method = "Gaussian semivariogram, gamma(h) = c0 + c(1 - exp(-(h/a)^2))")
}

#' @noRd
Gauvar <- morie_gaussian_variogram_model
