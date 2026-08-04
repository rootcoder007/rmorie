# R arm of morie/fn/expvar.py -- exponential semivariogram model.
#
# The Python body was a placeholder: it averaged `h` and never used c0, c
# or a. There was no R arm at all.
#
#   gamma(h) = c0 + c (1 - exp(-h/a)),  h > 0;   gamma(0) = 0
#
# c0 is the nugget, c the partial sill (sill = c0 + c) and a the range
# PARAMETER. The nugget is a discontinuity AT the origin: gamma(0) = 0 by
# definition even when c0 > 0, and gamma(0+) = c0.
#
# Two live range conventions. This module uses the range parameter a, as
# its own specification asks. Schabenberger & Gotway eq. (4.11) p. 144
# write C(h) = sigma^2 exp{-theta h} = sigma^2 exp{-3 h / alpha} on the
# PRACTICAL range alpha, so a = 1/theta = alpha/3 and the correlation has
# fallen to exp(-3) = 0.0498 at h = alpha. The shared core
# aaa_sp_vario_shared.R uses alpha; pass a = alpha/3 to move between them.
# Mixing the two silently rescales every fitted range by a factor of three.
#
# Cressie (1993), Statistics for Spatial Data, rev. edn., sec. 2.3.1.
# Cressie is not in the local corpus; the parameterisation was verified
# against the rendered Schabenberger & Gotway page.

#' @noRd
.expvar_lags <- function(h) {
  hs <- as.numeric(h)
  if (length(hs) == 0L) stop("h is empty.", call. = FALSE)
  if (any(is.na(hs)) || any(hs < 0)) {
    stop("lag distances must be non-negative", call. = FALSE)
  }
  hs
}

#' @noRd
morie_exponential_variogram_model <- function(h, c0 = 0, c = 1, a = 1) {
  hs <- .expvar_lags(h)
  c0 <- as.numeric(c0)[1L]; c <- as.numeric(c)[1L]; a <- as.numeric(a)[1L]
  if (c0 < 0) stop("c0 (nugget) must be >= 0", call. = FALSE)
  if (c < 0) stop("c (partial sill) must be >= 0", call. = FALSE)
  if (!(a > 0)) stop("a (range parameter) must be > 0", call. = FALSE)

  gamma <- ifelse(hs == 0, 0, c0 + c * (1 - exp(-hs / a)))
  cov <- ifelse(hs == 0, c0 + c, c * exp(-hs / a))

  list(gamma = gamma, covariance = cov, h = hs, c0 = c0, c = c, a = a,
       sill = c0 + c, practical_range = 3 * a, n = length(hs),
       method = "Exponential semivariogram, gamma(h) = c0 + c(1 - exp(-h/a))")
}

#' @noRd
Expvar <- morie_exponential_variogram_model
