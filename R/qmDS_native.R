# Empirical quantile mapping (bias correction / downscaling).
# Source: Gudmundsson, L., Bremnes, J. B., Haugen, J. E. and
# Engen-Skaugen, T. (2012), Technical note: downscaling RCM
# precipitation to the station scale using statistical transformations
# -- a comparison of methods, Hydrology and Earth System Sciences 16,
# 3383-3390: their eq. (2), x' = F_obs^{-1}(F_mod(x)), applied in the
# empirical "QUANT" form -- percentile tables with linear
# interpolation between them, the procedure of Boe et al. (2007) that
# their Sec. 2.1 adopts.
#
# Native implementation mirroring Python morie.fn.qmDS exactly: the
# empirical CDF used here is the exact inverse of the type-7
# interpolated quantile function, so mapping a sample onto itself is
# the identity and a pure shift is recovered exactly.

# piecewise-linear CDF through (x_(j), j/(n-1)), clamped to [0, 1]
#' Piecewise-linear CDF through (x_(j), j/(n-1)), clamped to \[0, 1\]
#'
#' A step of the qmDS_native implementation. Called by \code{morie_qmDS}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sorted_x A vector; its length is taken and its elements indexed.
#' @param v Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mor_qm_ecdf <- function(sorted_x, v) {
  n <- length(sorted_x)
  if (n == 1L) return(0.5)
  if (v <= sorted_x[1L]) return(0)
  if (v >= sorted_x[n]) return(1)
  lo <- 0L; hi <- n - 1L
  while (hi - lo > 1L) {
    mid <- (lo + hi) %/% 2L
    if (sorted_x[mid + 1L] <= v) lo <- mid else hi <- mid
  }
  x0 <- sorted_x[lo + 1L]; x1 <- sorted_x[lo + 2L]
  g <- if (x1 == x0) 0 else (v - x0) / (x1 - x0)
  (lo + g) / (n - 1L)
}

# inverse empirical CDF, type-7 convention h = (n - 1) p
#' Inverse empirical CDF, type-7 convention h = (n - 1) p
#'
#' A step of the qmDS_native implementation. Called by \code{morie_qmDS}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sorted_x A vector; its length is taken and its elements indexed.
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mor_qm_quantile <- function(sorted_x, p) {
  n <- length(sorted_x)
  if (n == 1L) return(sorted_x[1L])
  h <- (n - 1L) * p
  j <- as.integer(h)
  if (j >= n - 1L) return(sorted_x[n])
  g <- h - j
  sorted_x[j + 1L] * (1 - g) + sorted_x[j + 2L] * g
}

#' Empirical quantile mapping
#'
#' Bias-corrects a modelled series by
#' \eqn{x' = F_{obs}^{-1}(F_{mod}(x))} (Gudmundsson et al. 2012, eq.
#' 2) using empirical percentile tables with linear interpolation.
#' Because the CDF and quantile function used are exact inverses of
#' one another, mapping the calibration sample onto itself returns it
#' unchanged and a constant offset between model and observations is
#' removed exactly.
#'
#' @param x_mod Values to correct.
#' @param obs Observed calibration sample.
#' @param mod Modelled calibration sample.
#' @return A list with \code{estimate} (corrected values),
#'   \code{probs} (the intermediate CDF values), \code{n_obs},
#'   \code{n_mod}, \code{method}.
#' @references Gudmundsson, L. et al. (2012). Technical note:
#'   downscaling RCM precipitation to the station scale. Hydrology and
#'   Earth System Sciences, 16, 3383-3390.
#' @export
#' @examples
#' morie_qmDS(x_mod = c(1, 2, 3, 4, 5, 6, 7, 8), obs = c(1, 2, 3, 4, 5, 6, 7, 8), mod = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_qmDS <- function(x_mod, obs, mod) {
  xm <- as.numeric(x_mod)
  ob <- sort(as.numeric(obs))
  md <- sort(as.numeric(mod))
  if (length(ob) == 0L || length(md) == 0L)
    stop("qmDS: calibration samples must be non-empty")
  probs <- vapply(xm, function(v) .mor_qm_ecdf(md, v), numeric(1))
  out <- vapply(probs, function(p) .mor_qm_quantile(ob, p), numeric(1))
  list(estimate = out, probs = probs, n_obs = length(ob), n_mod = length(md),
       method = paste("empirical quantile mapping x' = F_obs^-1(F_mod(x))",
                      "(Gudmundsson 2012 Eq. 2, QUANT)"))
}
