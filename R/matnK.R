# SPDX-License-Identifier: AGPL-3.0-or-later
#' The Matern correlation function
#'
#' Matern, B. (1960), Spatial Variation, Meddelanden fran Statens
#' Skogsforskningsinstitut 49(5), 1-144 (reprinted as Springer Lecture Notes
#' in Statistics 36, 1986).  Pages 17 and 18 were rendered at 150 dpi with
#' pdftoppm and read as images; the scan text layer is Paper-Capture OCR and
#' mangles the Bessel subscripts.
#'
#' Section 2.4, "Examples of correlation functions", builds the family from
#' the spectral side.  Equation (2.4.5) p. 17 mixes the Gaussian correlation
#' exp(-a^2 v^2) over a type III distribution for a^2 and gets
#' (1 + v^2/b^2)^-s; eq. (2.4.6) p. 17 gives the matching spectral density
#' const. w^(s - n/2) K_(s - n/2)(wb).  Transforming that density back,
#' eq. (2.4.7) p. 18 states the correlation function itself,
#' 2 (b v / 2)^nu K_nu(b v) / Gamma(nu), for b, nu >= 0, where K_nu is the
#' modified Bessel function of the second kind and v is the lag.  Matern
#' cites Watson (1944, p. 80) for the constant.
#'
#' Two special cases are printed on the same page and are used as anchors,
#' being the author own numbers rather than ours: nu = 1/2 gives exp(-b v),
#' eq. (2.4.8) p. 18, and nu = 1 gives v b K_1(v b), eq. (2.4.9) p. 18.
#'
#' Parameterisation.  Modern usage writes the same function as
#' sigma^2 (2^(1-nu) / Gamma(nu)) (sqrt(2 nu) d / rho)^nu
#' K_nu(sqrt(2 nu) d / rho), which is exactly (2.4.7) with
#' b v = sqrt(2 nu) d / rho and an added scale sigma^2, because
#' 2 (z/2)^nu / Gamma(nu) = 2^(1-nu) z^nu / Gamma(nu).  The rho form is
#' what this function takes, and b is reported so the reader can get back
#' to Matern own variable.
#'
#' The value at v = 0 is sigma^2, since z^nu K_nu(z) -> 2^(nu-1) Gamma(nu)
#' as z -> 0, which is the point of the constant in (2.4.7).  As nu grows
#' the function tends to the Gaussian correlation exp(-d^2 / (2 rho^2)),
#' Matern eq. (2.4.2) p. 17.
#'
#' @param d lags v, all non-negative.
#' @param nu smoothness, Matern nu of (2.4.7); must be positive.
#' @param rho range.  Matern own inverse scale is b = sqrt(2 nu) / rho.
#' @param sigma2 variance at lag zero; (2.4.7) itself is the correlation, so
#'   the default 1 reproduces the printed function exactly.
#' @return list: estimate, k, d, nu, rho, sigma2, b, n, method.
#' @keywords internal
#' @examples
#' MatnK(c(0, 0.5, 1), 0.5, 2)$k
#' @export
MatnK <- function(d, nu, rho, sigma2 = 1) {
  v <- .s03vec(d)
  if (length(v) == 0L) stop("matern_kernel: no distances supplied")
  nu <- as.numeric(nu)
  rho <- as.numeric(rho)
  sigma2 <- as.numeric(sigma2)
  if (is.na(nu) || !(nu > 0)) stop("matern_kernel: the smoothness nu must be positive")
  if (is.na(rho) || !(rho > 0)) stop("matern_kernel: the range rho must be positive")
  if (is.na(sigma2) || sigma2 < 0) stop("matern_kernel: the variance sigma2 must be non-negative")
  k <- numeric(length(v))
  for (i in seq_along(v)) k[i] <- .s03maternk(v[i], nu, rho, sigma2)
  list(estimate = k[1], k = k, d = v, nu = nu, rho = rho, sigma2 = sigma2,
       b = sqrt(2 * nu) / rho, n = length(v),
       method = "Matern (1960) correlation function, eq. (2.4.7) p. 18")
}

# Equation (2.4.7) p. 18 at a single lag.  Written in logs so that large nu
# does not overflow (z/2)^nu before Gamma(nu) divides it out again.
.s03maternk <- function(d, nu, rho, sigma2) {
  if (d < 0) stop("matern_kernel: distances d must be non-negative")
  if (d == 0) return(sigma2)
  z <- sqrt(2 * nu) * d / rho
  lead <- log(2) + nu * log(z / 2) - lgamma(nu)
  sigma2 * exp(lead) * .s03besselk(nu, z)
}
