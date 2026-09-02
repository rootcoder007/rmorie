# Planck spectral radiance, wavelength form.
# Sources: Planck, M. (1901), Ueber das Gesetz der Energieverteilung
# im Normalspectrum, Annalen der Physik 4, 553-563; Rybicki, G. B. and
# Lightman, A. P. (1979), Radiative Processes in Astrophysics, Sec.
# 1.5-1.6 (the wavelength form, Wien displacement and the
# Stefan-Boltzmann integral).  Constants are the SI-2019 exact
# defining values of h, c and k_B.
#
# Native implementation mirroring Python morie.fn.plncF exactly,
# including the exponent clamp at 700 and the 1e-300 guard that keeps
# the denominator away from zero in the Rayleigh-Jeans limit.

#' Planck spectral radiance B(lambda, T)
#'
#' Spectral radiance per unit wavelength,
#' \eqn{B_\lambda = (2hc^2/\lambda^5)/(e^{hc/\lambda k_B T} - 1)}
#' (Planck 1901; Rybicki and Lightman 1979, Sec. 1.5), together with
#' the Wien displacement peak \eqn{\lambda_{max} = b/T} and the
#' Stefan-Boltzmann total emissive power \eqn{\sigma T^4}.
#'
#' @param lam Wavelengths in metres, all strictly positive.
#' @param T Temperature in kelvin, strictly positive.
#' @param h Planck constant (SI-2019 exact value by default).
#' @param c Speed of light in vacuum.
#' @param kB Boltzmann constant.
#' @return A list with \code{estimate} (radiances),
#'   \code{peak_wavelength}, \code{wien_constant},
#'   \code{total_power}, \code{T}, \code{method}.
#' @references Rybicki, G. B. and Lightman, A. P. (1979). Radiative
#'   Processes in Astrophysics. Wiley, Sections 1.5-1.6.
#' @export
#' @examples
#' morie_plncF(lam = c(1, 2, 3, 4, 5, 6, 7, 8), T = 5L)
morie_plncF <- function(lam, T, h = 6.62607015e-34, c = 299792458,
                        kB = 1.380649e-23) {
  la <- as.numeric(lam)
  T <- as.numeric(T)
  if (T <= 0) stop("plncF: temperature must be > 0")
  if (any(la <= 0)) stop("plncF: wavelengths must be > 0")
  x <- pmin(h * c / (la * kB * T), 700)
  vals <- (2 * h * c * c / la^5) / (exp(x) - 1 + 1e-300)
  # Wien displacement constant: b = hc / (kB x*), x* the root of
  # (x - 5) e^x + 5 = 0
  xstar <- 4.965114231744276
  b_wien <- h * c / (kB * xstar)
  sigma <- 2 * pi^5 * kB^4 / (15 * h^3 * c^2)
  list(estimate = vals, peak_wavelength = b_wien / T,
       wien_constant = b_wien, total_power = sigma * T^4, T = T,
       method = paste("Planck spectral radiance, wavelength form",
                      "B(lam,T) = 2hc^2/lam^5 / (exp(hc/lam kB T)-1)"))
}
