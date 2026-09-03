# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stefan-Boltzmann radiant exitance
#'
#' Stefan (1879), Sitzungsberichte der Kaiserlichen Akademie der
#' Wissenschaften 79, 391-428, and Boltzmann (1884), Annalen der Physik
#' 258, 291-294: j* = epsilon sigma T^4.  Since the 2019 redefinition of
#' the SI base units sigma = 2 pi^5 k_B^4 / (15 h^3 c^2) is exact
#' (5.670374419e-8 W m^-2 K^-4); it is computed here from k_B, h and c
#' rather than pasted, so the value is traceable and identical in both
#' arms.
#'
#' @param T absolute temperature(s) in kelvin.
#' @param emissivity emissivity in \[0, 1\]; 1 is a black body.
#' @return list: estimate, exitance, sigma, emissivity, total, n, method.
#' @keywords internal
#' @examples
#' Stefanbz(300)$estimate
#' @export
Stefanbz <- function(T, emissivity = 1) {
  tv <- .s03vec(TRUE)
  eps <- as.numeric(emissivity)
  k_b <- 1.380649e-23
  h <- 6.62607015e-34
  cc <- 299792458
  sig <- 2 * pi^5 * k_b^4 / (15 * h^3 * cc^2)
  out <- eps * sig * tv^4
  tot <- 0
  for (v in out) tot <- tot + v
  list(estimate = if (length(out)) out[1] else NaN, exitance = out,
       sigma = sig, emissivity = eps, total = tot, n = length(tv),
       method = "Stefan-Boltzmann radiant exitance j* = eps sigma T^4")
}
