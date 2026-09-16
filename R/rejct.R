# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hampel's rejection point, gross-error and local-shift sensitivities
#'
#' rho* = inf\{r : psi(x) = 0 for |x| > r\}, gamma* = sup|psi| / E\[psi'\],
#' lambda* = sup|psi(x)-psi(y)|/|x-y|, all at the standard normal.  Huber's psi
#' has rho* infinite, gamma* = k/(2 Phi(k) - 1) and lambda* = 1; Tukey's
#' biweight has rho* = c and sup|psi| = 16 c/(25 sqrt 5) at x = c/sqrt 5, with
#' E\[psi'\] by Simpson's rule on \[-c, c\].  Source consulted: Hampel (1974),
#' JASA 69(346), 383-393, section 2.
#'
#' @param psi "huber" or "bisquare".
#' @param tuning k or c; 1.345 and 4.685 by default.
#' @param panels even number of Simpson panels.
#' @return list: estimate, gross_error_sensitivity, local_shift_sensitivity,
#'   sup_psi, expected_psi_prime, tuning, psi, n, method.
#' @keywords internal
#' @examples
#' rejct("bisquare", 4.685)$estimate
#' @export
rejct <- function(psi = "huber", tuning = NULL, panels = 2000L) {
  fam <- tolower(as.character(psi))
  if (fam == "huber") {
    k <- if (is.null(tuning)) 1.345 else as.numeric(tuning)
    eprime <- 2 * stats::pnorm(k) - 1
    return(list(estimate = Inf, gross_error_sensitivity = k / eprime,
                local_shift_sensitivity = 1, sup_psi = k,
                expected_psi_prime = eprime, tuning = k, psi = "huber", n = 0L,
                method = "Hampel robustness measures for Huber's psi (Hampel 1974, sec. 2)"))
  }
  if (!(fam %in% c("bisquare", "biweight", "tukey")))
    stop("psi must be 'huber' or 'bisquare'")
  cc <- if (is.null(tuning)) 4.685 else as.numeric(tuning)
  dpsi <- function(t) (1 - 6 * t * t / (cc * cc) + 5 * t^4 / cc^4) * stats::dnorm(t)
  np <- as.integer(panels)
  h <- (2 * cc) / np
  tot <- dpsi(-cc) + dpsi(cc)
  for (i in seq_len(np - 1L)) tot <- tot + (if (i %% 2L == 1L) 4 else 2) * dpsi(-cc + i * h)
  eprime <- tot * h / 3
  sup <- 16 * cc / (25 * sqrt(5))
  list(estimate = cc, gross_error_sensitivity = sup / eprime,
       local_shift_sensitivity = 1, sup_psi = sup,
       expected_psi_prime = eprime, tuning = cc, psi = "bisquare", n = np,
       method = "Hampel robustness measures for Tukey's biweight (Hampel 1974, sec. 2)")
}

# CANONICAL TEST
# stopifnot(is.infinite(rejct("huber")$estimate),
#           abs(rejct("bisquare", 4.685)$estimate - 4.685) < 1e-15)

#' @rdname rejct
#' @keywords internal
#' @export
morie_rejct <- rejct
