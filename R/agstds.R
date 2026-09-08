# SPDX-License-Identifier: AGPL-3.0-or-later
#' Directly age-standardised rate
#'
#' Formula: ASR = sum_i w_i r_i / sum_i w_i; var(ASR) = sum_i w_i^2 r_i / n_i / (sum_i w_i)^2
#'
#' @param rates Age-specific rates.
#' @param standard_pop Standard population weights by age band.
#' @param person_time Person-time in each band of the study population.

#' @param rates See Usage.
#' @param standard_pop See Usage.
#' @param person_time See Usage.
#' @return List with ``asr``, ``variance``, ``se``, ``ci_lower``, ``ci_upper``, ``weights``, ``k``.
#' @references Boyle and Parkin (1991), Statistical methods for registries, in Jensen et
#' al (eds), Cancer Registration: Principles and Methods, IARC Scientific Publications
#' 95. Not held locally; the direct standardisation estimator and its Poisson variance
#' are the standard published forms.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Agestd(V, V)
Agestd <- function(rates, standard_pop, person_time = NULL) {
  r <- .t1_vec(rates)
  w <- .t1_vec(standard_pop)
  k <- length(r)
  if (k != length(w)) stop("rates and standard_pop must be the same length")
  sw <- sum(w)
  if (sw <= 0) stop("standard population must have positive total")
  asr <- sum(w * r) / sw
  var <- NA_real_
  se <- NA_real_
  lo <- NA_real_
  hi <- NA_real_
  if (!is.null(person_time)) {
    n <- .t1_vec(person_time)
    if (any(n <= 0)) stop("person-time must be positive")
    var <- sum(w^2 * r / n) / sw^2
    se <- sqrt(var)
    z <- stats::qnorm(0.975)
    lo <- asr - z * se
    hi <- asr + z * se
  }
  .t1_result(
    asr = asr, variance = var, se = se, ci_lower = lo, ci_upper = hi,
    weights = w / sw, k = k, method = "Directly age-standardised rate"
  )
}
