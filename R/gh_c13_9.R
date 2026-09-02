# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neutral-to-the-right Levy (Laplace) functional
#'
#' E exp(-int f dM) = exp(-int (1 - exp(-f u)) dnu) is the Levy-Khinchine
#' formula for an independent-increment process.  It is the only handle
#' on an NTR prior that stays closed form, and every posterior update in
#' the section is carried out through it rather than through a density.
#' Computed exactly here for a discrete Levy measure.
#'
#' Formula: exponent = sum_j m_j (1 - exp(-f_j));
#'   value = exp(-exponent).
#'
#' @param f_vals Values of the test function at the atoms of nu.
#' @param nu_masses Masses of the discrete Levy measure, non-negative.
#' @return List with \code{estimate} (the Laplace functional),
#'   \code{exponent}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalntrlevy(V, V)
Ghosalntrlevy <- function(f_vals, nu_masses) {
  fs <- as.numeric(f_vals)
  ms <- as.numeric(nu_masses)
  if (length(fs) == 0L) stop("f_vals must be non-empty")
  if (length(ms) != length(fs))
    stop("f_vals and nu_masses must have the same length")
  if (any(ms < 0)) stop("nu_masses must be non-negative")
  exponent <- sum(ms * (1 - exp(-fs)))
  .t1_result(estimate = exp(-exponent), exponent = exponent,
             method = "NTR Laplace functional (GvdV 2017 sec. 13.4)")
}
