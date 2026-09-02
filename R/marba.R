# SPDX-License-Identifier: AGPL-3.0-or-later
#' The variance of a standardised effect when the two scores are paired
#'
#' Treating a pre-post or crossover design as if the two arms were
#' independent inflates the variance and, in a meta-analysis, silently
#' down-weights exactly the designs that carry the most information. The
#' correlation between the paired measurements enters the variance
#' directly: at \code{rho = 1} the sampling variance of the raw difference
#' vanishes and only the term from estimating the standardiser survives.
#'
#' Formula: \code{V_g = J^2 (2(1 - rho)/n + g^2/(2(n-1)))} with the
#' small-sample correction \code{J = 1 - 3/(4(n-1) - 1)} -- Morris (2008)
#' eq. (7)-(8); Morris and DeShon (2002).
#'
#' @param g The corrected standardised mean difference.
#' @param n Number of subjects; \code{n >= 2}.
#' @param rho Correlation between the paired measurements, in \[-1, 1\].
#' @return List with \code{var_g}, \code{se}, \code{J}, \code{n},
#'   \code{rho}.
#' @references Morris, S. B. (2008). Organizational Research Methods
#'   11(2):364-386. \doi{10.1177/1094428106291059}.
#' @export
#' @examples
#' Marba(g = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, rho = 0.5)
Marba <- function(g, n, rho) {
  nn <- as.numeric(n); r <- as.numeric(rho)
  if (nn < 2) stop("n must be at least two")
  if (r < -1 || r > 1) stop("rho must lie in [-1, 1]")
  df <- nn - 1
  J <- 1 - 3 / (4 * df - 1)
  gg <- as.numeric(g)
  v <- J^2 * (2 * (1 - r) / nn + gg^2 / (2 * df))
  .t1_result(var_g = v, se = sqrt(v), J = J, n = nn, rho = r,
             method = "Variance of Hedges' g for a correlated design")
}
