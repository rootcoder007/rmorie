# SPDX-License-Identifier: AGPL-3.0-or-later
#' Target the variance parameter sigma^2 = E[D*^2] and bound it.
#'
#' The plug-in var(IC) has its own sampling error that a confidence
#' interval built on it treats as zero. Targeting sigma^2 gives it an
#' influence curve D*^2 - sigma^2 and therefore its own interval, on the
#' LOG scale because a variance is positive.
#'
#' Formula: sigma^2 = E[D*(O)^2]; IC = D*(O)^2 - sigma^2;
#'   se(sigma^2) = sqrt(var(D*^2)/n); se(psi) = sqrt(sigma^2/n)
#'
#' @param ic Efficient influence-curve values, mean zero.
#' @param level Confidence level.
#' @return List with \code{sigma2}, \code{se_sigma2}, \code{ci_lower},
#'   \code{ci_upper}, \code{se_psi}, \code{se_psi_lower},
#'   \code{se_psi_upper}, \code{kurtosis}, \code{ic_mean}, \code{n}.
#' @references The row cites vdL-Hubbard-Pajouh (2018). That paper was NOT
#'   obtainable, so what is implemented is the standard construction that
#'   follows from the definition: the influence curve of sigma^2 = E[D*^2]
#'   is D*^2 - sigma^2. The plug-in variance it refines is the
#'   var.psi <- var(IC)/n of the CRAN package tmle 2.1.1 (Gruber & van der
#'   Laan), which was fetched and read.
#' @export
Tmlevar <- function(ic, level = 0.95) {
  d <- .t1_vec(ic); n <- length(d)
  if (n < 3L)
    stop("at least three influence-curve values are required")
  if (level <= 0 || level >= 1)
    stop("level must lie strictly between 0 and 1")
  m <- mean(d)
  s2 <- sum(d^2) / n
  if (s2 <= 0) stop("the influence curve is identically zero")
  sq <- d^2
  ses <- sqrt(stats::var(sq) / n)
  z <- stats::qnorm((1 + level) / 2)
  ls <- ses / s2
  lo <- s2 * exp(-z * ls); hi <- s2 * exp(z * ls)
  .t1_result(sigma2 = s2, se_sigma2 = ses, ci_lower = lo, ci_upper = hi,
             se_psi = sqrt(s2 / n), se_psi_lower = sqrt(lo / n),
             se_psi_upper = sqrt(hi / n),
             kurtosis = (sum(d^4) / n) / s2^2, ic_mean = m,
             n = as.numeric(n),
             method = "Variance targeting: sigma^2 = E[D*^2] with its own IC")
}
