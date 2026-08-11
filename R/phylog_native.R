# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Root-to-tip regression dating (Phylog). Bit-identical mirror of
# src/morie/fn/phylog.py. Anchored against stats::lm coefficients
# and a hand-built strict-clock example with known rate and TMRCA.

#' Root-to-tip regression dating (TempEst)
#'
#' For heterochronous sequences on a rooted phylogeny with sampling
#' times \eqn{t_i} and root-to-tip distances \eqn{d_{r,i}}, a strict
#' molecular clock implies \eqn{E[d_{r,i}] = u (t_i - t_r)} (eq. 1
#' of the paper). Ordinary least squares of d on t estimates the
#' substitution rate u as the slope and the root time \eqn{t_r}
#' (time of the most recent common ancestor) as the x-intercept.
#' Correlation and R-squared diagnose temporal signal; residuals
#' flag incongruent sequences.
#'
#' @param dates Sampling times.
#' @param divergence Root-to-tip genetic distances.
#' @return List with \code{rate}, \code{intercept}, \code{tmrca},
#'   \code{correlation}, \code{r_squared}, \code{residuals},
#'   \code{n}, \code{method}.
#' @references Rambaut, A., Lam, T. T., Max Carvalho, L. and Pybus,
#'   O. G. (2016), Exploring the temporal structure of
#'   heterochronous sequences using TempEst (formerly Path-O-Gen),
#'   Virus Evolution 2(1), vew007. Equation (1) and Section 2,
#'   Root-to-tip regression. Local source:
#'   library/pdf/fetched-wave3/Rambaut-2016-TempEst-VirusEvolution.pdf.
#' @export
Phylog <- function(dates, divergence) {
  t <- as.numeric(dates)
  d <- as.numeric(divergence)
  n <- length(t)
  if (length(d) != n) {
    stop("dates and divergence must have equal length", call. = FALSE)
  }
  if (n < 3L) stop("need at least 3 tips", call. = FALSE)
  tbar <- mean(t); dbar <- mean(d)
  sxx <- sum((t - tbar)^2)
  sxy <- sum((t - tbar) * (d - dbar))
  syy <- sum((d - dbar)^2)
  if (sxx <= 0) stop("all sampling dates identical", call. = FALSE)
  u <- sxy / sxx
  a <- dbar - u * tbar
  tmrca <- if (u != 0) -a / u else NaN
  corr <- if (syy > 0) sxy / sqrt(sxx * syy) else NaN
  list(rate = u, intercept = a, tmrca = tmrca, correlation = corr,
       r_squared = corr * corr, residuals = d - (a + u * t), n = n,
       method = "root-to-tip regression dating (Rambaut et al. 2016)")
}
