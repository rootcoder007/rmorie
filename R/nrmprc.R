# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalized inverse-Gaussian process
#'
#' The NIG process normalizes a completely random measure whose Levy
#' intensity is the tilted stable-1/2 density
#' \code{rho(u) = (2 pi)^-1/2 u^-3/2 exp(-tau^2 u / 2)}, so
#' \code{int u rho(u) du = 1 / tau} exactly. Its tails are heavier than
#' the Dirichlet process's, which is why it is used in its place.
#' The Python arm delegates the quadrature to \code{gh_c14_14}, which
#' has no R mirror; the midpoint rule is five lines and is written here.
#'
#' @param y Observed values; only the count and the number of distinct
#'   values enter the summary.
#' @param alpha Total mass of the base measure, positive.
#' @param tau Exponential tilting parameter, positive.
#' @param u_max Upper quadrature limit.
#' @param n_grid Midpoint-rule grid size.
#' @return List with \code{estimate}, \code{theory}, \code{gap},
#'   \code{alpha}, \code{tau}, \code{n}, \code{n_distinct}.
#' @references Lijoi, A., Mena, R. H. & Prunster, I. (2005).
#'   Hierarchical mixture modeling with normalized inverse-Gaussian
#'   priors. Journal of the American Statistical Association, 100(472),
#'   1278-1291. Ghosal, S. & van der Vaart, A. (2017). Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 14.6.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Nrmprc(V)
Nrmprc <- function(y, alpha = 1, tau = 1, u_max = 10, n_grid = 6000) {
  v <- as.numeric(y)
  tt <- as.numeric(tau)
  a <- as.numeric(alpha)
  if (tt <= 0) stop("Nrmprc: tau must be positive")
  if (a <= 0) stop("Nrmprc: alpha must be positive")
  ng <- as.integer(n_grid); um <- as.numeric(u_max)
  i <- seq_len(ng)
  u <- (i - 0.5) * um / ng
  tot <- sum(u * u^(-1.5) * exp(-tt^2 * u / 2) / sqrt(2 * pi) * um / ng)
  .t1_result(estimate = tot, theory = 1 / tt, gap = abs(tot - 1 / tt),
             alpha = a, tau = tt, n = length(v),
             n_distinct = length(unique(v)),
             method = "Normalized inverse-Gaussian process (Lijoi-Mena-Prunster 2005)")
}
