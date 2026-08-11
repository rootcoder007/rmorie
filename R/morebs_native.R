# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical-Bayes standardized Moran I for area rates
#'
#' Assuncao-Reis adjustment of Moran I for rates from heterogeneous
#' populations. With cases \eqn{O_i}, populations \eqn{n_i}, raw rates
#' \eqn{p_i = O_i/n_i}: \eqn{b = \sum O / \sum n},
#' \eqn{s^2 = \sum n_i (p_i - b)^2 / \sum n},
#' \eqn{a = \max(0, s^2 - b/(\sum n / m))}, \eqn{v_i = a + b/n_i},
#' \eqn{z_i = (p_i - b)/\sqrt{v_i}}, and Moran I is computed on the
#' centred z: \eqn{EBI = (m/S_0) \sum_i \tilde z_i (W\tilde z)_i / \sum_i \tilde z_i^2}.
#'
#' @param cases Event counts, length m.
#' @param population Population at risk, length m, strictly positive.
#' @param W Spatial weights matrix, m by m.
#' @return List with statistic (EBI), z, rates, eb_rates, a, b, s2, S0, n.
#' @references Assuncao, R. M. and Reis, E. A. (1999). A new proposal to
#'   adjust Moran I for population density. Statistics in Medicine,
#'   18(16), 2147-2162.
#'
#'   Bivand, R. S., Pebesma, E. and Gomez-Rubio, V. (2013). Applied
#'   Spatial Data Analysis with R, 2nd ed., Springer, Sec. 9.3, p. 282.
#'   Local PDF: WD_BLACK/library/pdf/bivand2013.pdf.
#'
#'   Reference implementation spdep EBImoran.mc, EBest (CRAN); the
#'   subtract-mean-in-numerator convention (spdep default since 2016)
#'   is adopted.
#' @examples
#' W <- matrix(0, 4, 4); W[cbind(1:3, 2:4)] <- 1; W[cbind(2:4, 1:3)] <- 1
#' empirical_bayes_moran(c(5, 2, 9, 4), c(200, 90, 400, 150), W)
#' @export
empirical_bayes_moran <- function(cases, population, W) {
  O <- as.numeric(cases)
  n <- as.numeric(population)
  W <- as.matrix(W)
  m <- length(O)
  if (length(n) != m) stop("`cases` and `population` must have equal length")
  if (!all(dim(W) == c(m, m))) stop("W must be m by m")
  if (any(n <= 0)) stop("population must be strictly positive")
  if (any(O < 0)) stop("cases must be non-negative")
  p <- O / n
  b <- sum(O) / sum(n)
  s2 <- sum(n * (p - b)^2) / sum(n)
  a <- s2 - b / (sum(n) / m)
  if (a < 0) a <- 0
  v <- a + b / n
  z <- (p - b) / sqrt(v)
  eb_rates <- b + (a * (p - b)) / (a + b / n)
  S0 <- sum(W)
  zt <- z - mean(z)
  lz <- as.numeric(W %*% zt)
  ebi <- (m / S0) * sum(zt * lz) / sum(zt^2)
  list(statistic = ebi, z = z, rates = p, eb_rates = eb_rates,
       a = a, b = b, s2 = s2, S0 = S0, n = m,
       method = "Assuncao-Reis EB-standardized Moran I")
}
