# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of the sample variance
#'
#' Var(s^2) = (mu4 - sigma^4 (n - 3)/(n - 1)) / n for n i.i.d. draws from a pmf,
#' with mu4 the fourth central moment.
#'
#' @param values,probs Outcomes and probabilities (summing to 1).
#' @param n Sample size, >= 2.
#' @return list(var_s2, sigma2, mu4).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.94).
#' @examples
#' SampVarVar(1:6, rep(1 / 6, 6), 2)$var_s2
#' @export
SampVarVar <- function(values, probs, n) {
  x <- as.numeric(values)
  p <- as.numeric(probs)
  if (length(x) != length(p) || !length(x) || min(p) < 0 || abs(sum(p) - 1) > 1e-9) {
    stop("values and probs must match and probs must sum to 1", call. = FALSE)
  }
  if (length(n) != 1L || n != round(n) || n < 2) stop("n must be an integer >= 2", call. = FALSE)
  mu <- sum(p * x)
  s2 <- sum(p * (x - mu)^2)
  mu4 <- sum(p * (x - mu)^4)
  list(var_s2 = (mu4 - s2 * s2 * (n - 3) / (n - 1)) / n, sigma2 = s2, mu4 = mu4)
}

#' Expectation of the geometric distribution
#'
#' Sums 1 p + 2 (1-p) p + 3 (1-p)^2 p + ... directly and by the book's
#' rearrangement into geometric series, both equal to 1/p.
#'
#' @param p Success probability, 0 < p <= 1.
#' @param terms Number of series terms.
#' @return list(mean, series, row_sums).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (4.77)-(4.80).
#' @examples
#' GeomExp(0.25)$series
#' @export
GeomExp <- function(p, terms = 2000) {
  p <- as.numeric(p)
  if (length(p) != 1L || !(p > 0 && p <= 1) || terms < 1) stop("need 0 < p <= 1 and terms >= 1", call. = FALSE)
  k <- seq_len(terms)
  q <- 1 - p
  list(mean = 1 / p, series = sum(k * q^(k - 1) * p), row_sums = sum(q^(k - 1)))
}

#' Y = mX + Z model from sigma_x, sigma_y and r
#'
#' m = r sigma_y / sigma_x and sigma_z = sigma_y sqrt(1 - r^2).
#'
#' @param sigma_x,sigma_y Standard deviations, > 0.
#' @param r Correlation between -1 and 1.
#' @return list(m, sigma_z).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (6.16)-(6.18), (6.35).
#' @examples
#' LinModelInv(1, 2, 0.6)
#' @export
LinModelInv <- function(sigma_x, sigma_y, r) {
  if (sigma_x <= 0 || sigma_y <= 0 || abs(r) > 1) stop("need sigma_x, sigma_y > 0 and -1 <= r <= 1", call. = FALSE)
  list(m = r * sigma_y / sigma_x, sigma_z = sigma_y * sqrt(1 - r * r))
}

#' Joint density of X and Y = mX + Z
#'
#' Model form exp(-x^2/2sx^2 - (y - m x)^2/2sz^2)/(2 pi sx sz) and the
#' equivalent correlation form with sigma_y and r.
#'
#' @param x,y Point.
#' @param m Slope.
#' @param sigma_x,sigma_z Standard deviations, > 0.
#' @return list(density, density_r, sigma_y, r).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (6.28)-(6.34).
#' @examples
#' BvnModel(0.5, 1, 1, 1, 0.5)$density
#' @export
BvnModel <- function(x, y, m, sigma_x, sigma_z) {
  if (sigma_x <= 0 || sigma_z <= 0) stop("sigma_x and sigma_z must be > 0", call. = FALSE)
  d <- exp(-x^2 / (2 * sigma_x^2) - (y - m * x)^2 / (2 * sigma_z^2)) / (2 * pi * sigma_x * sigma_z)
  sy <- sqrt(m^2 * sigma_x^2 + sigma_z^2)
  r <- m * sigma_x / sy
  q <- x^2 / sigma_x^2 - 2 * r * x * y / (sigma_x * sy) + y^2 / sy^2
  list(density = d, density_r = exp(-q / (2 * (1 - r^2))) / (2 * pi * sigma_x * sy * sqrt(1 - r^2)), sigma_y = sy, r = r)
}
