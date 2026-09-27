# SPDX-License-Identifier: AGPL-3.0-or-later
#' Confidence interval for a correlation via Fisher's z
#'
#' tanh(atanh(r) -/+ z_(1 - alpha/2) / sqrt(n - 3)) as cor.test (Hedderich,
#' Sachs & Reynarowych 2023, eqs 6.151-6.154). method = "hotelling" uses
#' Hotelling's (1953) correction z_H = z - (3 z + r) / (4 n) with standard error
#' 1 / sqrt(n - 1) (eq 6.153).
#'
#' @param r Sample correlation in (-1, 1).
#' @param n Number of pairs (> 3).
#' @param conf_level Confidence level.
#' @param method "fisher" or "hotelling".
#' @return Named list: lower, upper, z, se_z.
#' @references Hotelling, H. (1953). JRSS B 15, 193-232.
#' @examples
#' corrci(0.687, 50)[c("lower", "upper")]
#' @export
corrci <- function(r, n, conf_level = 0.95, method = c("fisher", "hotelling")) {
  method <- match.arg(method)
  if (!(r > -1 && r < 1) || n <= 3) stop("need -1 < r < 1 and n > 3", call. = FALSE)
  z <- atanh(r)
  if (method == "fisher") {
    se <- 1 / sqrt(n - 3)
  } else {
    z <- z - (3 * z + r) / (4 * n)
    se <- 1 / sqrt(n - 1)
  }
  q <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(lower = tanh(z - q * se), upper = tanh(z + q * se), z = z, se_z = se)
}

#' Pairs needed for a correlation confidence interval of given limits
#'
#' n = 4 (z_(1 - alpha/2) / (atanh(upper) - atanh(lower)))^2 + 3 (Hedderich,
#' Sachs & Reynarowych 2023, eq 6.155).
#'
#' @param lower,upper Required limits, -1 < lower < upper < 1.
#' @param conf_level Confidence level.
#' @return Named list: n (rounded up), n_exact.
#' @references Hedderich, J., Sachs, L. & Reynarowych, Z. (2023). Applied
#'   Statistics: Methods Using R. Springer.
#' @examples
#' corcin(0.5, 0.8)$n
#' @export
corcin <- function(lower, upper, conf_level = 0.95) {
  if (!(lower > -1 && lower < upper && upper < 1)) stop("need -1 < lower < upper < 1", call. = FALSE)
  ne <- 4 * (stats::qnorm(1 - (1 - conf_level) / 2) / (atanh(upper) - atanh(lower)))^2 + 3
  list(n = ceiling(ne), n_exact = ne)
}

#' Admissible range of a correlation given two others
#'
#' r_ik r_jk -/+ sqrt((1 - r_ik^2) (1 - r_jk^2)), the bounds that keep a 3 x 3
#' correlation matrix positive semi-definite (Hedderich, Sachs & Reynarowych
#' 2023, p. 770).
#'
#' @param r_ik,r_jk Correlations in (-1, 1).
#' @return Named list: lower, upper.
#' @references Hedderich, J., Sachs, L. & Reynarowych, Z. (2023). Applied
#'   Statistics: Methods Using R. Springer.
#' @examples
#' corrng(0.6, 0.9)
#' @export
corrng <- function(r_ik, r_jk) {
  if (abs(r_ik) > 1 || abs(r_jk) > 1) stop("correlations must lie in [-1, 1]", call. = FALSE)
  h <- sqrt((1 - r_ik^2) * (1 - r_jk^2))
  list(lower = r_ik * r_jk - h, upper = r_ik * r_jk + h)
}

#' The (N - 1) chi-square test for a fourfold table
#'
#' (n - 1) (ad - bc)^2 / ((a + b) (c + d) (a + c) (b + d)) on 1 df (Hedderich,
#' Sachs & Reynarowych 2023, eq 7.277).
#'
#' @param table 2 x 2 matrix of counts.
#' @return Named list: statistic, df, p_value.
#' @references Campbell, I. (2007). Statistics in Medicine 26, 3661-3675.
#' @examples
#' chin1(matrix(c(1, 5, 5, 1), 2, byrow = TRUE))$statistic
#' @export
chin1 <- function(table) {
  t <- matrix(as.numeric(table), 2, 2)
  if (any(t < 0)) stop("counts must be non-negative", call. = FALSE)
  den <- prod(rowSums(t)) * prod(colSums(t))
  if (den == 0) stop("a margin is zero", call. = FALSE)
  x <- (sum(t) - 1) * (t[1, 1] * t[2, 2] - t[1, 2] * t[2, 1])^2 / den
  list(statistic = x, df = 1, p_value = stats::pchisq(x, 1, lower.tail = FALSE))
}

#' Pearson and adjusted residuals of a contingency table
#'
#' (n_ij - e_ij) / sqrt(e_ij) and the adjusted residual (n_ij - e_ij) /
#' sqrt(e_ij (1 - n_i. / n) (1 - n_.j / n)) under independence (Hedderich,
#' Sachs & Reynarowych 2023, eqs 7.357, 7.359; chisq.test residuals and stdres).
#'
#' @param table r x c matrix of counts.
#' @return Named list: expected, pearson, adjusted (matrices), statistic, df,
#'   p_value.
#' @references Haberman, S. J. (1973). Biometrics 29, 205-220.
#' @examples
#' ctresid(matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE))$adjusted
#' @export
ctresid <- function(table) {
  t <- as.matrix(table)
  storage.mode(t) <- "double"
  if (nrow(t) < 2 || ncol(t) < 2 || any(t < 0)) {
    stop("need an r x c table (r, c >= 2) of non-negative counts", call. = FALSE)
  }
  rs <- rowSums(t)
  cs <- colSums(t)
  n <- sum(t)
  if (min(rs) == 0 || min(cs) == 0) stop("a margin is zero", call. = FALSE)
  e <- outer(rs, cs) / n
  pr <- (t - e) / sqrt(e)
  ad <- (t - e) / sqrt(e * outer(1 - rs / n, 1 - cs / n))
  x <- sum(pr^2)
  df <- (nrow(t) - 1) * (ncol(t) - 1)
  list(expected = e, pearson = pr, adjusted = ad, statistic = x, df = df,
       p_value = stats::pchisq(x, df, lower.tail = FALSE))
}

#' Exact confidence interval and test for the ratio of two Poisson rates
#'
#' Conditional on m = k1 + k2, k1 is binomial; the Clopper-Pearson limits p_l,
#' p_u for its probability give (n2 p_l / (n1 (1 - p_l)), n2 p_u / (n1 (1 -
#' p_u))) (Hedderich, Sachs & Reynarowych 2023, eq 6.59), and the p-value is the
#' exact binomial test of n1 / (n1 + n2), as poisson.test.
#'
#' @param k1,k2 Event counts.
#' @param n1,n2 Exposures.
#' @param conf_level Confidence level.
#' @return Named list: ratio, lower, upper, p_value.
#' @references Price, R. M. & Bonett, D. G. (2000). Computational Statistics
#'   and Data Analysis 34, 345-356.
#' @examples
#' rrexct(40, 20, 22, 30)[c("lower", "upper")]
#' @export
rrexct <- function(k1, n1, k2, n2, conf_level = 0.95) {
  if (k1 < 0 || k2 < 0 || k1 + k2 == 0 || n1 <= 0 || n2 <= 0) {
    stop("need non-negative counts with k1 + k2 > 0 and positive exposures", call. = FALSE)
  }
  m <- k1 + k2
  a <- (1 - conf_level) / 2
  pl <- if (k1 == 0) 0 else stats::qbeta(a, k1, m - k1 + 1)
  pu <- if (k1 == m) 1 else stats::qbeta(1 - a, k1 + 1, m - k1)
  p0 <- n1 / (n1 + n2)
  d <- stats::dbinom(k1, m, p0) * (1 + 1e-7)
  dd <- stats::dbinom(0:m, m, p0)
  list(ratio = if (k2 == 0) Inf else (k1 / n1) / (k2 / n2),
       lower = n2 * pl / (n1 * (1 - pl)),
       upper = if (pu == 1) Inf else n2 * pu / (n1 * (1 - pu)),
       p_value = min(1, sum(dd[dd <= d])))
}

#' Gamma-conjugate posterior for an exponential or Poisson rate
#'
#' Exponential data: shape k0 + n, rate lambda0 + sum(x); Poisson counts:
#' shape k0 + sum(x), rate lambda0 + n (Hedderich, Sachs & Reynarowych 2023,
#' Overview 30, whose Poisson row prints lambda0 x).
#'
#' @param x Waiting times or counts.
#' @param k0,lambda0 Prior shape and rate.
#' @param likelihood "exponential" or "poisson".
#' @param cred_level Level of the equal-tailed credible interval.
#' @return Named list: shape, rate, mean, lower, upper.
#' @references Gelman, A. et al. (2013). Bayesian Data Analysis, Sec. 2.6.
#' @examples
#' gmconj(c(0.8, 1.9, 0.4, 2.7, 1.1), 2, 1)$mean
#' @export
gmconj <- function(x, k0, lambda0, likelihood = c("exponential", "poisson"), cred_level = 0.95) {
  likelihood <- match.arg(likelihood)
  x <- as.numeric(x)
  if (!length(x) || k0 <= 0 || lambda0 <= 0 || min(x) < 0) {
    stop("need non-empty non-negative data and positive prior parameters", call. = FALSE)
  }
  if (likelihood == "exponential") {
    k <- k0 + length(x)
    lam <- lambda0 + sum(x)
  } else {
    k <- k0 + sum(x)
    lam <- lambda0 + length(x)
  }
  a <- (1 - cred_level) / 2
  list(shape = k, rate = lam, mean = k / lam,
       lower = stats::qgamma(a, k, lam), upper = stats::qgamma(1 - a, k, lam))
}

#' Prevalence from inverse binomial sampling
#'
#' Haldane's estimate (k - 1) / (k + x - 1) and the limits (qbeta(alpha/2, k, x
#' + 1), qbeta(1 - alpha/2, k, x)) (Hedderich, Sachs & Reynarowych 2023, eqs
#' 6.30-6.31, as its R code).
#'
#' @param k Cases observed (>= 2).
#' @param x Non-cases before the k-th case.
#' @param conf_level Confidence level.
#' @return Named list: estimate, lower, upper.
#' @references Haldane, J. B. S. (1945). Biometrika 33, 222-225.
#' @examples
#' invbpr(20, 100)
#' @export
invbpr <- function(k, x, conf_level = 0.95) {
  if (k < 2 || x < 0) stop("need k >= 2 and x >= 0", call. = FALSE)
  a <- (1 - conf_level) / 2
  list(estimate = (k - 1) / (k + x - 1), lower = stats::qbeta(a, k, x + 1),
       upper = if (x == 0) 1 else stats::qbeta(1 - a, k, x))
}

#' Normal approximation to the binomial distribution function
#'
#' P(X <= x) ~ pnorm((x + 0.5 - n p) / sqrt(n p (1 - p))) (Hedderich, Sachs &
#' Reynarowych 2023, eq 5.54); correct = FALSE drops the 0.5.
#'
#' @param x,n,p Quantile, trials and success probability.
#' @param correct Apply the continuity correction.
#' @return Named list: z, approx, exact.
#' @references Hedderich, J., Sachs, L. & Reynarowych, Z. (2023). Applied
#'   Statistics: Methods Using R. Springer.
#' @examples
#' bnappx(3, 10, 0.4)
#' @export
bnappx <- function(x, n, p, correct = TRUE) {
  if (n < 1 || !(p > 0 && p < 1)) stop("need n >= 1 and 0 < p < 1", call. = FALSE)
  z <- (x + (if (correct) 0.5 else 0) - n * p) / sqrt(n * p * (1 - p))
  list(z = z, approx = stats::pnorm(z), exact = stats::pbinom(x, n, p))
}

#' Conditional distribution in a bivariate normal
#'
#' X | Y = y ~ N(mu_x + rho sd_x (y - mu_y) / sd_y, sd_x sqrt(1 - rho^2)) and
#' symmetrically for Y | X (Hedderich, Sachs & Reynarowych 2023, p. 324).
#'
#' @param value Observed value of the conditioning variable.
#' @param mu_x,mu_y,sd_x,sd_y Means and standard deviations.
#' @param rho Correlation.
#' @param given "y" or "x".
#' @return Named list: mean, sd.
#' @references Hedderich, J., Sachs, L. & Reynarowych, Z. (2023). Applied
#'   Statistics: Methods Using R. Springer.
#' @examples
#' bvncnd(80, 170, 70, 10, 12, 0.6)
#' @export
bvncnd <- function(value, mu_x, mu_y, sd_x, sd_y, rho, given = c("y", "x")) {
  given <- match.arg(given)
  if (sd_x <= 0 || sd_y <= 0 || abs(rho) > 1) {
    stop("need positive standard deviations and -1 <= rho <= 1", call. = FALSE)
  }
  if (given == "y") {
    list(mean = mu_x + rho * sd_x * (value - mu_y) / sd_y, sd = sd_x * sqrt(1 - rho^2))
  } else {
    list(mean = mu_y + rho * sd_y * (value - mu_x) / sd_x, sd = sd_y * sqrt(1 - rho^2))
  }
}
