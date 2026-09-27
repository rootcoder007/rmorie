# SPDX-License-Identifier: AGPL-3.0-or-later
#' Inverse of a compound-symmetric matrix
#'
#' Graybill (1983) Theorem 8.3.4, Schabenberger & Gotway (2005) Theorem 1.1:
#' the k x k matrix C = (a - b) I + b J is invertible if and only if a != b
#' and a != -(k - 1) b, and then C^-1 = (I - b / (a + (k - 1) b) J) / (a - b).
#'
#' @param k Dimension, at least 1.
#' @param a Diagonal element.
#' @param b Off-diagonal element.
#' @return Named list: exists, k, a, b and, when the inverse exists,
#'   inverse, diag, offdiag and sum_inverse (1' C^-1 1 = k / (a + (k - 1) b)).
#' @references Graybill, F. A. (1983). Matrices with Applications in
#'   Statistics, 2nd ed., Theorem 8.3.4, p. 190. Schabenberger & Gotway
#'   (2005), Theorem 1.1, p. 34.
#' @examples
#' csinv(4, 3, 0.7)$inverse
#' @export
csinv <- function(k, a, b) {
  k <- as.integer(k)
  if (k < 1L) stop("`k` must be at least 1", call. = FALSE)
  exists <- a != b && a != -(k - 1) * b
  out <- list(exists = exists, k = k, a = a, b = b)
  if (exists) {
    d <- a + (k - 1) * b
    off <- -b / ((a - b) * d)
    dg <- 1 / (a - b) + off
    m <- matrix(off, k, k)
    diag(m) <- dg
    out <- c(out, list(inverse = m, diag = dg, offdiag = off, sum_inverse = k / d))
  }
  out
}

#' Variance components of a two-stage nested design
#'
#' The model of Schabenberger & Gotway (2005) Example 1.1, Y_ijk = mu + tau_i
#' + e_ij + eps_ijk, with fixed groups, random units nested in groups and
#' sub-sampling errors. Two sub-samples of one unit share e_ij, so their
#' covariance is Var(e_ij). The components are the ANOVA estimators of the
#' balanced nested analysis of variance, sigma2_error = MS_E and
#' sigma2_unit = (MS_U(G) - MS_E) / n, which are the REML estimates when
#' positive.
#'
#' @param y Numeric responses.
#' @param group Fixed-effect group label of each response.
#' @param unit Random unit label, nested in `group`; every unit has the same
#'   number of sub-samples.
#' @return Named list: ms_unit, df_unit, ms_error, df_error, sigma2_unit,
#'   sigma2_error, within_unit_cov, icc, n_sub, n_units.
#' @references Schabenberger & Gotway (2005), Example 1.1, p. 2.
#' @examples
#' nestvc(c(1, 1.2, 2, 2.1, 3, 3.3, 4, 4.4), rep(1:2, each = 4), rep(1:4, each = 2))
#' @export
nestvc <- function(y, group, unit) {
  y <- as.numeric(y)
  if (!length(y) || length(group) != length(y) || length(unit) != length(y)) {
    stop("`y`, `group` and `unit` must be non-empty and the same length", call. = FALSE)
  }
  key <- paste(group, unit, sep = "\r")
  if (any(tapply(as.character(group), as.character(unit), function(g) length(unique(g))) > 1)) {
    stop("units must be nested in groups", call. = FALSE)
  }
  sz <- unique(as.vector(table(key)))
  if (length(sz) != 1L) stop("every unit must have the same number of sub-samples", call. = FALSE)
  n <- sz
  if (n < 2L) stop("each unit needs at least two sub-samples", call. = FALSE)
  cm <- tapply(y, key, mean)
  cg <- tapply(as.character(group), key, `[`, 1L)
  ss_unit <- 0
  df_unit <- 0L
  for (g in unique(cg)) {
    m <- cm[cg == g]
    ss_unit <- ss_unit + n * sum((m - mean(m))^2)
    df_unit <- df_unit + length(m) - 1L
  }
  if (df_unit < 1L) stop("at least one group needs two or more units", call. = FALSE)
  ss_err <- sum((y - cm[key])^2)
  df_err <- length(y) - length(cm)
  ms_unit <- ss_unit / df_unit
  ms_err <- ss_err / df_err
  s2u <- (ms_unit - ms_err) / n
  cv <- max(s2u, 0)
  list(ms_unit = ms_unit, df_unit = df_unit, ms_error = ms_err, df_error = df_err,
       sigma2_unit = s2u, sigma2_error = ms_err, within_unit_cov = cv,
       icc = if (cv + ms_err > 0) cv / (cv + ms_err) else NaN,
       n_sub = n, n_units = length(cm))
}

#' Bivariate Cauchy density
#'
#' f(z1, z2) = c / (2 pi) (c^2 + z1^2 + z2^2)^(-3/2), the bivariate t with one
#' degree of freedom and scale c^2 I (Mardia 1970, p. 86). Both marginals are
#' Cauchy, yet E(Z2 | Z1) does not exist (Schabenberger & Gotway 2005, p. 293).
#'
#' @param z1,z2 Numeric evaluation points of equal length.
#' @param c Scale, positive.
#' @return Named list: density, c.
#' @references Mardia, K. V. (1970). Families of Bivariate Distributions.
#'   Hafner, p. 86. Schabenberger & Gotway (2005), Sec. 5.8, p. 293.
#' @examples
#' bvcchy(c(0, 0.5), c(0, -1.2), 1.5)
#' @export
bvcchy <- function(z1, z2, c = 1) {
  if (!(c > 0)) stop("`c` must be positive", call. = FALSE)
  if (length(z1) != length(z2)) stop("`z1` and `z2` must have the same length", call. = FALSE)
  list(density = c / (2 * pi) * (c^2 + z1^2 + z2^2)^(-1.5), c = c)
}

#' Plackett's bivariate distribution
#'
#' Plackett's (1965) F12 for marginal values u = F1 and v = F2 solves
#' psi = F12 (1 - F1 - F2 + F12) / ((F1 - F12) (F2 - F12)):
#' F12 = (S - sqrt(S^2 - 4 psi (psi - 1) u v)) / (2 (psi - 1)), S = 1 + (psi - 1)(u + v),
#' and F12 = u v at psi = 1. Mardia's (1967) correlation is
#' ((psi - 1)(1 + psi) - 2 psi log(psi)) / (1 - psi)^2 (Schabenberger & Gotway
#' 2005, p. 293, whose denominator for psi misprints (F1 - F12)^2).
#'
#' @param u,v Marginal distribution values between 0 and 1 inclusive, equal length.
#' @param psi Association parameter, positive.
#' @return Named list: F12, rho, psi.
#' @references Plackett, R. L. (1965). JASA 60, 516-522. Mardia, K. V.
#'   (1967). Biometrika 54, 235-249. Schabenberger & Gotway (2005), p. 293.
#' @examples
#' plackt(c(0.2, 0.5), c(0.7, 0.5), 3.5)
#' @export
plackt <- function(u, v, psi) {
  if (!(psi > 0)) stop("`psi` must be positive", call. = FALSE)
  if (length(u) != length(v) || any(c(u, v) < 0 | c(u, v) > 1)) {
    stop("`u` and `v` must be the same length with values in [0, 1]", call. = FALSE)
  }
  if (psi == 1) {
    f <- u * v
    rho <- 0
  } else {
    s <- 1 + (psi - 1) * (u + v)
    f <- (s - sqrt(s^2 - 4 * psi * (psi - 1) * u * v)) / (2 * (psi - 1))
    rho <- ((psi - 1) * (1 + psi) - 2 * psi * log(psi)) / (1 - psi)^2
  }
  list(F12 = f, rho = rho, psi = psi)
}

#' Lower bound on the equicorrelation of exchangeable binary data
#'
#' For n exchangeable binary variables with mean mu, rho = (E(S(S - 1)) /
#' (n (n - 1)) - mu^2) / (mu (1 - mu)) with S their sum, so the smallest rho
#' puts all the mass of S on the two integers either side of n mu
#' (Gilliland & Schabenberger 2001; Schabenberger & Gotway 2005, p. 356). The
#' bound is -1/(n - 1) exactly when n mu is an integer.
#'
#' @param mu Common success probability in (0, 1).
#' @param n Number of variables, at least 2.
#' @return Named list: rho_lower, rho_min_any, support, probabilities, mu, n.
#' @references Gilliland, D. & Schabenberger, O. (2001). Limits on pairwise
#'   association for equi-correlated binary variables. Journal of Applied
#'   Statistical Science 10, 279-285. Schabenberger & Gotway (2005), p. 356.
#' @examples
#' rhobin(0.3, 5)$rho_lower
#' @export
rhobin <- function(mu, n) {
  n <- as.integer(n)
  if (n < 2L) stop("`n` must be at least 2", call. = FALSE)
  if (!(mu > 0 && mu < 1)) stop("`mu` must lie in (0, 1)", call. = FALSE)
  m <- n * mu
  lo <- floor(m)
  if (m == lo) {
    sup <- lo
    pr <- 1
  } else {
    sup <- c(lo, lo + 1)
    pr <- c(1 - (m - lo), m - lo)
  }
  e2 <- sum(pr * sup * (sup - 1)) / (n * (n - 1))
  list(rho_lower = (e2 - mu^2) / (mu * (1 - mu)), rho_min_any = -1 / (n - 1),
       support = as.integer(sup), probabilities = pr, mu = mu, n = n)
}

#' Multivariate gamma random field from a shared component
#'
#' Schabenberger & Gotway (2005) Problem 2.3: Z(s_i) = X_0 + X_i with
#' independent X_i ~ Gamma(alpha_i, beta) (mean alpha_i beta). Then
#' Cov(Z(s_i), Z(s_j)) = alpha_0 beta^2 for i != j, Var(Z(s_i)) =
#' (alpha_0 + alpha_i) beta^2, Z(s_i) ~ Gamma(alpha_0 + alpha_i, beta), and the
#' field is second-order stationary exactly when alpha_1 = ... = alpha_n.
#'
#' @param alpha Shapes (alpha_0, alpha_1, ..., alpha_n), positive.
#' @param beta Common scale, positive.
#' @return Named list: mean, cov, corr, shape, scale, stationary.
#' @references Schabenberger & Gotway (2005), Problem 2.3, p. 79.
#' @examples
#' mgamrf(c(2, 1, 1, 3), 0.5)$corr
#' @export
mgamrf <- function(alpha, beta) {
  if (length(alpha) < 2L || any(!(alpha > 0)) || !(beta > 0)) {
    stop("`alpha` needs alpha_0 and at least one alpha_i, all positive; `beta` positive",
         call. = FALSE)
  }
  a0 <- alpha[1L]
  ai <- alpha[-1L]
  cv <- matrix(a0 * beta^2, length(ai), length(ai))
  diag(cv) <- (a0 + ai) * beta^2
  cr <- a0 / sqrt(outer(a0 + ai, a0 + ai))
  diag(cr) <- 1
  list(mean = (a0 + ai) * beta, cov = cv, corr = cr, shape = a0 + ai, scale = beta,
       stationary = length(unique(ai)) == 1L)
}
