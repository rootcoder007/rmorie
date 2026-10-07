# SPDX-License-Identifier: AGPL-3.0-or-later
# Sensitivity analysis for matched observational studies (Rosenbaum).
# Identical to the Python arm morie.fn.obssens.

#' Observational-study sensitivity: amplification, U-statistic bounds, crosscut test and design sensitivity
#'
#' \code{GammaFromLambdaDelta}: \code{Gamma = (Lambda Delta + 1) / (Lambda + Delta)}
#' and the Wolfe-family bounds \code{1 / (1 + Delta)} and \code{Delta / (1 + Delta)}
#' on \code{Pr(W > 0 | C, |W| = w)} (Design of Observational Studies, eqs.
#' 3.28-3.29). \code{AmplifyGamma}: \code{Delta = (Gamma Lambda - 1) / (Lambda - Gamma)}
#' for each \code{Lambda > Gamma}. \code{UStatisticSensitivity}: normal upper
#' bound on the P-value of Rosenbaum's U-statistic \code{(m, m1, m2)} with scores
#' \code{sum_l C(q - 1, l - 1) C(I - q, m - l) / C(I, m)} (exact for
#' \code{I <= 50}) or their limit, as DOS2::senU. \code{CrosscutTest}: corner
#' table beyond the \code{ct} quantiles and the Fisher noncentral
#' hypergeometric upper tail with odds \code{Gamma} (as DOS2::crosscut).
#' \code{CrosscutDesignSensitivity}: \code{(p_LL p_HH) / (p_LH p_HL)} for
#' bivariate Normal dose and response, Sheppard's integral by tanh-sinh.
#'
#' @param lam Lambda value(s).
#' @param delta Delta value(s).
#' @param gamma Sensitivity parameter Gamma.
#' @param d Matched-pair differences.
#' @param m,m1,m2 U-statistic subset size and rank window.
#' @param exact Exact scores (default when there are at most 50 pairs).
#' @param alternative "greater", "less" or "twosided".
#' @param x,y Doses and responses.
#' @param ct Corner quantile (at most 1/2).
#' @param rho Correlation(s).
#' @param eta Corner quantile(s).
#' @return A list or numeric vector.
#' @references Rosenbaum, P. R. (2020). Design of Observational Studies, 2nd
#'   ed. Rosenbaum, P. R. and Silber, J. H. (2009). JASA 104, 1398-1405.
#'   Rosenbaum, P. R. (2011). Biometrics 67, 1017-1027. Rosenbaum, P. R.
#'   (2016). Biometrics 72, 175-183.
#' @examples
#' GammaFromLambdaDelta(2, 3)$gamma
#' AmplifyGamma(1.4, c(2, 3))
#' CrosscutDesignSensitivity(c(0.3, 0.3), c(0.5, 0.125))
#' @export
GammaFromLambdaDelta <- function(lam, delta) {
  lam <- as.numeric(lam)
  delta <- rep_len(as.numeric(delta), length(lam))
  list(gamma = (lam * delta + 1) / (lam + delta), abz_bounds = lapply(delta, function(b) c(1 / (1 + b), b / (1 + b))))
}

#' @rdname GammaFromLambdaDelta
#' @export
AmplifyGamma <- function(gamma, lam) {
  lam <- as.numeric(lam)
  ifelse(lam > gamma, (gamma * lam - 1) / (lam - gamma), NaN)
}

.os_choose <- function(n, k) {
  if (k < 0) return(0 * n)
  out <- 1
  for (j in seq_len(k) - 1) out <- out * (n - j) / (j + 1)
  out
}

#' @rdname GammaFromLambdaDelta
#' @export
UStatisticSensitivity <- function(d, gamma = 1, m = 2, m1 = 2, m2 = 2, exact = NULL, alternative = "greater") {
  .morie_arg(d, "n")
  I <- length(d)
  if (is.null(exact)) exact <- I <= 50
  pr <- gamma / (1 + gamma)
  dev <- function(dd) {
    ad <- abs(dd)
    q <- rank(ad)
    sc <- numeric(I)
    for (l in m1:m2) {
      sc <- sc + if (exact) .os_choose(q - 1, l - 1) / .os_choose(I, m) * .os_choose(I - q, m - l) else
        l * .os_choose(m, l) * (q / I)^(l - 1) * (1 - q / I)^(m - l)
    }
    sc <- sc * (ad > 0)
    ts <- sum(sc[dd > 0])
    c((ts - sum(pr * sc)) / sqrt(sum(sc * sc) * pr * (1 - pr)), ts)
  }
  if (alternative == "less") {
    z <- dev(-d)
    p <- 1 - stats::pnorm(z[1])
  } else if (alternative == "twosided") {
    zl <- dev(-d)
    z <- dev(d)
    p <- min(1, 2 * min(1 - stats::pnorm(zl[1]), 1 - stats::pnorm(z[1])))
  } else {
    z <- dev(d)
    p <- 1 - stats::pnorm(z[1])
  }
  list(p_value = p, deviate = z[1], statistic = z[2])
}

.os_q7 <- function(x, prob) {
  s <- sort(as.numeric(x))
  h <- (length(s) - 1) * prob
  lo <- floor(h)
  hi <- min(lo + 1, length(s) - 1)
  w <- h - lo
  if (w > 0) (1 - w) * s[lo + 1] + w * s[hi + 1] else s[lo + 1]
}

#' @rdname GammaFromLambdaDelta
#' @export
CrosscutTest <- function(x, y, ct = 0.25, gamma = 1) {
  qx1 <- .os_q7(x, ct)
  qx2 <- .os_q7(x, 1 - ct)
  qy1 <- .os_q7(y, ct)
  qy2 <- .os_q7(y, 1 - ct)
  use <- (x <= qx1 | x >= qx2) & (y <= qy1 | y >= qy2)
  tb <- matrix(0, 2, 2)
  for (i in which(use)) {
    r <- if (x[i] >= qx2) 2 else 1
    cc <- if (y[i] >= qy2) 2 else 1
    tb[r, cc] <- tb[r, cc] + 1
  }
  m1 <- tb[1, 1] + tb[1, 2]
  m2 <- tb[2, 1] + tb[2, 2]
  n <- tb[1, 1] + tb[2, 1]
  xs <- max(0, n - m2):min(n, m1)
  lw <- lgamma(m1 + 1) - lgamma(xs + 1) - lgamma(m1 - xs + 1) + lgamma(m2 + 1) - lgamma(n - xs + 1) -
    lgamma(m2 - n + xs + 1) + xs * log(gamma)
  w <- exp(lw - max(lw))
  list(table = tb, p_value = sum(w[xs >= tb[1, 1]]) / sum(w), quantiles = c(qx1, qx2, qy1, qy2))
}

.os_tanh_sinh <- function(g, a, b, h = 1 / 64, tmax = 3.5) {
  n <- round(tmax / h)
  cc <- (a + b) / 2
  r <- (b - a) / 2
  s <- 0
  for (k in -n:n) {
    t <- k * h
    v <- pi / 2 * sinh(t)
    x <- tanh(v)
    if (abs(x) >= 1) next
    s <- s + g(cc + r * x) * pi / 2 * cosh(t) / cosh(v)^2
  }
  s * h * r
}

.os_bvn <- function(h, k, rho) {
  if (rho == 0) return(stats::pnorm(h) * stats::pnorm(k))
  g <- function(th) exp(-(h * h + k * k - 2 * h * k * sin(th)) / (2 * cos(th)^2))
  stats::pnorm(h) * stats::pnorm(k) + .os_tanh_sinh(g, 0, asin(rho)) / (2 * pi)
}

#' @rdname GammaFromLambdaDelta
#' @export
CrosscutDesignSensitivity <- function(rho, eta) {
  vapply(seq_along(rho), function(i) {
    z <- .morie_normal_quantile(eta[i])
    (.os_bvn(z, z, rho[i]) / (eta[i] - .os_bvn(z, -z, rho[i])))^2
  }, 0)
}
