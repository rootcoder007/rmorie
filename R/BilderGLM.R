# SPDX-License-Identifier: AGPL-3.0-or-later
.bl_fit <- function(y, X, family, trials, offset) {
  fam <- if (family == "binomial") stats::binomial() else stats::poisson()
  yy <- if (family == "binomial") y / trials else y
  w <- if (family == "binomial") trials else rep(1, length(y))
  f <- stats::glm.fit(X, yy, weights = w, offset = offset, family = fam,
                      control = stats::glm.control(epsilon = 1e-14, maxit = 100))
  mu <- if (family == "binomial") trials * f$fitted.values else f$fitted.values
  ll <- if (family == "binomial") sum(y * log(f$fitted.values) + (trials - y) * log(1 - f$fitted.values)) else sum(y * log(mu) - mu)
  list(beta = unname(f$coefficients), mu = mu, ll = ll, fit = f)
}

#' Profile likelihood interval for a GLM coefficient
#'
#' Limits where -2 log(L(profile) / L(MLE)) equals the chi-squared quantile,
#' found by bracketing from the Wald interval and bisection.
#'
#' @param y Successes or counts.
#' @param X Design matrix (with any intercept column).
#' @param j Coefficient index (0-based, as in the Python arm).
#' @param family "binomial" or "poisson".
#' @param trials Binomial trials.
#' @param alpha Level.
#' @return list(estimate, se, ci, wald_ci, beta).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eqs (2.12)-(2.13).
#' @examples
#' GlmProfCI(c(0, 0, 1, 0, 1, 1, 1, 0, 1, 1), cbind(1, 0:9), 1)$ci
#' @export
GlmProfCI <- function(y, X, j, family = c("binomial", "poisson"), trials = NULL, alpha = 0.05) {
  family <- match.arg(family)
  X <- as.matrix(X)
  if (is.null(trials)) trials <- rep(1, length(y))
  jj <- j + 1
  full <- .bl_fit(y, X, family, trials, rep(0, length(y)))
  se <- sqrt(diag(solve(crossprod(X, full$fit$weights * X))))[jj]
  crit <- qchisq(1 - alpha, 1)
  lr <- function(b) {
    Xo <- X[, -jj, drop = FALSE]
    off <- X[, jj] * b
    llb <- if (ncol(Xo)) .bl_fit(y, Xo, family, trials, off)$ll else {
      eta <- off
      if (family == "binomial") sum(y * stats::plogis(eta, log.p = TRUE) + (trials - y) * stats::plogis(-eta, log.p = TRUE)) else sum(y * eta - exp(eta))
    }
    2 * (full$ll - llb) - crit
  }
  b <- full$beta[jj]
  lims <- vapply(c(-1, 1), function(s) {
    step <- se * sqrt(crit)
    k <- 0
    while (lr(b + s * step) < 0) {
      step <- 2 * step
      k <- k + 1
      if (k > 60) stop("profile interval is unbounded on one side", call. = FALSE)
    }
    lo <- min(b, b + s * step)
    hi <- max(b, b + s * step)
    for (it in 1:200) {
      mid <- (lo + hi) / 2
      inside <- lr(mid) < 0
      if (s < 0) { if (inside) hi <- mid else lo <- mid } else { if (inside) lo <- mid else hi <- mid }
      if (hi - lo < 1e-10 * (1 + abs(mid))) break
    }
    (lo + hi) / 2
  }, numeric(1))
  z <- sqrt(crit)
  list(estimate = b, se = se, ci = lims, wald_ci = b + c(-1, 1) * z * se, beta = full$beta)
}

#' Pearson and standardized Pearson residuals for binomial and Poisson GLMs
#'
#' @param y Successes or counts.
#' @param X Design matrix.
#' @param family "binomial" or "poisson".
#' @param trials Binomial trials.
#' @return list(fitted, pearson, standardized, hat, beta, deviance, pearson_chisq, df).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Sec 5.2.1.
#' @examples
#' GlmStdRes(c(2, 3, 6, 7, 8, 9, 10, 12, 15), cbind(1, 0:8), family = "poisson")$standardized
#' @export
GlmStdRes <- function(y, X, family = c("binomial", "poisson"), trials = NULL) {
  family <- match.arg(family)
  X <- as.matrix(X)
  if (is.null(trials)) trials <- rep(1, length(y))
  f <- .bl_fit(y, X, family, trials, rep(0, length(y)))
  v <- if (family == "binomial") f$mu * (1 - f$mu / trials) else f$mu
  inv <- solve(crossprod(X, v * X))
  h <- v * rowSums((X %*% inv) * X)
  e <- (y - f$mu) / sqrt(v)
  xlogx <- function(a, b) ifelse(a > 0, a * log(a / b), 0)
  dev <- if (family == "binomial") 2 * sum(xlogx(y, f$mu) + xlogx(trials - y, trials - f$mu)) else 2 * sum(xlogx(y, f$mu) - (y - f$mu))
  list(fitted = f$mu, pearson = e, standardized = e / sqrt(1 - h), hat = h, beta = f$beta,
       deviance = dev, pearson_chisq = sum(e^2), df = length(y) - ncol(X))
}

#' Odds ratio with an interaction term
#'
#' @param b2,b3 Main-effect and interaction coefficients.
#' @param x1 Level of the interacting variable.
#' @param var2,var3,cov23 Variances and covariance.
#' @param c Change in x2.
#' @param alpha Level.
#' @return list(odds_ratio, ci, se_log).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eqs (2.18), (3.9).
#' @examples
#' OrInt(0.5, 0.1, 2, 0.01, 0.001, -0.002)$odds_ratio
#' @export
OrInt <- function(b2, b3, x1, var2, var3, cov23, c = 1, alpha = 0.05) {
  v <- var2 + x1^2 * var3 + 2 * x1 * cov23
  if (v < 0) stop("variance of b2 + b3 x1 is negative", call. = FALSE)
  est <- c * (b2 + b3 * x1)
  h <- abs(c) * qnorm(1 - alpha / 2) * sqrt(v)
  list(odds_ratio = exp(est), ci = exp(est + c(-1, 1) * h), se_log = abs(c) * sqrt(v))
}

#' Inverse-prediction interval in a logistic model
#'
#' @param b0,b1 Coefficients.
#' @param var0,var1,cov01 Variances and covariance.
#' @param pi Target probability.
#' @param alpha Level.
#' @return list(estimate, ci, bounded).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eq (2.23).
#' @examples
#' InvPredCI(-4, 2, 0.25, 0.04, -0.09)$ci
#' @export
InvPredCI <- function(b0, b1, var0, var1, cov01, pi = 0.5, alpha = 0.05) {
  if (pi <= 0 || pi >= 1 || b1 == 0) stop("need 0 < pi < 1 and b1 != 0", call. = FALSE)
  L <- stats::qlogis(pi)
  z2 <- qnorm(1 - alpha / 2)^2
  a <- b1^2 - z2 * var1
  b <- 2 * b1 * (b0 - L) - 2 * z2 * cov01
  cc <- (b0 - L)^2 - z2 * var0
  disc <- b^2 - 4 * a * cc
  ci <- if (a > 0 && disc > 0) sort((-b + c(-1, 1) * sqrt(disc)) / (2 * a)) else NULL
  list(estimate = (L - b0) / b1, ci = ci, bounded = !is.null(ci))
}

#' Exact Poisson interval
#'
#' @param total Total count.
#' @param n Number of observations or exposure.
#' @param alpha Level.
#' @return list(estimate, ci).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Sec 4.1.
#' @examples
#' PoisExactCI(7, 3)$ci
#' @export
PoisExactCI <- function(total, n = 1, alpha = 0.05) {
  if (total < 0 || total != round(total) || n <= 0) stop("total must be a non-negative integer and n > 0", call. = FALSE)
  lo <- if (total == 0) 0 else qchisq(alpha / 2, 2 * total) / (2 * n)
  list(estimate = total / n, ci = c(lo, qchisq(1 - alpha / 2, 2 * (total + 1)) / (2 * n)))
}

#' Prevalence with a misclassifying test
#'
#' @param w Positive results.
#' @param n Sample size.
#' @param se,sp Sensitivity and specificity.
#' @param alpha Level.
#' @return list(estimate, apparent, ci, se).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eq (6.2).
#' @examples
#' RoganGladen(30, 200, 0.9, 0.95)$estimate
#' @export
RoganGladen <- function(w, n, se, sp, alpha = 0.05) {
  if (n <= 0 || w < 0 || w > n || se + sp <= 1) stop("need 0 <= w <= n, n > 0 and Se + Sp > 1", call. = FALSE)
  p <- w / n
  j <- se + sp - 1
  raw <- (p + sp - 1) / j
  s <- sqrt(p * (1 - p) / n) / j
  cl <- function(v) min(1, max(0, v))
  list(estimate = cl(raw), apparent = p, ci = c(cl(raw - qnorm(1 - alpha / 2) * s), cl(raw + qnorm(1 - alpha / 2) * s)), se = s)
}
