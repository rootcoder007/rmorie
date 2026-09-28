#' Lagrange multiplier tests for spatial dependence after OLS
#'
#' With OLS residuals e, \eqn{\sigma^2 = e'e/n}, \eqn{T = tr(W'W + WW)} and
#' \eqn{J = ((W\hat y)' M (W\hat y) + T\sigma^2)/(n\sigma^2)}: RSerr
#' \eqn{(e'We/\sigma^2)^2/T}, RSlag \eqn{(e'Wy/\sigma^2)^2/(nJ)}, the robust
#' adjRSerr and adjRSlag (Anselin, Bera, Florax and Yoon 1996) and SARMA
#' (adjRSlag + RSerr, 2 df), each against chi-square (NaN when a
#' denominator vanishes, e.g. W = cI); exactly
#' \code{spdep::lm.RStests} with \code{test = "all"}.
#'
#' @param y Response.
#' @param X Design matrix including any intercept column.
#' @param W Spatial weights matrix.
#' @return Named list (RSerr, RSlag, adjRSerr, adjRSlag, SARMA) of lists
#'   with \code{statistic}, \code{df}, \code{p_value}.
#' @references Anselin, L. (1988). Spatial Econometrics: Methods and Models.
#'   Kluwer, Dordrecht.
#'
#'   Anselin, L., Bera, A. K., Florax, R. and Yoon, M. J. (1996). Simple
#'   diagnostic tests for spatial dependence. Regional Science and Urban
#'   Economics 26, 77-104.
#' @examples
#' W <- 1 * (abs(outer(1:8, 1:8, "-")) == 1)
#' W <- W / rowSums(W)
#' X <- cbind(1, c(.1, .6, .2, .9, .3, .5, .8, .4))
#' LMSpatialTests(c(1, 2.2, 1.4, 3.1, .9, 2, 2.6, 1.1), X, W)$RSerr$statistic
#' @export
LMSpatialTests <- function(y, X, W) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  XtXi <- solve(crossprod(X))
  yhat <- as.vector(X %*% XtXi %*% crossprod(X, y))
  u <- y - yhat
  s2 <- sum(u^2) / n
  TrW <- sum(W * W + W * t(W))
  Wu <- as.vector(W %*% u)
  Wy <- as.vector(W %*% y)
  Wyh <- as.vector(W %*% yhat)
  XtWyh <- crossprod(X, Wyh)
  J <- (sum(Wyh^2) - as.numeric(t(XtWyh) %*% XtXi %*% XtWyh) + TrW * s2) / (n * s2)
  dutWu <- sum(u * Wu) / s2
  dutWy <- sum(u * Wy) / s2
  nJ <- n * J
  # undefined when W X beta lies in the span of X (e.g. W = c I): nJ = T
  ratio <- function(num, den) if (abs(den) > 1e-12 * max(abs(nJ), abs(TrW), 1e-300)) num / den else NaN
  st <- list(RSerr = c(ratio(dutWu^2, TrW), 1), RSlag = c(ratio(dutWy^2, nJ), 1),
             adjRSerr = c(ratio((dutWu - TrW / nJ * dutWy)^2, TrW * (1 - TrW / nJ)), 1),
             adjRSlag = c(ratio((dutWy - dutWu)^2, nJ - TrW), 1))
  st$SARMA <- c(st$adjRSlag[1] + st$RSerr[1], 2)
  lapply(st, function(v) list(statistic = v[1], df = v[2], p_value = stats::pchisq(v[1], v[2], lower.tail = FALSE)))
}
#' Local Getis-Ord Gi and Gi* statistics
#'
#' z-values \eqn{(\sum_j w_{ij} x_j - W_i \bar x) / (s \sqrt{(n S_{1i} -
#' W_i^2)/(n-1)})} for Gi* (self-weights on the diagonal of W; population
#' variance over all units) and, excluding unit i, with
#' \eqn{((n-1) S_{1i} - W_i^2)/(n-2)} for Gi; exactly \code{spdep::localG}
#' (\code{include.self} for Gi*).
#'
#' @param y Values.
#' @param W Spatial weights matrix.
#' @param star Gi* (default) or Gi.
#' @return List with \code{z}, \code{p_values}, \code{statistic} (largest
#'   absolute z), \code{star}.
#' @references Getis, A. and Ord, J. K. (1992). The analysis of spatial
#'   association by use of distance statistics. Geographical Analysis 24,
#'   189-206.
#'
#'   Ord, J. K. and Getis, A. (1995). Local spatial autocorrelation
#'   statistics: distributional issues and an application. Geographical
#'   Analysis 27, 286-306.
#' @examples
#' W <- matrix(c(1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1), 4)
#' LocalGetisOrd(c(1, 2, 4, 8), W)$z
#' @export
LocalGetisOrd <- function(y, W, star = TRUE) {
  x <- as.numeric(y)
  W <- as.matrix(W)
  n <- length(x)
  if (!identical(dim(W), c(n, n))) stop("W must be n x n with n = length(y)")
  if (n < 3) stop("need at least three units")
  if (!star) diag(W) <- 0
  Wi <- rowSums(W)
  S1 <- rowSums(W^2)
  lx <- as.vector(W %*% x)
  if (star) {
    xb <- rep(mean(x), n)
    s2 <- rep(sum((x - mean(x))^2) / n, n)
    VG <- s2 * (n * S1 - Wi^2) / (n - 1)
  } else {
    xb <- (sum(x) - x) / (n - 1)
    s2 <- (sum(x^2) - x^2) / (n - 1) - xb^2
    VG <- s2 * ((n - 1) * S1 - Wi^2) / (n - 2)
  }
  z <- unname(ifelse(VG > 0, (lx - Wi * xb) / sqrt(pmax(VG, 0)), NaN))
  list(z = z, p_values = 2 * stats::pnorm(abs(z), lower.tail = FALSE), statistic = max(abs(z), na.rm = TRUE),
       star = star)
}
#' SLX regression with impacts and a Wald test
#'
#' \eqn{y = X\beta + WX_*\theta + e} (X_* the non-constant columns) by OLS
#' as \code{spatialreg::lmSLX}; \eqn{\sigma^2 = e'e/(n-k)}. Impacts of
#' covariate r: direct \eqn{\beta_r}, indirect \eqn{\theta_r}, total
#' \eqn{\beta_r + \theta_r} (LeSage and Pace 2009; Halleck Vega and Elhorst
#' 2015) with OLS standard errors; Wald \eqn{\theta' V_\theta^{-1} \theta}
#' against chi-square.
#'
#' @param y Response.
#' @param X Design matrix including any intercept column.
#' @param W Spatial weights matrix.
#' @return List with \code{coefficients}, \code{se}, \code{cov},
#'   \code{sigma2}, \code{residuals}, \code{impacts} (data frame),
#'   \code{wald}, \code{wald_p}, \code{lagged_columns}.
#' @references Halleck Vega, S. and Elhorst, J. P. (2015). The SLX model.
#'   Journal of Regional Science 55, 339-363.
#'
#'   LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press, Boca Raton.
#' @examples
#' W <- 1 * (abs(outer(1:8, 1:8, "-")) == 1)
#' W <- W / rowSums(W)
#' X <- cbind(1, c(.1, .6, .2, .9, .3, .5, .8, .4))
#' SLXRegression(c(1, 2.2, 1.4, 3.1, .9, 2, 2.6, 1.1), X, W)$coefficients
#' @export
SLXRegression <- function(y, X, W) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  p <- ncol(X)
  lag <- which(apply(X, 2, function(v) max(v) - min(v) > 0))
  Z <- cbind(X, W %*% X[, lag, drop = FALSE])
  q <- ncol(Z)
  ZZi <- solve(crossprod(Z))
  b <- as.vector(ZZi %*% crossprod(Z, y))
  e <- as.vector(y - Z %*% b)
  s2 <- sum(e^2) / (n - q)
  V <- ZZi * s2
  se <- sqrt(diag(V))
  th <- p + seq_along(lag)
  imp <- data.frame(column = lag, direct = b[lag], indirect = b[th], total = b[lag] + b[th],
                    se_direct = se[lag], se_indirect = se[th],
                    se_total = sqrt(diag(V)[lag] + diag(V)[th] + 2 * V[cbind(lag, th)]))
  wald <- if (length(lag)) as.numeric(t(b[th]) %*% solve(V[th, th, drop = FALSE]) %*% b[th]) else 0
  list(coefficients = b, se = se, cov = V, sigma2 = s2, residuals = e, impacts = imp, wald = wald,
       wald_p = if (length(lag)) stats::pchisq(wald, length(lag), lower.tail = FALSE) else 1, lagged_columns = lag)
}

#' Direct, indirect and total impacts of a spatial lag or Durbin model
#'
#' With \eqn{S_r(W) = (I - \rho W)^{-1}(\beta_r I + \theta_r W)}, average
#' direct \eqn{tr(S_r)/n}, total \eqn{1'S_r 1/n}, indirect their difference
#' (LeSage and Pace 2009), as \code{spatialreg::impacts} with the exact
#' inverse. \code{beta} and \code{theta} exclude the intercept.
#'
#' @param rho Spatial autoregressive parameter.
#' @param beta Coefficients of the covariates.
#' @param W Spatial weights matrix.
#' @param theta Coefficients of the lagged covariates (Durbin), or NULL.
#' @return List with \code{direct}, \code{indirect}, \code{total}.
#' @references LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press, Boca Raton.
#' @examples
#' W <- matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3)
#' SpatialImpacts(0.4, 2, W)
#' @export
SpatialImpacts <- function(rho, beta, W, theta = NULL) {
  W <- as.matrix(W)
  n <- nrow(W)
  beta <- as.numeric(beta)
  theta <- if (is.null(theta)) rep(0, length(beta)) else as.numeric(theta)
  if (length(theta) != length(beta)) stop("theta must match beta")
  Ai <- solve(diag(n) - rho * W)
  AiW <- Ai %*% W
  direct <- (beta * sum(diag(Ai)) + theta * sum(diag(AiW))) / n
  total <- (beta * sum(Ai) + theta * sum(AiW)) / n
  list(direct = direct, indirect = total - direct, total = total)
}

#' Gravity model of flows by Poisson pseudo-maximum likelihood
#'
#' \eqn{E(F) = \exp(b_0 + b_1 \log M_o + b_2 \log M_d + b_3 \log d)} by the
#' Poisson score equations (IRLS, as \code{glm(family = poisson)});
#' consistent under heteroskedasticity and keeps zero flows (Santos Silva
#' and Tenreyro 2006). Returns HC0 sandwich and model-based standard errors,
#' log-likelihood, AIC and BIC.
#'
#' @param flows Non-negative flows.
#' @param mass_o,mass_d Positive origin and destination masses.
#' @param dist Positive distances.
#' @param tol Relative deviance tolerance.
#' @param max_iter Maximum IRLS iterations.
#' @return List with \code{coefficients}, \code{se_robust}, \code{se_model},
#'   \code{fitted}, \code{loglik}, \code{aic}, \code{bic}, \code{iterations}.
#' @references Santos Silva, J. M. C. and Tenreyro, S. (2006). The log of
#'   gravity. Review of Economics and Statistics 88, 641-658.
#' @examples
#' GravityPPML(c(12, 0, 30, 7, 55, 3), c(5, 5, 9, 9, 20, 20), c(9, 20, 5, 20, 5, 9),
#'             c(1, 3, 1, 2, 3, 2))$coefficients
#' @export
GravityPPML <- function(flows, mass_o, mass_d, dist, tol = 1e-12, max_iter = 100L) {
  Fl <- as.numeric(flows)
  n <- length(Fl)
  if (length(mass_o) != n || length(mass_d) != n || length(dist) != n) stop("all inputs must have the same length")
  if (min(Fl) < 0 || min(mass_o) <= 0 || min(mass_d) <= 0 || min(dist) <= 0) {
    stop("flows must be non-negative and masses and distances positive")
  }
  X <- cbind(1, log(mass_o), log(mass_d), log(dist))
  mu <- (Fl + mean(Fl)) / 2
  eta <- log(mu)
  dev_old <- Inf
  for (it in seq_len(max_iter)) {
    z <- eta + (Fl - mu) / mu
    A <- crossprod(X, X * mu)
    b <- as.vector(solve(A, crossprod(X, mu * z)))
    eta <- as.vector(X %*% b)
    mu <- exp(eta)
    dev <- 2 * sum(ifelse(Fl > 0, Fl * log(Fl / mu), 0) - (Fl - mu))
    if (abs(dev - dev_old) / (abs(dev) + 0.1) < tol) break
    dev_old <- dev
  }
  Ai <- solve(crossprod(X, X * mu))
  Vr <- Ai %*% crossprod(X, X * (Fl - mu)^2) %*% Ai
  ll <- sum(Fl * eta - mu - lgamma(Fl + 1))
  list(coefficients = b, se_robust = sqrt(diag(Vr)), se_model = sqrt(diag(Ai)), fitted = mu, loglik = ll,
       aic = -2 * ll + 8, bic = -2 * ll + 4 * log(n), iterations = it)
}
