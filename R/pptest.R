# SPDX-License-Identifier: AGPL-3.0-or-later
# Dickey-Fuller tables for the trend-included regression, as carried by
# tseries::pp.test.  Rows are sample sizes 25, 50, 100, 250, 500, inf.
.t4_PP_T <- c(25, 50, 100, 250, 500, 100000)
.t4_PP_P <- c(0.01, 0.025, 0.05, 0.10, 0.90, 0.95, 0.975, 0.99)
.t4_PP_ALPHA <- rbind(
  c(22.5, 19.9, 17.9, 15.6, 3.66, 2.51, 1.53, 0.43),
  c(25.7, 22.4, 19.8, 16.8, 3.71, 2.60, 1.66, 0.65),
  c(27.4, 23.6, 20.7, 17.5, 3.74, 2.62, 1.73, 0.75),
  c(28.4, 24.4, 21.3, 18.0, 3.75, 2.64, 1.78, 0.82),
  c(28.9, 24.8, 21.5, 18.1, 3.76, 2.65, 1.78, 0.84),
  c(29.5, 25.1, 21.8, 18.3, 3.77, 2.66, 1.79, 0.87))
.t4_PP_TALPHA <- rbind(
  c(4.38, 3.95, 3.60, 3.24, 1.14, 0.80, 0.50, 0.15),
  c(4.15, 3.80, 3.50, 3.18, 1.19, 0.87, 0.58, 0.24),
  c(4.04, 3.73, 3.45, 3.15, 1.22, 0.90, 0.62, 0.28),
  c(3.99, 3.69, 3.43, 3.13, 1.23, 0.92, 0.64, 0.31),
  c(3.98, 3.68, 3.42, 3.13, 1.24, 0.93, 0.65, 0.32),
  c(3.96, 3.66, 3.41, 3.12, 1.25, 0.94, 0.66, 0.33))

# Interpolated Dickey-Fuller critical values at sample size n.
.t4_ppcrit <- function(n, kind = "Z(t_alpha)") {
  tab <- if (kind == "Z(alpha)") .t4_PP_ALPHA else .t4_PP_TALPHA
  vapply(seq_along(.t4_PP_P),
         function(c) -stats::approx(.t4_PP_T, tab[, c], n, rule = 2)$y,
         numeric(1))
}

#' Phillips-Perron test for a unit root, trend included.
#'
#' The auxiliary regression is \eqn{y_t = \mu + \beta(t - n/2) +
#' \rho y_{t-1} + u_t}.  With \eqn{s^2 = \sum u^2/n} and the Bartlett
#' long-run variance \eqn{\lambda^2 = s^2 + (2/n)\sum_{i=1}^{l}
#' (1 - i/(l+1)) \sum_t u_t u_{t-i}}, the statistics are
#' \eqn{Z_\alpha = n(\rho-1) - n^6(\lambda^2 - s^2)/(24 D)} and
#' \eqn{Z_{t\alpha} = \sqrt{s^2/\lambda^2}\, t_\rho -
#' n^3(\lambda^2 - s^2)/(4\sqrt{3}\sqrt{D}\lambda)}, with \eqn{D} the
#' trend-corrected sum of squares.  The correction is non-parametric:
#' the OLS t is rescaled by short-run over long-run variance rather than
#' the regression being augmented with lagged differences.  Default
#' truncation lag \eqn{\lfloor 4(n/100)^{1/4}\rfloor}.
#'
#' @param x Series in time order.
#' @param lags Bartlett truncation lag; short-lag rule if NULL.
#' @param kind "Z(t_alpha)" or "Z(alpha)".
#' @return List with \code{statistic}, \code{p_value}, \code{rho},
#'   \code{lags}, \code{s2}, \code{lambda2}, \code{n}, \code{method}.
#' @references Phillips and Perron (1988), Biometrika 75:335-346.  Paywalled; the coded form, the pp_sum Bartlett kernel and the Dickey-Fuller tables were read from Trapletti and Hornik's tseries (R/test.R, src/ppsum.c), tarball tseries_0.10-62 from CRAN.  The p-value is interpolated in those tables, flat outside 1%-99%.
#' @export
Pptest <- function(x, lags = NULL, kind = "Z(t_alpha)") {
  x <- .t4_vec(x); nn <- length(x)
  if (nn < 6) stop("need at least 6 observations")
  if (!kind %in% c("Z(alpha)", "Z(t_alpha)")) stop("kind must be 'Z(alpha)' or 'Z(t_alpha)'")
  yt <- x[-1]; yt1 <- x[-nn]; n <- length(yt)
  lag <- if (!is.null(lags)) as.integer(lags) else as.integer(trunc(4 * (n / 100)^0.25))
  if (lag < 1L) lag <- 1L
  X <- cbind(1, seq_len(n) - n / 2, yt1)
  fit <- .t4_olsfit(X, yt)
  u <- fit$resid
  ssqru <- sum(u^2) / n
  ssqrtl <- .t4_lrvnw(u, lag)
  n2 <- n^2; s <- seq_len(n)
  sy <- sum(yt1); sty <- sum(s * yt1)
  D <- n2 * (n2 - 1) * sum(yt1^2) / 12 - n * sty^2 +
    n * (n + 1) * sty * sy - n * (n + 1) * (2 * n + 1) * sy^2 / 6
  rho <- fit$beta[3]
  if (kind == "Z(alpha)") {
    stat <- n * (rho - 1) - (n^6) / (24 * D) * (ssqrtl - ssqru)
  } else {
    sigma2 <- sum(u^2) / (n - 3)
    tstat <- (rho - 1) / sqrt(sigma2 * fit$xtxinv[3, 3])
    stat <- sqrt(ssqru) / sqrt(ssqrtl) * tstat -
      (n^3) / (4 * sqrt(3) * sqrt(D) * sqrt(ssqrtl)) * (ssqrtl - ssqru)
  }
  crit <- .t4_ppcrit(n, kind)
  p <- stats::approx(crit, .t4_PP_P, stat, rule = 2)$y
  .t4_result(statistic = stat, p_value = p, rho = rho, lags = as.integer(lag),
             s2 = ssqru, lambda2 = ssqrtl, n = as.integer(n),
             method = paste0("Phillips-Perron unit root test, ", kind))
}
