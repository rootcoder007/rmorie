# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weibull hazard model with unobserved heterogeneity: Honore estimator
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 6.1.3, pages 197-200 (volume
#' [Pages 189-232], read as rendered page images).  The model is
#' lambda(y | x) = alpha y^(alpha - 1) exp(-x beta - V) (6.17) and
#' alpha log Y = X beta + U + V (6.18), with V independent of (X, U) and
#' otherwise unrestricted.  The book notes below (6.19) why alpha cannot be
#' recovered from the moment condition (6.7): it holds for any value of alpha
#' once gamma = beta / alpha is free.  Honore (1990) instead recovers alpha
#' from the behaviour of the survivor function at the origin, (6.24), and its
#' sample analogue built from two order statistics (p. 200): with
#' m1 = n^(1 - delta1), m2 = n^(1 - delta2), 0 < delta2 < delta1 < 1,
#' rho = 1 - (1/2)(n^-delta1 - n^-delta2) / ((delta1 - delta2) log n),
#' a_n = -rho (delta1 - delta2) log n / (log Y_(m1) - log Y_(m2)) (6.25) and
#' sigma^2 = [1 / ((delta1 - delta2) log n)]^2 (n^delta1 - n^delta2) / n
#' (6.27).
#'
#' The book adds, in the paragraph after (6.27), that with covariates alpha is
#' estimated by (6.25) applied as if the covariates were unobserved, that
#' gamma_n comes from the least-squares regression of log Y on X, and that
#' beta is then b_n = a_n gamma_n.  That is exactly what is done here.  The
#' Weibull scale reported is exp(-beta_1), the baseline of (6.17) at x = 0,
#' which requires the first column of X to be the intercept.  Nothing is
#' random: the estimator is a function of two order statistics and an ordinary
#' least-squares fit.
#'
#' @param t Durations Y, strictly positive.
#' @param x n-by-p design matrix; the first column should be the intercept.
#' @param event Optional, 1 for an observed duration and 0 to drop it.
#' @param mixing_dist Label only; the distribution of V is left unrestricted,
#'   which is the whole point of (6.24), so anything other than
#'   "nonparametric" is refused rather than silently ignored.
#' @param delta1,delta2 The exponents of p. 200, needing
#'   0 < delta2 < delta1 < 1.
#' @return list: estimate, alpha_hat, beta_hat, gamma_hat, lambda_hat, rho,
#'   m1, m2, sigma2, se_alpha, n, method.
#' @keywords internal
#' @examples
#' y <- exp(seq(-3, 3, length.out = 40))
#' Hrzweib(y, cbind(1, seq_len(40) / 40))$alpha_hat
#' @export
Hrzweib <- function(t, x, event = NULL, mixing_dist = "nonparametric",
                    delta1 = 0.6, delta2 = 0.3) {
  tt <- .s03vec(t)
  XX <- .s03mat(x)
  n0 <- length(tt)
  if (n0 == 0L) stop("horowitz_weibull_heterogeneity: t is empty")
  if (nrow(XX) != n0) {
    stop("horowitz_weibull_heterogeneity: x has a different number of rows than t")
  }
  if (!identical(mixing_dist, "nonparametric")) {
    stop("horowitz_weibull_heterogeneity: only the nonparametric mixing of (6.17) is offered")
  }
  if (is.null(event)) {
    keep <- seq_len(n0)
  } else {
    ev <- .s03vec(event)
    if (length(ev) != n0) {
      stop("horowitz_weibull_heterogeneity: event has a different length than t")
    }
    keep <- which(ev != 0)
  }
  yv <- tt[keep]
  Xk <- XX[keep, , drop = FALSE]
  n <- length(yv)
  if (n < 3L) {
    stop("horowitz_weibull_heterogeneity: fewer than three uncensored durations")
  }
  if (any(yv <= 0)) stop("horowitz_weibull_heterogeneity: durations must be positive")
  d1 <- as.numeric(delta1)
  d2 <- as.numeric(delta2)
  if (!(d2 > 0 && d1 > d2 && d1 < 1)) {
    stop("horowitz_weibull_heterogeneity: need 0 < delta2 < delta1 < 1")
  }
  srt <- sort(yv)
  ln <- log(n)
  m1 <- as.integer(round(n^(1 - d1)))
  m2 <- as.integer(round(n^(1 - d2)))
  if (m1 < 1L) m1 <- 1L
  if (m2 > n) m2 <- as.integer(n)
  if (m1 >= m2) {
    stop("horowitz_weibull_heterogeneity: the two order statistics coincide, n is too small")
  }
  rho <- 1 - 0.5 * (n^(-d1) - n^(-d2)) / ((d1 - d2) * ln)
  den <- log(srt[m1]) - log(srt[m2])
  if (den == 0) stop("horowitz_weibull_heterogeneity: the two order statistics are tied")
  a_n <- -rho * (d1 - d2) * ln / den
  s2 <- (1 / ((d1 - d2) * ln))^2 * (n^d1 - n^d2) / n
  ly <- log(yv)
  gam <- .s03lstsq(Xk, ly, 1e-12)
  beta <- a_n * gam
  lam <- exp(-beta[1])
  list(estimate = a_n, alpha_hat = a_n, beta_hat = beta, gamma_hat = gam,
       lambda_hat = lam, rho = rho, m1 = m1, m2 = m2, sigma2 = s2,
       se_alpha = a_n * sqrt(s2), n = n,
       method = paste0("Horowitz (2009) eq. (6.24)-(6.27) pp.199-200, ",
                       "Honore two-order-statistic alpha"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzweib
#' @keywords internal
#' @export
morie_horowitz_weibull_heterogeneity <- Hrzweib
