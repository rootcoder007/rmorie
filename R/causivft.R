# SPDX-License-Identifier: AGPL-3.0-or-later
#' First-stage F statistic for weak instruments
#'
#' Source FETCHED (reference implementation): the partial-F used by
#' Stock, Wright and Yogo (2002), Journal of Business and Economic
#' Statistics 20, 518-529.  With RSS_u the residual sum of squares of
#' the first-stage regression \code{D ~ X_exog + Z} and RSS_r that of
#' the restricted fit on \code{X_exog} alone,
#' \code{F = ((RSS_r - RSS_u)/L) / (RSS_u/(n - k - L))} on
#' \code{(L, n - k - L)} degrees of freedom.  The Stock-Yogo threshold
#' of 10 is reported as \code{weak}, but it is a rule of thumb, not a
#' size-correct critical value.
#'
#' @param D Numeric endogenous regressor of length n.
#' @param Z Numeric n x L matrix of excluded instruments.
#' @param X_exog Optional n x q matrix of included exogenous covariates.
#' @param add_intercept Include an intercept.  Default TRUE.
#' @return list: statistic, p_value, df1, df2, rss_u, rss_r, n,
#'   n_instruments, weak, method.
#' @examples
#' set.seed(1)
#' Z <- matrix(rnorm(120), 60, 2)
#' Ivfstage(Z %*% c(1, 0.5) + rnorm(60), Z)$statistic
#' @export
Ivfstage <- function(D, Z, X_exog = NULL, add_intercept = TRUE) {
  d <- as.numeric(D)
  n <- length(d)
  Z <- as.matrix(Z)
  if (nrow(Z) != n) Z <- t(Z)
  C <- NULL
  if (add_intercept) C <- cbind(C, rep(1, n))
  if (!is.null(X_exog)) {
    Xe <- as.matrix(X_exog)
    if (nrow(Xe) != n) Xe <- t(Xe)
    C <- cbind(C, Xe)
  }
  k <- if (is.null(C)) 0L else ncol(C)
  L <- ncol(Z)
  df2 <- n - k - L
  if (L < 1 || df2 < 1) stop("need L >= 1 and n > k + L")
  rss <- function(Dm) {
    b <- qr.solve(Dm, d)
    r <- d - Dm %*% b
    sum(r * r)
  }
  if (k > 0) {
    rss_r <- rss(C)
    rss_u <- rss(cbind(C, Z))
  } else {
    rss_r <- sum(d * d)
    rss_u <- rss(Z)
  }
  stat <- ((rss_r - rss_u) / L) / (rss_u / df2)
  list(
    statistic = stat, p_value = stats::pf(stat, L, df2, lower.tail = FALSE),
    df1 = L, df2 = as.integer(df2), rss_u = rss_u, rss_r = rss_r,
    n = n, n_instruments = L, weak = stat < 10,
    method = "First-stage F for excluded instruments (Stock-Wright-Yogo 2002)"
  )
}
