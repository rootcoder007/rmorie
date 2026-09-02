# SPDX-License-Identifier: AGPL-3.0-or-later

# OLS coefficient j (1-based), its standard error and the residual df.
#' OLS coefficient j (1-based), its standard error and the residual df
#'
#' A step of the chzlt implementation. Called by \code{Chzlt}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param y A vector; its length is taken.
#' @param X Passed to \code{.s03mat}.
#' @param j See Usage.
#' @return A list with \code{beta}, \code{se}, \code{df}.
#' @export
.ch_ols_se <- function(y, X, j) {
  y <- .s03vec(y)
  D <- .s03mat(X)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (nrow(D) != n) stop("y and X must have the same number of rows")
  A <- cbind(rep(1, n), D)
  A <- matrix(as.numeric(A), n)
  p <- ncol(A)
  if (n <= p) stop("need more observations than columns")
  XtX <- .s03crossprod(A)
  beta <- .s03cholsolve(XtX, .s03matvec(t(A), y))
  fit <- .s03matvec(A, beta)
  e <- y - fit
  df <- n - p
  s2 <- sum(e * e) / df
  ej <- numeric(p)
  ej[j] <- 1
  col <- .s03cholsolve(XtX, ej)
  list(beta = beta[j], se = sqrt(s2 * col[j]), df = df)
}

# RV_q: the partial R2 an omitted confounder needs, eq. (9).
#' RV_q: the partial R2 an omitted confounder needs, eq. (9)
#'
#' A step of the chzlt implementation. Called by \code{Chzlt}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param t Numeric; passed to \code{abs}.
#' @param df Numeric; passed to \code{sqrt}.
#' @param q Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.ch_rv <- function(t, df, q = 1) {
  fq <- q * abs(t) / sqrt(df)
  rv <- 0.5 * (sqrt(fq^4 + 4 * fq^2) - fq^2)
  min(max(rv, 0), 1)
}

#' Cinelli-Hazlett sensitivity to an unobserved confounder
#'
#' Formula: adjusted estimate vs (R2_y~u.x, R2_d~u.x)
#'
#' The bias an omitted confounder U can produce is bounded by
#' |bias| = se sqrt(df) sqrt(R2_yu R2_du / (1 - R2_du)), so a claim
#' survives U exactly when the adjusted estimate keeps its sign.  The
#' robustness value RV_q is the common partial R2 at which the adjusted
#' estimate is driven to zero: feeding RV back in as both R2 values
#' returns an adjusted estimate of exactly zero, the identity used to
#' check this implementation.
#'
#' @param model Outcome vector y.
#' @param treat Treatment vector D.
#' @param cov An n x k matrix of observed covariates, or NULL.
#' @param R2_yu Hypothesised partial R2 with the outcome.
#' @param R2_du Hypothesised partial R2 with the treatment.
#' @param q Fraction of the estimate the confounder would explain.
#' @return List with \code{estimate}, \code{tau}, \code{se}, \code{t},
#'   \code{df}, \code{bias}, \code{adjusted_se}, \code{rv_q},
#'   \code{r2_yd_x}, \code{robust}, \code{n}, \code{method}.
#' @references Cinelli & Hazlett (2020), Making Sense of Sensitivity,
#'   JRSS B 82(1):39-67.
#' @export
#' @examples
#' set.seed(1)
#' Chzlt(model = rnorm(50), treat = rbinom(50, 1, 0.5),
#'       cov = matrix(rnorm(100), 50, 2), R2_yu = 0.1, R2_du = 0.1)
Chzlt <- function(model, treat = NULL, cov = NULL, R2_yu = 0, R2_du = 0,
                  q = 1) {
  y <- .s03vec(model)
  d <- .s03vec(treat)
  n <- length(y)
  if (n == 0L) stop("empty input: model has no observations")
  if (length(d) != n) stop("model and treat must have the same length")
  if (is.null(cov)) {
    D <- matrix(d, n, 1L)
  } else {
    Xm <- .s03mat(cov)
    if (nrow(Xm) != n) stop("cov must have one row per observation")
    D <- cbind(d, Xm)
  }
  if (!(R2_yu >= 0 && R2_yu <= 1 && R2_du >= 0 && R2_du < 1))
    stop("R2_yu must lie in [0, 1] and R2_du in [0, 1)")
  f <- .ch_ols_se(y, D, 2L)
  tau <- f$beta
  se <- f$se
  df <- f$df
  t <- if (se > 0) tau / se else NaN
  bias <- se * sqrt(df) * sqrt(R2_yu * R2_du / (1 - R2_du))
  adj <- if (tau >= 0) tau - bias else tau + bias
  adj_se <- if (df > 1) se * sqrt((1 - R2_yu) / (1 - R2_du)) *
    sqrt(df / (df - 1)) else NaN
  rv <- .ch_rv(t, df, q)
  r2_yd <- t * t / (t * t + df)
  .t1_result(estimate = adj, tau = tau, se = se, t = t, df = df,
             bias = bias, adjusted_se = adj_se, rv_q = rv,
             r2_yd_x = r2_yd, robust = as.integer(adj * tau > 0), n = n,
             method = "Cinelli-Hazlett omitted-variable-bias sensitivity")
}
