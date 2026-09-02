# SPDX-License-Identifier: AGPL-3.0-or-later
# Hansen (1992), Table 1: asymptotic critical values for L_c, indexed by
# degrees of freedom m+1 = 1..20, at 1%, 2.5%, 5%, 7.5%, 10%, 20%.
.t4_LC_LEVELS <- c(0.01, 0.025, 0.05, 0.075, 0.10, 0.20)
.t4_LC_TABLE <- rbind(
  c(0.748, 0.593, 0.470, 0.398, 0.353, 0.243),
  c(1.07, 0.898, 0.749, 0.670, 0.610, 0.469),
  c(1.35, 1.16, 1.01, 0.913, 0.846, 0.679),
  c(1.60, 1.39, 1.24, 1.14, 1.07, 0.883),
  c(1.88, 1.63, 1.47, 1.36, 1.28, 1.08),
  c(2.12, 1.89, 1.68, 1.58, 1.49, 1.28),
  c(2.35, 2.10, 1.90, 1.78, 1.69, 1.46),
  c(2.59, 2.33, 2.11, 1.99, 1.89, 1.66),
  c(2.82, 2.55, 2.32, 2.19, 2.10, 1.85),
  c(3.05, 2.76, 2.54, 2.40, 2.29, 2.03),
  c(3.27, 2.99, 2.75, 2.60, 2.49, 2.22),
  c(3.51, 3.18, 2.96, 2.81, 2.69, 2.41),
  c(3.69, 3.39, 3.15, 3.00, 2.89, 2.59),
  c(3.90, 3.60, 3.34, 3.19, 3.08, 2.77),
  c(4.07, 3.81, 3.54, 3.38, 3.26, 2.95),
  c(4.30, 4.01, 3.75, 3.58, 3.46, 3.14),
  c(4.51, 4.21, 3.95, 3.77, 3.64, 3.32),
  c(4.73, 4.40, 4.14, 3.96, 3.83, 3.50),
  c(4.92, 4.60, 4.33, 4.16, 4.03, 3.69),
  c(5.13, 4.79, 4.52, 4.36, 4.22, 3.86))

#' .t4_lccrit
#'
#' A step of the nybhsn implementation. Called by \code{Lctest}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param df Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{out}, as built in the body.
#' @export
.t4_lccrit <- function(df) {
  df <- as.integer(df)
  if (df < 1L || df > nrow(.t4_LC_TABLE))
    stop("Hansen's Table 1 covers 1 to 20 degrees of freedom")
  out <- as.numeric(.t4_LC_TABLE[df, ])
  names(out) <- as.character(.t4_LC_LEVELS)
  out
}

#' Nyblom-Hansen test that every OLS parameter is stable.
#'
#' Fit \eqn{y = Xb + e} by least squares and form the first-order
#' conditions \eqn{f_{it} = x_{it} e_t} for the m regression parameters
#' and \eqn{f_{m+1,t} = e_t^2 - \sigma^2} for the error variance, whose
#' cumulative sums \eqn{S_t} are zero at \eqn{t = n} by construction.
#' With \eqn{V = \sum_t f_t f_t'} the joint statistic is
#' \eqn{L_c = (1/n)\sum_t S_t' V^{-1} S_t}, and the individual ones are
#' \eqn{L_i = (1/n)\sum_t S_{it}^2 / V_{ii}}.  Because \eqn{S_n = 0} the
#' cumulative sums are a tied-down random walk under the null, so the
#' limit is a Brownian bridge and the critical values depend only on the
#' number of parameters tested.  Including the variance score is what
#' gives power against a shift in \eqn{\sigma^2}.
#'
#' @param y Response, in the order stability is tested against.
#' @param X n by p regressors, ordered the same way.
#' @param add_intercept Prepend a column of ones.
#' @param variance Include the error-variance score (df = m + 1).
#' @return List with \code{statistic} (L_c), \code{df},
#'   \code{individual}, \code{critical}, \code{n}, \code{method}.
#' @references Nyblom (1989), JASA 84:223-230; Hansen (1992), Journal of Policy Modeling 14:517-533.  Hansen's paper was fetched in full from his own page (users.ssc.wisc.edu/~bhansen/papers/jpm_92.pdf): eqs (3)-(4) give the scores, (9)-(10) give L_c, and Table 1 -- reproduced verbatim above -- is the critical-value table.  No p-value is returned: Table 1 gives six points and interpolating between them would invent precision the published table does not carry.  strucchange's sctest(type="Nyblom-Hansen") averages over the n+1 points of the empirical process including the zero at t=0, so its value is n/(n+1) times the L_c of eq (9) returned here.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Lctest(D, V)
Lctest <- function(y, X, add_intercept = TRUE, variance = TRUE) {
  y <- .t4_vec(y)
  Xm <- if (is.matrix(X) || is.data.frame(X)) as.matrix(X) else matrix(as.numeric(X), ncol = 1L)
  n <- length(y)
  if (nrow(Xm) != n) stop("X must have one row per element of y")
  if (add_intercept) Xm <- cbind(1, Xm)
  p <- ncol(Xm)
  if (n <= p + 1L) stop("need more observations than parameters")
  fit <- .t4_olsfit(Xm, y)
  e <- fit$resid
  sigma2 <- sum(e^2) / n
  f <- Xm * e
  if (variance) f <- cbind(f, e^2 - sigma2)
  k <- ncol(f)
  V <- crossprod(f)
  S <- apply(f, 2, cumsum)
  if (k == 1L) S <- matrix(S, ncol = 1L)
  Vinv <- solve(V)
  lc <- sum(rowSums((S %*% Vinv) * S)) / n
  indiv <- vapply(seq_len(k), function(a)
    if (V[a, a] > 0) sum(S[, a]^2) / (n * V[a, a]) else NaN, numeric(1))
  .t4_result(statistic = lc, df = as.integer(k), individual = indiv,
             critical = .t4_lccrit(k), n = as.integer(n),
             method = "Nyblom-Hansen joint parameter stability test")
}
