# SPDX-License-Identifier: AGPL-3.0-or-later

# Two-sided Student-t quantile by Cornish-Fisher expansion on the normal.
#' Two-sided Student-t quantile by Cornish-Fisher expansion on the
#' normal
#'
#' A step of the Drbsze implementation. Called by \code{Drbsze}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param p Passed to \code{.s03qnorm}.
#' @param df Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.drbsze_tquant <- function(p, df) {
  z <- .s03qnorm(p)
  g1 <- (z^3 + z) / 4
  g2 <- (5 * z^5 + 16 * z^3 + 3 * z) / 96
  g3 <- (3 * z^7 + 19 * z^5 + 17 * z^3 - 15 * z) / 384
  g4 <- (79 * z^9 + 776 * z^7 + 1482 * z^5 - 1920 * z^3 - 945 * z) / 92160
  z + g1 / df + g2 / df^2 + g3 / df^3 + g4 / df^4
}

#' Finite-sample size correction for the doubly robust DiD estimator
#'
#' The asymptotic standard error of the doubly robust moment of
#' Sant'Anna and Zhao (2020) does not charge for the degrees of freedom
#' the nuisance fits consume, so the test over-rejects when \code{n} is
#' small or the treated group is thin.  Two standard corrections are
#' applied: the classical HC1 inflation \code{sqrt(n/(n - k))} and a
#' Student-t critical value on \code{min(n1, n0) - 1} degrees of
#' freedom in place of the normal quantile.  Both are conservative and
#' both tend to their asymptotic limits as \code{n} grows.
#'
#' @param y Outcome change, one entry per unit.
#' @param D Binary treatment indicator.
#' @param X Optional baseline covariates.
#' @param alpha Two-sided level, in (0, 1).
#' @return List with \code{estimate}, \code{se}, \code{se_corrected},
#'   \code{crit_normal}, \code{crit_t}, \code{df}, \code{ci_lo},
#'   \code{ci_hi}, \code{reject}, \code{reject_naive}, \code{n}.
#' @references Sant'Anna, P. H. C. and Zhao, J. (2020). Journal of
#'   Econometrics 219(1), 101-122, equation (2.6).  MacKinnon, J. G. and
#'   White, H. (1985). Journal of Econometrics 29(3), 305-325.  Bell,
#'   R. M. and McCaffrey, D. F. (2002). Survey Methodology 28(2),
#'   169-181.
#' @export
#' @examples
#' set.seed(1)
#' r <- Drbsze(y = rnorm(10), D = rbinom(10, 1, 0.5)); TRUE
Drbsze <- function(y, D, X = NULL, alpha = 0.05) {
  yv <- .s03vec(y); dv <- .s03vec(D); n <- length(yv)
  if (n == 0L) stop("Drbsze: empty input, y has no observations")
  if (length(dv) != n) stop("Drbsze: y and D must have the same length")
  if (!(alpha > 0 && alpha < 1))
    stop("Drbsze: alpha must lie strictly between 0 and 1")
  n1 <- sum(dv >= 0.5); n0 <- n - n1
  if (n1 == 0L || n0 == 0L)
    stop("Drbsze: D must contain both treated and control units")
  fit <- .s03drdid(yv, dv, X)
  nk <- 1L + if (is.null(X)) 0L else ncol(.s03mat(X))
  infl <- if (n > nk) sqrt(n / (n - nk)) else Inf
  se_c <- fit$se * infl
  df <- max(min(n1, n0) - 1, 1)
  zc <- .s03qnorm(1 - alpha / 2)
  tc <- .drbsze_tquant(1 - alpha / 2, df)
  .t1_result(estimate = fit$tau, se = fit$se, se_corrected = se_c,
             crit_normal = zc, crit_t = tc, df = df,
             ci_lo = fit$tau - tc * se_c, ci_hi = fit$tau + tc * se_c,
             reject = if (abs(fit$tau) > tc * se_c) 1 else 0,
             reject_naive = if (abs(fit$tau) > zc * fit$se) 1 else 0,
             n = n, method = "DR-DiD with finite-sample size correction")
}
