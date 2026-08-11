# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Rubin's rules for combining multiple-imputation estimates (Miefcl).
# Bit-identical mirror of src/morie/fn/miefcl.py. Anchored against
# mice::pool on the nhanes example (qbar, ubar, b, t, df, riv, lambda,
# fmi all matched).

#' Combine multiple-imputation estimates by Rubin's rules
#'
#' For a scalar estimand with completed-data estimates Q_i and standard
#' errors sqrt(U_i), i = 1..m: Qbar = mean(Q_i), Ubar = mean(U_i),
#' B = var(Q_i), T = Ubar + (1 + 1/m) B. Inference uses t with
#' \eqn{\nu_{old} = (m-1)/\lambda^2}, \eqn{\lambda = (1+1/m)B/T}; when
#' \code{dfcom} is supplied the Barnard-Rubin (1999) small-sample
#' adjustment \eqn{\nu = \nu_{old}\nu_{obs}/(\nu_{old}+\nu_{obs})} with
#' \eqn{\nu_{obs} = \nu_{com}(\nu_{com}+1)(1-\lambda)/(\nu_{com}+3)}
#' is used instead.
#'
#' @param estimates Numeric vector of completed-data point estimates.
#' @param ses Numeric vector of completed-data standard errors.
#' @param dfcom Optional complete-data degrees of freedom.
#' @return List with \code{estimate}, \code{se}, \code{t}, \code{ubar},
#'   \code{b}, \code{m}, \code{df}, \code{riv}, \code{lambda},
#'   \code{fmi}, \code{method}.
#' @references Rubin, D. B. (1987), Multiple Imputation for Nonresponse
#'   in Surveys, Wiley, ch. 3; Barnard, J. and Rubin, D. B. (1999),
#'   Small-sample degrees of freedom with multiple imputation,
#'   Biometrika 86(4), 948-955; van Buuren, S. (2018), Flexible
#'   Imputation of Missing Data, 2nd ed., CRC Press, sec. 2.3,
#'   eqs. 2.17-2.32 (source snapshot
#'   library/pdf/fetched-wave3/fimd-whyandwhen.html). Anchored against
#'   mice::pool.
#' @export
Miefcl <- function(estimates, ses, dfcom = NULL) {
  q <- as.numeric(estimates)
  s <- as.numeric(ses)
  m <- length(q)
  if (length(s) != m) stop("estimates and ses must have equal length", call. = FALSE)
  if (m < 2L) stop("need m >= 2 imputations", call. = FALSE)
  u <- s * s
  qbar <- mean(q)
  ubar <- mean(u)
  b <- sum((q - qbar)^2) / (m - 1)
  tt <- ubar + (1 + 1 / m) * b
  riv <- if (ubar > 0) (1 + 1 / m) * b / ubar else Inf
  lam <- if (tt > 0) (1 + 1 / m) * b / tt else NaN
  df_old <- if (lam < 1e-12) Inf else (m - 1) / (lam * lam)
  if (is.null(dfcom)) {
    df <- df_old
  } else {
    dfcom <- as.numeric(dfcom)
    nu_obs <- dfcom * (dfcom + 1) * (1 - lam) / (dfcom + 3)
    df <- if (is.infinite(df_old)) nu_obs else df_old * nu_obs / (df_old + nu_obs)
  }
  fmi <- if (is.infinite(df)) riv / (1 + riv) else (riv + 2 / (df + 3)) / (1 + riv)
  list(
    estimate = qbar,
    se = sqrt(tt),
    t = tt,
    ubar = ubar,
    b = b,
    m = m,
    df = df,
    riv = riv,
    lambda = lam,
    fmi = fmi,
    method = "Rubin's rules MI combination"
  )
}
