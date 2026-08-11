# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Barnard-Rubin degrees of freedom for multiple imputation (Midegf).
# Bit-identical mirror of src/morie/fn/midegf.py. Anchored against
# mice:::barnard.rubin over a grid of (m, B, W, dfcom).

#' Degrees of freedom for multiple-imputation inference
#'
#' With between variance B, within variance W and m imputations,
#' T = W + (1 + 1/m) B, \eqn{\lambda = (1+1/m)B/T} and Rubin's
#' \eqn{\nu_{old} = (m-1)/\lambda^2}. Barnard and Rubin (1999) adjust
#' for finite complete-data degrees of freedom \eqn{\nu_{com}}:
#' \eqn{\nu_{obs} = \nu_{com}(\nu_{com}+1)(1-\lambda)/(\nu_{com}+3)}
#' and \eqn{\nu_{adj} = \nu_{old}\nu_{obs}/(\nu_{old}+\nu_{obs})}.
#'
#' @param B Between-imputation variance.
#' @param W Within-imputation variance (Ubar).
#' @param m Number of imputations (>= 2).
#' @param dfcom Optional complete-data degrees of freedom.
#' @return List with \code{estimate} (the df), \code{df_old},
#'   \code{nu_obs}, \code{lambda}, \code{m}, \code{dfcom},
#'   \code{method}.
#' @references Barnard, J. and Rubin, D. B. (1999), Small-sample
#'   degrees of freedom with multiple imputation, Biometrika 86(4),
#'   948-955; Rubin, D. B. (1987), Multiple Imputation for Nonresponse
#'   in Surveys, Wiley, ch. 3; van Buuren, S. (2018), Flexible
#'   Imputation of Missing Data, 2nd ed., sec. 2.3.6, eqs. 2.30-2.32
#'   (source snapshot library/pdf/fetched-wave3/fimd-whyandwhen.html).
#'   Anchored against mice ::: barnard.rubin (snapshot
#'   library/pdf/fetched-wave3/mice-barnard-rubin-source.R).
#' @export
Midegf <- function(B, W, m, dfcom = NULL) {
  B <- as.numeric(B)
  W <- as.numeric(W)
  m <- as.integer(m)
  if (m < 2L) stop("need m >= 2 imputations", call. = FALSE)
  if (B < 0 || W < 0) stop("variances must be nonnegative", call. = FALSE)
  tt <- W + (1 + 1 / m) * B
  lam <- if (tt > 0) (1 + 1 / m) * B / tt else NaN
  df_old <- if (lam < 1e-12) Inf else (m - 1) / (lam * lam)
  if (is.null(dfcom)) {
    df <- df_old
    nu_obs <- NaN
  } else {
    dfcom <- as.numeric(dfcom)
    nu_obs <- dfcom * (dfcom + 1) * (1 - lam) / (dfcom + 3)
    df <- if (is.infinite(df_old)) nu_obs else df_old * nu_obs / (df_old + nu_obs)
  }
  list(
    estimate = df,
    df_old = df_old,
    nu_obs = nu_obs,
    lambda = lam,
    m = m,
    dfcom = if (is.null(dfcom)) NaN else dfcom,
    method = "Barnard-Rubin MI degrees of freedom"
  )
}
