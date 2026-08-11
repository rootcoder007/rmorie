# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Rao-Scott corrected chi-square for complex surveys (Raoscot).
# Bit-identical mirror of src/morie/fn/raoscot.py. P-values anchored
# against base R pchisq/pf; limiting case (equal deffs) collapses
# RS2 to RS1 with df = nu.

#' Rao-Scott corrected chi-square tests
#'
#' Given the Pearson statistic X2 with nu simple-random-sampling
#' degrees of freedom and generalized design effects delta_1..delta_nu
#' with mean dbar and squared coefficient of variation
#' \eqn{c^2 = \sum_l (\delta_l - \bar d)^2 / (\nu \bar d^2)}: the
#' first-order correction refers \eqn{X^2_{RS1} = X^2/\bar d} to
#' \eqn{\chi^2_\nu}; the second-order correction refers
#' \eqn{X^2_{RS2} = X^2/(\bar d(1+c^2))} to
#' \eqn{\chi^2_{\nu/(1+c^2)}}; the Thomas-Rao F statistic
#' \eqn{F_{TR} = X^2/(\nu\bar d)} is referred to an F distribution
#' with \eqn{\nu/(1+c^2)} and \eqn{\kappa\nu/(1+c^2)} degrees of
#' freedom, where kappa is the design variance-estimation df.
#'
#' @param X2 Pearson chi-square statistic from the weighted table.
#' @param df Simple-random-sampling degrees of freedom nu.
#' @param deltas Generalized design effects (scalar = common deff).
#' @param kappa Optional variance-estimation degrees of freedom.
#' @return List with \code{estimate} (X2_RS2), \code{rs1}, \code{rs2},
#'   \code{f_tr}, \code{df}, \code{df2}, \code{ddf}, \code{dbar},
#'   \code{c2}, \code{p_rs1}, \code{p_rs2}, \code{p_f}, \code{method}.
#' @references Rao, J. N. K. and Scott, A. J. (1981), The analysis of
#'   categorical data from complex sample surveys, JASA 76(374),
#'   221-230; Thomas, D. R. and Rao, J. N. K. (1987), JASA 82(398),
#'   630-636; Bilder, C. R. and Loughin, T. M. (2014), Analysis of
#'   Categorical Data with R, CRC Press, sec. 6.3.5 and eq. 6.12
#'   (local source: library/pdf/Analysis of Categorical Data with R,
#'   pp. 469-470).
#' @export
Raoscot <- function(X2, df, deltas, kappa = NULL) {
  X2 <- as.numeric(X2)
  nu <- as.numeric(df)
  if (X2 < 0) stop("X2 must be nonnegative", call. = FALSE)
  if (nu < 1) stop("df must be >= 1", call. = FALSE)
  d <- as.numeric(deltas)
  if (length(d) == 1L) {
    dbar <- d[1]
    c2 <- 0
  } else {
    if (length(d) != as.integer(nu)) {
      stop("need one generalized deff per degree of freedom", call. = FALSE)
    }
    dbar <- mean(d)
    c2 <- sum((d - dbar)^2) / (nu * dbar * dbar)
  }
  if (dbar <= 0) stop("design effects must be positive", call. = FALSE)
  rs1 <- X2 / dbar
  rs2 <- X2 / (dbar * (1 + c2))
  df2 <- nu / (1 + c2)
  chisq_sf <- function(x, k) {
    if (x <= 0) return(1)
    pgamma(x / 2, shape = k / 2, lower.tail = FALSE)
  }
  p_rs1 <- chisq_sf(rs1, nu)
  p_rs2 <- chisq_sf(rs2, df2)
  f_tr <- X2 / (nu * dbar)
  if (is.null(kappa)) {
    p_f <- NaN
    ddf <- NaN
  } else {
    ddf <- as.numeric(kappa) * df2
    z <- ddf / (ddf + df2 * f_tr)
    p_f <- pbeta(z, ddf / 2, df2 / 2)
  }
  list(
    estimate = rs2,
    rs1 = rs1,
    rs2 = rs2,
    f_tr = f_tr,
    df = nu,
    df2 = df2,
    ddf = ddf,
    dbar = dbar,
    c2 = c2,
    p_rs1 = p_rs1,
    p_rs2 = p_rs2,
    p_f = p_f,
    method = "Rao-Scott corrected chi-square (first/second order + Thomas-Rao F)"
  )
}
