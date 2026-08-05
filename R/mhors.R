# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mantel-Haenszel pooled odds ratio across 2x2 strata
#'
#' Fitting a stratum effect per table costs a parameter per table, and
#' with sparse tables the maximum-likelihood estimate is badly biased --
#' the classic Neyman-Scott problem. The Mantel-Haenszel weights step
#' around it: they stay consistent both when the strata are few and large
#' and when they are many and small, which no likelihood-based weighting
#' achieves at once.
#'
#' Formula: \code{OR_MH = sum(a_k d_k/n_k) / sum(b_k c_k/n_k)}. The
#' variance of its logarithm is the Robins-Breslow-Greenland expression
#' \code{P/(2 S^2) + Q/(2 S T) + R/(2 T^2)} with
#' \code{P = sum (a+d) a d / n^2}, \code{Q = sum ((a+d) b c + (b+c) a d)
#' / n^2}, \code{R = sum (b+c) b c / n^2}, \code{S = sum a d / n},
#' \code{T = sum b c / n}.
#'
#' @param tables Matrix or data frame with one stratum per row and four
#'   columns \code{a, b, c, d}: exposed cases, exposed non-cases,
#'   unexposed cases, unexposed non-cases.
#' @param confidence Confidence level.
#' @return List with \code{measure}, \code{estimate}, \code{se},
#'   \code{ci_lower}, \code{ci_upper}, \code{n}, \code{n_strata}.
#' @references Mantel, N. and Haenszel, W. (1959). Journal of the National
#'   Cancer Institute 22(4):719-748. \doi{10.1093/jnci/22.4.719}. Robins,
#'   J., Breslow, N. and Greenland, S. (1986). Biometrics 42(2):311-323.
#'   \doi{10.2307/2531052}.
#' @export
Mhors <- function(tables, confidence = 0.95) {
  M <- as.matrix(tables)
  if (!nrow(M)) stop("At least one table required")
  if (ncol(M) != 4L) stop("tables must have four columns: a, b, c, d")
  a <- as.numeric(M[, 1]); b <- as.numeric(M[, 2])
  cc <- as.numeric(M[, 3]); d <- as.numeric(M[, 4])
  nk <- a + b + cc + d
  keep <- nk != 0
  a <- a[keep]; b <- b[keep]; cc <- cc[keep]; d <- d[keep]; nk <- nk[keep]
  num <- sum(a * d / nk)
  den <- sum(b * cc / nk)
  P <- sum((a + d) * a * d / nk^2)
  Q <- sum(((a + d) * b * cc + (b + cc) * a * d) / nk^2)
  R <- sum((b + cc) * b * cc / nk^2)
  S <- num
  if (den == 0) stop("Denominator is zero")
  or_mh <- num / den
  Tt <- den
  var_ln <- P / (2 * S^2) + Q / (2 * S * Tt) + R / (2 * Tt^2)
  se_ln <- if (var_ln > 0) sqrt(var_ln) else 0
  z <- .s03qnorm((1 + as.numeric(confidence)) / 2)
  ln_or <- log(or_mh)
  .t1_result(measure = "OR_MH", estimate = or_mh, se = se_ln,
             ci_lower = exp(ln_or - z * se_ln),
             ci_upper = exp(ln_or + z * se_ln),
             n = sum(nk), n_strata = length(nk))
}
