# SPDX-License-Identifier: AGPL-3.0-or-later

#' Unmatched case-control OR
#'
#' Formula: OR = ad/bc from the 2x2
#'
#' a exposed cases, b unexposed cases, c exposed controls, d unexposed
#' controls.  In a case-control design the risks themselves are not
#' identified -- sampling is on the outcome -- but the odds ratio is,
#' which is Cornfield's point.  The interval uses Woolf's standard error
#' on the log scale, sqrt(1/a + 1/b + 1/c + 1/d).
#'
#' @param cases Either (a, b) counts, or a 0/1 exposure indicator.
#' @param controls Either (c, d) counts, or a 0/1 exposure indicator.
#' @param exposed,unexposed Ignored; kept for the stub signature.
#' @param conf Confidence level.
#' @return List with \code{estimate}, \code{a}, \code{b}, \code{c},
#'   \code{d}, \code{log_or}, \code{se_log}, \code{ci_low},
#'   \code{ci_high}, \code{chisq}, \code{significant}, \code{n},
#'   \code{method}.
#' @references Cornfield (1951), Proc. 2nd Berkeley Symp. 4:135-148;
#'   Woolf (1955), Ann. Human Genetics 19(4):251-253.
#' @export
#' @examples
#' Ccdsgn(cases = list(a = 1, b = 2), controls = list(a = 1, b = 2))
Ccdsgn <- function(cases, controls, exposed = NULL, unexposed = NULL,
                   conf = 0.95) {
  cs <- .s03vec(cases)
  ct <- .s03vec(controls)
  if (!length(cs) || !length(ct))
    stop("empty input: cases and controls are required")
  if (length(cs) == 2L && all(cs == as.integer(cs)) && all(cs >= 0)) {
    a <- cs[1]
    b <- cs[2]
  } else {
    if (any(!(cs %in% c(0, 1))))
      stop("cases must be counts (a, b) or 0/1 indicators")
    a <- sum(cs)
    b <- length(cs) - a
  }
  if (length(ct) == 2L && all(ct == as.integer(ct)) && all(ct >= 0)) {
    cc <- ct[1]
    d <- ct[2]
  } else {
    if (any(!(ct %in% c(0, 1))))
      stop("controls must be counts (c, d) or 0/1 indicators")
    cc <- sum(ct)
    d <- length(ct) - cc
  }
  if (!(conf > 0 && conf < 1)) stop("conf must lie strictly in (0, 1)")
  n <- a + b + cc + d
  if (n <= 0) stop("the 2x2 table is empty")
  orr <- if (b * cc == 0) (if (a * d > 0) Inf else NaN) else (a * d) / (b * cc)
  if (min(a, b, cc, d) > 0) {
    se <- sqrt(1 / a + 1 / b + 1 / cc + 1 / d)
    lo <- log(orr)
    z <- .s03qnorm(0.5 + conf / 2)
    ci_l <- exp(lo - z * se)
    ci_h <- exp(lo + z * se)
  } else {
    se <- NaN
    lo <- NaN
    ci_l <- NaN
    ci_h <- NaN
  }
  r1 <- a + b
  r2 <- cc + d
  c1 <- a + cc
  c2 <- b + d
  chi <- if (min(r1, r2, c1, c2) > 0)
    n * (a * d - b * cc)^2 / (r1 * r2 * c1 * c2) else NaN
  .t1_result(estimate = orr, a = a, b = b, c = cc, d = d, log_or = lo,
             se_log = se, ci_low = ci_l, ci_high = ci_h, chisq = chi,
             significant = as.integer(!is.nan(chi) && chi > 3.841458820694124),
             n = n, method = "unmatched case-control odds ratio")
}
