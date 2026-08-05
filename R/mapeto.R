# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pool 2x2 tables when the events are rare enough to break the others
#'
#' With a zero cell the ordinary log odds ratio is undefined and the usual
#' repair -- adding 0.5 everywhere -- biases the pooled estimate. Peto's
#' estimator never forms a ratio per study: it accumulates
#' observed-minus-expected counts against their hypergeometric variance,
#' so a zero cell contributes without special handling. The price is a
#' bias when the odds ratio is far from one or the groups are badly
#' unbalanced.
#'
#' Formula: \code{ln OR = sum(O_i - E_i)/sum(V_i)} with \code{O = a},
#' \code{E = (a+b)(a+c)/N} and
#' \code{V = (a+b)(c+d)(a+c)(b+d)/(N^2 (N-1))};
#' \code{se = 1/sqrt(sum V)} -- Peto et al. (1977), Appendix.
#'
#' @param a,b,c,d Per-study cells: events and non-events in the treated
#'   arm, then in the control arm.
#' @param level Confidence level.
#' @return List with \code{OR}, \code{log_OR}, \code{se_log}, \code{ci},
#'   \code{O_E}, \code{V}, \code{k}.
#' @references Peto, R. et al. (1977). British Journal of Cancer
#'   35(1):1-39. \doi{10.1038/bjc.1977.1}.
#' @export
Mapeto <- function(a, b, c, d, level = 0.95) {
  A <- as.numeric(a); B <- as.numeric(b)
  C <- as.numeric(c); D <- as.numeric(d)
  k <- length(A)
  if (k == 0L) stop("no tables")
  if (length(B) != k || length(C) != k || length(D) != k)
    stop("the four cell vectors must have equal length")
  if (any(c(A, B, C, D) < 0)) stop("cell counts must be non-negative")
  n <- A + B + C + D
  if (any(n <= 1)) stop("each table needs at least two observations")
  e <- (A + B) * (A + C) / n
  v <- (A + B) * (C + D) * (A + C) * (B + D) / (n^2 * (n - 1))
  oe <- sum(A - e); vv <- sum(v)
  if (vv <= 0)
    stop("the pooled variance is zero; no table is informative")
  lor <- oe / vv; se <- 1 / sqrt(vv)
  z <- .s03qnorm(1 - (1 - as.numeric(level)) / 2)
  .t1_result(OR = exp(lor), log_OR = lor, se_log = se,
             ci = c(exp(lor - z * se), exp(lor + z * se)),
             O_E = oe, V = vv, k = k,
             method = "Peto one-step pooled odds ratio")
}
