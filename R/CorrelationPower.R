#' Power of the correlation test and Cohen's benchmarks
#'
#' \code{PwrRTest}: power, sample size, correlation or level of the test of
#' zero correlation, with exactly one argument \code{NULL} (Cohen's
#' Fisher-z approximation as in \code{pwr.r.test}). \code{CohenRMagnitude}:
#' Cohen's small (.10), medium (.30) and large (.50) labels. Identical to
#' the Python arm \code{morie.fn.corrpower}.
#'
#' @param n Sample size.
#' @param r Correlation under the alternative.
#' @param sig_level Significance level.
#' @param power Power.
#' @param alternative One of "two.sided", "less", "greater".
#' @return List (character label for \code{CohenRMagnitude}).
#' @references Cohen, J. (1988). Statistical Power Analysis for the
#'   Behavioral Sciences, 2nd edn. Lawrence Erlbaum.
#'
#'   Lovett, B. J. (2021). Practical Psychometrics: A Guide for Test Users.
#'   Guilford Press.
#' @examples
#' PwrRTest(n = 100, r = 0.3)$power
#' PwrRTest(r = 0.3, power = 0.8)$n
#' CohenRMagnitude(0.35)
#' @export
PwrRTest <- function(n = NULL, r = NULL, sig_level = 0.05, power = NULL, alternative = "two.sided") {
  if (sum(vapply(list(n, r, power, sig_level), is.null, TRUE)) != 1) {
    stop("exactly one of n, r, power and sig_level must be NULL")
  }
  if (!alternative %in% c("two.sided", "less", "greater")) stop("alternative must be 'two.sided', 'less' or 'greater'")
  if (!is.null(n) && n < 4) stop("number of observations must be at least 4")
  pw <- function(n, r, a) {
    if (alternative == "two.sided") {
      r <- abs(r)
      ttt <- qt(1 - a / 2, n - 2)
    } else {
      if (alternative == "less") r <- -r
      ttt <- qt(1 - a, n - 2)
    }
    rc <- sqrt(ttt * ttt / (ttt * ttt + n - 2))
    zr <- atanh(r) + r / (2 * (n - 1))
    zrc <- atanh(rc)
    p <- pnorm((zr - zrc) * sqrt(n - 3))
    if (alternative == "two.sided") p <- p + pnorm((-zr - zrc) * sqrt(n - 3))
    p
  }
  bis <- function(g, lo, hi) {
    glo <- g(lo)
    for (it in 1:200) {
      mid <- 0.5 * (lo + hi)
      gm <- g(mid)
      if ((gm > 0) == (glo > 0)) {
        lo <- mid
        glo <- gm
      } else {
        hi <- mid
      }
      if (hi - lo <= 1e-15 * max(1, abs(mid))) break
    }
    0.5 * (lo + hi)
  }
  if (is.null(power)) {
    power <- pw(n, r, sig_level)
  } else if (is.null(n)) {
    n <- bis(function(v) pw(v, r, sig_level) - power, 4 + 1e-10, 1e9)
  } else if (is.null(r)) {
    lo <- if (alternative == "two.sided") 1e-10 else -1 + 1e-10
    r <- bis(function(v) pw(n, v, sig_level) - power, lo, 1 - 1e-10)
  } else {
    sig_level <- bis(function(v) pw(n, r, v) - power, 1e-10, 1 - 1e-10)
  }
  list(n = n, r = r, sig_level = sig_level, power = power, alternative = alternative)
}

#' @rdname PwrRTest
#' @export
CohenRMagnitude <- function(r) {
  a <- abs(r)
  if (a > 1) stop("a correlation lies in [-1, 1]")
  if (a < 0.1) "negligible" else if (a < 0.3) "small" else if (a < 0.5) "medium" else "large"
}
