.pwa_bisect <- function(fn, lo, hi, tol = 1e-12, max_iter = 300) {
  flo <- fn(lo)
  fhi <- fn(hi)
  if (flo == 0) return(lo)
  if (fhi == 0) return(hi)
  if ((flo > 0) == (fhi > 0)) stop(sprintf("no solution in [%g, %g]", lo, hi))
  for (it in seq_len(max_iter)) {
    mid <- 0.5 * (lo + hi)
    if (hi - lo <= tol * max(1, abs(mid))) break
    fm <- fn(mid)
    if (fm == 0) return(mid)
    if ((fm > 0) == (flo > 0)) {
      lo <- mid
      flo <- fm
    } else {
      hi <- mid
    }
  }
  0.5 * (lo + hi)
}

.pwa_up <- function(fn, lo, hi_max) {
  hi <- 2 * lo
  while (fn(hi) < 0 && hi < hi_max) {
    lo <- hi
    hi <- min(2 * hi, hi_max)
  }
  .pwa_bisect(fn, lo, hi)
}

.pwa_t <- function(n, delta, sd, alpha, tsample, tside, strict) {
  nu <- max(1e-7, n - 1) * tsample
  qu <- stats::qt(alpha / tside, nu, lower.tail = FALSE)
  ncp <- sqrt(n / tsample) * delta / sd
  p <- stats::pt(qu, nu, ncp = ncp, lower.tail = FALSE)
  if (strict && tside == 2) p <- p + stats::pt(-qu, nu, ncp = ncp)
  p
}

.pwa_prop <- function(n, p1, p2, alpha, tside, strict) {
  qu <- stats::qnorm(alpha / tside, lower.tail = FALSE)
  d <- abs(p1 - p2)
  pbar <- (p1 + p2) / 2
  s0 <- sqrt(2 * pbar * (1 - pbar))
  s1 <- sqrt(p1 * (1 - p1) + p2 * (1 - p2))
  p <- stats::pnorm((sqrt(n) * d - qu * s0) / s1)
  if (strict && tside == 2) p <- p + stats::pnorm((sqrt(n) * d + qu * s0) / s1, lower.tail = FALSE)
  p
}

.pwa_h <- function(n, p1, p2, alpha, tside) {
  h <- abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
  dl <- h * sqrt(n / 2)
  qu <- stats::qnorm(alpha / tside, lower.tail = FALSE)
  p <- stats::pnorm(qu - dl, lower.tail = FALSE)
  if (tside == 2) p <- p + stats::pnorm(-qu - dl)
  p
}

.pwa_anova <- function(ntot, k, f, alpha, df1 = NULL) {
  d1 <- if (is.null(df1)) k - 1 else df1
  d2 <- if (is.null(df1)) ntot - k else ntot - d1 - 1
  if (d1 <= 0 || d2 <= 0) stop("degrees of freedom must be positive")
  stats::pf(stats::qf(1 - alpha, d1, d2), d1, d2, ncp = f^2 * ntot, lower.tail = FALSE)
}

.pwa_side <- function(alternative) {
  s <- switch(alternative, "two-sided" = 2, "one-sided" = 1, greater = 1, NULL)
  if (is.null(s)) stop("alternative must be 'two-sided' or 'one-sided'")
  s
}

#' Power analysis for t, proportion and ANOVA F tests
#'
#' R arm of the Python modules \code{morie.fn.pwr_t}, \code{pwr_p},
#' \code{pwr_av} and \code{i_pwr}; the missing quantity is found by
#' bisection to 1e-12. \code{PowerTTest}: the power function of
#' \code{stats::power.t.test} (noncentral t, ncp = sqrt(n/tsample) delta/sd);
#' \code{strict = TRUE} (default) includes the far rejection tail of a
#' two-sided test, \code{FALSE} is R's default. \code{PowerPropTest}: the
#' Fleiss normal approximation of \code{stats::power.prop.test}, or Cohen's
#' arcsine h (\code{pwr::pwr.2p.test}). \code{PowerAnova}: one-way ANOVA
#' power with Cohen's f (ncp = f^2 k n). \code{CalculateInteractionPower}:
#' noncentral-F power with ncp = f^2 N on (df1, N - df1 - 1).
#'
#' @param n Sample size per group (\code{PowerTTest}, \code{PowerPropTest},
#'   \code{PowerAnova}).
#' @param delta True difference in means.
#' @param sd Standard deviation.
#' @param alpha Significance level.
#' @param power Target power.
#' @param alternative \code{"two-sided"} or \code{"one-sided"}.
#' @param type \code{"two-sample"}, \code{"one-sample"} or \code{"paired"}.
#' @param strict Include the far rejection tail of a two-sided test.
#' @param p1,p2 Proportions.
#' @param method \code{"fleiss"} or \code{"cohen_h"}.
#' @param k Number of groups.
#' @param f Cohen's f.
#' @param sample_size Total sample size N.
#' @param effect_size Cohen's f of the tested terms.
#' @param df1 Numerator degrees of freedom.
#' @return The missing quantity (a number).
#' @references Cohen, J. (1988). Statistical Power Analysis for the
#'   Behavioral Sciences, 2nd ed. Erlbaum.
#'
#'   Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions,
#'   2nd ed. Wiley.
#'
#'   Faul, F., Erdfelder, E., Lang, A.-G. and Buchner, A. (2007). G*Power
#'   3. Behavior Research Methods 39, 175-191.
#' @examples
#' PowerTTest(n = 20, delta = 1)
#' PowerPropTest(n = 100, p1 = 0.5, p2 = 0.7)
#' PowerAnova(n = 20, k = 3, f = 0.25)
#' CalculateInteractionPower(200)
#' @export
PowerTTest <- function(n = NULL, delta = NULL, sd = 1, alpha = 0.05, power = NULL, alternative = "two-sided",
                       type = "two-sample", strict = TRUE) {
  if (sum(vapply(list(n, delta, power), is.null, NA)) != 1) stop("Exactly one of n, delta, or power must be NULL.")
  if (sd <= 0) stop("sd must be > 0")
  tside <- .pwa_side(alternative)
  tsample <- switch(type, "two-sample" = 2, "one-sample" = 1, paired = 1)
  if (!is.null(delta) && tside == 2) delta <- abs(delta)
  pw <- function(nn, dd) .pwa_t(nn, dd, sd, alpha, tsample, tside, strict)
  if (is.null(power)) return(pw(n, delta))
  if (is.null(n)) return(.pwa_up(function(v) pw(v, delta) - power, 2, 1e7))
  .pwa_up(function(v) pw(n, v) - power, sd * 1e-7, sd * 1e7)
}

#' @rdname PowerTTest
#' @export
PowerPropTest <- function(n = NULL, p1 = NULL, p2 = NULL, alpha = 0.05, power = NULL, alternative = "two-sided",
                          strict = TRUE, method = "fleiss") {
  if (is.null(p1) || is.null(p2)) stop("p1 and p2 must both be provided.")
  if (is.null(n) == is.null(power)) stop("Provide exactly one of (n, power).")
  tside <- .pwa_side(alternative)
  pw <- switch(method,
               fleiss = function(nn) .pwa_prop(nn, p1, p2, alpha, tside, strict),
               cohen_h = function(nn) .pwa_h(nn, p1, p2, alpha, tside),
               stop("method must be 'fleiss' or 'cohen_h'"))
  if (is.null(power)) return(pw(n))
  .pwa_up(function(v) pw(v) - power, 1, 1e7)
}

#' @rdname PowerTTest
#' @export
PowerAnova <- function(n = NULL, k = NULL, f = NULL, alpha = 0.05, power = NULL) {
  if (sum(vapply(list(n, k, f, power), is.null, NA)) != 1) stop("Exactly one of n, k, f, or power must be NULL.")
  pw <- function(nn, kk, ff) .pwa_anova(nn * kk, kk, ff, alpha)
  if (is.null(power)) return(pw(n, k, f))
  if (is.null(n)) return(.pwa_up(function(v) pw(v, k, f) - power, 1 + 1e-9, 1e7))
  if (is.null(f)) return(.pwa_up(function(v) pw(n, k, v) - power, 1e-8, 20))
  for (kk in 2:199) if (pw(n, kk, f) >= power) return(kk)
  stop("Could not find k in [2, 200) achieving the desired power.")
}

#' @rdname PowerTTest
#' @export
CalculateInteractionPower <- function(sample_size, alpha = 0.05, effect_size = 0.2, df1 = 1) {
  .pwa_anova(sample_size, 2, effect_size, alpha, df1 = df1)
}
