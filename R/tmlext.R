# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE with an external comparator arm
#'
#' van der Laan and Rubin (2006), The International Journal of
#' Biostatistics 2(1), art. 11, for the targeting step; Stuart, Cole,
#' Bradshaw and Leaf (2011), The use of propensity scores to assess the
#' generalizability of results from randomized trials, JRSS-A 174(2),
#' 369-386, for the sampling-score reweighting: with S the indicator of
#' being in the current study and p(X) = P(S = 1 | X), external units are
#' weighted by w = p(X)/(1 - p(X)), the odds of study membership, which
#' maps the external population onto the trial population.  Neither was
#' retrievable here as a full text; the weight is quoted in its standard
#' published form.  The borrowed weight is reported, not hidden: `ess` is
#' Kish's effective sample size (Kish 1965, Survey Sampling), (sum w)^2 /
#' sum w^2, so borrowing a hundred external controls at wildly unequal
#' weights shows up as an effective size of a dozen.
#'
#' @param y,D outcome and treatment for the pooled sample.
#' @param X covariates for the pooled sample.
#' @param external indicator, 1 for external units.
#' @param alpha interval level.
#' @return list: estimate, se, ci_lo, ci_hi, ess, n_external, psi_internal,
#'   n, method.
#' @keywords internal
#' @examples
#' Tmleext(c(1, 0, 1, 1, 0, 1), c(1, 0, 1, 0, 1, 0), NULL,
#'         c(0, 0, 0, 1, 1, 1))$ess
#' @export
Tmleext <- function(y, D, X = NULL, external = NULL, alpha = 0.05) {
  yv <- .s03vec(y); d <- .s03vec(D); n <- length(yv)
  S <- 1 - as.numeric(if (!is.null(external)) external else rep(0, n))
  Z <- .s03design(X, n)
  ps <- vapply(.s03matvec(Z, .s03logit(Z, S, 60L)), .s03sigmoid, 0)
  w <- numeric(n)
  for (i in seq_len(n)) {
    w[i] <- if (S[i] > 0.5) 1 else if (ps[i] < 1) ps[i] / (1 - ps[i]) else 0
  }
  we <- w[S < 0.5]
  s1 <- 0; s2 <- 0
  for (x in we) { s1 <- s1 + x; s2 <- s2 + x * x }
  ess <- if (s2 > 0) (s1 * s1) / s2 else 0
  fit <- .s03tmle(yv, d, X)
  ic <- fit$inf
  num <- 0; den <- 0
  for (i in seq_len(n)) {
    num <- num + w[i] * (fit$q1[i] - fit$q0[i]) * fit$scale
    den <- den + w[i]
  }
  psi <- if (den > 0) num / den else NaN
  v <- 0
  for (i in seq_len(n)) v <- v + (w[i] * ic[i])^2
  se <- if (den > 0) sqrt(v / (den * den)) else NaN
  idx <- which(S > 0.5)
  pin <- NaN
  if (length(idx) >= 3L) {
    di <- d[idx]
    if (sum(di) > 0 && sum(di) < length(di)) {
      Xi <- if (!is.null(X)) .s03mat(X)[idx, , drop = FALSE] else NULL
      pin <- .s03tmle(yv[idx], di, Xi)$psi
    }
  }
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = psi, se = se, ci_lo = psi - z * se, ci_hi = psi + z * se,
       ess = ess, n_external = length(we), psi_internal = pin, n = n,
       method = "TMLE with external controls reweighted by the odds of study participation (Stuart et al. 2011); Kish ESS reported")
}
