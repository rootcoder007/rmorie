# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Crump-trimmed Hajek IPW ATE (Causipw). Bit-identical mirror of
# src/morie/fn/causipw.py.

#' IPW average treatment effect on the Crump-trimmed overlap sample
#'
#' Discards units with estimated propensity score outside the interval
#' from alpha to 1 - alpha (Crump, Hotz, Imbens and Mitnik 2009;
#' rule-of-thumb cutoff 0.1) and estimates the ATE on the retained
#' sample by the ratio (Hajek) IPW form
#' \eqn{\tau = \frac{\sum T Y / e}{\sum T / e} -
#' \frac{\sum (1-T) Y / (1-e)}{\sum (1-T)/(1-e)}}.
#' With \code{alpha = NULL} the optimal cutoff of their Corollary 5.1
#' (homoskedastic case) is computed from the scores: alpha = 0 when
#' \eqn{\sup 1/(e(1-e)) \le 2 E[1/(e(1-e))]}, otherwise
#' \eqn{1/(\alpha(1-\alpha)) = 2 E[1/(e(1-e)) \mid 1/(e(1-e)) \le
#' 1/(\alpha(1-\alpha))]}. No analytic standard error is reported;
#' inference on the trimmed estimand is left to the caller.
#'
#' @param treat Binary treatment indicator, 0/1.
#' @param y Outcome.
#' @param ps Estimated propensity scores strictly inside (0, 1).
#' @param alpha Trimming cutoff in 0 to 0.5 (default 0.1), or NULL for
#'   the Corollary 5.1 optimal cutoff.
#' @return List with \code{estimate}, \code{alpha}, \code{n},
#'   \code{n_kept}, \code{n_treat_kept}, \code{n_control_kept},
#'   \code{mean_treated}, \code{mean_control}, \code{method}.
#' @references Crump, R. K., Hotz, V. J., Imbens, G. W. and Mitnik,
#'   O. A. (2009), Dealing with limited overlap in estimation of
#'   average treatment effects, Biometrika 96(1), 187-199,
#'   doi:10.1093/biomet/asn055; Corollary 5.1 and Section 5 in the
#'   NBER TWP 330 (2006) version; local copy
#'   fetched-wave3/crump-hotz-imbens-mitnik-2009-limited-overlap-biometrika.pdf.
#' @export
#' @examples
#' set.seed(1)
#' Causipw(treat = rbinom(50, 1, 0.5), y = rnorm(50), ps = runif(50, 0.2, 0.8))
Causipw <- function(treat, y, ps, alpha = 0.1) {
  t <- as.numeric(treat); yv <- as.numeric(y); e <- as.numeric(ps)
  n <- length(t)
  if (length(yv) != n || length(e) != n) {
    stop("treat, y, ps must have equal length", call. = FALSE)
  }
  if (!all(t %in% c(0, 1))) stop("treat must be binary 0/1", call. = FALSE)
  if (any(e <= 0) || any(e >= 1)) {
    stop("propensity scores must lie strictly in (0, 1)", call. = FALSE)
  }
  if (is.null(alpha)) {
    k <- sort(1 / (e * (1 - e)))
    if (k[n] <= 2 * sum(k) / n) {
      alpha <- 0
    } else {
      best <- NA_real_
      csum <- 0
      for (jj in seq_len(n)) {
        csum <- csum + k[jj]
        gamma <- 2 * csum / jj
        if (k[jj] <= gamma) best <- gamma
      }
      alpha <- if (is.na(best) || best < 4) 0 else 0.5 - sqrt(0.25 - 1 / best)
    }
  }
  alpha <- as.numeric(alpha)
  if (alpha < 0 || alpha >= 0.5) {
    stop("alpha must lie in 0 <= alpha < 0.5", call. = FALSE)
  }
  keep <- which(e >= alpha & e <= 1 - alpha)
  k1 <- keep[t[keep] == 1]
  k0 <- keep[t[keep] == 0]
  if (length(k1) == 0L || length(k0) == 0L) {
    stop("trimming removed an entire treatment arm", call. = FALSE)
  }
  w1 <- 1 / e[k1]
  w0 <- 1 / (1 - e[k0])
  mu1 <- sum(w1 * yv[k1]) / sum(w1)
  mu0 <- sum(w0 * yv[k0]) / sum(w0)
  list(estimate = mu1 - mu0, alpha = alpha, n = n,
       n_kept = length(keep),
       n_treat_kept = length(k1), n_control_kept = length(k0),
       mean_treated = mu1, mean_control = mu0,
       method = "Crump et al. (2009) overlap trimming + Hajek IPW")
}
