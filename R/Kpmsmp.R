# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hall-Wellner simultaneous confidence band for a Kaplan-Meier curve
#'
#' With sigma^2(t) the Greenwood sum, the band over the observed range is
#' S(t) +/- h_alpha n^\{-1/2\} (1 + n sigma^2(t)) S(t), where h_alpha is
#' the upper alpha point of the supremum of a Brownian bridge.  h is
#' found by bisection on the Kolmogorov series rather than read from a
#' table, so any alpha may be used; at alpha = 0.05 it reproduces the
#' tabulated 1.3581.  Unlike the pointwise Greenwood interval the band
#' holds simultaneously over t, so it is strictly wider.
#'
#' Formula: P(sup|B| > h) = 2 sum_k (-1)^\{k+1\} exp(-2 k^2 h^2) = alpha.
#'
#' @param fit Risk table with \code{time}, \code{n_risk}, \code{n_event}.
#' @param alpha Simultaneous error rate.
#' @return List with \code{estimate} (half-width at the last time),
#'   \code{time}, \code{surv}, \code{half_width}, \code{sigma2},
#'   \code{lower}, \code{upper}, \code{h}, \code{alpha}, \code{n_times},
#'   \code{n_risk_start}, \code{n}, \code{method}.
#' @references Hall and Wellner (1980), Confidence bands for a survival
#'   curve from censored data, Biometrika 67(1):133-143.
#'   \doi{10.1093/biomet/67.1.133}
#' @export
Kpmsmp <- function(fit, alpha) {
  rt <- .kpm_risk_table(fit)
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("km_simultaneous_band: alpha must lie in (0, 1)")
  m <- rt$m; nr <- rt$nr; d <- rt$d; n <- nr[1]
  h <- .kpm_hw_crit(a)
  S <- numeric(m); sig2 <- numeric(m); s <- 1; v <- 0
  for (j in seq_len(m)) {
    s <- s * (1 - d[j] / nr[j])
    v <- if (nr[j] > d[j]) v + d[j] / (nr[j] * (nr[j] - d[j])) else Inf
    S[j] <- s; sig2[j] <- v
  }
  half <- ifelse(is.finite(sig2), h / sqrt(n) * (1 + n * sig2) * S, NaN)
  lo <- pmax(S - half, 0); hi <- pmin(S + half, 1)
  .t1_result(estimate = half[m], time = rt$t, surv = S, half_width = half,
             sigma2 = sig2, lower = lo, upper = hi, h = h, alpha = a,
             n_times = m, n_risk_start = n, n = m,
             method = "S(t) +/- h_alpha n^-1/2 (1 + n sigma^2(t)) S(t), Hall & Wellner (1980)")
}

#' @keywords internal
#' @noRd
.kpm_sup_bb_tail <- function(h) {
  tot <- 0
  for (k in 1:100) {
    term <- exp(-2 * k * k * h * h)
    tot <- tot + if (k %% 2 == 1) term else -term
    if (term < 1e-18) break
  }
  2 * tot
}

#' @keywords internal
#' @noRd
.kpm_hw_crit <- function(alpha) {
  lo <- 0.05; hi <- 5
  for (i in 1:200) {
    mid <- 0.5 * (lo + hi)
    if (.kpm_sup_bb_tail(mid) > alpha) lo <- mid else hi <- mid
  }
  0.5 * (lo + hi)
}
