# Seasonal Hybrid ESD (S-H-ESD) anomaly detection.
# Source: Hochenbaum, J., Vallis, O. S. and Kejariwal, A. (2017),
# Automatic anomaly detection in the cloud via statistical learning,
# arXiv:1704.07706 (the method behind Twitter's AnomalyDetection).
# Their Sec. 3.5 combines two pieces:
#   - the generalised extreme studentised deviate test of Rosner, B.
#     (1983), Technometrics 25(2), 165-172, their eqs. (5)-(6): remove
#     the most extreme point up to k times, comparing
#     C_i = max|x - centre| / scale against
#     lambda_i = (n-i) t_{p, n-i-1} /
#                sqrt((n-i-1+t^2)(n-i+1)),
#     with p = 1 - alpha / (2(n-i+1));
#   - the HYBRID robustification of their eqs. (7)-(8): the sample mean
#     and standard deviation are replaced by the median and
#     1.4826 MAD, so that the anomalies being hunted do not inflate
#     the very scale used to detect them.
# The seasonal component is removed by STL first and the MEDIAN (not
# the STL trend) is subtracted, which is their explicit choice: using
# the trend would let a large anomaly bend the trend towards itself
# and hide.
#
# Native implementation mirroring Python morie.fn.ttsAn exactly: same
# Student-t CDF via the regularised incomplete beta, same 200-step
# bisection for the quantile, same first-maximum tie-break in the ESD
# loop, and the same "last i with C_i > lambda_i" anomaly count.

#' .mor_tts_t_cdf
#'
#' A step of the ttsAn_native implementation. Called by \code{morie_t_quantile}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Numeric; combined arithmetically in the body.
#' @param v Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_tts_t_cdf <- function(t, v) {
  x <- v / (v + t * t)
  p <- 0.5 * pbeta(x, v / 2, 0.5)
  if (t >= 0) 1 - p else p
}

#' Student-t quantile by bisection on the incomplete beta
#'
#' Inverse of the Student-t CDF, used by \code{\link{morie_ttsAn}} for
#' the generalised ESD critical values.
#'
#' @param p Probability in (0, 1).
#' @param v Degrees of freedom.
#' @return The quantile.
#' @references Rosner, B. (1983). Percentage points for a generalized
#'   ESD many-outlier procedure. Technometrics, 25(2), 165-172.
#' @export
morie_t_quantile <- function(p, v) {
  if (!(p > 0 && p < 1)) stop("p in (0,1) required")
  if (p == 0.5) return(0)
  neg <- p < 0.5
  pp <- if (neg) 1 - p else p
  lo <- 0; hi <- 1
  while (.mor_tts_t_cdf(hi, v) < pp) {
    hi <- hi * 2
    if (hi > 1e300) break
  }
  for (it in seq_len(200L)) {
    mid <- 0.5 * (lo + hi)
    if (.mor_tts_t_cdf(mid, v) < pp) lo <- mid else hi <- mid
  }
  q <- 0.5 * (lo + hi)
  if (neg) -q else q
}

#' .mor_tts_median
#'
#' A step of the ttsAn_native implementation. Called by \code{.mor_tts_esd}, \code{morie_ttsAn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_tts_median <- function(v) {
  s <- sort(v); n <- length(s); mid <- n %/% 2L
  if (n %% 2L == 1L) s[mid + 1L] else 0.5 * (s[mid] + s[mid + 1L])
}

#' .mor_tts_esd
#'
#' A step of the ttsAn_native implementation. Called by \code{morie_ttsAn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param res A vector; its length is taken.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @param alpha Numeric; combined arithmetically in the body.
#' @param hybrid A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{anoms}, \code{stats}, \code{lams}.
#' @export
.mor_tts_esd <- function(res, k, alpha, hybrid = TRUE) {
  n <- length(res)
  idx <- seq_len(n) - 1L
  vals <- res
  removed <- integer(0); stats <- numeric(0); lams <- numeric(0)
  n_anom <- 0L
  for (i in seq_len(k)) {
    if (hybrid) {
      ctr <- .mor_tts_median(vals)
      scale <- 1.4826 * .mor_tts_median(abs(vals - ctr))
    } else {
      ctr <- mean(vals)
      scale <- sqrt(sum((vals - ctr)^2) / (length(vals) - 1))
    }
    if (scale <= 0) break
    d <- abs(vals - ctr)
    best <- 1L; bdev <- -1
    for (j in seq_along(vals)) if (d[j] > bdev) { bdev <- d[j]; best <- j }
    C <- bdev / scale
    p <- 1 - alpha / (2 * (n - i + 1))
    tq <- morie_t_quantile(p, n - i - 1)
    lam <- (n - i) * tq / sqrt((n - i - 1 + tq * tq) * (n - i + 1))
    stats <- c(stats, C); lams <- c(lams, lam)
    removed <- c(removed, idx[best])
    if (C > lam) n_anom <- i
    vals <- vals[-best]; idx <- idx[-best]
    if (length(vals) < 3L) break
  }
  list(anoms = if (n_anom > 0L) removed[seq_len(n_anom)] else integer(0),
       stats = stats, lams = lams)
}

#' Seasonal Hybrid ESD anomaly detection
#'
#' Removes the STL seasonal component and the series median, then runs
#' the robust generalised ESD test of Hochenbaum, Vallis and Kejariwal
#' (2017) on the residual.  Up to \code{k} anomalies can be found; the
#' test reports the largest \eqn{i} for which the \eqn{i}-th extreme
#' deviate exceeds its critical value, so the anomalies are those
#' removed up to that point.
#'
#' @param x Numeric series.
#' @param period Seasonal period passed to
#'   \code{\link{morie_stl_decompose}}.
#' @param k Maximum number of anomalies; \code{NULL} uses
#'   \code{floor(0.02 n)}, capped at \code{0.49 n}.
#' @param alpha Test level.
#' @param s_window Seasonal loess span for the STL step.
#' @param hybrid \code{TRUE} (default) uses median and MAD, the
#'   hybrid form of their eqs. (7)-(8); \code{FALSE} uses the classic
#'   Rosner mean and standard deviation.  Both routes are available.
#' @param direction \code{"both"} (default), \code{"pos"} or
#'   \code{"neg"} to restrict the search to one tail.
#' @return A list with \code{anomalies} (1-based indices),
#'   \code{n_anomalies}, \code{statistics}, \code{critical_values},
#'   \code{residual}, \code{k}, \code{alpha}, \code{estimate},
#'   \code{n}, \code{method}.
#' @references Hochenbaum, J., Vallis, O. S. and Kejariwal, A. (2017).
#'   Automatic anomaly detection in the cloud via statistical
#'   learning. arXiv:1704.07706.
#' @export
morie_ttsAn <- function(x, period, k = NULL, alpha = 0.05, s_window = 7L,
                        hybrid = TRUE, direction = "both") {
  xs <- as.numeric(x)
  n <- length(xs)
  if (is.null(k)) k <- max(1, floor(0.02 * n))
  k <- as.integer(k)
  if (k > as.integer(0.49 * n)) k <- as.integer(0.49 * n)
  if (k < 1L) stop("series too short for ESD")
  fit <- morie_stl_decompose(xs, period, s_window = s_window)
  S <- fit$seasonal
  med <- .mor_tts_median(xs)
  R <- xs - S - med
  Ruse <- if (direction == "pos") pmax(R, 0) else
    if (direction == "neg") pmin(R, 0) else R
  e <- .mor_tts_esd(Ruse, k, alpha, hybrid = hybrid)
  anoms1 <- sort(e$anoms + 1L)
  list(anomalies = as.numeric(anoms1), n_anomalies = length(anoms1),
       statistics = e$stats, critical_values = e$lams, residual = R,
       k = k, alpha = alpha, estimate = as.numeric(anoms1), n = n,
       method = "Seasonal Hybrid ESD (Hochenbaum-Vallis-Kejariwal 2017)")
}
