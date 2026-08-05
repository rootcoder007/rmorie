# SPDX-License-Identifier: AGPL-3.0-or-later

#' Chernozhukov-Lee-Rosen intersection bounds
#'
#' Formula: theta = inf_v m(v); precision-corrected critical values
#'
#' An upper bound valid for every cell v of the instrument is valid at
#' their MINIMUM, but plugging in the sample minimum is biased downward
#' because the noisiest cell wins.  The half-median-unbiased estimator
#' keeps only the cells within precision of the minimum -- the estimated
#' contact set -- and takes the minimum over that set of m_v - k se_v.
#' With one cell it reduces to the ordinary one-sided interval, and with
#' zero sampling noise to the plain minimum.
#'
#' @param y Outcome.
#' @param X Ignored; kept for the stub signature.
#' @param instrument Cell label per observation, or NULL.
#' @param alpha One-sided level of the reported bound.
#' @param gamma Precision level of the contact set, or NULL.
#' @param beta Retained for the contact-set width.
#' @return List with \code{estimate}, \code{bound}, \code{naive_min},
#'   \code{cells}, \code{means}, \code{ses}, \code{contact_set},
#'   \code{k_alpha}, \code{n_cells}, \code{n}, \code{method}.
#' @references Chernozhukov, Lee & Rosen (2013), Intersection Bounds,
#'   Econometrica 81(2):667-737.
#' @export
Chrbnd <- function(y, X = NULL, instrument = NULL, alpha = 0.05,
                   gamma = NULL, beta = 0.1) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("empty input: y has no observations")
  if (!(alpha > 0 && alpha < 1))
    stop("alpha must lie strictly in (0, 1)")
  ids <- if (is.null(instrument)) rep(0L, n) else instrument
  if (length(ids) != n) stop("y and instrument must have the same length")
  keys <- unique(ids)
  V <- length(keys)
  means <- numeric(V); ses <- numeric(V); sizes <- numeric(V)
  for (q in seq_len(V)) {
    vals <- yv[ids == keys[q]]
    m <- length(vals)
    if (m < 2L) stop("every instrument cell needs two observations")
    mu <- sum(vals) / m
    sd <- 0
    for (v in vals) sd <- sd + (v - mu)^2
    sd <- sqrt(sd / (m - 1))
    means[q] <- mu
    ses[q] <- sd / sqrt(m)
    sizes[q] <- m
  }
  if (is.null(gamma)) gamma <- if (V > 1L) 1 - 1 / log(V + 1) else 0.9
  if (!(gamma > 0 && gamma < 1))
    stop("gamma must lie strictly in (0, 1)")
  k_gamma <- .s03qnorm(gamma)
  naive <- min(means)
  thr <- min(means + 2 * k_gamma * ses)
  contact <- which(means - 2 * k_gamma * ses <= thr)
  if (!length(contact)) contact <- which.min(means)
  k_alpha <- .s03qnorm(1 - alpha / length(contact))
  bound <- min(means[contact] - k_alpha * ses[contact])
  .t1_result(estimate = bound, bound = bound, naive_min = naive,
             cells = sizes, means = means, ses = ses,
             contact_set = contact - 1L, k_alpha = k_alpha, n_cells = V,
             n = n, method = "Chernozhukov-Lee-Rosen intersection bounds")
}
