# SPDX-License-Identifier: AGPL-3.0-or-later

#' Chernozhukov-Lee-Rosen intersection bounds with independent cells
#'
#' The target is the minimum over cells of the conditional means. Each
#' cell is precision-corrected before the minimum is taken,
#' \eqn{\hat\theta(p) = \min_{v \in \hat V} (m_v + k_{\hat V}(p) s_v)}, where
#' for independent cells \eqn{k_S(p) = \Phi^{-1}(p^{1/|S|})} is the
#' p-quantile of the maximum of \eqn{|S|} independent standard normals and the
#' contact set keeps the cells with
#' \eqn{m_v \le \min_u (m_u + k_V(\gamma_n) s_u) + 2 k_V(\gamma_n) s_v},
#' \eqn{\gamma_n = 1 - 0.1/\log n}. Identical to the Python arm
#' \code{morie.fn.chrbnd.chernozhukov_rosen_bounds}. (This function used to
#' subtract a multiple of the standard error, the wrong direction.)
#'
#' @param y Numeric outcome.
#' @param X Ignored (signature parity).
#' @param instrument Cell label per observation; \code{NULL} is one cell.
#' @param alpha One-sided level of the upper confidence bound.
#' @param gamma Contact-set level; default \eqn{1 - 0.1/\log n}.
#' @param beta Unused (signature parity).
#' @return A list with \code{estimate} and \code{bound} (the one-sided upper
#'   confidence bound), \code{hmu_estimate} (half-median-unbiased),
#'   \code{naive_min}, \code{cells}, \code{means}, \code{ses},
#'   \code{contact_set} (zero-based), \code{k_alpha}, \code{k_gamma},
#'   \code{n_cells}, \code{n} and \code{method}.
#' @references Chernozhukov, V., Lee, S. and Rosen, A. M. (2013). Intersection
#'   bounds: estimation and inference. Econometrica 81, 667-737.
#' @examples
#' Chrbnd(c(3, 3.5, 2.8, 1, 1.6, 1.2, 5, 4.1),
#'   instrument = c(0, 0, 0, 1, 1, 1, 2, 2))$bound
#' @export
Chrbnd <- function(y, X = NULL, instrument = NULL, alpha = 0.05,
                   gamma = NULL, beta = 0.1) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0) stop("empty input: y has no observations")
  if (!(alpha > 0 && alpha < 1)) stop("alpha must lie strictly in (0, 1)")
  ids <- if (is.null(instrument)) rep(0, n) else instrument
  if (length(ids) != n) stop("y and instrument must have the same length")
  keys <- unique(ids)
  V <- length(keys)
  means <- ses <- sizes <- numeric(V)
  for (j in seq_len(V)) {
    v <- yv[ids == keys[j]]
    m <- length(v)
    if (m < 2) stop("every instrument cell needs two observations")
    means[j] <- sum(v) / m
    ses[j] <- sqrt(sum((v - means[j])^2) / (m - 1)) / sqrt(m)
    sizes[j] <- m
  }
  if (is.null(gamma)) gamma <- if (n > 1) 1 - 0.1 / log(n) else 0.9
  if (!(gamma > 0 && gamma < 1)) stop("gamma must lie strictly in (0, 1)")
  kmax <- function(p, m) stats::qnorm(p^(1 / m))
  kg <- kmax(gamma, V)
  thr <- min(means + kg * ses)
  contact <- which(means <= thr + 2 * kg * ses)
  ka <- kmax(1 - alpha, length(contact))
  kh <- kmax(0.5, length(contact))
  bound <- min(means[contact] + ka * ses[contact])
  list(estimate = bound, bound = bound, hmu_estimate = min(means[contact] + kh * ses[contact]),
       naive_min = min(means), cells = sizes, means = means, ses = ses,
       contact_set = contact - 1L, k_alpha = ka, k_gamma = kg, n_cells = V, n = n,
       method = "Chernozhukov-Lee-Rosen intersection bounds, independent cells")
}
