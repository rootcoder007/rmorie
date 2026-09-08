# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric (kernel) worst-case regression bound
#'
#' The worst-case decomposition is applied at each covariate value with
#' Nadaraya-Watson estimates of \code{E(y | X = x, D = t)} and
#' \code{P(D = t | X = x)}, and the pointwise bounds are averaged over the
#' empirical distribution of \code{X}. As the bandwidth grows the estimator
#' collapses to the unconditional worst-case bound; for any finite bandwidth
#' the averaged interval is no wider.
#'
#' Formula: \code{E_X [ m_1(X) p_1(X) + y_0 (1 - p_1(X)) - m_0(X) p_0(X)
#' - y_1 (1 - p_0(X)) ]} for the lower bound and the mirror expression for
#' the upper, with Gaussian kernel weights.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @param X Scalar conditioning covariate, one value per unit.
#' @param bw Kernel bandwidth, strictly positive.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{bw}, \code{n}.
#' @references Manski, C. F. (2003). Partial Identification of Probability
#'   Distributions. Springer, New York. The conditional worst-case bound is
#'   equation (2.11) of Molinari, F. (2021), Handbook of Econometrics 7A
#'   (arXiv:2004.11751 p. 17), applied at each \code{x}.
#' @export
#' @examples
#' set.seed(1)
#' r <- Bndnpr(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10), bw = 0.5); TRUE
Bndnpr <- function(y, D, X, bw) {
  z <- .bnd_yd(y, D, "Bndnpr")
  xv <- as.numeric(unlist(X))
  n <- length(z$y)
  if (length(xv) != n) stop("Bndnpr: X must have one value per unit")
  h <- as.numeric(bw)[1]
  if (!(h > 0)) stop("Bndnpr: bw must be positive")
  y0 <- min(z$y)
  y1 <- max(z$y)
  slo <- 0
  shi <- 0
  for (i in seq_len(n)) {
    k <- exp(-0.5 * ((xv[i] - xv) / h)^2)
    w1 <- sum(k[z$d == 1])
    w0 <- sum(k[z$d == 0])
    s1 <- sum(k[z$d == 1] * z$y[z$d == 1])
    s0 <- sum(k[z$d == 0] * z$y[z$d == 0])
    wt <- w1 + w0
    p1 <- w1 / wt
    p0 <- w0 / wt
    m1 <- if (w1 > 0) s1 / w1 else 0
    m0 <- if (w0 > 0) s0 / w0 else 0
    a1 <- .bnd_wc_arm(m1, p1, y0, y1)
    a0 <- .bnd_wc_arm(m0, p0, y0, y1)
    slo <- slo + a1[1] - a0[2]
    shi <- shi + a1[2] - a0[1]
  }
  lo <- slo / n
  hi <- shi / n
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), bw = h, n = n,
             method = "Nonparametric regression bound")
}
