# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intersection bounds from many candidate lower and upper bounds
#'
#' When several inequalities each bound the same parameter, the binding one
#' is the largest lower and the smallest upper. Taking a min of noisy
#' estimates biases it downward, so the plug-in upper bound is too tight;
#' the precision correction pushes each candidate out by its own standard
#' error before the min is taken. The correction reported here is the
#' Bonferroni one, exact at a single candidate (the multiplier is then
#' zero) and conservative beyond it.
#'
#' Formula: \code{\[max_k m_k, min_j m_j\]} plug-in, and
#' \code{\[max_k (m_k - z_K s_k / sqrt(n)), min_j (m_j + z_J s_j / sqrt(n))\]}
#' with \code{z_J = qnorm(1 - 1 / (2 J))} for the half-median-unbiased
#' version.
#'
#' @param theta An (n, K) matrix of observation-level estimates of K
#'   candidate lower bounds.
#' @param moments An (n, J) matrix of observation-level estimates of J
#'   candidate upper bounds.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{lower_pc}, \code{upper_pc}, \code{width_pc}, \code{K}, \code{J},
#'   \code{n}.
#' @references Chernozhukov, V., Lee, S. and Rosen, A. M. (2013).
#'   Intersection bounds: estimation and inference. Econometrica 81(2),
#'   667-737. \doi{10.3982/ECTA8718}. The half-median-unbiased criterion
#'   the correction targets is equation (4.9) of Molinari, F. (2021),
#'   Handbook of Econometrics 7A (arXiv:2004.11751 p. 96); the multiplier
#'   used here is Bonferroni rather than the paper's bootstrap, and is
#'   labelled as such.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Bndlmm(V, V)
Bndlmm <- function(theta, moments) {
  L <- as.matrix(theta)
  U <- as.matrix(moments)
  n <- nrow(L)
  if (n < 2L) stop("Bndlmm: need at least two observations")
  if (nrow(U) != n)
    stop("Bndlmm: theta and moments must have the same number of rows")
  K <- ncol(L)
  J <- ncol(U)
  rn <- sqrt(n)
  zK <- stats::qnorm(1 - 0.5 / K)
  zJ <- stats::qnorm(1 - 0.5 / J)
  mL <- apply(L, 2, mean)
  sL <- apply(L, 2, stats::sd)
  mU <- apply(U, 2, mean)
  sU <- apply(U, 2, stats::sd)
  lo <- max(mL)
  hi <- min(mU)
  lo_pc <- max(mL - zK * sL / rn)
  hi_pc <- min(mU + zJ * sU / rn)
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             lower_pc = lo_pc, upper_pc = hi_pc, width_pc = hi_pc - lo_pc,
             K = K, J = J, n = n, method = "Linear min-max bound")
}
