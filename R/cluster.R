# SPDX-License-Identifier: AGPL-3.0-or-later
#' One-stage cluster sample: every element of each sampled cluster
#'
#' The variance depends on the spread of the cluster means and not at all
#' on the spread within a cluster. The design effect against simple random
#' sampling of the same number of elements is returned.
#'
#' Formula: ybar_c = (1/m) sum_i ybar_i;
#'   v(ybar_c) = (1 - m/M) s_b^2 / m,
#'   s_b^2 = sum (ybar_i - ybar_c)^2 / (m - 1)
#'
#' @param Y Sampled clusters, one row per cluster, all of size k.
#' @param M Number of clusters in the population; \code{Inf} if unknown.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{cluster_mean}, \code{between_var},
#'   \code{within_var}, \code{deff}, \code{rho}, \code{m}, \code{k}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   9. Chapter 9 was NOT in the scanned excerpt available to this batch,
#'   so the standard published form is used; the finite-population factor
#'   (M - m)/M matches the convention of the sibling Cochran modules.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Clus1(V)
Clus1 <- function(Y, M = Inf, level = 0.95) {
  Y <- as.matrix(Y)
  m <- nrow(Y)
  k <- ncol(Y)
  if (m < 2L) stop("at least two clusters are needed for a variance")
  if (k < 1L) stop("clusters must be non-empty")
  cm <- rowMeans(Y)
  est <- mean(cm)
  sb2 <- stats::var(cm)
  M <- as.numeric(M)
  fpc <- if (is.infinite(M)) 1 else (M - m) / M
  var <- fpc * sb2 / m
  se <- sqrt(var)
  S2 <- stats::var(as.numeric(t(Y)))
  within <- if (k > 1L) mean(apply(Y, 1, stats::var)) else 0
  vsrs <- S2 / (m * k)
  deff <- if (vsrs > 0) var / vsrs else NaN
  rho <- if (k > 1L) (deff - 1) / (k - 1) else NaN
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = est, se = se, ci_lower = est - z * se,
             ci_upper = est + z * se, cluster_mean = cm, between_var = sb2,
             within_var = within, deff = deff, rho = rho, m = m, k = k,
             method = "One-stage cluster sampling, equal cluster sizes")
}
