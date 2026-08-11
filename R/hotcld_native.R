# SPDX-License-Identifier: AGPL-3.0-or-later
#' Getis-Ord Gi-star hot and cold spot map with FDR screening
#'
#' Computes the Getis-Ord \eqn{G_i^*} z-score at each site (sums include
#' j = i; population standard deviation), converts to two-sided normal
#' p-values \eqn{p_i = 2(1 - \Phi(|z_i|))}, screens them with the
#' Benjamini-Hochberg step-up rule at level `alpha`, and classifies each
#' site as hot (+1, positive significant), cold (-1, negative
#' significant) or neither (0).
#'
#' The \eqn{G_i^*} z-score is
#' \eqn{(\sum_j w_{ij} x_j - \bar x \sum_j w_{ij}) / (s \sqrt{(n \sum_j w_{ij}^2 - (\sum_j w_{ij})^2)/(n-1)})}
#' with \eqn{s} the population standard deviation of x.
#'
#' @param x Observed values, length n.
#' @param W Spatial weights matrix, n by n; the diagonal may be nonzero
#'   (self-weights are part of the star statistic).
#' @param alpha FDR level for the Benjamini-Hochberg screen.
#' @return List with z, p, significant, category, n_hot, n_cold, alpha, n.
#' @references Getis, A. and Ord, J. K. (1992). The analysis of spatial
#'   association by use of distance statistics. Geographical Analysis,
#'   24(3), 189-206.
#'
#'   Ord, J. K. and Getis, A. (1995). Local spatial autocorrelation
#'   statistics: distributional issues and an application. Geographical
#'   Analysis, 27(4), 286-306.
#'
#'   Benjamini, Y. and Hochberg, Y. (1995). Controlling the false
#'   discovery rate. Journal of the Royal Statistical Society B, 57(1),
#'   289-300 (step-up rule, Sec. 3).
#'
#'   Python mirror of the verified Gi-star core src/morie/fn/getis.py.
#' @examples
#' W <- matrix(0, 5, 5); diag(W) <- 1
#' W[cbind(1:4, 2:5)] <- 1; W[cbind(2:5, 1:4)] <- 1
#' Hotcld(c(10, 8, 1, 1, 1), W, alpha = 0.1)
#' @export
Hotcld <- function(x, W, alpha = 0.05) {
  x <- as.numeric(x)
  W <- as.matrix(W)
  n <- length(x)
  if (!all(dim(W) == c(n, n))) stop("W must be n by n")
  if (!(alpha > 0 && alpha < 1)) stop("alpha must be in (0, 1)")
  xbar <- mean(x)
  s <- sqrt(mean((x - xbar)^2))
  if (s == 0) {
    z <- rep(0, n)
  } else {
    z <- vapply(seq_len(n), function(i) {
      wi <- W[i, ]
      sw <- sum(wi)
      sw2 <- sum(wi^2)
      den <- s * sqrt((n * sw2 - sw^2) / (n - 1))
      if (den > 0) (sum(wi * x) - xbar * sw) / den else 0
    }, 0)
  }
  p <- 2 * stats::pnorm(-abs(z))
  ord <- order(p)
  ranked <- p[ord]
  thresh <- alpha * seq_len(n) / n
  pass <- which(ranked <= thresh)
  significant <- rep(FALSE, n)
  if (length(pass)) significant[ord[seq_len(max(pass))]] <- TRUE
  category <- ifelse(significant & z > 0, 1L, ifelse(significant & z < 0, -1L, 0L))
  list(z = z, p = p, significant = significant, category = category,
       n_hot = sum(category == 1L), n_cold = sum(category == -1L),
       alpha = alpha, n = n,
       method = "Getis-Ord Gi* hot/cold spots, BH-FDR screened")
}

#' @rdname Hotcld
#' @export
hot_cold_spots <- Hotcld
