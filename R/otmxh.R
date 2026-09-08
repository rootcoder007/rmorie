# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transport between mixtures, restricted to plans that stay Gaussian
#'
#' The true \code{W_2} between two Gaussian mixtures has no closed form
#' and its optimal plan splits components, so the geodesic leaves the
#' mixture family altogether. Restricting the couplings to those that move
#' whole components onto whole components keeps everything inside the
#' family and reduces the problem to a small discrete transport whose
#' ground cost is the closed-form Gaussian \code{W_2^2}. The restriction
#' can only raise the cost, so \code{MW_2 >= W_2} always.
#'
#' Formula: \code{MW_2^2 = min_{w in Pi(p,q)} sum_kl w_kl W_2^2(N(m_k,S_k),
#' N(m'_l,S'_l))} -- Delon and Desolneux (2020) Definition 4.1.
#'
#' @param mus1 Component means of the first mixture, K1 by d.
#' @param Sigmas1 List of K1 component covariances.
#' @param w1 Mixture weights, rescaled to sum to one.
#' @param mus2,Sigmas2,w2 The same for the second mixture.
#' @return List with \code{MW2}, \code{MW2_sq}, \code{T}, \code{C},
#'   \code{K1}, \code{K2}, \code{d}.
#' @references Delon, J. and Desolneux, A. (2020). SIAM Journal on Imaging
#'   Sciences 13(2):936-970. \doi{10.1137/19M1301047}.
#' @export
#' @examples
#' Otmxh(mus1 = c(1, 2, 3, 4, 5, 6, 7, 8), Sigmas1 = c(1, 2, 3, 4, 5, 6, 7, 8), w1 = c(1,
#' 2, 3, 4, 5, 6, 7, 8), mus2 = c(1, 2, 3, 4, 5, 6, 7, 8), Sigmas2 = c(1, 2, 3, 4, 5, 6,
#' 7, 8), w2 = c(1, 2, 3, 4, 5, 6, 7, 8))
Otmxh <- function(mus1, Sigmas1, w1, mus2, Sigmas2, w2) {
  M1 <- as.matrix(mus1)
  M2 <- as.matrix(mus2)
  S1 <- lapply(Sigmas1, as.matrix)
  S2 <- lapply(Sigmas2, as.matrix)
  p <- .ot_hist(w1, normalise = TRUE)
  q <- .ot_hist(w2, normalise = TRUE)
  K1 <- nrow(M1)
  K2 <- nrow(M2)
  d <- ncol(M1)
  if (ncol(M2) != d)
    stop("the two mixtures must live in the same dimension")
  if (length(S1) != K1 || length(S2) != K2 || length(p) != K1 ||
      length(q) != K2)
    stop("means, covariances and weights must agree in count")
  C <- matrix(0, K1, K2)
  for (k in seq_len(K1)) for (l in seq_len(K2))
    C[k, l] <- .ot_w2gauss(M1[k, ], S1[[k]], M2[l, ], S2[[l]])
  r <- .ot_emd(p, q, C)
  .t1_result(MW2 = sqrt(r$cost), MW2_sq = r$cost, T = r$T, C = C,
             K1 = K1, K2 = K2, d = d,
             method = "Mixture Wasserstein distance MW2")
}
