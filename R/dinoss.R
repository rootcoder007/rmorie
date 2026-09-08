# SPDX-License-Identifier: AGPL-3.0-or-later
#' DINO teacher centring and sharpening
#'
#' Formula: P_t = softmax((g_t - C)/tau_t); C <- m C + (1-m) (1/B) sum_i g_t(x_i)
#'
#' @param g_t Teacher logits, one row per view.
#' @param center Current centre C of length K; zeros if omitted.
#' @param m Centre EMA rate.
#' @param tau_t Teacher temperature.

#' @param g_t See Usage.
#' @param center See Usage.
#' @param m See Usage.
#' @param tau_t See Usage.
#' @return List with ``p_t``, ``center``, ``center_old``, ``batch_mean``, ``B``, ``K``.
#' @references Caron, Touvron, Misra, Jegou, Mairal, Bojanowski and Joulin (2021),
#' Emerging Properties in Self-Supervised Vision Transformers, ICCV/arXiv:2104.14294.
#' Verified against the paper: equation (1) for the temperature softmax, equation (4) for
#' the centre update, and Algorithm 1's pseudocode for the order of centre-then-sharpen.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dinocenter(V)
Dinocenter <- function(g_t, center = NULL, m = 0.9, tau_t = 0.04) {
  G <- as.matrix(g_t)
  B <- nrow(G)
  K <- ncol(G)
  c0 <- if (is.null(center)) rep(0, K) else .t1_vec(center)
  if (length(c0) != K) stop("center must have length K")
  Z <- sweep(G, 2, c0, "-") / as.numeric(tau_t)
  Z <- Z - apply(Z, 1, max)
  E <- exp(Z)
  P <- E / rowSums(E)
  bm <- colMeans(G)
  .t1_result(p_t = P, center = as.numeric(m) * c0 + (1 - as.numeric(m)) * bm,
             center_old = c0, batch_mean = bm, B = B, K = K,
             method = "DINO teacher centring and sharpening")
}
