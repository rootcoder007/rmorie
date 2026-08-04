# SPDX-License-Identifier: AGPL-3.0-or-later
#' DINO self-distillation cross-entropy.
#'
#' Formula: P_s = softmax(s/tau_s), P_t = softmax((t - C)/tau_t), loss = -(1/B) sum_i sum_k P_t(i,k) log P_s(i,k)
#'
#' @param s_logits Student logits.
#' @param t_logits Teacher logits.
#' @param tau_s Student temperature.
#' @param tau_t Teacher temperature.
#' @param center Teacher centre C; zeros if omitted.

#' @return List with ``loss``, ``per_view``, ``p_s``, ``p_t``, ``B``, ``K``.
#' @references Caron, Touvron, Misra, Jegou, Mairal, Bojanowski and Joulin (2021), Emerging Properties in Self-Supervised Vision Transformers, ICCV/arXiv:2104.14294. Verified against the paper: equation (1) for the temperature softmax, equation (4) for the centre update, and Algorithm 1's pseudocode for the order of centre-then-sharpen.
#' @export
Dinoloss <- function(s_logits, t_logits, tau_s = 0.1, tau_t = 0.04, center = NULL) {
  Sm <- as.matrix(s_logits); Tm <- as.matrix(t_logits)
  B <- nrow(Sm); K <- ncol(Sm)
  if (!all(dim(Tm) == c(B, K)))
    stop("student and teacher logits must have the same shape")
  c0 <- if (is.null(center)) rep(0, K) else .t1_vec(center)
  sm <- function(M, tau, off) {
    Z <- sweep(M, 2, off, "-") / as.numeric(tau)
    Z <- Z - apply(Z, 1, max)
    E <- exp(Z); E / rowSums(E)
  }
  Ps <- sm(Sm, tau_s, rep(0, K)); Pt <- sm(Tm, tau_t, c0)
  per <- -rowSums(Pt * log(Ps))
  .t1_result(loss = mean(per), per_view = per, p_s = Ps, p_t = Pt,
             B = B, K = K, method = "DINO self-distillation loss")
}
