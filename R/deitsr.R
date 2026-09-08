# SPDX-License-Identifier: AGPL-3.0-or-later
#' DeiT distillation loss
#'
#' Touvron, Cord, Douze, Massa, Sablayrolles and Jegou (2021), Training
#' data-efficient image transformers and distillation through attention,
#' ICML 139, 10347-10357 (arXiv:2012.12877 -- FETCHED).  Soft
#' distillation, eq. (2): L = (1 - lambda) L_CE(psi(Z_s), y) + lambda
#' tau^2 KL(psi(Z_s/tau), psi(Z_t/tau)).  Hard-label distillation, eq.
#' (3): L = (1/2) L_CE(psi(Z_s), y) + (1/2) L_CE(psi(Z_s), y_t) with y_t =
#' argmax_c Z_t(c).  The paper finds the hard variant works better and
#' uses it for the distillation token, so mode = "hard" is the default.
#'
#' @param x student logits Z_s.
#' @param teacher teacher logits Z_t.
#' @param y the true label (zero-based).
#' @param mode "hard" or "soft".
#' @param lam the soft-distillation lambda.
#' @param tau the distillation temperature.
#' @return list: estimate, ce, kd, y_teacher, mode, method.
#' @keywords internal
#' @examples
#' Deitkd(c(2, 1, 0), c(0, 3, 1), 0)$estimate
#' @export
Deitkd <- function(x, teacher = NULL, y = NULL, mode = "hard", lam = 0.5,
                   tau = 1) {
  eps <- 1e-300
  zs <- .s03vec(x)
  zt <- if (!is.null(teacher)) .s03vec(teacher) else numeric(0)
  ps <- .s03softmax(zs)
  logps <- log(pmax(ps, eps))
  yy <- if (!is.null(y)) as.integer(y) else 0L
  ce <- -logps[yy + 1L]
  yt <- 1L
  if (length(zt) > 1L) for (i in seq(2L, length(zt))) if (zt[i] > zt[yt]) yt <- i
  if (identical(mode, "soft")) {
    t <- as.numeric(tau)
    pst <- .s03softmax(zs / t)
    ptt <- .s03softmax(zt / t)
    kl <- 0
    for (i in seq_along(ptt)) {
      if (ptt[i] > 0) kl <- kl + ptt[i] * (log(ptt[i]) - log(max(pst[i], eps)))
    }
    kd <- as.numeric(lam) * t * t * kl
    total <- (1 - as.numeric(lam)) * ce + kd
  } else {
    kd <- 0.5 * (-logps[yt])
    total <- 0.5 * ce + kd
  }
  list(estimate = total, ce = ce, kd = kd, y_teacher = yt - 1L, mode = mode,
       method = "DeiT distillation loss (Touvron et al. 2021, eqs. 2-3)")
}
