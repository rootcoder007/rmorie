# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rotary position embedding rescaled to reach past the trained length
#'
#' Interpolating every frequency by the same factor stretches the
#' high-frequency dimensions too, and those carry local word order, so
#' plain position interpolation blurs what attention needs at short
#' range. The NTK-aware variant scales the base instead: low frequencies
#' stretch a lot, high frequencies barely at all.
#'
#' Formula: \code{theta' = theta (L_new/L_train)^(d/(d-2))} inside
#' \code{theta_i = theta'^(-2i/d)}, then rotate by \code{m theta_i}.
#'
#' @param y Unused; keeps the family signature.
#' @param q Query or key vector, d even.
#' @param m Position index.
#' @param theta Base.
#' @param L_new,L_train Target and trained context lengths.
#' @return List with \code{estimate}, \code{theta_base}, \code{freqs},
#'   \code{scale}, \code{d}.
#' @references Su et al. (2024), RoFormer, Neurocomputing 568:127063.
#'   The NTK-aware base rescaling is bloc97 (2023), a LocalLLaMA
#'   community note, cited as such because no peer-reviewed source exists.
#' @export
#' @examples
#' Ropedy(y = c(1, 2, 3, 4, 5, 6, 7, 8), q = 0.5, m = 5L)
Ropedy <- function(y, q, m, theta = 10000, L_new = NULL, L_train = NULL) {
  qv <- as.numeric(q); d <- length(qv)
  scale <- 1; base <- as.numeric(theta)
  if (!is.null(L_new) && !is.null(L_train) && L_train > 0) {
    a <- as.numeric(L_new) / as.numeric(L_train)
    scale <- if (d > 2) a^(d / (d - 2)) else a
    base <- base * scale
  }
  half <- d %/% 2L
  freqs <- base^(-2 * (seq_len(half) - 1) / d)
  out <- qv
  for (i in seq_len(half)) {
    ang <- as.numeric(m) * freqs[i]
    c_ <- cos(ang); s_ <- sin(ang)
    a0 <- qv[2 * i - 1]; a1 <- qv[2 * i]
    out[2 * i - 1] <- a0 * c_ - a1 * s_
    out[2 * i] <- a0 * s_ + a1 * c_
  }
  .t1_result(estimate = out, theta_base = base, freqs = freqs, scale = scale,
             d = d, method = "NTK-aware dynamically scaled RoPE")
}
