# SPDX-License-Identifier: AGPL-3.0-or-later
#' Quantize one token value vector by normalise-then-round
#'
#' The value cache does not need the sign trick. Key errors are amplified
#' through a softmax; values are simply averaged, so plain token-wise
#' scaling is enough, and the paper says so. Doing the simple thing here
#' leaves the bit budget for the keys.
#'
#' Determinism: half-away-from-zero rounding rather than \code{round()},
#' because both languages round half to even but disagree which binary
#' values are exactly half.
#'
#' Formula: \code{s = max|v|}, \code{v_q = round(v/s (2^b - 1))},
#' \code{v_hat = v_q s / (2^b - 1)}.
#'
#' @param v Value embedding for one token.
#' @param bits Bits per entry.
#' @return List with \code{v_q}, \code{s}, \code{v_hat}, \code{estimate},
#'   \code{bits}, \code{d}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   section 3.2.
#' @export
Tqval <- function(v, bits = 4) {
  vv <- as.numeric(unlist(v)); b <- as.integer(bits)
  lev <- 2^b - 1
  s <- max(abs(vv))
  if (s <= 0) s <- 1
  vq <- .s4_rnd(vv / s * lev)
  vhat <- vq * s / lev
  err <- sum((vv - vhat)^2) / length(vv)
  .t1_result(v_q = vq, s = s, v_hat = vhat, estimate = sqrt(err), bits = b,
             d = length(vv), method = "Token-wise value-cache quantization")
}
