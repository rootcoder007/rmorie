# SPDX-License-Identifier: AGPL-3.0-or-later
#' MuZero categorical value head and its invertible scaling
#'
#' Softmax the head over the integer support -support..support, take the
#' expectation y, then invert h(x) = sign(x)(sqrt(|x|+1) - 1 + eps x):
#' h^\{-1\}(y) = sign(y)\[((sqrt(1 + 4 eps(|y| + 1 + eps)) - 1)/(2 eps))^2 - 1\].
#'
#' @param logits Head outputs over the support, length 2*support + 1.
#' @param support Half-width of the integer support.
#' @param epsilon eps in the transform.
#'
#' @return List with value, expected, prob, support, epsilon, k.
#' @references Schrittwieser et al. (2020), arXiv:1911.08265, Appendix F.
#'   Read from the ar5iv rendering of the arXiv source.
#' @export
#' @examples
#' Mzvalue(c(0, 1, 0), support = 1)
Mzvalue <- function(logits, support = 300, epsilon = 0.001) {
  z <- .t1_vec(logits); s <- as.integer(support)
  if (length(z) != 2L * s + 1L) stop("logits must have length 2*support + 1")
  eps <- as.numeric(epsilon)
  if (eps <= 0) stop("epsilon must be strictly positive")
  e <- exp(z - max(z)); p <- e / sum(e)
  y <- sum(p * (seq_len(2L * s + 1L) - 1L - s))
  sg <- if (y >= 0) 1 else -1
  a <- (sqrt(1 + 4 * eps * (abs(y) + 1 + eps)) - 1) / (2 * eps)
  .t1_result(value = sg * (a * a - 1), expected = y, prob = p,
             support = s, epsilon = eps, k = 2L * s + 1L,
             method = "MuZero categorical value head (Schrittwieser et al. 2020 App. F)")
}
