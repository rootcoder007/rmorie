# SPDX-License-Identifier: AGPL-3.0-or-later
#' ALiBi linear bias applied to precomputed attention scores
#'
#' The same method as \code{Atalib} -- Press, Smith and Lewis (2022), "Train
#' short, test long", ICLR 2022, arXiv:2108.12409 -- entered at a different
#' point: Atalib takes Q, K and V and returns the attention output, while this
#' function takes scores that have already been formed and only adds the bias.
#'
#' There is exactly one implementation.  The bias matrix and slope schedule
#' come from atalib.R rather than being written again here: a second copy
#' would agree with the first at 1e-9 forever and be indistinguishable from
#' correct work while permanently doubling the surface under a name that reads
#' right.
#'
#' @param scores n_q by n_k pre-softmax scores, already scaled by 1/sqrt(d).
#' @param slopes the head slope m; defaults to the paper's single-head 2^-8.
#' @param causal mask keys after the query position.
#' @return list: biased, estimate, bias, slope, n_q, n_k, causal, method.
#' @keywords internal
#' @examples
#' Alibi(matrix(0, 3, 3))$bias
#' @export
Alibi <- function(scores, slopes = NULL, causal = FALSE) {
  S <- as.matrix(scores); storage.mode(S) <- "double"
  nq <- nrow(S); nk <- ncol(S)
  if (nq == 0L || nk == 0L) stop("alibi: scores is empty")
  m <- if (is.null(slopes)) 2^-8 else as.numeric(slopes)
  B <- .atalib_bias(nq, nk, m, causal)
  out <- S + B
  list(biased = out, estimate = out[1, 1], bias = B, slope = m, n_q = nq, n_k = nk,
       causal = isTRUE(causal),
       method = "scores - m|i-j|; shared implementation with morie.fn.atalib")
}
