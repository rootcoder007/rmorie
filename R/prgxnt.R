# SPDX-License-Identifier: AGPL-3.0-or-later
#' Perplexity of a language model
#'
#' Formula: PPL = exp(-(1/N) sum_i log p(x_i)); cross-entropy H = -(1/N) sum_i log2 p(x_i)
#'
#' @param log_probs Natural-log probabilities assigned to the observed tokens.
#' @param N Token count to normalise by; ``len(log_probs)`` if omitted.

#' @param log_probs See Usage.
#' @param N See Usage.
#' @return List with ``perplexity``, ``cross_entropy_nats``, ``cross_entropy_bits``, ``N``.
#' @references Brown, Della Pietra, Mercer, Della Pietra and Lai (1992), An estimate of an upper bound for the entropy of English, Computational Linguistics 18:31-40. Not held locally; perplexity as the exponentiated per-token cross-entropy is the standard published definition.
#' @export
Perplex <- function(log_probs, N = NULL) {
  lp <- .t1_vec(log_probs)
  if (!length(lp)) stop("need at least one log-probability")
  if (any(lp > 0)) stop("log-probabilities must be non-positive")
  n <- if (is.null(N)) length(lp) else as.numeric(N)
  if (n <= 0) stop("N must be positive")
  h <- -sum(lp) / n
  .t1_result(perplexity = exp(h), cross_entropy_nats = h,
             cross_entropy_bits = h / log(2), N = n, method = "Perplexity")
}
