# SPDX-License-Identifier: AGPL-3.0-or-later
#' Store one key as a sign vector plus its norm
#'
#' Only the norm survives sign quantization, so it is kept alongside;
#' that pair is the whole cache entry. The estimator is asymmetric --
#' the query is projected but not quantized -- which is what keeps the
#' inner product unbiased. Quantizing both sides gives an unbiased angle
#' and then a biased product once the cosine is applied.
#'
#' Formula: store \code{k_tilde = sign(S k)} and \code{nu = ||k||_2};
#' \code{Prod(q, k) = (sqrt(pi/2)/m) nu <S q, k_tilde>}.
#'
#' @param k Key embedding to cache.
#' @param S_mat JL sketch matrix, m by d.
#' @param q Optional query embedding.
#' @return List with \code{k_tilde}, \code{nu}, \code{m}, \code{d}, \code{estimate}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   definition 3.1 equation (4) and lemma 3.2.
#' @export
Tqalg1 <- function(k, S_mat, q = NULL) {
  kv <- as.numeric(k); Sm <- as.matrix(S_mat); m <- nrow(Sm)
  ktil <- .s4_sgn(as.numeric(Sm %*% kv))
  nu <- sqrt(sum(kv * kv))
  est <- nu
  if (!is.null(q)) {
    sq <- as.numeric(Sm %*% as.numeric(q))
    est <- sqrt(pi / 2) / m * nu * sum(sq * ktil)
  }
  .t1_result(k_tilde = ktil, nu = nu, m = m, d = length(kv), estimate = est,
             method = "QJL online key quantizer with unbiased inner product")
}
