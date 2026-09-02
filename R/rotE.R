# SPDX-License-Identifier: AGPL-3.0-or-later
#' RotatE knowledge-graph embedding score
#'
#' Sun, Deng, Nie and Tang (2019), RotatE: knowledge graph embedding by
#' relational rotation in complex space, ICLR (arXiv:1902.10197 --
#' FETCHED), states verbatim: "Given a triplet (h, r, t), we expect that t
#' = h o r, where h, r, t in C^k are the embeddings, the modulus |r_i| = 1
#' and o denotes the Hadamard (element-wise) product", so the score of its
#' table 1 is d_r(h, t) = ||h o r - t||.  The unit modulus is what makes
#' the relation a rotation rather than a general scaling, and it is
#' enforced here rather than assumed: relations are supplied as phases.
#'
#' @param triples flat vector \[h_re, h_im, theta, t_re, t_im\] when the
#'   components are not given separately.
#' @param dim the embedding dimension k.
#' @param h_re,h_im head embedding.
#' @param theta relation phases; the relation is exp(i theta).
#' @param t_re,t_im tail embedding.
#' @param gamma optional margin; the score gamma - d is then returned.
#' @return list: estimate, distance, score, per_dim, dim, method.
#' @keywords internal
#' @examples
#' Rotatesc(NULL, 1, 1, 0, pi / 2, 0, 1)$distance
#' @export
Rotatesc <- function(triples, dim = NULL, h_re = NULL, h_im = NULL,
                     theta = NULL, t_re = NULL, t_im = NULL, gamma = NULL) {
  if (is.null(h_re)) {
    flat <- .s03vec(triples)
    d <- if (!is.null(dim)) as.integer(dim) else length(flat) %/% 5L
    h_re <- flat[seq_len(d)]
    h_im <- flat[d + seq_len(d)]
    theta <- flat[2 * d + seq_len(d)]
    t_re <- flat[3 * d + seq_len(d)]
    t_im <- flat[4 * d + seq_len(d)]
  }
  hr <- .s03vec(h_re)
  hi <- .s03vec(h_im)
  th <- .s03vec(theta)
  tr <- .s03vec(t_re)
  ti <- .s03vec(t_im)
  d <- length(hr)
  per <- numeric(d)
  s <- 0
  for (j in seq_len(d)) {
    cr <- cos(th[j])
    si <- sin(th[j])
    re <- hr[j] * cr - hi[j] * si - tr[j]
    im <- hr[j] * si + hi[j] * cr - ti[j]
    m <- sqrt(re * re + im * im)
    per[j] <- m
    s <- s + m * m
  }
  dist <- sqrt(s)
  list(estimate = dist, distance = dist,
       score = if (!is.null(gamma)) as.numeric(gamma) - dist else NaN,
       per_dim = per, dim = d,
       method = "RotatE d_r(h, t) = ||h o r - t|| with |r_i| = 1 (Sun et al. 2019)")
}
