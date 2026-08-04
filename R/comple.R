# SPDX-License-Identifier: AGPL-3.0-or-later
#' Score knowledge-graph triples in a complex embedding space
#'
#' A real bilinear model must either be symmetric, losing antisymmetric
#' relations, or full, costing d^2 parameters. ComplEx moves to complex
#' vectors and takes the real part of a Hermitian product; conjugating
#' the tail breaks the symmetry while the parameter count stays linear.
#'
#' Formula: \code{Re(<e_h, w_r, conj(e_t))>} =
#' \code{sum_k [Rh Rr Rt + Rh Ir It + Ih Rr It - Ih Ir Rt]}.
#'
#' @param triples Matrix of zero-based \code{[head, relation, tail]} indices.
#' @param dim Embedding dimension.
#' @param re_e,im_e Entity embeddings, optional.
#' @param re_r,im_r Relation embeddings, optional.
#' @param seed Seed for the shared Lehmer minstd default embeddings.
#' @return List with \code{estimate}, \code{scores}, \code{m}, \code{dim}.
#' @references Trouillon, T. et al. (2016). Complex embeddings for
#'   simple link prediction. ICML 33, 2071-2080, equation (11).
#' @export
Comple <- function(triples, dim, re_e = NULL, im_e = NULL, re_r = NULL, im_r = NULL, seed = 1) {
  T_ <- matrix(as.integer(as.matrix(triples)), ncol = 3)
  d <- as.integer(dim)
  ne <- max(T_[, 1], T_[, 3]) + 1L
  nr <- max(T_[, 2]) + 1L
  g <- .t1_lcg(seed)
  draw <- function(rows) matrix(vapply(seq_len(rows * d), function(i) g$norm(), 0), rows, d, byrow = TRUE)
  if (is.null(re_e)) re_e <- draw(ne) else re_e <- as.matrix(re_e)
  if (is.null(im_e)) im_e <- draw(ne) else im_e <- as.matrix(im_e)
  if (is.null(re_r)) re_r <- draw(nr) else re_r <- as.matrix(re_r)
  if (is.null(im_r)) im_r <- draw(nr) else im_r <- as.matrix(im_r)
  scores <- numeric(nrow(T_))
  for (i in seq_len(nrow(T_))) {
    h <- T_[i, 1] + 1L; r <- T_[i, 2] + 1L; t <- T_[i, 3] + 1L
    scores[i] <- sum(re_e[h, ] * re_r[r, ] * re_e[t, ] +
                     re_e[h, ] * im_r[r, ] * im_e[t, ] +
                     im_e[h, ] * re_r[r, ] * im_e[t, ] -
                     im_e[h, ] * im_r[r, ] * re_e[t, ])
  }
  .t1_result(estimate = sum(scores) / length(scores), scores = scores,
             m = length(scores), dim = d, method = "ComplEx triple score")
}
