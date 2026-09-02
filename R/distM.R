# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bilinear triple score with a diagonal relation matrix
#'
#' Restricting the relation matrix to its diagonal drops the parameter
#' count from d^2 to d and makes the score a three-way inner product. The
#' cost is the symmetry it cannot escape:
#' \code{score(h, r, t) = score(t, r, h)} always, so an antisymmetric
#' relation is unrepresentable. That is what ComplEx was written to fix.
#'
#' Formula: \code{score = <h, r, t> = sum_k h_k r_k t_k}.
#'
#' @param triples Rows \code{[head, relation, tail]}, zero-based.
#' @param dim Embedding dimension.
#' @param E Entity embeddings, optional.
#' @param R Relation embeddings, optional.
#' @param seed Seed for the shared generator.
#' @return List with \code{estimate}, \code{scores},
#'   \code{symmetric_gap}, \code{m}, \code{dim}.
#' @references Yang, Yih, He, Gao & Deng (2015). ICLR 2015, equation (3).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' DistM(V, V)
DistM <- function(triples, dim, E = NULL, R = NULL, seed = 1) {
  T_ <- matrix(as.integer(as.matrix(triples)), ncol = 3)
  d <- as.integer(dim)
  ne <- max(T_[, 1], T_[, 3]) + 1L; nr <- max(T_[, 2]) + 1L
  g <- .t1_lcg(seed)
  draw <- function(rows) matrix(vapply(seq_len(rows * d), function(i) g$norm(), 0),
                                rows, d, byrow = TRUE)
  Em <- if (is.null(E)) draw(ne) else as.matrix(E)
  Rm <- if (is.null(R)) draw(nr) else as.matrix(R)
  sc <- numeric(nrow(T_)); gap <- 0
  for (i in seq_len(nrow(T_))) {
    h <- T_[i, 1] + 1L; r <- T_[i, 2] + 1L; t <- T_[i, 3] + 1L
    s <- sum(Em[h, ] * Rm[r, ] * Em[t, ])
    rev <- sum(Em[t, ] * Rm[r, ] * Em[h, ])
    sc[i] <- s
    if (abs(s - rev) > gap) gap <- abs(s - rev)
  }
  .t1_result(estimate = sum(sc) / length(sc), scores = sc, symmetric_gap = gap,
             m = length(sc), dim = d, method = "DistMult triple score")
}
