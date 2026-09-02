# SPDX-License-Identifier: AGPL-3.0-or-later
#' ViT class token and position embedding: the rest of Equation (1)
#'
#' SOURCE.  Dosovitskiy et al. (2021), "An Image is Worth 16x16 Words:
#' Transformers for Image Recognition at Scale", ICLR 2021; arXiv:2010.11929v2.
#' Read from the PDF rendered as page images.
#'
#' Section 3.1, p. 3: "Similar to BERT's [class] token, we prepend a learnable
#' embedding to the sequence of embedded patches (z_0^0 = x_class), whose state
#' at the output of the Transformer encoder (z_L^0) serves as the image
#' representation y (Eq. 4). ... Position embeddings are added to the patch
#' embeddings to retain positional information.  We use standard learnable 1D
#' position embeddings, since we have not observed significant performance
#' gains from using more advanced 2D-aware position embeddings."
#'
#' Equation (1), p. 4:
#' z_0 = \[x_class; x_p^1 E; x_p^2 E; ...; x_p^N E\] + E_pos, with
#' E in R^\{(P^2 . C) x D\} and E_pos in R^\{(N+1) x D\}.
#'
#' This module takes the patch embeddings x_p^i E produced by Vitptm and
#' returns z_0.  Note that E_pos has N+1 rows: the position embedding is added
#' to the class token as well, which the "+ E_pos" outside the bracket in
#' Eq. (1) makes explicit.
#'
#' x_class and E_pos are learned in the paper.  Here they come from the shared
#' deterministic normal stream so both language arms hold identical numbers.
#'
#' @param patches N-by-D matrix of patch embeddings x_p^i E.
#' @param n_patches N; checked against the number of rows of patches if given.
#' @param w_scale scales x_class and E_pos; 0 makes both zero.
#' @param skip offset into the shared deterministic stream.
#' @return list: estimate (N+1), z0, cls, pos, n_patches, seq_len, embed_dim,
#'   n, skip_used, method.
#' @keywords internal
#' @examples
#' Vitcls(Vitptm(matrix(1:16, 4, 4), 2, 3)$embeddings)$seq_len
#' @export
Vitcls <- function(patches, n_patches = NULL, w_scale = 1, skip = 0) {
  P <- .s03mat(patches)
  n <- nrow(P)
  if (n < 1L) stop("vit_cls_token: need at least one patch embedding")
  d <- ncol(P)
  if (!is.null(n_patches) && as.integer(n_patches) != n) {
    stop("vit_cls_token: n_patches does not match the number of rows of patches")
  }
  cls <- as.numeric(.vitdraw(1L, d, skip, w_scale)[1L, ])
  pos <- .vitdraw(n + 1L, d, as.integer(skip) + d, w_scale)
  z0 <- matrix(0, n + 1L, d)
  z0[1L, ] <- cls + pos[1L, ]
  for (i in seq_len(n)) z0[i + 1L, ] <- P[i, ] + pos[i + 1L, ]
  list(estimate = as.numeric(n + 1L), z0 = z0, cls = cls, pos = pos,
       n_patches = n, seq_len = n + 1L, embed_dim = d, n = n,
       skip_used = as.integer(skip) + d + (n + 1L) * d,
       method = paste0("z_0 = [x_class; x_p^1 E; ...; x_p^N E] + E_pos ",
                       "(Dosovitskiy et al. 2021, Eq. (1) p. 4)"))
}
