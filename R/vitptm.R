# SPDX-License-Identifier: AGPL-3.0-or-later
#' ViT patch embedding: the flattened-patch reshape and its linear map
#'
#' SOURCE.  Dosovitskiy, A., Beyer, L., Kolesnikov, A., Weissenborn, D., Zhai,
#' X., Unterthiner, T., Dehghani, M., Minderer, M., Heigold, G., Gelly, S.,
#' Uszkoreit, J. and Houlsby, N. (2021), "An Image is Worth 16x16 Words:
#' Transformers for Image Recognition at Scale", ICLR 2021; arXiv:2010.11929v2.
#' Read from the PDF rendered as page images, not from the text layer.
#'
#' Section 3.1, p. 3: "we reshape the image x in R^\{H x W x C\} into a sequence
#' of flattened 2D patches x_p in R^\{N x (P^2 . C)\}, where (H, W) is the
#' resolution of the original image, C is the number of channels, (P, P) is the
#' resolution of each image patch, and N = HW/P^2 is the resulting number of
#' patches, which also serves as the effective input sequence length for the
#' Transformer.  The Transformer uses constant latent vector size D through all
#' of its layers, so we flatten the patches and map to D dimensions with a
#' trainable linear projection (Eq. 1)."
#'
#' Equation (1), p. 4, gives that projection as E in R^\{(P^2 . C) x D\}, and the
#' patch embeddings are the products x_p^i E.  This module produces x_p and
#' x_p E; the class token and the position embeddings, which are the rest of
#' Eq. (1), are Vitcls.
#'
#' The paper does not fix an ordering inside a flattened patch, only its length
#' P^2 . C.  This implementation uses channel-major, then row, then column, and
#' raster order (top-to-bottom, left-to-right) over the patch grid; that
#' convention is this module's own and is stated here rather than attributed.
#'
#' E is not trained here.  It is drawn from the shared deterministic normal
#' stream so the Python and R arms hold identical numbers.
#'
#' @param image H-by-W matrix (C = 1), or a list of C such matrices.
#' @param patch_size P; must divide both H and W.
#' @param embed_dim D, the constant latent vector size.
#' @param w_scale scales the deterministic projection E; 0 gives E = 0.
#' @param skip offset into the shared deterministic stream.
#' @return list: estimate (N), patches, embeddings, projection, n_patches,
#'   patch_dim, embed_dim, grid_rows, grid_cols, n_channels, n, skip_used,
#'   method.
#' @keywords internal
#' @examples
#' Vitptm(matrix(1:16, 4, 4), 2, 3)$n_patches
#' @export
Vitptm <- function(image, patch_size, embed_dim, w_scale = 1, skip = 0) {
  ch <- .vitchan(image)
  nc <- length(ch)
  h <- nrow(ch[[1L]])
  w <- ncol(ch[[1L]])
  p <- as.integer(patch_size)
  d <- as.integer(embed_dim)
  if (is.na(p) || p < 1L) {
    stop("vit_patch_embed: patch_size must be a positive integer")
  }
  if (is.na(d) || d < 1L) {
    stop("vit_patch_embed: embed_dim must be a positive integer")
  }
  if (h %% p != 0L || w %% p != 0L) {
    stop("vit_patch_embed: patch_size must divide both H and W")
  }
  gr <- h %/% p
  gc <- w %/% p
  n <- gr * gc
  pdim <- p * p * nc
  patches <- matrix(0, n, pdim)
  i <- 0L
  for (pr in seq_len(gr)) {
    for (pc in seq_len(gc)) {
      i <- i + 1L
      j <- 0L
      for (c in seq_len(nc)) {
        for (r in seq_len(p)) {
          for (s in seq_len(p)) {
            j <- j + 1L
            patches[i, j] <- ch[[c]][(pr - 1L) * p + r, (pc - 1L) * p + s]
          }
        }
      }
    }
  }
  E <- .vitdraw(pdim, d, skip, w_scale)
  emb <- .s03matmul(patches, E)
  list(estimate = as.numeric(n), patches = patches, embeddings = emb,
       projection = E, n_patches = n, patch_dim = pdim, embed_dim = d,
       grid_rows = gr, grid_cols = gc, n_channels = nc, n = n,
       skip_used = as.integer(skip) + pdim * d,
       method = paste0("x_p in R^{N x (P^2 C)}, N = HW/P^2, then x_p E ",
                       "(Dosovitskiy et al. 2021, Sec. 3.1 p. 3 and Eq. (1) p. 4)"))
}
