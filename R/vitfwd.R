# SPDX-License-Identifier: AGPL-3.0-or-later
#' Vision Transformer forward pass, Equations (1)-(4)
#'
#' SOURCE.  Dosovitskiy, A., Beyer, L., Kolesnikov, A., Weissenborn, D., Zhai,
#' X., Unterthiner, T., Dehghani, M., Minderer, M., Heigold, G., Gelly, S.,
#' Uszkoreit, J. and Houlsby, N. (2021), "An Image is Worth 16x16 Words:
#' Transformers for Image Recognition at Scale", ICLR 2021; arXiv:2010.11929v2.
#' Read from the PDF rendered as page images, not from the text layer.
#'
#' Equations (1)-(4), p. 4:
#' z_0 = \[x_class; x_p^1 E; ...; x_p^N E\] + E_pos, E in R^\{(P^2 . C) x D\},
#' E_pos in R^\{(N+1) x D\} (1); z'_l = MSA(LN(z_\{l-1\})) + z_\{l-1\},
#' l = 1 ... L (2); z_l = MLP(LN(z'_l)) + z'_l, l = 1 ... L (3);
#' y = LN(z_L^0) (4).
#'
#' Section 3.1, p. 3: "The Transformer encoder (Vaswani et al., 2017) consists
#' of alternating layers of multiheaded self-attention (MSA, see Appendix A)
#' and MLP blocks (Eq. 2, 3).  Layernorm (LN) is applied before every block, and
#' residual connections after every block."  That is the pre-norm arrangement
#' Eqs. (2) and (3) show: LN inside, the residual added outside.
#'
#' MSA is Appendix A, p. 13, Eqs. (5)-(8): per head, \[q, k, v\] = z U_qkv with
#' U_qkv in R^\{D x 3 D_h\}; A = softmax(q k^T / sqrt(D_h)); SA(z) = A v; and
#' MSA(z) = \[SA_1(z); ...; SA_k(z)\] U_msa with U_msa in R^\{(k . D_h) x D\}.
#' D_h is "typically set to D/k", which this module requires: num_heads must
#' divide embed_dim.
#'
#' The MLP hidden width is 4D, the ratio in Table 1, p. 5 (768/3072, 1024/4096,
#' 1280/5120).
#'
#' y is the image representation.  Section 3.1, p. 3: the state of the class
#' token at the output of the encoder, z_L^0, "serves as the image
#' representation y (Eq. 4)".  Attaching a head to y is Vitfsv.
#'
#' This assembles Vitptm (Eq. 1 left), Vitcls (Eq. 1 right), Vitatt
#' (Eqs. 6-7) and Vitmlp (the MLP of Eq. 3) rather than restating them.  All
#' parameters -- E, x_class, E_pos, U_qkv and U_msa per head and layer, W_1 and
#' W_2 per layer -- are drawn in that order from the single shared deterministic
#' normal stream, so the Python and R arms hold identical weights.  The network
#' is untrained by construction; this is a reference implementation of the
#' architecture, not of a fitted model.
#'
#' @param x H-by-W matrix (C = 1), or a list of C such matrices.
#' @param patch_size P; must divide H and W.
#' @param embed_dim D; must be divisible by num_heads.
#' @param num_heads k; D_h = D/k.
#' @param num_layers L; 0 is allowed and returns LN(z_0^0).
#' @param w_scale scales every parameter matrix; 0 reduces Eqs. (2)-(3) to
#'   z_l = z_\{l-1\}.
#' @param mlp_ratio hidden width of the MLP as a multiple of D; 4 per Table 1.
#' @return list: estimate, y, z0, zL, attn, patches, n_patches, seq_len,
#'   embed_dim, d_head, num_heads, num_layers, hidden_dim, n, skip_used, method.
#' @keywords internal
#' @examples
#' Vitfwd(matrix(1:16 / 16, 4, 4), 2, 4, 2, 1)$y
#' @export
Vitfwd <- function(x, patch_size, embed_dim, num_heads, num_layers,
                   w_scale = 1, mlp_ratio = 4) {
  d <- as.integer(embed_dim)
  k <- as.integer(num_heads)
  L <- as.integer(num_layers)
  if (is.na(d) || d < 1L) stop("vit_forward: embed_dim must be a positive integer")
  if (is.na(k) || k < 1L) stop("vit_forward: num_heads must be a positive integer")
  if (is.na(L) || L < 0L) stop("vit_forward: num_layers must be non-negative")
  if (d %% k != 0L) {
    stop("vit_forward: num_heads must divide embed_dim (D_h = D/k)")
  }
  dh <- d %/% k
  ratio <- as.integer(mlp_ratio)
  if (is.na(ratio) || ratio < 1L) {
    stop("vit_forward: mlp_ratio must be a positive integer")
  }
  hid <- ratio * d

  pe <- Vitptm(x, patch_size, d, w_scale, 0)
  skip <- pe$skip_used
  ct <- Vitcls(pe$embeddings, pe$n_patches, w_scale, skip)
  skip <- ct$skip_used
  z0 <- ct$z0
  ns <- nrow(z0)

  z <- z0
  attn <- NULL
  for (l in seq_len(L)) {
    zn <- .vitlnrows(z)
    heads <- vector("list", k)
    for (hh in seq_len(k)) {
      Uq <- .vitdraw(d, dh, skip, w_scale)
      skip <- skip + d * dh
      Uk <- .vitdraw(d, dh, skip, w_scale)
      skip <- skip + d * dh
      Uv <- .vitdraw(d, dh, skip, w_scale)
      skip <- skip + d * dh
      sa <- Vitatt(.s03matmul(zn, Uq), .s03matmul(zn, Uk), .s03matmul(zn, Uv))
      heads[[hh]] <- sa$output
      attn <- sa$attn
    }
    cat_ <- matrix(0, ns, k * dh)
    for (hh in seq_len(k)) {
      for (c in seq_len(dh)) cat_[, (hh - 1L) * dh + c] <- heads[[hh]][, c]
    }
    Umsa <- .vitdraw(k * dh, d, skip, w_scale)
    skip <- skip + k * dh * d
    msa <- .s03matmul(cat_, Umsa)
    z <- z + msa
    zn2 <- .vitlnrows(z)
    mb <- Vitmlp(zn2, hid, w_scale, skip)
    skip <- mb$skip_used
    z <- z + mb$output
  }

  y <- .vitln(z[1L, ])
  list(estimate = sum(y) / d, y = y, z0 = z0, zL = z, attn = attn,
       patches = pe$patches, n_patches = pe$n_patches, seq_len = ns,
       embed_dim = d, d_head = dh, num_heads = k, num_layers = L,
       hidden_dim = hid, n = pe$n_patches, skip_used = skip,
       method = paste0("z_0 Eq.(1); z'_l = MSA(LN(z_{l-1})) + z_{l-1} Eq.(2); ",
                       "z_l = MLP(LN(z'_l)) + z'_l Eq.(3); y = LN(z_L^0) ",
                       "Eq.(4) (Dosovitskiy et al. 2021, p. 4)"))
}
