# SPDX-License-Identifier: AGPL-3.0-or-later

.CLIP_BACKBONES <- list(
  "vit-l/14" = c(patch = 14, width = 1024, layers = 24, heads = 16, embed = 768),
  "vit-b/32" = c(patch = 32, width = 768, layers = 12, heads = 12, embed = 512),
  "vit-b/16" = c(patch = 16, width = 768, layers = 12, heads = 12, embed = 512))

#' CLIP image encoder
#'
#' Formula: ViT-L/14 or ResNet-50x16
#'
#' The image is cut into non-overlapping patch x patch squares, each
#' flattened and linearly projected to the transformer width, a class
#' token is prepended and a learned position embedding added; the class
#' token is then projected to the joint embedding space and
#' L2-normalised, which is what makes the cosine of two encodings a
#' similarity.  The projection weights come from the deterministic
#' stream rather than trained checkpoints, so the geometry is exact and
#' reproducible but the semantics are not learned.
#'
#' @param image An H x W matrix of pixel values; H and W must be
#'   multiples of the backbone patch size.
#' @param backbone One of vit-l/14, vit-b/16, vit-b/32.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{embedding},
#'   \code{n_patches}, \code{grid}, \code{patch}, \code{width},
#'   \code{embed_dim}, \code{norm}, \code{method}.
#' @references Radford et al. (2021), ICML 139:8748-8763;
#'   Dosovitskiy et al. (2021), ICLR 2021.
#' @export
Clipxi <- function(image, backbone = "vit-l/14", seed = 42) {
  key <- tolower(as.character(backbone))
  if (!(key %in% names(.CLIP_BACKBONES)))
    stop("backbone must be one of ", paste(sort(names(.CLIP_BACKBONES)),
                                           collapse = ", "))
  cfg <- .CLIP_BACKBONES[[key]]
  P <- as.integer(cfg[["patch"]])
  M <- .s03mat(image)
  H <- nrow(M)
  if (H == 0L) stop("empty input: image has no rows")
  W <- ncol(M)
  if (H %% P != 0L || W %% P != 0L)
    stop("image dimensions must be multiples of the patch size")
  gh <- H %/% P; gw <- W %/% P
  npatch <- gh * gw
  dim <- min(as.integer(cfg[["width"]]), 64L)
  out_dim <- min(as.integer(cfg[["embed"]]), 32L)
  e <- .ghc_rng(seed)
  proj <- matrix(0, P * P, dim)
  for (q in seq_len(P * P)) for (k in seq_len(dim))
    proj[q, k] <- .ghc_norm(e, 1L, 0, 1) / sqrt(P * P)
  pos <- matrix(0, npatch + 1L, dim)
  for (q in seq_len(npatch + 1L)) for (k in seq_len(dim))
    pos[q, k] <- .ghc_norm(e, 1L, 0, 0.02)
  head <- matrix(0, dim, out_dim)
  for (k in seq_len(dim)) for (j in seq_len(out_dim))
    head[k, j] <- .ghc_norm(e, 1L, 0, 1) / sqrt(dim)
  tokens <- matrix(0, npatch + 1L, dim)
  tokens[1, ] <- pos[1, ]
  idx <- 1L
  for (a in seq_len(gh)) for (b in seq_len(gw)) {
    flat <- numeric(P * P)
    p <- 1L
    for (r in seq_len(P)) for (cc in seq_len(P)) {
      flat[p] <- M[(a - 1L) * P + r, (b - 1L) * P + cc]
      p <- p + 1L
    }
    idx <- idx + 1L
    for (k in seq_len(dim)) {
      s <- 0
      for (q in seq_len(P * P)) s <- s + flat[q] * proj[q, k]
      tokens[idx, k] <- s + pos[1L + (a - 1L) * gw + b, k]
    }
  }
  cls <- tokens[1, ]
  for (k in seq_len(dim)) {
    s <- 0
    for (i in seq_len(npatch)) s <- s + tokens[1L + i, k]
    cls[k] <- cls[k] + s / npatch
  }
  mu <- 0
  for (v in cls) mu <- mu + v
  mu <- mu / dim
  sd <- 0
  for (v in cls) sd <- sd + (v - mu)^2
  sd <- sqrt(sd / dim)
  cls <- (cls - mu) / (if (sd > 0) sd else 1)
  emb <- numeric(out_dim)
  for (j in seq_len(out_dim)) {
    s <- 0
    for (k in seq_len(dim)) s <- s + cls[k] * head[k, j]
    emb[j] <- s
  }
  emb <- .clip_l2norm(emb)
  .t1_result(estimate = emb[1], embedding = emb, n_patches = npatch,
             grid = c(gh, gw), patch = P,
             width = as.integer(cfg[["width"]]),
             embed_dim = as.integer(cfg[["embed"]]),
             norm = sqrt(sum(emb * emb)),
             method = "CLIP vision-transformer image encoder")
}
