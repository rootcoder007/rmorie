# LayoutLMv3: one masking recipe for text and image alike.
# Sources: Huang, Y., Lv, T., Cui, L., Lu, Y. & Wei, F. (2022)
# "LayoutLMv3: Pre-training for Document AI with Unified Text and
# Image Masking", MM '22, 4083-4091, doi:10.1145/3503161.3548112,
# arXiv:2204.08387. Multimodal document models pre-training image and
# text with different objectives; unification through masked language
# modelling and masked image modelling with discrete image tokens;
# word-patch alignment predicting whether the corresponding image
# patch of an unmasked text word is masked; and linear image patch
# embeddings in place of a CNN backbone. Dosovitskiy, A. et al.
# (2021) "An Image is Worth 16x16 Words", ICLR 2021, arXiv:2010.11929,
# for the linear patch embedding. Bao, H., Dong, L., Piao, S. & Wei,
# F. (2022) "BEiT: BERT Pre-Training of Image Transformers", ICLR
# 2022, arXiv:2106.08254, for the discrete visual tokens.

# Base R only, faithful translation of ocrwit_python_reference.py.

.OCRWIT_EPS <- 1e-12

.ocrwit_clip_int <- function(v, lo, hi) {
  v <- as.integer(round(v))
  if (v < lo) return(as.integer(lo))
  if (v > hi) return(as.integer(hi))
  v
}

normalise_bbox <- function(box, width, height, scale = 1000) {
  if (length(box) < 4L)
    stop("ocrwit: the box must have four coordinates")
  x0 <- as.numeric(box[1])
  y0 <- as.numeric(box[2])
  x1 <- as.numeric(box[3])
  y1 <- as.numeric(box[4])
  W <- as.numeric(width)
  H <- as.numeric(height)
  if (W <= 0.0 || H <= 0.0)
    stop("ocrwit: the page dimensions must be positive")
  if (x1 < x0 || y1 < y0)
    stop("ocrwit: the box is inverted")
  s <- as.integer(scale)
  c(.ocrwit_clip_int(x0 / W * s, 0, s),
    .ocrwit_clip_int(y0 / H * s, 0, s),
    .ocrwit_clip_int(x1 / W * s, 0, s),
    .ocrwit_clip_int(y1 / H * s, 0, s))
}

segment_layout_boxes <- function(boxes, segment_ids, width, height,
                                 scale = 1000) {
  segs <- as.list(segment_ids)
  B <- if (is.list(boxes)) boxes else lapply(seq_len(nrow(boxes)),
                                              function(i) boxes[i, ])
  if (length(segs) != length(B))
    stop("ocrwit: ", length(B), " boxes but ", length(segs),
         " segment ids")
  normed <- mapply(function(b, s) normalise_bbox(b, width, height, scale),
                   B, segs, SIMPLIFY = FALSE)
  by_seg <- list()
  for (i in seq_along(segs)) {
    sk <- as.character(segs[[i]])
    if (is.null(by_seg[[sk]])) by_seg[[sk]] <- list()
    by_seg[[sk]][[length(by_seg[[sk]]) + 1L]] <- normed[[i]]
  }
  seg_box <- list()
  for (sk in names(by_seg)) {
    bs <- by_seg[[sk]]
    seg_box[[sk]] <- c(min(vapply(bs, function(b) b[1L], numeric(1L))),
                       min(vapply(bs, function(b) b[2L], numeric(1L))),
                       max(vapply(bs, function(b) b[3L], numeric(1L))),
                       max(vapply(bs, function(b) b[4L], numeric(1L))))
  }
  per_token <- lapply(segs, function(s) seg_box[[as.character(s)]])
  list(
    segment_boxes = seg_box,
    per_token = per_token,
    n_segments = length(seg_box),
    note = "one box per segment, cheaper than per word and closer to the document's structure"
  )
}

mask_units <- function(n_units, rate = 0.3, seed = 0, block = 1) {
  n <- as.integer(n_units)
  r <- as.numeric(rate)
  if (n < 1L)
    stop("ocrwit: there is nothing to mask")
  if (!(r > 0.0 && r < 1.0))
    stop("ocrwit: the mask rate must lie in (0,1)")
  e <- .ghc_rng(as.numeric(seed))
  b <- max(1L, as.integer(block))
  masked <- integer(0)
  target <- max(1L, as.integer(round(n * r)))
  guard <- 0L
  while (length(masked) < target && guard < 1000L * n) {
    s <- as.integer(.ghc_unif(e, 1L) * n) %% n
    hi <- min(n, s + b)
    if (s < hi) {
      add <- seq.int(s, hi - 1L)
      masked <- unique(c(masked, add))
    }
    guard <- guard + 1L
  }
  all_idx <- seq_len(n) - 1L
  kept <- setdiff(all_idx, masked)
  list(
    masked = as.integer(sort(masked)),
    kept = as.integer(sort(kept)),
    rate = length(masked) / as.numeric(n),
    block = b,
    note = "the same recipe for both modalities, which is the unification"
  )
}

patch_of_box <- function(box, width, height, patch_grid = 14) {
  g <- as.integer(patch_grid)
  nb <- normalise_bbox(box, width, height, g)
  x0 <- nb[1L]; y0 <- nb[2L]; x1 <- nb[3L]; y1 <- nb[4L]
  rs <- seq.int(min(y0, g - 1L), min(max(y1, y0 + 1L), g) - 1L)
  cs <- seq.int(min(x0, g - 1L), min(max(x1, x0 + 1L), g) - 1L)
  if (length(rs) == 0L || length(cs) == 0L) return(integer(0))
  out <- as.vector(outer(rs, cs, function(r, c) r * g + c))
  sort(unique(out))
}

word_patch_alignment <- function(text_boxes, masked_patches, width,
                                 height, patch_grid = 14,
                                 masked_text = list()) {
  mp <- unique(as.integer(unlist(masked_patches)))
  mt <- unique(as.integer(unlist(masked_text)))
  labels <- list()
  covered <- list()
  for (i in seq_along(text_boxes)) {
    if (i - 1L %in% mt) next
    b <- text_boxes[[i]]
    ps <- patch_of_box(b, width, height, patch_grid)
    covered[[as.character(i - 1L)]] <- ps
    labels[[as.character(i - 1L)]] <-
      as.integer(any(ps %in% mp))
  }
  if (length(labels) == 0L)
    stop("ocrwit: every text token is masked, so the alignment objective has no examples")
  n_ex <- length(labels)
  pos <- sum(unlist(labels))
  list(
    estimate = labels,
    labels = labels,
    patches = covered,
    n_examples = n_ex,
    positive_rate = pos / as.numeric(n_ex),
    method = "word-patch alignment; Huang, Lv, Cui, Lu & Wei (2022)",
    note = "unmasked words only -- a masked word would leak its own reconstruction target"
  )
}

.ocrwit_cheatsheet <- function() {
  paste("ocrwit: document models pre-trained text and image with ",
        "DIFFERENT objectives, giving two spaces and a bridge. ",
        "LayoutLMv3 makes them symmetric -- mask and reconstruct ",
        "text tokens, mask and reconstruct image patches as DISCRETE ",
        "tokens -- so one encoder learns one space. Linear patch ",
        "embeddings, so no CNN backbone or detector. WORD-PATCH ",
        "ALIGNMENT binds them: for an UNMASKED word, predict whether ",
        "its patch was masked, which is the only objective that ",
        "forces the model to know where a word sits. Layout is ",
        "SEGMENT-level 2D position.", sep = "")
}

# compact alias per ledger/NAMING.md
layoutlmv3 <- word_patch_alignment

# public names resolved by fn/_lazy_map.json
ocr_wit_layout <- word_patch_alignment
ocrwitlayout <- word_patch_alignment

morie_ocrwit <- word_patch_alignment
