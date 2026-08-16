# SAM's mask decoder: two-way attention, then a dynamic classifier.
# Sources: Kirillov, A., Mintun, E., Ravi, N., Mao, H., Rolland, C.,
# Gustafson, L., Xiao, T., Whitehead, S., Berg, A. C., Lo, W.-Y.,
# Dollar, P. & Girshick, R. (2023) "Segment Anything", *ICCV 2023*,
# 4015-4026, arXiv:2304.02643. Sec. 3 and Appendix A: the mask
# decoder mapping the image embedding, prompt embeddings and an
# output token to a mask; the modified Transformer decoder block
# using prompt self-attention and cross-attention in TWO directions
# to update all embeddings; two such blocks followed by upsampling
# the image embedding and an MLP mapping the output token to a
# dynamic linear classifier that computes the mask foreground
# probability at each location; and supervision by a linear
# combination of focal loss and dice loss. Lin, T.-Y., Goyal, P.,
# Girshick, R., He, K. & Dollar, P. (2017) "Focal Loss for Dense
# Object Detection", *ICCV 2017*, 2980-2988, arXiv:1708.02002. The
# (1-p_t)^gamma down-weighting. Milletari, F., Navab, N. & Ahmadi,
# S.-A. (2016) "V-Net: Fully Convolutional Neural Networks for
# Volumetric Medical Image Segmentation", *3DV 2016*, 565-571,
# arXiv:1606.04797. The dice loss. Vaswani, A. et al. (2017)
# "Attention Is All You Need", *NIPS 2017*, 5998-6008,
# arXiv:1706.03762. The decoder block being modified.

.SAMDEC_EPS <- 1e-12

.samdec_mat <- function(x) {
  if (is.matrix(x)) return(x)
  if (is.numeric(x) && is.null(dim(x))) {
    x <- as.matrix(x)
    return(x)
  }
  stop("samdec: expected a matrix-like input")
}

.samdec_vec <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) == 1L) return(as.numeric(x[1, ]))
    if (ncol(x) == 1L) return(as.numeric(x[, 1]))
  }
  as.numeric(x)
}

.samdec_attend <- function(Q, K, V) {
  d <- ncol(Q)
  out <- matrix(0, nrow = nrow(Q), ncol = d)
  W <- matrix(0, nrow = nrow(Q), ncol = nrow(K))
  for (i in seq_len(nrow(Q))) {
    q <- Q[i, ]
    sc <- as.numeric((K %*% q) / sqrt(d))
    m <- max(sc)
    e <- exp(sc - m)
    z <- sum(e)
    w <- e / z
    W[i, ] <- w
    out[i, ] <- crossprod(w, V)  # sum_j w_j V_j[a] -> length-d vector
  }
  list(out = out, W = W)
}

#' two_way_block
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param prompt_tokens See Usage.
#' @param image_tokens See Usage.
#' @return A list with \code{prompt_tokens}, \code{image_tokens}, \code{prompt_to_image}, \code{image_to_prompt}, \code{note}.
#' @export
two_way_block <- function(prompt_tokens, image_tokens) {
  P <- .samdec_mat(prompt_tokens)
  I <- .samdec_mat(image_tokens)
  if (ncol(P) != ncol(I))
    stop("samdec: prompt tokens are ", ncol(P),
         "-dimensional but image tokens are ", ncol(I))
  sa <- .samdec_attend(P, P, P)$out
  P1 <- P + sa
  p2i <- .samdec_attend(P1, I, I)
  P2 <- P1 + p2i$out
  w_p2i <- p2i$W
  i2p <- .samdec_attend(I, P2, P2)
  I2 <- I + i2p$out
  w_i2p <- i2p$W
  list(prompt_tokens = P2, image_tokens = I2,
       prompt_to_image = w_p2i, image_to_prompt = w_i2p,
       note = "both directions, so both embeddings move")
}

#' upsample
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param grid See Usage.
#' @param factor Defaults to \code{2}.
#' @return One of two values, depending on the branch taken.
#' @export
upsample <- function(grid, factor = 2) {
  G <- .samdec_mat(grid)
  f <- as.integer(factor)
  if (f < 1L)
    stop("samdec: the upsampling factor must be >= 1")
  H <- nrow(G)
  W <- ncol(G)
  d <- 1L  # unused; we return a list of rows
  # We need to handle that G may be a list of rows (each row a vector)
  # but in the Python arm grid is [[float, ...], ...] so each row is a
  # list. Here we accept a matrix where columns are the per-pixel
  # vectors, OR a list of numeric vectors. We return a matrix where
  # each row is an upsampled pixel vector.
  if (is.list(grid) && !is.matrix(grid)) {
    Grows <- lapply(grid, function(r) as.numeric(r))
    d <- length(Grows[[1]])
    out <- vector("list", H * f)
    for (i in seq_len(H * f)) {
      src <- Grows[[ (i - 1L) %/% f + 1L ]]
      out[[i]] <- rep(src, each = f)
    }
    out
  } else {
    # matrix: rows are spatial locations, columns are channels
    out <- G[rep.int(seq_len(H), f), , drop = FALSE]
    out <- out[, rep.int(seq_len(W), f), drop = FALSE]
    out
  }
}

#' dynamic_mask_head
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param output_token See Usage.
#' @param image_grid_vectors See Usage.
#' @param mlp Defaults to \code{NULL}.
#' @return A list with \code{logits}, \code{probability}, \code{weights}, \code{note}.
#' @export
dynamic_mask_head <- function(output_token, image_grid_vectors,
                              mlp = NULL) {
  w <- .samdec_vec(output_token)
  if (!is.null(mlp)) {
    w <- .samdec_vec(mlp(w))
  }
  # image_grid_vectors: matrix with rows = (H*W) and columns = d
  # OR a list of length H each being a list of length W each being a
  # length-d numeric vector.
  if (is.list(image_grid_vectors) && !is.matrix(image_grid_vectors)) {
    H <- length(image_grid_vectors)
    W <- length(image_grid_vectors[[1]])
    d <- length(image_grid_vectors[[1]][[1]])
    if (length(w) != d)
      stop("samdec: the dynamic classifier is ", length(w),
           "-wide but the spatial vectors are ", d)
    logits <- matrix(0, nrow = H, ncol = W)
    for (i in seq_len(H)) {
      for (j in seq_len(W)) {
        v <- as.numeric(image_grid_vectors[[i]][[j]])
        logits[i, j] <- sum(w * v)
      }
    }
  } else {
    G <- as.matrix(image_grid_vectors)
    d <- ncol(G)
    if (length(w) != d)
      stop("samdec: the dynamic classifier is ", length(w),
           "-wide but the spatial vectors are ", d)
    logits <- matrix(as.numeric(G %*% w), nrow = sqrt(nrow(G)))
  }
  clamp <- pmin(60, pmax(-60, as.numeric(logits)))
  prob <- 1.0 / (1.0 + exp(-clamp))
  probmat <- matrix(prob, nrow = nrow(logits))
  list(logits = logits, probability = probmat, weights = w,
       note = "the classifier weights come from the PROMPT")
}

#' focal_loss
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param prob See Usage.
#' @param target See Usage.
#' @param gamma Defaults to \code{2}.
#' @param alpha Defaults to \code{0.25}.
#' @return A list with \code{loss}, \code{modulating}, \code{gamma}, \code{note}.
#' @export
focal_loss <- function(prob, target, gamma = 2.0, alpha = 0.25) {
  p <- as.numeric(prob)
  t <- as.numeric(target)
  if (length(p) != length(t))
    stop("samdec: the prediction and target differ in size")
  g <- as.numeric(gamma)
  a <- as.numeric(alpha)
  tot <- 0
  mods <- numeric(length(p))
  for (i in seq_along(p)) {
    pt <- if (t[i] > 0.5) p[i] else 1.0 - p[i]
    at <- if (t[i] > 0.5) a else 1.0 - a
    mod <- (1.0 - pt)^g
    mods[i] <- mod
    tot <- tot + (-at * mod * log(max(pt, .SAMDEC_EPS)))
  }
  list(loss = tot / length(p), modulating = mods, gamma = g,
       note = "an easy pixel with p_t = 0.9 keeps only (1-0.9)^gamma of its weight")
}

#' dice_loss
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param prob See Usage.
#' @param target See Usage.
#' @return A list with \code{loss}, \code{dice}.
#' @export
dice_loss <- function(prob, target) {
  p <- as.numeric(prob)
  t <- as.numeric(target)
  if (length(p) != length(t))
    stop("samdec: the prediction and target differ in size")
  inter <- sum(p * t)
  tot <- sum(p) + sum(t)
  if (tot <= .SAMDEC_EPS)
    return(list(loss = 0.0, dice = 1.0,
                note = "both empty, which is a perfect match"))
  d <- 2.0 * inter / tot
  list(loss = 1.0 - d, dice = d)
}

#' decode_mask
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param prompt_tokens See Usage.
#' @param image_tokens See Usage.
#' @param grid_shape See Usage.
#' @param n_blocks Defaults to \code{2}.
#' @param upsample_factor Defaults to \code{2}.
#' @param output_index Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{mask}, \code{logits}, \code{shape}, \code{n_blocks}, \code{method}, \code{note}.
#' @export
decode_mask <- function(prompt_tokens, image_tokens, grid_shape,
                        n_blocks = 2, upsample_factor = 2,
                        output_index = 0) {
  P <- .samdec_mat(prompt_tokens)
  I <- .samdec_mat(image_tokens)
  H <- as.integer(grid_shape[1])
  W <- as.integer(grid_shape[2])
  if (H * W != nrow(I))
    stop("samdec: ", nrow(I), " image tokens do not fill a ",
         H, "x", W, " grid")
  for (b in seq_len(as.integer(n_blocks))) {
    r <- two_way_block(P, I)
    P <- r$prompt_tokens
    I <- r$image_tokens
  }
  f <- as.integer(upsample_factor)
  # Build the (H, W) list-of-lists-of-vectors image grid from the
  # flattened I matrix, then upsample it in list form so we preserve
  # the per-pixel vectors.
  grid <- vector("list", H)
  for (i in seq_len(H)) {
    row <- vector("list", W)
    for (j in seq_len(W)) {
      row[[j]] <- as.numeric(I[(i - 1L) * W + j, ])
    }
    grid[[i]] <- row
  }
  big <- upsample(grid, factor = f)
  head <- dynamic_mask_head(P[as.integer(output_index) + 1L, ,
                              drop = FALSE], big)
  list(estimate = head$probability, mask = head$probability,
       logits = head$logits,
       shape = c(H * f, W * f),
       n_blocks = as.integer(n_blocks),
       method = "SAM mask decoder; Kirillov et al. (2023)",
       note = paste("two-way attention updates prompt AND image, then",
                    "a dynamic linear classifier built from the output",
                    "token scores every location"))
}

#' morie_samdec
#'
#' Part of the samdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param prompt_tokens See Usage.
#' @param image_tokens See Usage.
#' @param grid_shape See Usage.
#' @param n_blocks Defaults to \code{2}.
#' @param upsample_factor Defaults to \code{2}.
#' @param output_index Defaults to \code{0}.
#' @return The value of \code{decode_mask}.
#' @export
morie_samdec <- function(prompt_tokens, image_tokens, grid_shape,
                         n_blocks = 2, upsample_factor = 2,
                         output_index = 0) {
  decode_mask(prompt_tokens, image_tokens, grid_shape,
              n_blocks = n_blocks, upsample_factor = upsample_factor,
              output_index = output_index)
}

sam_mask_decoder <- decode_mask
sammaskdecoder <- decode_mask

.samdec_cheatsheet <- function() {
  paste("samdec: image embedding + prompt embeddings + a learned",
        "OUTPUT TOKEN -> mask. The decoder block does prompt",
        "self-attention and cross-attention in BOTH directions, so",
        "both embeddings are updated -- one direction would let the",
        "prompt read the image without the image knowing what was",
        "asked. After two blocks the image embedding is upsampled",
        "and an MLP turns the output token into a DYNAMIC linear",
        "classifier, so the mask is a dot product against weights",
        "built from the prompt. Loss is FOCAL + DICE, both chosen",
        "for the foreground/background imbalance.")
}
