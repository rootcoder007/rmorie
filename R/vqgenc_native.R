# morie.fn -- function file (rootcoder007/morie)
# r"""VQ-GAN's encoder: a codebook of context-rich visual parts.
#
# Transformers have no locality prior, so they must learn every
# relationship -- expressive, but quadratic in sequence length, which
# makes megapixel images infeasible. The fix is not a cheaper attention
# but a **shorter sequence**: use a convolutional encoder to compress an
# image into a grid of discrete indices into a learned codebook, and let
# the transformer model the composition of those parts instead of
# pixels.
#
# **Quantisation is nearest neighbour, and it is not differentiable.**
# Each encoder output :math:`\hat z_{ij}` is replaced by the closest
# codebook entry, and gradients are carried across the gap by a
# **straight-through estimator** -- the decoder's gradient is copied
# unchanged to the encoder. So the whole thing trains end to end even
# though the forward pass contains an ``argmin``.
#
# **The loss has three parts, and the stop-gradients decide who learns
# what**:
#
# .. math:: L_{VQ} = \|x - \hat x\|^2
#           + \|\mathrm{sg}[E(x)] - z_q\|_2^2
#           + \|\mathrm{sg}[z_q] - E(x)\|_2^2,
#
# the second term moving the *codebook* toward the encoder and the third
# -- the **commitment loss** -- moving the *encoder* toward its code. Drop
# the commitment term and the encoder is free to run away from a
# codebook that can never catch it.
#
# **And the compression is the point.** A :math:`256\times256` image at
# a downsampling factor of 16 becomes a :math:`16\times16 = 256`-token
# sequence, which is what brings a transformer into range at all.
#
# References
# ----------
# Esser, P., Rombach, R. & Ommer, B. (2021) "Taming Transformers for
# High-Resolution Image Synthesis", *Proceedings of the IEEE/CVF
# Conference on Computer Vision and Pattern Recognition (CVPR 2021)*,
# 12873-12883, arXiv:2012.09841. Sec. 3.1: transformers containing no
# inductive prior on locality and therefore having to learn all
# relationships, with quadratically increasing cost; the use of a
# convolutional approach to learn a codebook of context-rich visual
# parts and a transformer to model their global composition; the
# nearest-code quantisation s_ij = k such that (z_q)_ij = z_k; the
# straight-through gradient estimator copying gradients from the decoder
# to the encoder so model and codebook train end to end; and the loss
# L_VQ = ||x - x_hat||^2 + ||sg[E(x)] - z_q||^2 + ||sg[z_q] - E(x)||^2
# whose last term is the commitment loss.
#
# van den Oord, A., Vinyals, O. & Kavukcuoglu, K. (2017) "Neural
# Discrete Representation Learning", *NIPS 2017*, 6306-6315,
# arXiv:1711.00937. VQ-VAE, the commitment loss and the
# straight-through estimator this builds on.
# """

.vqgenc_as_matrix <- function(x) {
  if (is.matrix(x)) {
    m <- x
    storage.mode(m) <- "double"
    return(m)
  }
  if (is.data.frame(x)) {
    m <- as.matrix(x)
    storage.mode(m) <- "double"
    return(m)
  }
  if (is.list(x)) {
    n <- length(x)
    if (n == 0L) return(matrix(0, nrow = 0L, ncol = 0L))
    first <- x[[1]]
    d <- length(first)
    result <- matrix(0, nrow = n, ncol = d)
    for (i in seq_len(n)) {
      result[i, ] <- as.numeric(x[[i]])
    }
    return(result)
  }
  if (is.numeric(x)) {
    if (is.null(dim(x))) {
      return(matrix(x, nrow = 1L))
    }
  }
  stop("vqgenc: cannot convert input to matrix")
}

.vqgenc_as_vector <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) == 1L) {
      return(as.numeric(x))
    }
    if (ncol(x) == 1L) {
      return(as.numeric(x))
    }
    return(as.numeric(t(x)))
  }
  if (is.data.frame(x)) {
    return(as.numeric(unlist(x)))
  }
  if (is.list(x)) {
    return(as.numeric(unlist(x)))
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  stop("vqgenc: cannot convert input to vector")
}

.vqgenc_quantize <- function(vectors, codebook) {
  Z <- .vqgenc_as_matrix(codebook)
  V <- .vqgenc_as_matrix(vectors)

  if (nrow(Z) == 0L) {
    stop("vqgenc: the codebook is empty")
  }
  if (ncol(Z) != ncol(V)) {
    stop(sprintf("vqgenc: codebook entries are %d-wide but the encoder output is %d",
                 ncol(Z), ncol(V)))
  }

  n <- nrow(V)
  k <- nrow(Z)
  d <- ncol(V)

  idx <- integer(n)
  codes <- matrix(0, nrow = n, ncol = d)
  dists <- numeric(n)

  for (i in seq_len(n)) {
    d2 <- numeric(k)
    for (j in seq_len(k)) {
      diff <- V[i, ] - Z[j, ]
      d2[j] <- sum(diff * diff)
    }
    j_min <- which.min(d2)
    idx[i] <- j_min
    codes[i, ] <- Z[j_min, ]
    dists[i] <- sqrt(d2[j_min])
  }

  used <- length(unique(idx))

  list(
    indices = as.integer(idx),
    codes = codes,
    distance = dists,
    codebook_size = as.integer(k),
    used = as.integer(used),
    usage_fraction = used / as.numeric(k),
    note = "an argmin, hence not differentiable -- see straight_through"
  )
}

.vqgenc_straight_through <- function(encoder_output, quantized, upstream_gradient) {
  e <- .vqgenc_as_vector(encoder_output)
  q <- .vqgenc_as_vector(quantized)
  g <- .vqgenc_as_vector(upstream_gradient)

  if (length(e) != length(q) || length(q) != length(g)) {
    stop("vqgenc: the encoder output, code and gradient differ in length")
  }

  list(
    forward = q,
    backward = g,
    jacobian_is_identity = TRUE,
    note = "forward passes the CODE, backward passes the gradient through as if quantisation were absent"
  )
}

.vqgenc_codebook_loss <- function(encoder_output, quantized) {
  e <- .vqgenc_as_vector(encoder_output)
  q <- .vqgenc_as_vector(quantized)

  if (length(e) != length(q)) {
    stop("vqgenc: the vectors differ in length")
  }

  loss <- sum((e - q) ^ 2)

  list(
    loss = loss,
    gradient_flows_to = "codebook",
    note = "sg on the encoder side, so only z_q moves"
  )
}

.vqgenc_commitment_loss <- function(encoder_output, quantized, beta = 0.25) {
  e <- .vqgenc_as_vector(encoder_output)
  q <- .vqgenc_as_vector(quantized)

  if (length(e) != length(q)) {
    stop("vqgenc: the vectors differ in length")
  }

  b <- as.numeric(beta)
  if (b < 0) {
    stop("vqgenc: beta cannot be negative")
  }

  loss <- b * sum((e - q) ^ 2)

  list(
    loss = loss,
    beta = b,
    gradient_flows_to = "encoder",
    note = "sg on the code side, so only E(x) moves"
  )
}

.vqgenc_sequence_length <- function(height, width, downsample = 16L) {
  H <- as.integer(height)
  W <- as.integer(width)
  f <- as.integer(downsample)

  if (f < 1L || (H %% f) != 0L || (W %% f) != 0L) {
    stop(sprintf("vqgenc: %dx%d is not divisible by the downsampling factor %d", H, W, f))
  }

  n <- (H %/% f) * (W %/% f)

  list(
    tokens = as.integer(n),
    pixels = as.integer(H * W),
    compression = (H * W) / as.numeric(n),
    attention_cost_pixels = (H * W) ^ 2,
    attention_cost_tokens = n * n,
    speedup = ((H * W) ^ 2) / as.numeric(n * n),
    note = "attention is quadratic, so the saving is the SQUARE of the compression"
  )
}

.vqgenc_encode <- function(vectors, codebook, beta = 0.25, target = NULL) {
  q <- .vqgenc_quantize(vectors, codebook)
  V <- .vqgenc_as_matrix(vectors)

  n <- nrow(V)

  cb <- 0
  for (i in seq_len(n)) {
    cb <- cb + .vqgenc_codebook_loss(V[i, ], q$codes[i, ])$loss
  }

  cm <- 0
  for (i in seq_len(n)) {
    cm <- cm + .vqgenc_commitment_loss(V[i, ], q$codes[i, ], beta)$loss
  }

  rec <- 0
  if (!is.null(target)) {
    T_mat <- .vqgenc_as_matrix(target)
    rec <- sum((T_mat - q$codes) ^ 2)
  }

  list(
    estimate = q$indices,
    indices = q$indices,
    codes = q$codes,
    codebook_loss = cb,
    commitment_loss = cm,
    reconstruction = rec,
    loss = rec + cb + cm,
    usage_fraction = q$usage_fraction,
    method = "VQ-GAN encoder and codebook; Esser, Rombach & Ommer (2021)",
    note = "three terms; the stop-gradients decide whether the codebook or the encoder moves"
  )
}

.vqgenc_cheatsheet <- function() {
  paste("vqgenc: transformers have no locality prior and cost O(n^2),",
        "so shorten the SEQUENCE rather than cheapen the attention --",
        "a convolutional encoder compresses the image to a grid of",
        "indices into a learned CODEBOOK of visual parts.",
        "Quantisation is nearest-neighbour and NOT differentiable,",
        "so gradients cross by a STRAIGHT-THROUGH estimator (forward",
        "the code, backward the identity). The loss is reconstruction",
        "+ ||sg[E(x)] - z_q||^2 (moves the CODEBOOK) + ||sg[z_q] - E(x)||^2",
        "(the COMMITMENT loss, moves the ENCODER). Compression f=16",
        "turns 256x256 into 256 tokens; the attention saving is its SQUARE.",
        sep = " ")
}

morie_vqgenc <- .vqgenc_encode
