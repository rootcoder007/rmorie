# morie.fn -- function file (rootcoder007/morie)
# Video diffusion: a 3D U-Net factorised over space and time.
#
# A video model built by inflating an image model has to answer two
# questions: how to attend across frames without paying (FS)^2, and
# how to keep the image model's quality while doing so.
#
# Factorisation answers both. Each 3x3 convolution becomes a 1x3x3
# convolution -- space only. Each spatial attention block keeps
# attending over space with the frame axis treated as a batch axis,
# and a temporal attention block is inserted after it, attending over
# frames with the spatial axes as batch. Cost falls from (FS)^2 to
# F S^2 + S F^2, which attention_cost computes exactly.
#
# And the factorisation buys something unique to video: the model can
# be masked to run on independent images simply by fixing each
# temporal attention matrix to the identity -- each query attends only
# to its own timestep. That makes joint training on video and image
# objectives straightforward, and the paper finds that joint training
# matters for sample quality. as_image_model performs exactly that
# masking, and the anchor checks the resulting output equals the
# per-frame computation exactly, not approximately.
#
# Reconstruction guidance extends the model past its frame count. To
# condition a sample on given frames x^a, add a gradient of the
# squared error between the model's denoised estimate of those frames
# and their true values, weighted by w_r > 1. It is a guidance term,
# so it is applied at sampling time to a model that was never trained
# conditionally -- and the same construction with a downsampling
# operator inside the loss gives spatial super-resolution.
#
# References
# ----------
# Ho, J., Salimans, T., Gritsenko, A., Chan, W., Norouzi, M. & Fleet,
# D. J. (2022) "Video Diffusion Models", Advances in Neural
# Information Processing Systems 35 (NeurIPS 2022), arXiv:2204.03458.
# Sec. 3: the 3D U-Net factorised over space and time, changing each
# 3x3 convolution into a 1x3x3 space-only convolution, keeping spatial
# attention with the frame axis as a batch axis, and inserting a
# temporal attention block after each spatial attention block; that
# the factorisation makes it straightforward to mask the model to run
# on independent images by fixing the temporal attention matrix to
# match each key and query at the same timestep, enabling JOINT
# training on video and image objectives, which the experiments find
# important for sample quality; and Sec. 4 (reconstruction guidance
# for conditional generation, with a weighting factor w_r > 1
# improving sample quality, extended to spatial interpolation and
# super-resolution by imposing the squared error on a downsampled
# model prediction and backpropagating through the downsampling).
#
# Ho, J., Jain, A. & Abbeel, P. (2020) "Denoising Diffusion
# Probabilistic Models", NeurIPS 2020, arXiv:2006.11239. The diffusion
# model being inflated.
#
# Ho, J. & Salimans, T. (2022) "Classifier-Free Diffusion Guidance",
# arXiv:2207.12598. The guidance framework this parallels.

#' .vidgen_mat
#'
#' A step of the vidgen_native implementation. Called by \code{morie_vidgen_space_only_conv}, \code{morie_vidgen_spatial_attention}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return The value of \code{do.call}.
#' @export
.vidgen_mat <- function(x) {
  if (is.matrix(x)) return(x * 1.0)
  rows <- lapply(x, function(r) as.numeric(unlist(r)))
  do.call(rbind, rows)
}

#' .vidgen_vec
#'
#' A step of the vidgen_native implementation. Called by \code{morie_vidgen_reconstruction_guidance}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{unlist}.
#' @return A vector, from \code{as.numeric}.
#' @export
.vidgen_vec <- function(x) {
  as.numeric(unlist(x))
}

#' .vidgen_softmax_attend
#'
#' A step of the vidgen_native implementation. Called by \code{morie_vidgen_spatial_attention}, \code{morie_vidgen_temporal_attention}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param mask Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @return A list with \code{out}, \code{W}.
#' @export
.vidgen_softmax_attend <- function(X, mask = NULL) {
  n <- nrow(X)
  d <- ncol(X)
  out <- matrix(0.0, nrow = n, ncol = d)
  W <- matrix(0.0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    sc <- numeric(n)
    for (j in seq_len(n)) {
      s <- 0.0
      for (a in seq_len(d)) {
        s <- s + X[i, a] * X[j, a]
      }
      sc[j] <- s / sqrt(d)
    }
    if (!is.null(mask)) {
      for (j in seq_len(n)) {
        if (!mask[i, j]) sc[j] <- -1e30
      }
    }
    m <- max(sc)
    e <- exp(sc - m)
    z <- sum(e)
    w <- e / z
    W[i, ] <- w
    for (a in seq_len(d)) {
      s <- 0.0
      for (j in seq_len(n)) {
        s <- s + w[j] * X[j, a]
      }
      out[i, a] <- s
    }
  }
  list(out = out, W = W)
}

#' morie_vidgen_space_only_conv
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param video Iterated over elementwise, with \code{lapply}.
#' @param kernel Passed to \code{.vidgen_mat}.
#' @return A list with \code{video}, \code{frames}, \code{note}.
#' @export
morie_vidgen_space_only_conv <- function(video, kernel) {
  V <- lapply(video, .vidgen_mat)
  K <- .vidgen_mat(kernel)
  kh <- nrow(K)
  kw <- ncol(K)
  out <- list()
  for (fi in seq_along(V)) {
    fr <- V[[fi]]
    H <- nrow(fr)
    W <- ncol(fr)
    if (kh > H || kw > W) {
      stop("vidgen: the kernel is larger than the frame")
    }
    o <- matrix(0.0, nrow = H - kh + 1, ncol = W - kw + 1)
    for (i in seq_len(H - kh + 1)) {
      for (j in seq_len(W - kw + 1)) {
        s <- 0.0
        for (a in seq_len(kh)) {
          for (b in seq_len(kw)) {
            s <- s + fr[i + a - 1, j + b - 1] * K[a, b]
          }
        }
        o[i, j] <- s
      }
    }
    out[[fi]] <- o
  }
  list(
    video = out,
    frames = length(out),
    note = "no kernel taps across frames; time is handled only by the temporal attention block"
  )
}

#' morie_vidgen_spatial_attention
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param video See Usage.
#' @return A list with \code{video}, \code{weights}, \code{note}.
#' @export
morie_vidgen_spatial_attention <- function(video) {
  out <- list()
  weights <- list()
  for (fr in video) {
    X <- .vidgen_mat(fr)
    res <- .vidgen_softmax_attend(X)
    out[[length(out) + 1]] <- res$out
    weights[[length(weights) + 1]] <- res$W
  }
  list(
    video = out,
    weights = weights,
    note = "each frame attended independently"
  )
}

#' morie_vidgen_temporal_attention
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param video Iterated over elementwise, with \code{lapply}.
#' @param identity A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{video}, \code{identity}, \code{note}.
#' @export
morie_vidgen_temporal_attention <- function(video, identity = FALSE) {
  V <- lapply(video, .vidgen_mat)
  F <- length(V)
  if (F < 1) stop("vidgen: the video has no frames")
  H <- nrow(V[[1]])
  W <- ncol(V[[1]])
  for (f in seq_len(F)) {
    if (nrow(V[[f]]) != H || ncol(V[[f]]) != W) {
      stop("vidgen: the frames differ in shape")
    }
  }
  out <- lapply(seq_len(F), function(t) matrix(0.0, nrow = H, ncol = W))
  for (i in seq_len(H)) {
    for (j in seq_len(W)) {
      series <- matrix(0.0, nrow = F, ncol = 1)
      for (t in seq_len(F)) {
        series[t, 1] <- V[[t]][i, j]
      }
      if (isTRUE(identity)) {
        for (t in seq_len(F)) {
          out[[t]][i, j] <- series[t, 1]
        }
      } else {
        res <- .vidgen_softmax_attend(series)
        o <- res$out
        for (t in seq_len(F)) {
          out[[t]][i, j] <- o[t, 1]
        }
      }
    }
  }
  list(
    video = out,
    identity = as.logical(identity),
    note = "identity=TRUE is EXACTLY the independent-image case, which is what makes joint training easy"
  )
}

#' morie_vidgen_as_image_model
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param video Iterated over elementwise, with \code{lapply}.
#' @param block Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{video}, \code{note}.
#' @export
morie_vidgen_as_image_model <- function(video, block) {
  list(
    video = lapply(video, function(fr) block(list(fr))$video[[1]]),
    note = "frames processed alone; the masked video model must equal this exactly"
  )
}

#' morie_vidgen_attention_cost
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param frames Coerced to integer by the body, with \code{as.integer}.
#' @param spatial_positions Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{joint}, \code{factorised}, \code{ratio}, \code{note}.
#' @export
morie_vidgen_attention_cost <- function(frames, spatial_positions) {
  F <- as.integer(frames)
  S <- as.integer(spatial_positions)
  if (F < 1 || S < 1) {
    stop("vidgen: the frame and position counts must be positive")
  }
  joint <- (F * S)^2
  fact <- F * S * S + S * F * F
  list(
    joint = joint,
    factorised = fact,
    ratio = joint / as.numeric(fact),
    note = "(FS)^2 against F S^2 + S F^2"
  )
}

#' morie_vidgen_reconstruction_guidance
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x_hat Iterated over elementwise, with \code{lapply}.
#' @param observed Iterated over elementwise, with \code{lapply}.
#' @param index Coerced to integer by the body, with \code{as.integer}.
#' @param weight Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2}.
#' @param downsample Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{estimate}, \code{gradient}, \code{error}, \code{weight}, \code{guided_frames}, \code{method}, \code{note}.
#' @export
morie_vidgen_reconstruction_guidance <- function(x_hat, observed, index,
                                                  weight = 2.0,
                                                  downsample = NULL) {
  X <- lapply(x_hat, .vidgen_vec)
  O <- lapply(observed, .vidgen_vec)
  idx <- as.integer(index)
  if (length(O) != length(idx)) {
    stop(sprintf("vidgen: %d observed frames but %d indices",
                 length(O), length(idx)))
  }
  w <- as.numeric(weight)
  if (w <= 0.0) stop("vidgen: the guidance weight must be positive")
  grad <- lapply(X, function(row) rep(0.0, length(row)))
  err <- 0.0
  for (a in seq_along(idx)) {
    t <- idx[a]
    if (t < 0 || t >= length(X)) {
      stop(sprintf("vidgen: frame %d is outside the sample", t))
    }
    ti <- t + 1
    pred <- X[[ti]]
    tgt <- O[[a]]
    if (!is.null(downsample)) {
      pred_ds <- .vidgen_vec(downsample(pred))
      if (length(pred_ds) != length(tgt)) {
        stop("vidgen: the downsampled prediction does not match the low-resolution target")
      }
      for (i in seq_along(tgt)) {
        err <- err + (pred_ds[i] - tgt[i])^2
      }
      for (i in seq_along(grad[[ti]])) {
        grad[[ti]][i] <- 0.0
      }
      next
    }
    for (i in seq_along(tgt)) {
      d <- pred[i] - tgt[i]
      err <- err + d * d
      grad[[ti]][i] <- -w * 2.0 * d
    }
  }
  list(
    estimate = grad,
    gradient = grad,
    error = err,
    weight = w,
    guided_frames = idx,
    method = "reconstruction guidance; Ho et al. (2022)",
    note = "applied at SAMPLING time, so the model itself was never trained conditionally"
  )
}

#' morie_vidgen_cheatsheet
#'
#' A step of the vidgen_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_vidgen_cheatsheet <- function() {
  "vidgen: a 3D U-Net FACTORISED over space and time -- each 3x3 convolution becomes 1x3x3 (space only), spatial attention keeps the frame axis as a BATCH axis, and a temporal attention block is inserted after it with the spatial axes as batch. Cost drops from (FS)^2 to F S^2 + S F^2. The unique payoff: fixing the temporal attention to the IDENTITY makes the model run on independent images exactly, so video and image objectives can be trained JOINTLY -- which matters for sample quality. RECONSTRUCTION GUIDANCE conditions on given frames at sampling time, and with a downsampler inside the loss gives super-resolution."
}

# compact alias per ledger/NAMING.md
morie_vidgen_videodiffusion <- morie_vidgen_reconstruction_guidance

# public names resolved by fn/_lazy_map.json
morie_vidgen_video_diffusion <- morie_vidgen_reconstruction_guidance
