# morie.fn -- function file (rootcoder007/morie)
# SAM 2: the same model, plus a memory of what it already segmented.
#
# An image is a static snapshot; a video is the same objects deforming,
# occluding one another, leaving the frame and coming back. SAM 2
# generalises promptable segmentation to video by keeping a streaming
# memory: frames arrive one at a time, and the current frame's
# features are conditioned on memories of past frames before the mask
# decoder ever sees them.
#
# The reduction to SAM is exact and is the design claim. With an
# empty memory bank the memory attention has nothing to attend to and
# the model behaves exactly like SAM on a single image. propagate
# therefore returns the unconditioned features unchanged on the first
# frame, and the anchor asserts that equality rather than a tolerance.
#
# The memory bank is two FIFO queues, not one. Up to N recent frames,
# and separately up to M prompted frames. In the common video-object-
# segmentation case the only prompt is the first frame's mask, so the
# bank must keep that memory permanently while recent memories churn
# past it -- one queue would evict the thing the user actually
# specified.
#
# Temporal position embeddings go on the recent memories only. The
# recent frames carry short-term motion, so their ordering is
# informative. Prompted frames are deliberately left unembedded: the
# training signal from them is sparser, and at inference they may come
# from a temporal range never seen in training, so encoding their
# distance would not generalise.
#
# Object pointers are separate from spatial memory. Lightweight
# vectors taken from the mask decoder's output tokens carry high-level
# semantics ("this object"), and memory attention cross-attends to
# both them and the spatial feature maps -- appearance can change
# completely while the pointer stays the same object.
#
# References
# ----------
# Ravi, N., Gabeur, V., Hu, Y.-T., Hu, R., Ryali, C., Ma, T., Khedr, H.,
# Radle, R., Rolland, C., Gustafson, L., Mintun, E., Pan, J., Alwala,
# K. V., Carion, N., Wu, C.-Y., Girshick, R., Dollar, P. &
# Feichtenhofer, C. (2024) "SAM 2: Segment Anything in Images and
# Videos", arXiv:2408.00714. Sec. 4: the streaming architecture
# processing video frames one at a time with a memory attention module
# attending to previous memories of the target object, and that when
# applied to images the memory is empty and the model behaves like SAM;
# the memory attention stacking L blocks of self-attention followed by
# cross-attention to memories of prompted and unprompted frames and to
# OBJECT POINTERS, followed by an MLP; the memory bank as a FIFO queue
# of up to N recent frames plus a separate FIFO queue of up to M
# prompted frames, both stored as spatial feature maps; the object
# pointers taken from mask decoder output tokens; and temporal
# position information embedded into the N recent memories but NOT
# into prompted frames, because their training signal is sparser and
# generalising to unseen temporal ranges is harder. Also the reported
# results: 3x fewer interactions in video and 6x faster than SAM on
# images, with the SA-V dataset of 35.5M masks across 50.9K videos.
#
# Kirillov, A. et al. (2023) "Segment Anything", ICCV 2023,
# 4015-4026, arXiv:2304.02643. The image model generalised here.

# ---- private helpers --------------------------------------------------

.sam2vd_to_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.list(x)) return(as.numeric(unlist(x)))
  as.numeric(x)
}

# ---- public API --------------------------------------------------------

morie_sam2vd_memory_bank <- function(n_recent = 7, m_prompted = 1) {
  N <- as.integer(n_recent)
  M <- as.integer(m_prompted)
  if (N < 1L || M < 1L) {
    stop("sam2vd: both queue capacities must be >= 1")
  }
  list(
    recent = list(),
    prompted = list(),
    pointers = list(),
    n_recent = N,
    m_prompted = M,
    note = "one queue would evict the frame the user actually prompted"
  )
}

morie_sam2vd_push_memory <- function(bank, frame_index, features,
                                    prompted = FALSE, object_pointer = NULL) {
  b <- bank
  b$recent    <- bank$recent
  b$prompted  <- bank$prompted
  b$pointers  <- bank$pointers

  entry <- list(
    frame    = as.integer(frame_index),
    features = .sam2vd_to_num(features),
    prompted = as.logical(prompted)
  )

  if (isTRUE(prompted)) {
    b$prompted <- c(b$prompted, list(entry))
    if (length(b$prompted) > b$m_prompted) {
      b$prompted <- b$prompted[-1L]
    }
  } else {
    b$recent <- c(b$recent, list(entry))
    if (length(b$recent) > b$n_recent) {
      b$recent <- b$recent[-1L]
    }
  }

  if (!is.null(object_pointer)) {
    ptr <- list(
      frame  = as.integer(frame_index),
      vector = .sam2vd_to_num(object_pointer)
    )
    b$pointers <- c(b$pointers, list(ptr))
    cap <- b$n_recent + b$m_prompted
    if (length(b$pointers) > cap) {
      b$pointers <- b$pointers[-1L]
    }
  }

  b
}

morie_sam2vd_temporal_embedding <- function(entry, current_frame,
                                            dim = NULL, scale = 0.1) {
  v <- entry$features
  if (isTRUE(entry$prompted)) {
    return(list(
      features  = v,
      embedded  = FALSE,
      note      = "prompted memories carry no temporal position, by design"
    ))
  }

  d <- as.integer(current_frame) - as.integer(entry$frame)
  n <- if (is.null(dim)) length(v) else as.integer(dim)
  lim <- min(n, length(v))

  out <- v
  if (lim > 0L) {
    # Python: out[i] = v[i] + sin(scale * d * (i + 1)) for i in 0..lim-1
    # R 1-based: out[i] = v[i] + sin(scale * d * i)      for i in 1..lim
    for (i in seq_len(lim)) {
      out[i] <- v[i] + sin(as.numeric(scale) * d * i)
    }
  }

  list(features = out, embedded = TRUE, distance = d)
}

morie_sam2vd_memory_attention <- function(frame_features, bank, current_frame,
                                          n_blocks = 1, include_pointers = TRUE) {
  x <- .sam2vd_to_num(frame_features)

  mem <- list()
  all_entries <- c(bank$prompted, bank$recent)
  for (e in all_entries) {
    t_emb <- morie_sam2vd_temporal_embedding(e, current_frame)
    if (length(t_emb$features) != length(x)) {
      stop(sprintf("sam2vd: a memory has width %d but the frame has %d",
                   length(t_emb$features), length(x)))
    }
    mem[[length(mem) + 1L]] <- t_emb$features
  }

  if (isTRUE(include_pointers)) {
    for (p in bank$pointers) {
      if (length(p$vector) == length(x)) {
        mem[[length(mem) + 1L]] <- p$vector
      }
    }
  }

  if (length(mem) == 0L) {
    return(list(
      features   = x,
      attended   = FALSE,
      n_memories = 0L,
      weights    = numeric(0),
      note       = "empty memory: the model IS SAM here"
    ))
  }

  out <- x
  w   <- numeric(0)
  for (block in seq_len(as.integer(n_blocks))) {
    d  <- length(out)
    sc <- sapply(mem, function(m) sum(out * m) / sqrt(d))
    top <- max(sc)
    e   <- exp(sc - top)
    z   <- sum(e)
    w   <- e / z

    # mem is a list of equal-length numeric vectors; build a matrix
    # so that mem_mat %*% w yields the context vector directly.
    mem_mat <- do.call(rbind, mem)
    ctx <- as.numeric(mem_mat %*% w)
    out <- out + ctx
  }

  list(
    features   = out,
    attended   = TRUE,
    n_memories = length(mem),
    weights    = w,
    note       = "self-attention, then cross-attention to spatial memories AND object pointers"
  )
}

morie_sam2vd_propagate <- function(frames, encoder, decoder, prompts = NULL,
                                   n_recent = 7, m_prompted = 1) {
  P <- if (is.null(prompts)) list() else prompts

  bank <- morie_sam2vd_memory_bank(n_recent, m_prompted)
  masks       <- list()
  conditioned <- list()

  for (t in seq_along(frames)) {
    f   <- frames[[t]]
    t0  <- t - 1L  # 0-based frame index, matching the Python reference
    raw <- .sam2vd_to_num(encoder(f))

    att <- morie_sam2vd_memory_attention(raw, bank, t0)
    conditioned[[length(conditioned) + 1L]] <- att$attended

    pkey  <- as.character(t0)
    p_val <- if (pkey %in% names(P)) P[[pkey]] else NULL
    m <- decoder(att$features, p_val)
    masks[[length(masks) + 1L]] <- m

    is_prompted <- pkey %in% names(P)
    bank <- morie_sam2vd_push_memory(bank, t0, att$features,
                                     prompted = is_prompted,
                                     object_pointer = att$features)
  }

  first_frame_is_sam <- if (length(conditioned) > 0L) !conditioned[[1L]] else TRUE

  list(
    estimate           = masks,
    masks              = masks,
    conditioned        = conditioned,
    n_frames           = length(masks),
    first_frame_is_sam = first_frame_is_sam,
    method             = "streaming memory propagation; Ravi et al. (2024)",
    note               = "frame 0 has an empty memory, so it is exactly the image model"
  )
}

morie_sam2vd_cheatsheet <- function() {
  paste0(
    "sam2vd: video is the same objects deforming, occluding ",
    "and re-appearing, so carry a STREAMING MEMORY -- condition ",
    "each frame's features on memories of past frames before ",
    "decoding. With an EMPTY memory the model is exactly SAM, ",
    "which is the design claim. The bank is TWO FIFO queues: N ",
    "recent frames and M PROMPTED frames, because one queue ",
    "would evict the frame the user specified. Temporal ",
    "position embeddings go on recent memories only -- prompted ",
    "frames may sit at distances never trained on. OBJECT ",
    "POINTERS carry identity when appearance changes ",
    "completely."
  )
}

# ---- entry point and aliases ------------------------------------------

# Main entry point
morie_sam2vd <- morie_sam2vd_propagate

# Compact alias per ledger/NAMING.md
morie_sam2vd_sam2video <- morie_sam2vd_propagate

# Public names resolved by fn/_lazy_map.json
morie_sam2vd_sam2_video_propagation <- morie_sam2vd_propagate

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank

#' @rdname morie_sam2vd_memory_bank
#' @export
morie_sam2vd <- morie_sam2vd_memory_bank
