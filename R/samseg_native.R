# Segment Anything: promptable segmentation as a pre-training task.
# Sources: Kirillov, A., Mintun, E., Ravi, N., Mao, H., Rolland, C.,
# Gustafson, L., Xiao, T., Whitehead, S., Berg, A. C., Lo, W.-Y.,
# Dollar, P. & Girshick, R. (2023) "Segment Anything", *Proceedings
# of the IEEE/CVF International Conference on Computer Vision (ICCV
# 2023)*, 4015-4026, arXiv:2304.02643. Sec. 2 (the promptable
# segmentation task, and the requirement that the output be a
# reasonable mask for at least one object even when the prompt is
# ambiguous), Sec. 3 (the three constraints -- flexible prompting,
# amortised real-time computation, ambiguity-awareness; the image
# encoder run once per image; sparse prompts as positional encodings
# summed with learned per-type embeddings and dense mask prompts
# embedded by convolution and summed with the image embedding; ~50
# ms per prompt in a browser), and Sec. 5 (SA-1B: over 1B masks on
# 11M licensed, privacy-respecting images). Dosovitskiy, A. et al.
# (2021) "An Image is Worth 16x16 Words", *ICLR 2021*,
# arXiv:2010.11929. The ViT image encoder. He, K., Chen, X., Xie, S.,
# Li, Y., Dollar, P. & Girshick, R. (2022) "Masked Autoencoders Are
# Scalable Vision Learners", *CVPR 2022*, 16000-16009,
# arXiv:2111.06377. The MAE pre-training used for it.

.SAMSEG_EPS <- 1e-12

#' .samseg_pos_enc
#'
#' A step of the samseg_native implementation. Called by \code{encode_box_prompt}, \code{encode_point_prompt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param dim Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param scale Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
.samseg_pos_enc <- function(x, y, dim = 8, scale = 1.0) {
  out <- numeric(0)
  for (j in 0:(as.integer(dim) %/% 2L - 1L)) {
    f <- (2.0^j) * pi * as.numeric(scale)
    out <- c(out, sin(f * as.numeric(x)), cos(f * as.numeric(y)))
  }
  out
}

#' encode_point_prompt
#'
#' A step of the samseg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param points Iterated over elementwise, with \code{lapply}.
#' @param labels Passed to \code{unlist}.
#' @param dim Passed to \code{.samseg_pos_enc}. Defaults to \code{8}.
#' @param type_embeddings Defaults to \code{NULL}.
#' @return A list with \code{tokens}, \code{n_prompts}, \code{sparse}, \code{note}.
#' @export
encode_point_prompt <- function(points, labels, dim = 8, type_embeddings = NULL) {
  P <- lapply(points, function(p) c(as.numeric(p[1]), as.numeric(p[2])))
  L <- as.integer(unlist(labels))
  if (length(P) != length(L))
    stop(sprintf("samseg: %d points but %d labels", length(P), length(L)))
  if (any(!(L %in% c(0L, 1L))))
    stop("samseg: a point label must be 1 (foreground) or 0 (background)")
  te <- type_embeddings
  if (is.null(te)) te <- list()
  out <- list()
  for (i in seq_along(P)) {
    e <- .samseg_pos_enc(P[[i]][1], P[[i]][2], dim)
    name <- if (L[i] == 1L) "foreground" else "background"
    t <- if (!is.null(te[[name]])) as.numeric(te[[name]]) else rep(0, length(e))
    if (length(t) != length(e))
      stop("samseg: the type embedding has the wrong width")
    out[[i]] <- as.numeric(e) + t
  }
  list(tokens = out, n_prompts = length(out), sparse = TRUE,
       note = paste("a background click at the same place is a ",
                    "DIFFERENT token, by the type embedding", sep = ""))
}

#' encode_box_prompt
#'
#' A step of the samseg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param box The body requires: samseg: the box is empty or inverted.
#' @param dim Passed to \code{.samseg_pos_enc}. Defaults to \code{8}.
#' @param type_embeddings Defaults to \code{NULL}.
#' @return A list with \code{tokens}, \code{n_prompts}, \code{sparse}.
#' @export
encode_box_prompt <- function(box, dim = 8, type_embeddings = NULL) {
  v <- as.numeric(unlist(box))
  x0 <- v[1]; y0 <- v[2]; x1 <- v[3]; y1 <- v[4]
  if (x1 <= x0 || y1 <= y0)
    stop("samseg: the box is empty or inverted")
  te <- type_embeddings
  if (is.null(te)) te <- list()
  a <- .samseg_pos_enc(x0, y0, dim)
  b <- .samseg_pos_enc(x1, y1, dim)
  ta <- if (!is.null(te$box_tl)) as.numeric(te$box_tl) else rep(0, length(a))
  tb <- if (!is.null(te$box_br)) as.numeric(te$box_br) else rep(0, length(b))
  list(tokens = list(a + ta, b + tb), n_prompts = 2L, sparse = TRUE)
}

#' encode_mask_prompt
#'
#' A step of the samseg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mask See Usage.
#' @param image_embedding See Usage.
#' @param weight Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{embedding}, \code{sparse}, \code{note}.
#' @export
encode_mask_prompt <- function(mask, image_embedding, weight = 1.0) {
  M <- mask
  if (is.list(M) && !is.matrix(M)) M <- do.call(rbind, M)
  storage.mode(M) <- "double"
  E <- image_embedding
  if (is.list(E) && !is.matrix(E)) E <- do.call(rbind, E)
  storage.mode(E) <- "double"
  if (nrow(M) != nrow(E) || ncol(M) != ncol(E))
    stop(sprintf("samseg: the mask prompt is %dx%d but the image embedding is %dx%d",
                 nrow(M), ncol(M), nrow(E), ncol(E)))
  w <- as.numeric(weight)
  list(embedding = E + w * M, sparse = FALSE,
       note = "summed, so the decoder input shape is unchanged")
}

#' amortised_cost
#'
#' A step of the samseg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param encoder_ms Coerced to numeric by the body, with \code{as.numeric}.
#' @param decoder_ms Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_prompts Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{total_ms}, \code{per_prompt_ms}, \code{naive_ms}, \code{speedup}, \code{interactive}, \code{note}.
#' @export
amortised_cost <- function(encoder_ms, decoder_ms, n_prompts) {
  e <- as.numeric(encoder_ms); d <- as.numeric(decoder_ms)
  P <- as.integer(n_prompts)
  if (P < 1L) stop("samseg: at least one prompt is needed")
  if (e <= 0 || d <= 0) stop("samseg: the timings must be positive")
  total <- e + P * d
  list(total_ms = total, per_prompt_ms = total / P,
       naive_ms = P * (e + d), speedup = P * (e + d) / total,
       interactive = d < 100.0,
       note = "the image embedding is computed once and reused")
}

#' promptable_segment
#'
#' A step of the samseg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param image_embedding Passed to \code{decoder}.
#' @param prompt_tokens Passed to \code{decoder}.
#' @param decoder The body requires: samseg: the decoder returned no mask; the task requires a valid mask for ANY prompt.
#' @param multimask Coerced to logical by the body, with \code{as.logical}. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{masks}, \code{n_masks}, \code{multimask}, \code{method}, \code{note}.
#' @export
promptable_segment <- function(image_embedding, prompt_tokens, decoder,
                               multimask = TRUE) {
  masks <- decoder(image_embedding, prompt_tokens, multimask)
  if (length(masks) == 0)
    stop("samseg: the decoder returned no mask; the task requires a valid mask for ANY prompt")
  list(estimate = masks[[1]], masks = masks, n_masks = length(masks),
       multimask = as.logical(multimask),
       method = "promptable segmentation; Kirillov et al. (2023)",
       note = paste("a valid mask for any prompt, and for an ",
                    "ambiguous prompt a valid mask for at least one ",
                    "intended object", sep = ""))
}

#' .samseg_cheatsheet
#'
#' A step of the samseg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.samseg_cheatsheet <- function() {
  paste("samseg: the task is 'return a VALID mask for any prompt, ",
        "and for an AMBIGUOUS prompt a valid mask for at least one ",
        "intended object' -- which is what makes it usable as ",
        "pre-training and for zero-shot transfer by prompting. Three ",
        "constraints force the architecture: flexible prompts, ",
        "amortised real-time use, ambiguity-awareness. So a heavy ",
        "image encoder runs ONCE per image and a light prompt encoder ",
        "plus mask decoder run per prompt (~50 ms). Sparse prompts are ",
        "positional encodings plus a learned PER-TYPE embedding; dense ",
        "mask prompts are SUMMED with the image embedding.", sep = "")
}

segmentanything <- promptable_segment
sam_segment <- promptable_segment
samsegment <- promptable_segment

morie_samseg <- promptable_segment
