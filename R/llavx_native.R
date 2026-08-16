# LLaVA: visual instruction tuning.
# Sources: Liu, H., Li, C., Wu, Q. & Lee, Y. J. (2023) "Visual Instruction
# Tuning", *Advances in Neural Information Processing Systems 36
# (NeurIPS 2023)*, arXiv:2304.08485. The abstract: instruction tuning
# improves zero-shot capabilities on new tasks but is less explored in
# the multimodal field; the first attempt to use LANGUAGE-ONLY GPT-4 to
# generate multimodal language-image instruction-following data; LLaVA
# as an end-to-end trained large multimodal model connecting a vision
# encoder and an LLM for general-purpose visual and language
# understanding; two evaluation benchmarks constructed for visual
# instruction following; a 85.1% relative score compared with GPT-4 on a
# synthetic multimodal instruction-following dataset; and 92.53%
# accuracy on Science QA from the synergy of LLaVA and GPT-4.
#
# Radford, A. et al. (2021) "Learning Transferable Visual Models From
# Natural Language Supervision", *ICML 2021*, PMLR 139, 8748-8763,
# arXiv:2103.00020. The vision encoder.
#
# Li, J., Li, D., Savarese, S. & Hoi, S. (2023) "BLIP-2", *ICML 2023*,
# PMLR 202, 19730-19742, arXiv:2301.12597. The alternative bridge, a
# Q-Former rather than a projection; implemented in blip2v.

.LLAVX_KINDS <- c("conversation", "detailed_description", "complex_reasoning")

#' symbolic_representation
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @param captions See Usage.
#' @param boxes See Usage.
#' @return A list with \code{text}, \code{n_captions}, \code{n_boxes}, \code{note}.
#' @export
symbolic_representation <- function(captions, boxes) {
  caps <- vapply(as.list(captions), as.character, character(1))
  bx <- as.list(boxes)
  if (length(caps) == 0L && length(bx) == 0L)
    stop("llavx: an image with no captions and no boxes has no symbolic representation")
  lines <- as.character(caps)
  for (rec in bx) {
    rec <- as.list(rec)
    name <- as.character(rec[[1L]])
    x <- as.numeric(rec[[2L]]); y <- as.numeric(rec[[3L]])
    w <- as.numeric(rec[[4L]]); h <- as.numeric(rec[[5L]])
    lines <- c(lines, paste0(name, ": [", sprintf("%.3f", x), ", ",
                             sprintf("%.3f", y), ", ", sprintf("%.3f", w),
                             ", ", sprintf("%.3f", h), "]"))
  }
  list(text = paste(lines, collapse = "\n"),
       n_captions = length(caps),
       n_boxes = length(bx),
       note = "the generator is LANGUAGE-ONLY; the image itself never reaches it")
}

#' instruction_prompt
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @param symbolic See Usage.
#' @param kind Defaults to \code{"conversation"}.
#' @return A list with \code{prompt}, \code{kind}.
#' @export
instruction_prompt <- function(symbolic, kind = "conversation") {
  if (!(kind %in% .LLAVX_KINDS))
    stop("llavx: kind must be one of ",
         paste(.LLAVX_KINDS, collapse = ", "), ", got ", shQuote(kind))
  ask <- switch(kind,
    "conversation" = "Ask and answer questions about this image as if you can see it.",
    "detailed_description" = "Describe this image in detail.",
    "complex_reasoning" = "Give a question requiring step-by-step reasoning about this image, and answer it.",
    stop("llavx: kind must be one of ",
         paste(.LLAVX_KINDS, collapse = ", "), ", got ", shQuote(kind))
  )
  list(prompt = paste0(symbolic[["text"]], "\n\n", ask), kind = kind)
}

#' project_patches
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @param patch_features See Usage.
#' @param W See Usage.
#' @param b Defaults to \code{NULL}.
#' @return The value of \code{lapply}.
#' @export
project_patches <- function(patch_features, W, b = NULL) {
  Fmat <- lapply(patch_features, function(r) as.numeric(r))
  d_in <- length(Fmat[[1L]])
  d_out <- length(W)
  if (length(W[[1L]]) != d_in)
    stop("llavx: the projection expects ", length(W[[1L]]),
         " features but got ", d_in)
  Wmat <- do.call(rbind, lapply(W, as.numeric))
  if (is.null(b)) {
    bvec <- rep(0, d_out)
  } else {
    bvec <- as.numeric(b)
  }
  out <- tcrossprod(Wmat, do.call(rbind, Fmat))
  out <- sweep(out, 1L, bvec, "+")
  lapply(seq_len(nrow(out)), function(i) out[i, ])
}

#' build_sequence
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @param visual_tokens See Usage.
#' @param text_embeddings See Usage.
#' @return A list with \code{estimate}, \code{sequence}, \code{n_visual}, \code{n_text}, \code{method}, \code{note}.
#' @export
build_sequence <- function(visual_tokens, text_embeddings) {
  V <- lapply(visual_tokens, function(r) as.numeric(r))
  T <- lapply(text_embeddings, function(r) as.numeric(r))
  if (length(V) > 0L && length(T) > 0L && length(V[[1L]]) != length(T[[1L]]))
    stop("llavx: visual tokens are ", length(V[[1L]]),
         "-dimensional but text embeddings are ", length(T[[1L]]),
         " -- the projection target is wrong")
  est <- c(V, T)
  list(estimate = est, sequence = est,
       n_visual = length(V), n_text = length(T),
       method = "visual instruction tuning; Liu, Li, Wu & Lee (2023)",
       note = "projected patches ARE tokens -- no cross-attention layers are introduced")
}

visualinstruction <- build_sequence
llava_visual_chat <- build_sequence

#' training_stage
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @param stage See Usage.
#' @return A list with \code{stage}, \code{trainable}, \code{frozen}, \code{data}, \code{note}.
#' @export
training_stage <- function(stage) {
  s <- as.integer(stage)
  if (!(s %in% c(1L, 2L)))
    stop("llavx: the stage must be 1 or 2, got ", format(stage))
  if (s == 1L)
    return(list(stage = 1L, trainable = list("projection"),
                frozen = list("vision_encoder", "language_model"),
                data = "image-caption pairs",
                note = "align the spaces before tuning anything on them"))
  list(stage = 2L, trainable = list("projection", "language_model"),
       frozen = list("vision_encoder"),
       data = "GPT-4 generated instruction-following data",
       note = "tuning the language model first would tune it against features that do not yet mean anything")
}

#' .llavx_cheatsheet
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.llavx_cheatsheet <- function() {
  paste("llavx: instruction tuning works in language and lacked ",
        "MULTIMODAL data, so generate it with a LANGUAGE-ONLY ",
        "GPT-4 fed a SYMBOLIC image -- captions and boxes. The ",
        "image never reaches the generator, which is what makes ",
        "the pipeline possible and also caps it: what the captions ",
        "omit cannot be asked about. Architecture is deliberately ",
        "thin: ONE projection matrix into the word-embedding ",
        "space, projected patches used as tokens, no ",
        "cross-attention. Stage 1 trains only the projection; ",
        "stage 2 adds the language model.", sep = "")
}

#' morie_llavx
#'
#' Part of the llavx_native implementation; see the file header for the
#' source it follows.
#'
#' @param op See Usage.
#' @param ... Passed through.
#' @return The value of \code{switch}.
#' @export
morie_llavx <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("llavx: op must be one of symbolic_representation, instruction_prompt, project_patches, build_sequence, training_stage, cheatsheet")
  op <- as.character(op)
  switch(op,
    "symbolic_representation" = symbolic_representation(...),
    "instruction_prompt" = instruction_prompt(...),
    "project_patches" = project_patches(...),
    "build_sequence" = build_sequence(...),
    "visualinstruction" = build_sequence(...),
    "llava_visual_chat" = build_sequence(...),
    "training_stage" = training_stage(...),
    "cheatsheet" = list(cheatsheet = .llavx_cheatsheet()),
    stop("llavx: unknown op ", shQuote(op))
  )
}
