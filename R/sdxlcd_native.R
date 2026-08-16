# morie.fn -- function file (rootcoder007/morie)
# SDXL: conditioning on the things the pipeline used to throw away.
#
# Two of SDXL's improvements are not architectural at all. They take
# metadata that a latent diffusion pipeline already has and would
# otherwise discard, and feed it to the model as conditioning -- costing
# no extra supervision and removing two concrete failure modes.
#
# References
# Podell, D., English, Z., Lacey, K., Blattmann, A., Dockhorn, T.,
# Muller, J., Penna, J. & Rombach, R. (2023) "SDXL: Improving Latent
# Diffusion Models for High-Resolution Image Synthesis",
# arXiv:2307.01952 (ICLR 2024). Sec. 2.1 (a three times larger UNet
# backbone, more attention blocks, a second text encoder), Sec. 2.2
# ("Conditioning the Model on Image Size": the two existing approaches of
# discarding images below a minimum resolution or upscaling them, the
# measured 39% of data that would be discarded at 256 pixels, and the
# proposal to condition on the original height and width, each embedded
# by Fourier features, concatenated and ADDED to the timestep embedding;
# "Conditioning the Model on Cropping Parameters": random cropping during
# training leaking into samples as cut-off objects, uniformly sampling
# c_top and c_left and feeding them as Fourier-embedded conditioning, and
# setting (0,0) at inference to obtain object-centred samples), Sec. 2.3
# (multi-aspect finetuning with buckets keeping the pixel count close to
# 1024^2), and Sec. 2.5 (the separate refinement model applying a
# noising-denoising process to SDXL's latents).
#
# Rombach, R., Blattmann, A., Lorenz, D., Esser, P. & Ommer, B. (2022)
# "High-Resolution Image Synthesis with Latent Diffusion Models",
# CVPR 2022, 10684-10695, arXiv:2112.10752. The latent diffusion
# model being improved.
#
# Ho, J., Jain, A. & Abbeel, P. (2020) "Denoising Diffusion
# Probabilistic Models", NeurIPS 2020, arXiv:2006.11239.

#' morie_sdxlcd_fourier_embedding
#'
#' A step of the sdxlcd_native implementation. Called by \code{morie_sdxlcd_crop_conditioning}, \code{morie_sdxlcd_size_conditioning}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param value Coerced to numeric by the body, with \code{as.numeric}.
#' @param dim Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param scale Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_sdxlcd_fourier_embedding <- function(value, dim = 8, scale = 0.001) {
  v <- as.numeric(value)
  n <- as.integer(dim)
  if (n < 2 || (n %% 2) != 0) {
    stop("sdxlcd: the embedding width must be even and at least 2")
  }
  j <- seq_len(n %/% 2) - 1L
  f <- (2.0 ^ j) * pi * as.numeric(scale)
  sin_f_v <- sin(f * v)
  cos_f_v <- cos(f * v)
  m <- cbind(sin_f_v, cos_f_v)
  out <- as.numeric(t(m))
  out
}

#' morie_sdxlcd_size_conditioning
#'
#' A step of the sdxlcd_native implementation. Called by \code{morie_sdxlcd_condition_vector}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h_original Coerced to numeric by the body, with \code{as.numeric}.
#' @param w_original Coerced to numeric by the body, with \code{as.numeric}.
#' @param dim Passed to \code{morie_sdxlcd_fourier_embedding}. Defaults to \code{8}.
#' @return A list with \code{c_size}, \code{embedding}, \code{note}.
#' @export
morie_sdxlcd_size_conditioning <- function(h_original, w_original, dim = 8) {
  h <- as.numeric(h_original)
  w <- as.numeric(w_original)
  if (h <= 0.0 || w <= 0.0) {
    stop("sdxlcd: the original size must be positive")
  }
  list(c_size = c(h, w),
       embedding = c(morie_sdxlcd_fourier_embedding(h, dim),
                     morie_sdxlcd_fourier_embedding(w, dim)),
       note = "the ORIGINAL size, so no training image has to be thrown away or upscaled")
}

#' morie_sdxlcd_crop_conditioning
#'
#' A step of the sdxlcd_native implementation. Called by \code{morie_sdxlcd_condition_vector}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param c_top Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param c_left Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param dim Passed to \code{morie_sdxlcd_fourier_embedding}. Defaults to \code{8}.
#' @return A list with \code{c_crop}, \code{embedding}, \code{object_centred}, \code{note}.
#' @export
morie_sdxlcd_crop_conditioning <- function(c_top = 0, c_left = 0, dim = 8) {
  top <- as.numeric(c_top)
  left <- as.numeric(c_left)
  if (top < 0.0 || left < 0.0) {
    stop("sdxlcd: crop offsets cannot be negative")
  }
  list(c_crop = c(top, left),
       embedding = c(morie_sdxlcd_fourier_embedding(top, dim),
                     morie_sdxlcd_fourier_embedding(left, dim)),
       object_centred = (top == 0.0 && left == 0.0),
       note = "(0,0) at inference asks for an UNCROPPED image")
}

#' morie_sdxlcd_sample_crop
#'
#' A step of the sdxlcd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param height Coerced to integer by the body, with \code{as.integer}.
#' @param width Coerced to integer by the body, with \code{as.integer}.
#' @param target_h Coerced to integer by the body, with \code{as.integer}.
#' @param target_w Coerced to integer by the body, with \code{as.integer}.
#' @param rng Passed to \code{.ghc_unif}.
#' @return A list with \code{c_top}, \code{c_left}.
#' @export
morie_sdxlcd_sample_crop <- function(height, width, target_h, target_w, rng) {
  H <- as.integer(height)
  W <- as.integer(width)
  th <- as.integer(target_h)
  tw <- as.integer(target_w)
  if (th > H || tw > W) {
    stop("sdxlcd: the target is larger than the image")
  }
  u1 <- .ghc_unif(rng, 1)
  u2 <- .ghc_unif(rng, 1)
  top <- as.integer(as.numeric(u1) * (H - th + 1))
  left <- as.integer(as.numeric(u2) * (W - tw + 1))
  list(c_top = min(top, H - th), c_left = min(left, W - tw))
}

#' morie_sdxlcd_discarded_fraction
#'
#' A step of the sdxlcd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sizes A matrix; indexed by row and column.
#' @param minimum Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{256}.
#' @return A list with \code{discarded}, \code{total}, \code{fraction}, \code{kept_with_conditioning}, \code{minimum}, \code{note}.
#' @export
morie_sdxlcd_discarded_fraction <- function(sizes, minimum = 256) {
  if (is.matrix(sizes)) {
    S <- lapply(seq_len(nrow(sizes)), function(i) c(as.numeric(sizes[i, 1]), as.numeric(sizes[i, 2])))
  } else if (is.list(sizes)) {
    S <- lapply(sizes, function(x) c(as.numeric(x[1]), as.numeric(x[2])))
  } else {
    stop("sdxlcd: sizes must be a list of pairs or a 2-column matrix")
  }
  if (length(S) == 0) {
    stop("sdxlcd: no image sizes given")
  }
  m <- as.numeric(minimum)
  lost <- 0
  for (pair in S) {
    h <- pair[1]
    w <- pair[2]
    if (h < m || w < m) {
      lost <- lost + 1
    }
  }
  n <- length(S)
  list(discarded = lost, total = n,
       fraction = lost / as.numeric(n),
       kept_with_conditioning = n,
       minimum = m,
       note = "conditioning keeps every image; filtering does not")
}

#' morie_sdxlcd_aspect_ratio_buckets
#'
#' A step of the sdxlcd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ratios See Usage.
#' @param pixels Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1024 * 1024}.
#' @param multiple Coerced to integer by the body, with \code{as.integer}. Defaults to \code{64}.
#' @return A list with \code{buckets}, \code{max_pixel_error}, \code{note}.
#' @export
morie_sdxlcd_aspect_ratio_buckets <- function(ratios, pixels = 1024 * 1024, multiple = 64) {
  out <- list()
  for (r in ratios) {
    a <- as.numeric(r)
    if (a <= 0.0) {
      stop("sdxlcd: an aspect ratio must be positive")
    }
    h <- sqrt(as.numeric(pixels) / a)
    w <- a * h
    M <- as.integer(multiple)
    hh <- max(M, as.integer(round(h / M)) * M)
    ww <- max(M, as.integer(round(w / M)) * M)
    out[[length(out) + 1]] <- list(aspect = a, height = hh, width = ww,
                                    pixels = hh * ww,
                                    pixel_error = abs(hh * ww - as.numeric(pixels)) / as.numeric(pixels))
  }
  errors <- sapply(out, function(x) x$pixel_error)
  max_error <- if (length(errors) > 0) max(errors) else 0.0
  list(buckets = out,
       max_pixel_error = max_error,
       note = "square output is an unnatural default for landscape and portrait screens")
}

#' morie_sdxlcd_condition_vector
#'
#' A step of the sdxlcd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h_original Passed to \code{morie_sdxlcd_size_conditioning}.
#' @param w_original Passed to \code{morie_sdxlcd_size_conditioning}.
#' @param c_top Passed to \code{morie_sdxlcd_crop_conditioning}. Defaults to \code{0}.
#' @param c_left Passed to \code{morie_sdxlcd_crop_conditioning}. Defaults to \code{0}.
#' @param timestep_embedding Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param dim Passed to \code{morie_sdxlcd_size_conditioning}. Defaults to \code{8}.
#' @return A list with \code{estimate}, \code{vector}, \code{width}, \code{c_size}, \code{c_crop}, \code{method}, \code{note}.
#' @export
morie_sdxlcd_condition_vector <- function(h_original, w_original, c_top = 0, c_left = 0,
                                          timestep_embedding = NULL, dim = 8) {
  s <- morie_sdxlcd_size_conditioning(h_original, w_original, dim)
  crop <- morie_sdxlcd_crop_conditioning(c_top, c_left, dim)
  cat_emb <- c(s$embedding, crop$embedding)
  if (is.null(timestep_embedding)) {
    vec <- cat_emb
  } else {
    t_vec <- as.numeric(timestep_embedding)
    if (length(t_vec) != length(cat_emb)) {
      stop(sprintf("sdxlcd: the timestep embedding is %d wide but the conditioning is %d",
                   length(t_vec), length(cat_emb)))
    }
    vec <- t_vec + cat_emb
  }
  list(estimate = vec, vector = vec, width = length(vec),
       c_size = s$c_size, c_crop = crop$c_crop,
       method = "SDXL micro-conditioning; Podell et al. (2023)",
       note = "concatenated, then ADDED to the timestep embedding in the UNet")
}

#' morie_sdxlcd_cheatsheet
#'
#' A step of the sdxlcd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_sdxlcd_cheatsheet <- function() {
  "sdxlcd: two improvements that add NO supervision -- they condition on metadata the pipeline already had and threw away. SIZE: filtering below a minimum resolution discarded 39% of the data and upscaling bakes in artefacts, so give the UNet the ORIGINAL (h,w) as Fourier-embedded conditioning added to the timestep embedding. CROP: batching forces a random crop that LEAKS into samples (cut-off heads), so condition on (c_top,c_left) and set (0,0) at inference to ask for an uncropped image. Plus multi-aspect buckets at ~1024^2 pixels."
}

# compact alias per ledger/NAMING.md
morie_sdxlcd_sdxlconditioning <- morie_sdxlcd_condition_vector

# public names resolved by fn/_lazy_map.json
morie_sdxlcd_sdxl_unet <- morie_sdxlcd_condition_vector
morie_sdxlcd_sdxlunet <- morie_sdxlcd_condition_vector

# main entry point
morie_sdxlcd <- morie_sdxlcd_condition_vector













