# morie.fn -- function file (rootcoder007/morie)
# U-Net: segmentation from very few annotated images.
#
# The premise of the paper is a constraint, not an architecture:
# biomedical segmentation has thousands of pixels of supervision but
# almost no annotated *images*, so the network and the training strategy
# have to make heavy use of data augmentation and of every pixel that
# exists.
#
# **The shape.** A **contracting path** captures context by pooling; a
# symmetric **expanding path** restores resolution, with pooling replaced
# by upsampling. High-resolution features from the contracting path are
# concatenated into the upsampled ones -- the **skip connections** --
# because localisation needs detail that pooling destroyed, and context
# alone cannot supply it. The expansive path keeps a large number of
# feature channels so context propagates to the high-resolution layers,
# which is what makes it *symmetric* to the contracting path and gives
# the u-shape.
#
# **No fully connected layers, and only valid convolutions.** So the
# segmentation map contains exactly those pixels for which the full
# context is available in the input -- the output is smaller than the
# input by construction, and the arithmetic of that shrinkage is
# something an implementation gets right or silently mis-crops.
# ``valid_output_size`` computes it.
#
# **The overlap-tile strategy** segments arbitrarily large images: to
# predict a tile, the input includes a border of context around it, and
# missing data at the image edge is extrapolated by **mirroring**. This
# is what lets a fixed-size network handle any image without seams.
#
# **Weighted loss for touching objects.** Where instances of the same
# class touch, the separating background must be learned, so a
# precomputed **weight map** raises the loss on those narrow borders --
# otherwise the network merges adjacent cells and the pixel accuracy
# barely notices.
#
# References
# ----------
# Ronneberger, O., Fischer, P. & Brox, T. (2015) "U-Net: Convolutional
# Networks for Biomedical Image Segmentation", *Medical Image Computing
# and Computer-Assisted Intervention (MICCAI 2015)*, LNCS 9351,
# 234-241, doi:10.1007/978-3-319-24574-4_28, arXiv:1505.04597. The
# abstract (training deep networks is thought to need many thousand
# annotated samples; the strategy relies on strong data augmentation; a
# contracting path to capture context and a symmetric expanding path
# enabling precise localization; end-to-end training from very few
# images; winning the ISBI cell tracking challenge 2015; segmentation of
# a 512x512 image in under a second). Sec. 2 (pooling operators replaced
# by upsampling; high-resolution features from the contracting path
# combined with the upsampled output; a large number of feature channels
# in the expansive path propagating context to higher resolution layers,
# making it symmetric and yielding the u-shape; no fully connected
# layers and only the valid part of each convolution, so the map
# contains only pixels with full context). Figure 2 and Sec. 3 (the
# overlap-tile strategy with missing input extrapolated by mirroring,
# and the weight map for separating touching objects).
#
# Long, J., Shelhamer, E. & Darrell, T. (2015) "Fully convolutional
# networks for semantic segmentation", *CVPR 2015*, 3431-3440,
# doi:10.1109/CVPR.2015.7298965. The fully convolutional predecessor.

#' .unetbk_as_matrix
#'
#' A step of the unetbk_native implementation. Called by \code{mirror_pad},
#' \code{separation_weight_map}, \code{skip_concat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return A matrix, from \code{as.matrix}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .unetbk_as_matrix(x = x)
#' res
.unetbk_as_matrix <- function(x) {
  if (is.matrix(x)) return(x)
  if (is.list(x)) {
    return(do.call(rbind, x))
  }
  as.matrix(x)
}

#' valid_output_size
#'
#' A step of the unetbk_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param input_size Coerced to integer by the body, with \code{as.integer}.
#' @param depth A count; the body uses it as \code{seq_len(...)}. Defaults to \code{4L}.
#' @param convs_per_block Coerced to integer by the body, with \code{as.integer}.
#' Defaults to \code{2L}.
#' @param kernel Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3L}.
#' @return A list with \code{output}, \code{input}, \code{border_lost},
#' \code{skip_sizes}, \code{note}.
#' @export
valid_output_size <- function(input_size, depth = 4L, convs_per_block = 2L, kernel = 3L) {
  s <- as.integer(input_size)
  kk <- as.integer(kernel) - 1L
  if (s < 1L || depth < 0L) {
    stop("unetbk: the input size must be positive and the depth non-negative")
  }
  sizes <- integer(0)
  if (depth > 0L) {
    for (d in seq_len(depth)) {
      s <- s - kk * as.integer(convs_per_block)
      if (s < 1L) {
        stop("unetbk: the input is too small for this depth")
      }
      sizes <- c(sizes, s)
      if ((s %% 2L) != 0L) {
        stop(sprintf("unetbk: size %d is odd before pooling; U-Net requires even sizes at every pooling step", s))
      }
      s <- s %/% 2L
    }
  }
  s <- s - kk * as.integer(convs_per_block)
  if (depth > 0L) {
    for (d in seq.int(from = depth - 1L, to = 0L, by = -1L)) {
      s <- s * 2L
      s <- min(s, sizes[d + 1L])
      s <- s - kk * as.integer(convs_per_block)
      if (s < 1L) {
        stop("unetbk: the expansive path ran out of size")
      }
    }
  }
  list(
    output = s,
    input = as.integer(input_size),
    border_lost = as.integer(input_size) - s,
    skip_sizes = sizes,
    note = "valid convolutions only, so the output covers only pixels with full context"
  )
}

#' mirror_pad
#'
#' A step of the unetbk_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param image Passed to \code{.unetbk_as_matrix}.
#' @param pad Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{out}, as built in the body.
#' @export
mirror_pad <- function(image, pad) {
  img <- .unetbk_as_matrix(image)
  p <- as.integer(pad)
  if (p < 0L) {
    stop("unetbk: the pad must be non-negative")
  }
  h <- nrow(img)
  w <- ncol(img)
  if (p >= h || p >= w) {
    stop(sprintf("unetbk: the mirror pad (%d) must be smaller than the image (%dx%d)", p, h, w))
  }

  out <- matrix(0, nrow = h + 2L * p, ncol = w + 2L * p)
  for (i in (-p):(h + p - 1L)) {
    ii <- if (i < 0L) -i else if (i >= h) 2L * h - 2L - i else i
    for (j in (-p):(w + p - 1L)) {
      jj <- if (j < 0L) -j else if (j >= w) 2L * w - 2L - j else j
      out[i + p + 1L, j + p + 1L] <- img[ii + 1L, jj + 1L]
    }
  }
  out
}

#' overlap_tiles
#'
#' A step of the unetbk_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param height Coerced to integer by the body, with \code{as.integer}.
#' @param width Coerced to integer by the body, with \code{as.integer}.
#' @param tile Coerced to integer by the body, with \code{as.integer}.
#' @param border Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{tiles}, \code{n_tiles}, \code{output_size}, \code{note}.
#' @export
overlap_tiles <- function(height, width, tile, border) {
  t <- as.integer(tile)
  b <- as.integer(border)
  if (t < 1L || b < 0L) {
    stop("unetbk: the tile must be positive and the border non-negative")
  }
  out <- t - 2L * b
  if (out < 1L) {
    stop("unetbk: the border consumes the whole tile")
  }

  tiles <- list()
  h_total <- as.integer(height)
  w_total <- as.integer(width)
  for (i in seq(0L, h_total - 1L, by = out)) {
    for (j in seq(0L, w_total - 1L, by = out)) {
      tiles[[length(tiles) + 1L]] <- list(
        output_origin = c(i, j),
        input_origin = c(i - b, j - b),
        input_size = t,
        output_size = out
      )
    }
  }

  list(
    tiles = tiles,
    n_tiles = length(tiles),
    output_size = out,
    note = "inputs overlap by the border; outputs abut, so there are no seams"
  )
}

#' skip_concat
#'
#' A step of the unetbk_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param upsampled Passed to \code{.unetbk_as_matrix}.
#' @param contracting Passed to \code{.unetbk_as_matrix}.
#' @return A list with \code{concatenated}, \code{crop_offset}, \code{channels}, \code{note}.
#' @export
skip_concat <- function(upsampled, contracting) {
  up <- .unetbk_as_matrix(upsampled)
  co <- .unetbk_as_matrix(contracting)
  hu <- nrow(up)
  wu <- ncol(up)
  hc <- nrow(co)
  wc <- ncol(co)
  if (hc < hu || wc < wu) {
    stop(sprintf("unetbk: the contracting map (%dx%d) is smaller than the upsampled one (%dx%d)", hc, wc, hu, wu))
  }
  oi <- (hc - hu) %/% 2L
  oj <- (wc - wu) %/% 2L

  crop_rows <- (oi + 1L):(oi + hu)
  crop_cols <- (oj + 1L):(oj + wu)
  crop <- co[crop_rows, crop_cols, drop = FALSE]

  concatenated <- cbind(up, crop)

  list(
    concatenated = concatenated,
    crop_offset = c(oi, oj),
    channels = 2L,
    note = "localisation needs the detail pooling destroyed; context alone cannot supply it"
  )
}

#' separation_weight_map
#'
#' A step of the unetbk_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param labels Passed to \code{.unetbk_as_matrix}.
#' @param w0 Numeric; combined arithmetically in the body. Defaults to \code{10}.
#' @param sigma Numeric; combined arithmetically in the body. Defaults to \code{5}.
#' @return A list with \code{weights}, \code{n_instances}, \code{max_weight}, \code{note}.
#' @export
separation_weight_map <- function(labels, w0 = 10.0, sigma = 5.0) {
  lab <- .unetbk_as_matrix(labels)
  h <- nrow(lab)
  w <- ncol(lab)
  ids <- sort(unique(as.vector(lab[lab > 0])))

  out <- matrix(1.0, nrow = h, ncol = w)

  for (i in seq_len(h)) {
    for (j in seq_len(w)) {
      if (lab[i, j] > 0) {
        out[i, j] <- 1.0
        next
      }
      ds <- numeric(0)
      for (idv in ids) {
        positions <- which(lab == idv, arr.ind = TRUE)
        if (nrow(positions) > 0L) {
          dists <- sqrt((positions[, 1] - i)^2 + (positions[, 2] - j)^2)
          ds <- c(ds, min(dists))
        }
      }
      ds <- sort(ds)
      if (length(ds) >= 2L) {
        out[i, j] <- 1.0 + w0 * exp(-(ds[1] + ds[2])^2 / (2.0 * sigma^2))
      } else {
        out[i, j] <- 1.0
      }
    }
  }

  list(
    weights = out,
    n_instances = length(ids),
    max_weight = max(out),
    note = "the separating background must be LEARNED, so it is weighted up"
  )
}

#' .unetbk_cheatsheet
#'
#' A step of the unetbk_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .unetbk_cheatsheet()
#' res
.unetbk_cheatsheet <- function() {
  "unetbk: built for the case where annotated IMAGES are scarce though pixels are plentiful. Contracting path for context, symmetric expanding path for localisation, and SKIP CONNECTIONS carrying high-resolution detail that pooling destroyed -- context alone cannot localise. Only VALID convolutions and no fully connected layers, so the output is smaller than the input and covers only pixels with full context; hence the OVERLAP-TILE strategy with missing border data MIRRORED. A weight map raises the loss on the thin background between touching objects."
}

# compact alias per ledger/NAMING.md
unet <- valid_output_size
unet_backbone <- valid_output_size
unetbackbone <- valid_output_size
