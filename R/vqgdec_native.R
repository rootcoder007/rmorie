# morie.fn -- function file (rootcoder007/morie)
# VQ-GAN's decoder: perceptual quality at high compression.
#
# The encoder side (vqgenc) buys a short sequence. The decoder side
# has to pay for it: at a compression factor of 16 an L2 reconstruction
# loss produces blur, because L2 is minimised by the conditional mean
# of every image consistent with the code. VQ-GAN replaces it with a
# perceptual loss and adds an adversarial term from a patch-based
# discriminator, so the codebook has to capture perceptually important
# local structure rather than average it away.
#
# The adversarial weight is computed, not tuned. The paper sets
#   lambda = grad_GL[L_rec] / (grad_GL[L_GAN] + delta),
# with gradients taken with respect to the last layer of the decoder.
# So the two losses arrive at the final layer with matched magnitudes
# automatically, whatever their scales -- and when the GAN gradient
# explodes, lambda shrinks rather than the training collapsing.
#
# Decoding is exact where it needs to be. Indices map to codes by
# lookup, so decode_indices inverts quantize exactly on any vector
# already in the codebook -- an identity the anchor checks rather
# than approximating.
#
# Sliding-window generation produces images larger than the
# transformer's context: the model is applied patch-wise across the
# latent grid. That is valid as long as the dataset statistics are
# roughly spatially invariant or spatial conditioning is available --
# and when it is not, the paper's own remedy is to condition on image
# coordinates.
#
# References
# ----------
# Esser, P., Rombach, R. & Ommer, B. (2021) "Taming Transformers for
# High-Resolution Image Synthesis", CVPR 2021, 12873-12883,
# arXiv:2012.09841. Sec. 3.1 ("Learning a Perceptually Rich Codebook":
# replacing the L2 reconstruction loss with a perceptual loss and
# introducing adversarial training with a PATCH-BASED discriminator to
# keep perceptual quality at increased compression), Sec. 3.2 (the
# adaptive weight lambda = grad_GL[L_rec] / (grad_GL[L_GAN] + delta)
# computed with respect to the last layer L of the decoder), and Sec. 3.3
# (sliding-window generation, valid when the dataset statistics are
# approximately spatially invariant or spatial conditioning is
# available, with conditioning on image coordinates as the remedy
# otherwise).
#
# Isola, P., Zhu, J.-Y., Zhou, T. & Efros, A. A. (2017) "Image-to-Image
# Translation with Conditional Adversarial Networks", CVPR 2017,
# 1125-1134, arXiv:1611.07004. The patch-based discriminator.
#
# Zhang, R., Isola, P., Efros, A. A., Shechtman, E. & Wang, O. (2018)
# "The Unreasonable Effectiveness of Deep Features as a Perceptual
# Metric", CVPR 2018, 586-595, arXiv:1801.03924. The perceptual loss.

# Private helper: coerce a codebook or image (list-of-vectors or matrix)
# to a 2-D numeric matrix with one row per entry.
.vqgdec_to_matrix <- function(x) {
    if (is.matrix(x)) {
        return(`storage.mode<-`(x, "double"))
    }
    if (is.list(x)) {
        n <- length(x)
        if (n == 0L) {
            return(matrix(numeric(0), nrow = 0L, ncol = 0L))
        }
        d <- length(x[[1L]])
        M <- matrix(0, nrow = n, ncol = d)
        for (i in seq_len(n)) {
            M[i, ] <- as.numeric(x[[i]])
        }
        return(M)
    }
    if (is.numeric(x) && !is.null(dim(x))) {
        return(`storage.mode<-`(x, "double"))
    }
    as.matrix(x)
}

# decode_indices: lookup. Exactly inverts quantisation for in-codebook
# vectors. Indices are 0-based, matching the Python implementation.
morie_vqgdec_decode_indices <- function(indices, codebook) {
    Z <- .vqgdec_to_matrix(codebook)
    n_code <- nrow(Z)
    idx <- as.integer(indices)

    if (length(idx) > 0L && any(idx < 0L | idx >= n_code)) {
        bad <- idx[idx < 0L | idx >= n_code][1L]
        stop(sprintf("vqgdec: index %d is outside a codebook of %d",
                     bad, n_code))
    }

    out <- vector("list", length(idx))
    for (k in seq_along(idx)) {
        out[[k]] <- as.numeric(Z[idx[k] + 1L, ])
    }

    list(
        codes = out,
        n = length(out),
        note = "exact: the index IS the code"
    )
}

# adaptive_weight: lambda = grad_GL[L_rec] / (grad_GL[L_GAN] + delta)
# Balances the two losses at the decoder's last layer.
morie_vqgdec_adaptive_weight <- function(grad_rec, grad_gan,
                                          delta = 1e-6, clip = 1e4) {
    gr <- abs(as.numeric(grad_rec))
    gg <- abs(as.numeric(grad_gan))
    d <- as.numeric(delta)

    if (d <= 0) {
        stop("vqgdec: delta must be positive")
    }

    lam <- gr / (gg + d)
    list(
        lambda = min(lam, as.numeric(clip)),
        raw = lam,
        clipped = lam > as.numeric(clip),
        note = "gradients taken w.r.t. the LAST layer of the decoder"
    )
}

# patch_discriminator: score patches, not the whole image. Gives a
# dense signal about local texture.
morie_vqgdec_patch_discriminator <- function(image, patch = 4,
                                              scorer = NULL) {
    I <- .vqgdec_to_matrix(image)
    p <- as.integer(patch)
    H <- nrow(I)
    W <- ncol(I)

    if (p < 1L || (H %% p) != 0L || (W %% p) != 0L) {
        stop(sprintf("vqgdec: a %dx%d image does not tile into %dx%d patches",
                     H, W, p, p))
    }

    n_row <- H %/% p
    n_col <- W %/% p
    scores <- vector("list", n_row)
    flat <- numeric(0)

    for (i_idx in seq_len(n_row)) {
        r0 <- (i_idx - 1L) * p
        row_scores <- numeric(n_col)
        for (j_idx in seq_len(n_col)) {
            c0 <- (j_idx - 1L) * p
            blk <- as.numeric(I[(r0 + 1L):(r0 + p), (c0 + 1L):(c0 + p)])
            if (!is.null(scorer)) {
                row_scores[j_idx] <- as.numeric(scorer(blk))
            } else {
                row_scores[j_idx] <- sum(blk) / length(blk)
            }
        }
        scores[[i_idx]] <- row_scores
        flat <- c(flat, row_scores)
    }

    list(
        scores = scores,
        n_patches = length(flat),
        mean = if (length(flat) > 0L) sum(flat) / length(flat) else 0,
        note = "one verdict per patch, not one per image"
    )
}

# sliding_windows: generate windows for images larger than the
# transformer's context.
morie_vqgdec_sliding_windows <- function(height, width, window,
                                          stride = NULL) {
    H <- as.integer(height)
    W <- as.integer(width)
    w <- as.integer(window)
    s <- if (is.null(stride)) w else as.integer(stride)

    if (w < 1L || s < 1L || w > H || w > W) {
        stop("vqgdec: the window must fit inside the latent grid")
    }

    pos_list <- list()
    i <- 0L
    while ((i + w) <= H) {
        j <- 0L
        while ((j + w) <= W) {
            pos_list[[length(pos_list) + 1L]] <- c(i, j)
            j <- j + s
        }
        if ((j - s + w) < W) {
            pos_list[[length(pos_list) + 1L]] <- c(i, as.integer(W - w))
        }
        i <- i + s
    }
    if ((i - s + w) < H) {
        j <- 0L
        while ((j + w) <= W) {
            pos_list[[length(pos_list) + 1L]] <- c(as.integer(H - w), j)
            j <- j + s
        }
        pos_list[[length(pos_list) + 1L]] <- c(as.integer(H - w),
                                                as.integer(W - w))
    }

    if (length(pos_list) == 0L) {
        wins <- matrix(integer(0), ncol = 2L)
    } else {
        wins <- do.call(rbind, pos_list)
        wins <- unique(wins)
        wins <- wins[order(wins[, 1L], wins[, 2L]), , drop = FALSE]
    }

    # Compute coverage with a hashed environment
    covered <- new.env(hash = TRUE, parent = emptyenv())
    if (nrow(wins) > 0L) {
        for (k in seq_len(nrow(wins))) {
            a <- wins[k, 1L]
            b <- wins[k, 2L]
            for (x in seq.int(a, a + w - 1L)) {
                for (y in seq.int(b, b + w - 1L)) {
                    key <- paste(x, y, sep = ",")
                    covered[[key]] <- TRUE
                }
            }
        }
    }

    n_cov <- length(ls(covered))
    windows_list <- lapply(seq_len(nrow(wins)),
                            function(i) as.integer(wins[i, ]))

    list(
        windows = windows_list,
        n_windows = nrow(wins),
        covers_everything = n_cov == (H * W),
        context = w * w,
        note = "spatially invariant statistics, or condition on coordinates"
    )
}

# decode: indices to codes to image, with the adaptive GAN weight.
morie_vqgdec_decode <- function(indices, codebook, generator = NULL,
                                 grad_rec = NULL, grad_gan = NULL) {
    d <- morie_vqgdec_decode_indices(indices, codebook)
    if (!is.null(generator)) {
        img <- generator(d$codes)
    } else {
        img <- d$codes
    }
    lam <- NULL
    if (!is.null(grad_rec) && !is.null(grad_gan)) {
        lam <- morie_vqgdec_adaptive_weight(grad_rec, grad_gan)$lambda
    }
    list(
        estimate = img,
        image = img,
        codes = d$codes,
        n_tokens = d$n,
        adaptive_lambda = lam,
        method = "VQ-GAN decoder with perceptual and adversarial losses; Esser, Rombach & Ommer (2021)",
        note = "L2 at this compression gives blur, because it returns the conditional MEAN of every consistent image"
    )
}

# cheatsheet
morie_vqgdec_cheatsheet <- function() {
    paste(
        "vqgdec: at compression 16 an L2 loss returns the conditional",
        "MEAN of every image consistent with the code, which is blur.",
        "So use a PERCEPTUAL loss plus an adversarial term from a",
        "PATCH-BASED discriminator -- one verdict per patch, not per",
        "image. The adversarial weight is COMPUTED, not tuned:",
        "lambda = grad[L_rec] / (grad[L_GAN] + delta) at the decoder's",
        "LAST layer, so a runaway GAN gradient shrinks lambda instead",
        "of wrecking training. Sliding-window generation exceeds the",
        "context, valid under spatially invariant statistics or",
        "coordinate conditioning."
    )
}

# Compact aliases (per ledger/NAMING.md)
morie_vqgdec_vqgandecoder <- morie_vqgdec_decode
morie_vqgdec_vqgan_decode <- morie_vqgdec_decode
morie_vqgdec_vqgandecode <- morie_vqgdec_decode

# Main entry point
morie_vqgdec <- morie_vqgdec_decode
