# Mask R-CNN: instance segmentation by adding a mask branch.
# Sources: He, K., Gkioxari, G., Dollar, P. & Girshick, R. (2017) "Mask
# R-CNN", *Proceedings of the IEEE International Conference on Computer
# Vision (ICCV 2017)*, 2980-2988, doi:10.1109/ICCV.2017.322,
# arXiv:1703.06870. Sec. 1 and 3 (extending Faster R-CNN with a mask
# branch in parallel with the existing classification and box branches;
# that Faster R-CNN was not designed for pixel-to-pixel alignment
# between inputs and outputs, most evident in RoIPool's coarse spatial
# quantization; the quantization-free RoIAlign layer that faithfully
# preserves exact spatial locations, described as a seemingly minor
# change with a large impact; the multi-task loss
# L = L_cls + L_box + L_mask; and the per-class binary masks with a
# per-pixel sigmoid, decoupling mask and class prediction, against the
# per-pixel softmax which makes classes compete).
#
# Ren, S., He, K., Girshick, R. & Sun, J. (2015) "Faster R-CNN: Towards
# Real-Time Object Detection with Region Proposal Networks", *NeurIPS
# 2015*, 91-99, arXiv:1506.01497. The detector being extended.

.MASRCN_EPS <- 1e-12

.masrcn_bilinear <- function(F, y, x) {
  h <- length(F); w <- length(F[[1L]])
  y <- min(max(as.numeric(y), 0), h - 1)
  x <- min(max(as.numeric(x), 0), w - 1)
  y0 <- as.integer(floor(y)); x0 <- as.integer(floor(x))
  y1 <- min(y0 + 1L, h - 1L); x1 <- min(x0 + 1L, w - 1L)
  dy <- y - y0; dx <- x - x0
  F[[y0 + 1L]][[x0 + 1L]] * (1 - dy) * (1 - dx) +
    F[[y1 + 1L]][[x0 + 1L]] * dy * (1 - dx) +
    F[[y0 + 1L]][[x1 + 1L]] * (1 - dy) * dx +
    F[[y1 + 1L]][[x1 + 1L]] * dy * dx
}

.masrcn_mat <- function(X) {
  if (is.matrix(X)) {
    out <- vector("list", nrow(X))
    for (i in seq_len(nrow(X))) out[[i]] <- as.numeric(X[i, ])
    out
  } else {
    lapply(X, function(r) as.numeric(unlist(r)))
  }
}

roi_pool <- function(features, box, out_size = 2L, stride = 1.0) {
  F <- .masrcn_mat(features)
  y0 <- as.numeric(box[1L]) / as.numeric(stride)
  x0 <- as.numeric(box[2L]) / as.numeric(stride)
  y1 <- as.numeric(box[3L]) / as.numeric(stride)
  x1 <- as.numeric(box[4L]) / as.numeric(stride)
  qy0 <- as.integer(floor(y0)); qx0 <- as.integer(floor(x0))
  qy1 <- as.integer(floor(y1)); qx1 <- as.integer(floor(x1))
  if (qy1 <= qy0 || qx1 <= qx0)
    stop("masrcn: the box collapsed under quantisation, which is itself the problem")
  n <- as.integer(out_size)
  bh <- (qy1 - qy0) / n; bw <- (qx1 - qx0) / n
  out <- list()
  for (i in seq_len(n) - 1L) {
    row <- list()
    for (j in seq_len(n) - 1L) {
      a0 <- qy0 + as.integer(floor(i * bh))
      a1 <- max(a0 + 1L, qy0 + as.integer(floor((i + 1L) * bh)))
      b0 <- qx0 + as.integer(floor(j * bw))
      b1 <- max(b0 + 1L, qx0 + as.integer(floor((j + 1L) * bw)))
      vals <- c()
      for (a in a0:min(a1 - 1L, length(F) - 1L)) {
        for (b in b0:min(b1 - 1L, length(F[[1L]]) - 1L)) {
          vals <- c(vals, F[[a + 1L]][[b + 1L]])
        }
      }
      row[[length(row) + 1L]] <- if (length(vals) > 0L) max(vals) else 0.0
    }
    out[[length(out) + 1L]] <- row
  }
  pooled <- lapply(out, function(r) vapply(r, unname, numeric(1)))
  list(pooled = pooled,
       quantised_box = c(qy0, qx0, qy1, qx1),
       quantisation_shift = c(y0 - qy0, x0 - qx0),
       caveat = "the box AND the bins are rounded to the feature grid")
}

roi_align <- function(features, box, out_size = 2L, stride = 1.0,
                      samples = 2L) {
  F <- .masrcn_mat(features)
  y0 <- as.numeric(box[1L]) / as.numeric(stride)
  x0 <- as.numeric(box[2L]) / as.numeric(stride)
  y1 <- as.numeric(box[3L]) / as.numeric(stride)
  x1 <- as.numeric(box[4L]) / as.numeric(stride)
  if (y1 <= y0 || x1 <= x0)
    stop("masrcn: the box has non-positive extent")
  n <- as.integer(out_size); s <- as.integer(samples)
  bh <- (y1 - y0) / n; bw <- (x1 - x0) / n
  out <- list()
  for (i in seq_len(n) - 1L) {
    row <- list()
    for (j in seq_len(n) - 1L) {
      acc <- numeric(0)
      for (a in seq_len(s) - 1L) {
        for (b in seq_len(s) - 1L) {
          yy <- y0 + bh * (i + (a + 0.5) / s)
          xx <- x0 + bw * (j + (b + 0.5) / s)
          acc <- c(acc, .masrcn_bilinear(F, yy, xx))
        }
      }
      row[[length(row) + 1L]] <- sum(acc) / length(acc)
    }
    out[[length(out) + 1L]] <- row
  }
  pooled <- lapply(out, function(r) vapply(r, unname, numeric(1)))
  list(pooled = pooled, exact_box = c(y0, x0, y1, x1),
       samples_per_bin = s * s,
       note = "no quantisation of the box or the bins")
}

alignment_error <- function(features, box, out_size = 2L, stride = 1.0) {
  p <- roi_pool(features, box, out_size, stride)
  shift <- p$quantisation_shift
  list(feature_shift = shift,
       input_pixel_shift = c(shift[1L] * as.numeric(stride),
                             shift[2L] * as.numeric(stride)),
       stride = as.numeric(stride),
       note = "a sub-pixel error on the feature map is a several-pixel error in the image at stride 16 or 32")
}

mask_loss <- function(logits, target, decoupled = TRUE) {
  L <- .masrcn_mat(logits)
  T <- .masrcn_mat(target)
  if (length(L) != length(T) || length(L[[1L]]) != length(T[[1L]]))
    stop("masrcn: the logits and target differ in shape")
  tot <- 0.0; m <- 0L
  if (decoupled) {
    for (i in seq_along(L)) {
      for (j in seq_along(L[[1L]])) {
        p <- if (L[[i]][j] > -700) 1.0 / (1.0 + exp(-L[[i]][j])) else 0.0
        p <- min(max(p, .MASRCN_EPS), 1.0 - .MASRCN_EPS)
        tot <- tot - (T[[i]][j] * log(p) + (1 - T[[i]][j]) * log(1 - p))
        m <- m + 1L
      }
    }
    return(list(loss = tot / m, kind = "per-pixel sigmoid",
                note = "classes do not compete; the class branch decides the category"))
  }
  flat <- c()
  for (i in seq_along(L)) for (j in seq_along(L[[1L]])) flat <- c(flat, L[[i]][j])
  mx <- max(flat)
  z <- sum(exp(flat - mx))
  for (i in seq_along(L)) {
    for (j in seq_along(L[[1L]])) {
      p <- exp(L[[i]][j] - mx) / z
      tot <- tot - T[[i]][j] * log(max(p, .MASRCN_EPS))
      m <- m + 1L
    }
  }
  list(loss = tot / m, kind = "per-pixel softmax",
       caveat = "classes COMPETE, so a pixel assigned to one is evidence against another")
}

multitask_loss <- function(l_cls, l_box, l_mask) {
  list(total = as.numeric(l_cls) + as.numeric(l_box) + as.numeric(l_mask),
       cls = as.numeric(l_cls), box = as.numeric(l_box),
       mask = as.numeric(l_mask),
       note = "an unweighted sum, which the decoupling permits")
}

maskrcnn <- roi_align
mask_rcnn_segmentation <- roi_align

.masrcn_cheatsheet <- function() {
  paste("masrcn: Faster R-CNN plus a THIRD branch predicting a ",
        "binary mask per RoI. Two details carry it. RoIPool ",
        "QUANTISES twice -- box and bins -- which is fine for a ",
        "box and a several-pixel misalignment for a mask at stride ",
        "16 or 32; RoIAlign removes both roundings and samples ",
        "bilinearly. And the mask is DECOUPLED from the class: K ",
        "binary masks with a per-pixel SIGMOID, loss on the ",
        "ground-truth class only, because a per-pixel softmax ",
        "makes classes compete. The decoupling is what lets the ",
        "losses simply add.", sep = "")
}

morie_masrcn <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("masrcn: op must be one of roi_pool, roi_align, alignment_error, mask_loss, multitask_loss, cheatsheet")
  op <- as.character(op)
  switch(op,
    "roi_pool" = roi_pool(...),
    "roi_align" = roi_align(...),
    "maskrcnn" = roi_align(...),
    "mask_rcnn_segmentation" = roi_align(...),
    "alignment_error" = alignment_error(...),
    "mask_loss" = mask_loss(...),
    "multitask_loss" = multitask_loss(...),
    "cheatsheet" = list(cheatsheet = .masrcn_cheatsheet()),
    stop("masrcn: unknown op ", shQuote(op))
  )
}
