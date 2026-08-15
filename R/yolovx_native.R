```r
# morie.fn -- function file (rootcoder007/morie)
# YOLOX: anchor-free, decoupled head, dynamic label assignment.
#
# Three advances had arrived in detection and had not reached the YOLO
# family: anchor-free prediction, decoupled heads, and advanced label
# assignment. YOLOX integrates all three.
#
# **The coupled head is a measured harm, not a style preference.** One
# branch predicting classification and localisation together is a
# known conflict; the paper's own experiments show replacing the YOLO
# head with a **lite decoupled head** -- a 1x1 convolution
# to reduce channels, then two parallel branches of 3x3
# convolutions -- improves AP, at a cost of 1.1 ms (11.6 vs 10.5 ms).
# That trade is stated so it can be judged.
#
# **Anchor-free removes tuned priors.** Instead of matching to
# pre-clustered boxes, each location predicts four offsets
# (l, t, r, b) from itself, decoded with the feature stride.
# decode_box inverts encode_box exactly, which is the property
# that makes the parametrisation usable at all.
#
# **Center sampling fixes a starvation problem.** Assigning only the
# single center location as positive discards high-quality predictions
# whose gradients would help and worsens the positive/negative
# imbalance; the center 3x3 area is assigned positive
# instead.
#
# **SimOTA: dynamic top-k instead of optimal transport.** Label
# assignment is posed globally as an optimal-transport problem in OTA,
# which costs 25% extra training time -- for 300 epochs, expensive.
# YOLOX approximates it: compute a cost per (ground truth, prediction)
# pair, give each ground truth a **dynamic** k from the sum of its
# top IoUs, and take its k cheapest predictions. No Sinkhorn-Knopp,
# no extra solver hyperparameters, and 45.0 to 47.3 AP.
#
# References
# ----------
# Ge, Z., Liu, S., Wang, F., Li, Z. & Sun, J. (2021) "YOLOX: Exceeding
# YOLO Series in 2021", arXiv:2107.08430. Sec. 2: the switch to an
# anchor-free manner together with a decoupled head and the leading
# label assignment strategy SimOTA; that the conflict between
# classification and regression tasks is well known and the coupled
# detection head may harm performance, with the lite decoupled head
# built from a 1x1 convolution reducing channels followed by two
# parallel branches of 3x3 convolutions and adding 1.1 ms (11.6 vs 10.5
# ms); that the anchor-free version selects only ONE positive (the
# center) per object and ignores other high-quality predictions, fixed
# by assigning the center 3x3 area as positives ("center sampling");
# and that OTA formulates assignment as an Optimal Transport problem but
# costs 25% extra training time, so it is simplified to a dynamic top-k
# strategy, SimOTA, which avoids the Sinkhorn-Knopp solver's
# hyperparameters and raises AP from 45.0% to 47.3%.
#
# Ge, Z., Liu, S., Li, Z., Yoshie, O. & Sun, J. (2021) "OTA: Optimal
# Transport Assignment for Object Detection", CVPR 2021, 303-312,
# arXiv:2103.14259. The assignment being approximated.
#
# Tian, Z., Shen, C., Chen, H. & He, T. (2019) "FCOS: Fully
# Convolutional One-Stage Object Detection", ICCV 2019, 9627-9636,
# arXiv:1904.01355. Center sampling and the anchor-free parametrisation.

.EPS <- 1e-12

.yolovx_decoupled_head <- function(channels, reduced = 256, n_classes = 80) {
  c_ <- as.integer(channels)
  r <- as.integer(reduced)
  if (c_ < 1L || r < 1L) {
    stop("yolovx: the channel counts must be positive")
  }
  n <- as.integer(n_classes)
  reduce_p <- c_ * r
  cls_p <- r * r * 9L + r * n
  reg_p <- r * r * 9L + r * (4L + 1L)
  coupled_p <- c_ * (n + 5L) * 9L
  list(
    reduce_params = reduce_p,
    cls_params = cls_p,
    reg_params = reg_p,
    total = reduce_p + cls_p + reg_p,
    coupled_total = coupled_p,
    branches = c("classification", "regression+objectness"),
    extra_latency_ms = 1.1,
    note = "measured cost 11.6 ms against 10.5 ms coupled, for a stated AP gain"
  )
}

.yolovx_encode_box <- function(box, cx, cy, stride = 1.0) {
  box <- as.numeric(box)
  x0 <- box[1]; y0 <- box[2]; x1 <- box[3]; y1 <- box[4]
  s <- as.numeric(stride)
  if (s <= 0.0) {
    stop("yolovx: the stride must be positive")
  }
  px <- (as.numeric(cx) + 0.5) * s
  py <- (as.numeric(cy) + 0.5) * s
  if (!(x0 <= px && px <= x1 && y0 <= py && py <= y1)) {
    stop("yolovx: the location is outside the box, so it cannot be a positive sample")
  }
  list(
    ltrb = c((px - x0) / s, (py - y0) / s, (x1 - px) / s, (y1 - py) / s),
    center = c(px, py),
    stride = s
  )
}

.yolovx_decode_box <- function(ltrb, cx, cy, stride = 1.0) {
  v <- as.numeric(ltrb)
  l <- v[1]; t <- v[2]; r <- v[3]; b <- v[4]
  s <- as.numeric(stride)
  px <- (as.numeric(cx) + 0.5) * s
  py <- (as.numeric(cy) + 0.5) * s
  if (l < 0 || t < 0 || r < 0 || b < 0) {
    stop("yolovx: the distances cannot be negative")
  }
  c(px - l * s, py - t * s, px + r * s, py + b * s)
}

.yolovx_box_iou <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  ax0 <- a[1]; ay0 <- a[2]; ax1 <- a[3]; ay1 <- a[4]
  bx0 <- b[1]; by0 <- b[2]; bx1 <- b[3]; by1 <- b[4]
  ix <- max(0.0, min(ax1, bx1) - max(ax0, bx0))
  iy <- max(0.0, min(ay1, by1) - max(ay0, by0))
  inter <- ix * iy
  ua <- (ax1 - ax0) * (ay1 - ay0) + (bx1 - bx0) * (by1 - by0) - inter
  if (ua > .EPS) inter / ua else 0.0
}

.yolovx_center_sampling <- function(box, grid_w, grid_h, stride = 1.0, radius = 1.5) {
  box <- as.numeric(box)
  x0 <- box[1]; y0 <- box[2]; x1 <- box[3]; y1 <- box[4]
  s <- as.numeric(stride)
  cx <- 0.5 * (x0 + x1)
  cy <- 0.5 * (y0 + y1)
  gw <- as.integer(grid_w)
  gh <- as.integer(grid_h)
  rad <- as.numeric(radius)
  if (gw < 0L || gh < 0L) {
    stop("yolovx: grid_w and grid_h must be non-negative")
  }
  empty_mat <- matrix(integer(0), ncol = 2L)
  if (gw == 0L || gh == 0L) {
    inside <- empty_mat
    center <- empty_mat
  } else {
    # Iterate in same order as Python: j outer, i inner, so the k-th
    # position has i = k %% gw, j = k %/% gw (0-based).
    k_idx <- seq_len(gh * gw) - 1L
    ii <- k_idx %% gw
    jj <- k_idx %/% gw
    px <- (as.numeric(ii) + 0.5) * s
    py <- (as.numeric(jj) + 0.5) * s
    in_box_mask <- (x0 <= px) & (px <= x1) & (y0 <= py) & (py <= y1)
    in_center_mask <- (abs(px - cx) <= rad * s) & (abs(py - cy) <= rad * s)
    inside <- cbind(ii[in_box_mask], jj[in_box_mask])
    center <- cbind(ii[in_center_mask], jj[in_center_mask])
  }
  cand <- unique(rbind(inside, center))
  if (nrow(cand) > 0L) {
    cand <- cand[order(cand[, 1L], cand[, 2L]), , drop = FALSE]
  }
  list(
    in_box = inside,
    in_center = center,
    candidates = cand,
    n_candidates = nrow(cand),
    single_center = 1L,
    note = "one positive per object starves the model of useful gradients"
  )
}

.yolovx_simota_assign <- function(costs, ious, top_q = 10, max_k = NULL) {
  C <- as.matrix(costs)
  storage.mode(C) <- "double"
  I <- as.matrix(ious)
  storage.mode(I) <- "double"
  G <- nrow(C)
  P <- ncol(C)
  if (nrow(I) != G || ncol(I) != P) {
    stop("yolovx: the cost and IoU matrices differ in shape")
  }
  q <- min(as.integer(top_q), P)
  assign_list <- vector("list", G)
  ks <- integer(G)
  for (g in seq_len(G)) {
    row_i <- I[g, ]
    sorted_row <- sort(row_i, decreasing = TRUE)
    top <- if (q == 0L) numeric(0) else sorted_row[seq_len(q)]
    kg <- max(1L, as.integer(round(sum(top))))
    if (!is.null(max_k)) {
      kg <- min(kg, as.integer(max_k))
    }
    kg <- min(kg, P)
    ks[g] <- kg
    row_c <- C[g, ]
    order_g <- order(row_c)
    if (kg == 0L) {
      assign_list[[g]] <- integer(0)
    } else {
      # order_g is 1-based; subtract 1 to make indices 0-based like Python.
      assign_list[[g]] <- as.integer(order_g[seq_len(kg)] - 1L)
    }
  }
  # owner[p] = 0-based ground truth index that "owns" prediction p,
  # or NA_integer_ if unowned.
  owner <- rep(NA_integer_, P)
  for (g in seq_len(G)) {
    g0 <- g - 1L
    for (p0 in assign_list[[g]]) {
      p <- p0 + 1L
      if (!is.na(owner[p])) {
        a0 <- owner[p]
        if (C[a0 + 1L, p] <= C[g, p]) {
          # keep current owner
        } else {
          owner[p] <- g0
        }
      } else {
        owner[p] <- g0
      }
    }
  }
  final <- vector("list", G)
  for (g in seq_len(G)) {
    g0 <- g - 1L
    p0s <- assign_list[[g]]
    if (length(p0s) == 0L) {
      final[[g]] <- integer(0)
    } else {
      final[[g]] <- sort(p0s[!is.na(owner[p0s + 1L]) & owner[p0s + 1L] == g0])
    }
  }
  contested <- 0L
  for (p1 in seq_len(P)) {
    cnt <- 0L
    p0 <- p1 - 1L
    for (g in seq_len(G)) {
      if (p0 %in% assign_list[[g]]) cnt <- cnt + 1L
    }
    if (cnt > 1L) contested <- contested + 1L
  }
  n_pos <- sum(lengths(final))
  list(
    estimate = final,
    assignment = final,
    dynamic_k = as.integer(ks),
    n_positives = n_pos,
    contested = contested,
    method = "SimOTA dynamic top-k; Ge et al. (2021)",
    note = "k is DYNAMIC per ground truth, from the sum of its top IoUs -- no Sinkhorn, no extra hyperparameter"
  )
}

.yolovx_cheatsheet <- function() {
  paste0(
    "yolovx: fold three advances into YOLO. DECOUPLED HEAD -- ",
    "classification and localisation conflict in one branch; ",
    "1x1 reduce then two parallel 3x3 branches
