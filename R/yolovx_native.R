# morie.fn -- function file (rootcoder007/morie)
# YOLOX: anchor-free, decoupled head, dynamic label assignment.
#
# Three advances had arrived in detection and had not reached the YOLO
# family: anchor-free prediction, decoupled heads, and advanced label
# assignment. YOLOX integrates all three.
#
# The coupled head is a measured harm: one branch predicting
# classification and localisation together is a known conflict; a lite
# decoupled head -- a 1x1 convolution to reduce channels, then two
# parallel branches of 3x3 convolutions -- improves AP at a cost of
# 1.1 ms (11.6 vs 10.5 ms).
#
# Anchor-free removes tuned priors: each location predicts four offsets
# (l, t, r, b) from itself, decoded with the feature stride. decode_box
# inverts encode_box exactly.
#
# Center sampling: assigning only the single center location as
# positive starves the model, so the center 3x3 area is assigned
# positive instead.
#
# SimOTA: label assignment is posed globally as optimal transport in
# OTA, which costs 25% extra training time. YOLOX approximates it:
# compute a cost per (ground truth, prediction) pair, give each ground
# truth a dynamic k from the sum of its top IoUs, and take its k
# cheapest predictions. No Sinkhorn-Knopp, no extra hyperparameters,
# 45.0 to 47.3 AP.
#
# References
# ----------
# Ge, Z., Liu, S., Wang, F., Li, Z. & Sun, J. (2021) "YOLOX: Exceeding
# YOLO Series in 2021", arXiv:2107.08430. Sec. 2.
#
# Ge, Z., Liu, S., Li, Z., Yoshie, O. & Sun, J. (2021) "OTA: Optimal
# Transport Assignment for Object Detection", CVPR 2021, 303-312,
# arXiv:2103.14259.
#
# Tian, Z., Shen, C., Chen, H. & He, T. (2019) "FCOS: Fully
# Convolutional One-Stage Object Detection", ICCV 2019, 9627-9636,
# arXiv:1904.01355.

.yolovx_EPS <- 1e-12

#' morie_yolovx_decoupled_head
#'
#' A step of the yolovx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param channels Coerced to integer by the body, with \code{as.integer}.
#' @param reduced Coerced to integer by the body, with \code{as.integer}. Defaults to \code{256}.
#' @param n_classes Coerced to integer by the body, with \code{as.integer}. Defaults to \code{80}.
#' @return A list with \code{reduce_params}, \code{cls_params}, \code{reg_params}, \code{total}, \code{coupled_total}, \code{branches}, \code{extra_latency_ms}, \code{note}.
#' @export
morie_yolovx_decoupled_head <- function(channels, reduced=256,
                                        n_classes=80) {
  # 1x1 to reduce, then TWO parallel 3x3 branches.
  c <- as.integer(channels)
  r <- as.integer(reduced)
  if (c < 1L || r < 1L) {
    stop("yolovx: the channel counts must be positive")
  }
  n <- as.integer(n_classes)
  reduce_p <- c * r
  cls_p <- r * r * 9L + r * n
  reg_p <- r * r * 9L + r * (4L + 1L)
  coupled_p <- c * (n + 5L) * 9L
  list(reduce_params=reduce_p, cls_params=cls_p, reg_params=reg_p,
       total=reduce_p + cls_p + reg_p, coupled_total=coupled_p,
       branches=c("classification", "regression+objectness"),
       extra_latency_ms=1.1,
       note=paste0("measured cost 11.6 ms against 10.5 ms coupled, ",
                   "for a stated AP gain"))
}

#' Four distances from a location to the box sides
#'
#' A step of the yolovx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param box Coerced to numeric by the body, with \code{as.numeric}.
#' @param cx Coerced to numeric by the body, with \code{as.numeric}.
#' @param cy Coerced to numeric by the body, with \code{as.numeric}.
#' @param stride Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{ltrb}, \code{center}, \code{stride}.
#' @export
morie_yolovx_encode_box <- function(box, cx, cy, stride=1.0) {
  # Four distances from a location to the box sides.
  b <- as.numeric(box)
  x0 <- b[1L]; y0 <- b[2L]; x1 <- b[3L]; y1 <- b[4L]
  s <- as.numeric(stride)
  if (s <= 0.0) {
    stop("yolovx: the stride must be positive")
  }
  px <- (as.numeric(cx) + 0.5) * s
  py <- (as.numeric(cy) + 0.5) * s
  if (!(x0 <= px && px <= x1 && y0 <= py && py <= y1)) {
    stop(paste0("yolovx: the location is outside the box, so it cannot ",
                "be a positive sample"))
  }
  list(ltrb=c((px - x0) / s, (py - y0) / s, (x1 - px) / s, (y1 - py) / s),
       center=c(px, py), stride=s)
}

#' Back to corners. Inverts encode_box exactly
#'
#' A step of the yolovx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ltrb Passed to \code{.s03vec}.
#' @param cx Coerced to numeric by the body, with \code{as.numeric}.
#' @param cy Coerced to numeric by the body, with \code{as.numeric}.
#' @param stride Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A vector, from \code{c}.
#' @export
morie_yolovx_decode_box <- function(ltrb, cx, cy, stride=1.0) {
  # Back to corners. Inverts encode_box exactly.
  v <- .s03vec(ltrb)
  l <- v[1L]; t <- v[2L]; r <- v[3L]; b <- v[4L]
  s <- as.numeric(stride)
  px <- (as.numeric(cx) + 0.5) * s
  py <- (as.numeric(cy) + 0.5) * s
  if (l < 0 || t < 0 || r < 0 || b < 0) {
    stop("yolovx: the distances cannot be negative")
  }
  c(px - l * s, py - t * s, px + r * s, py + b * s)
}

#' Intersection over union of two corner boxes
#'
#' A step of the yolovx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; indexed elementwise.
#' @param b A vector; indexed elementwise.
#' @return One of two values, depending on the branch taken.
#' @export
morie_yolovx_box_iou <- function(a, b) {
  # Intersection over union of two corner boxes.
  a <- as.numeric(a)
  b <- as.numeric(b)
  ix <- max(0.0, min(a[3L], b[3L]) - max(a[1L], b[1L]))
  iy <- max(0.0, min(a[4L], b[4L]) - max(a[2L], b[2L]))
  inter <- ix * iy
  ua <- (a[3L] - a[1L]) * (a[4L] - a[2L]) +
    (b[3L] - b[1L]) * (b[4L] - b[2L]) - inter
  if (ua > .yolovx_EPS) inter / ua else 0.0
}

#' morie_yolovx_center_sampling
#'
#' A step of the yolovx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param box Coerced to numeric by the body, with \code{as.numeric}.
#' @param grid_w Coerced to integer by the body, with \code{as.integer}.
#' @param grid_h Coerced to integer by the body, with \code{as.integer}.
#' @param stride Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param radius Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.5}.
#' @return A list with \code{in_box}, \code{in_center}, \code{candidates}, \code{n_candidates}, \code{single_center}, \code{note}.
#' @export
morie_yolovx_center_sampling <- function(box, grid_w, grid_h, stride=1.0,
                                         radius=1.5) {
  # The center 3x3 area is positive, not only the center cell. Grid
  # coordinates are 0-based (i, j) to match the Python.
  bx <- as.numeric(box)
  x0 <- bx[1L]; y0 <- bx[2L]; x1 <- bx[3L]; y1 <- bx[4L]
  s <- as.numeric(stride)
  cx <- 0.5 * (x0 + x1)
  cy <- 0.5 * (y0 + y1)
  inside <- list()
  center <- list()
  seen_c <- character(0)
  cand_keys <- character(0)
  cand <- list()
  for (j in seq_len(as.integer(grid_h)) - 1L) {
    for (i in seq_len(as.integer(grid_w)) - 1L) {
      px <- (i + 0.5) * s
      py <- (j + 0.5) * s
      is_in <- (x0 <= px && px <= x1 && y0 <= py && py <= y1)
      is_ctr <- (abs(px - cx) <= as.numeric(radius) * s &&
                 abs(py - cy) <= as.numeric(radius) * s)
      if (is_in) {
        inside <- c(inside, list(c(i, j)))
      }
      if (is_ctr) {
        center <- c(center, list(c(i, j)))
      }
      if (is_in || is_ctr) {
        key <- paste(i, j, sep=",")
        if (!(key %in% cand_keys)) {
          cand_keys <- c(cand_keys, key)
          cand <- c(cand, list(c(i, j)))
        }
      }
    }
  }
  # sorted set order: by (i, j)
  if (length(cand) > 0L) {
    ci <- vapply(cand, function(p) p[1L], numeric(1))
    cj <- vapply(cand, function(p) p[2L], numeric(1))
    cand <- cand[order(ci, cj)]
  }
  list(in_box=inside, in_center=center, candidates=cand,
       n_candidates=length(cand), single_center=1L,
       note=paste0("one positive per object starves the model of ",
                   "useful gradients"))
}

#' Dynamic top-k, an approximation to optimal transport. k_g is the
#'
#' rounded sum of the q largest IoUs for that ground truth. Returns the
#' assignment keyed by 0-based ground-truth index.
#'
#' @param costs Passed to \code{.s03mat}.
#' @param ious Passed to \code{.s03mat}.
#' @param top_q Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.
#' @param max_k Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{estimate}, \code{assignment}, \code{dynamic_k}, \code{n_positives}, \code{contested}, \code{method}, \code{note}.
#' @export
morie_yolovx_simota_assign <- function(costs, ious, top_q=10, max_k=NULL) {
  # Dynamic top-k, an approximation to optimal transport. k_g is the
  # rounded sum of the q largest IoUs for that ground truth. Returns
  # the assignment keyed by 0-based ground-truth index.
  C <- .s03mat(costs)
  I <- .s03mat(ious)
  G <- nrow(C)
  P <- ncol(C)
  if (nrow(I) != G || ncol(I) != P) {
    stop("yolovx: the cost and IoU matrices differ in shape")
  }
  q <- min(as.integer(top_q), P)
  assign_ <- vector("list", G)
  ks <- integer(G)
  for (g in seq_len(G)) {
    top <- sort(I[g, ], decreasing=TRUE)[seq_len(q)]
    kg <- max(1L, as.integer(round(sum(top))))
    if (!is.null(max_k)) {
      kg <- min(kg, as.integer(max_k))
    }
    ks[g] <- kg
    order_ <- order(C[g, ])
    assign_[[g]] <- sort(order_[seq_len(kg)])
  }
  # resolve contested predictions to the cheaper ground truth
  owner <- list()  # keyed by prediction index (character)
  for (g in seq_len(G)) {
    for (p in assign_[[g]]) {
      pk <- as.character(p)
      if (!is.null(owner[[pk]])) {
        a <- owner[[pk]]
        owner[[pk]] <- if (C[a, p] <= C[g, p]) a else g
      } else {
        owner[[pk]] <- g
      }
    }
  }
  final <- list()
  n_pos <- 0L
  for (g in seq_len(G)) {
    kept <- sort(Filter(function(p) owner[[as.character(p)]] == g,
                        assign_[[g]]))
    final[[as.character(g - 1L)]] <- kept
    n_pos <- n_pos + length(kept)
  }
  # contested = predictions claimed by more than one ground truth
  claim_counts <- table(unlist(assign_))
  contested <- sum(claim_counts > 1)
  list(estimate=final, assignment=final, dynamic_k=ks, n_positives=n_pos,
       contested=as.integer(contested),
       method="SimOTA dynamic top-k; Ge et al. (2021)",
       note=paste0("k is DYNAMIC per ground truth, from the sum of its ",
                   "top IoUs -- no Sinkhorn, no extra hyperparameter"))
}

#' morie_yolovx_cheatsheet
#'
#' A step of the yolovx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_yolovx_cheatsheet <- function() {
  paste0(
    "yolovx: fold three advances into YOLO. DECOUPLED HEAD -- ",
    "classification and localisation conflict in one branch; ",
    "1x1 reduce then two parallel 3x3 branches, +1.1 ms ",
    "(11.6 vs 10.5) for an AP gain. ANCHOR-FREE -- each ",
    "location predicts (l,t,r,b) with the stride, so ",
    "decode inverts encode exactly and no clustered priors are ",
    "tuned. CENTER SAMPLING -- one positive per object starves ",
    "the model, so the center 3x3 is positive. SIMOTA -- OTA's ",
    "optimal transport costs 25% extra training time, so ",
    "approximate it with DYNAMIC top-k from the sum of top ",
    "IoUs: 45.0 to 47.3 AP with no solver hyperparameters."
  )
}

# compact alias per ledger/NAMING.md
morie_yolovx_yoloxhead <- morie_yolovx_simota_assign
# public names resolved by fn/_lazy_map.json
morie_yolovx_yolo_decoupled_head <- morie_yolovx_simota_assign

#' @export
morie_yolovx <- morie_yolovx_simota_assign
