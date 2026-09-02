# SPDX-License-Identifier: AGPL-3.0-or-later

# Exact minimum-cost assignment (Jonker-Volgenant shortest paths) on a
# rectangular cost with rows <= columns.  Returns the column per row.
#' Exact minimum-cost assignment (Jonker-Volgenant shortest paths) on a
#'
#' rectangular cost with rows <= columns.  Returns the column per row.
#'
#' @param cost A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
.detr_hungarian <- function(cost) {
  n <- nrow(cost); m <- ncol(cost)
  if (n > m) stop("hungarian: need at least as many columns as rows")
  u <- numeric(n + 1L); v <- numeric(m + 1L)
  p <- integer(m + 1L); way <- integer(m + 1L)
  for (i in seq_len(n)) {
    p[1] <- i
    j0 <- 1L
    minv <- rep(Inf, m + 1L)
    used <- rep(FALSE, m + 1L)
    repeat {
      used[j0] <- TRUE
      i0 <- p[j0]
      delta <- Inf; j1 <- 1L
      for (j in 2:(m + 1L)) {
        if (used[j]) next
        cur <- cost[i0, j - 1L] - u[i0] - v[j]
        if (cur < minv[j]) { minv[j] <- cur; way[j] <- j0 }
        if (minv[j] < delta) { delta <- minv[j]; j1 <- j }
      }
      for (j in seq_len(m + 1L)) {
        if (used[j]) {
          if (p[j] > 0L) u[p[j]] <- u[p[j]] + delta
          v[j] <- v[j] - delta
        } else minv[j] <- minv[j] - delta
      }
      j0 <- j1
      if (p[j0] == 0L) break
    }
    repeat {
      j1 <- way[j0]
      p[j0] <- p[j1]
      j0 <- j1
      if (j0 == 1L) break
    }
  }
  out <- integer(n)
  for (j in 2:(m + 1L)) if (p[j] > 0L) out[p[j]] <- j - 1L
  out
}

#' DETR set prediction
#'
#' Formula: transformer decoder; bipartite matching loss
#'
#' Every query predicts one box, and the loss is defined only after a
#' one-to-one assignment between predictions and ground truth found by
#' the Hungarian algorithm on the pairwise cost.  There is no
#' non-maximum suppression and no anchor set: the matching is what
#' removes the duplicates.  Unmatched queries train to the no-object
#' class.
#'
#' @param image A Q x 4 matrix of predicted boxes (cx, cy, w, h).
#' @param queries A G x 4 matrix of ground-truth boxes.
#' @param n_objects Number of ground-truth objects, or NULL.
#' @param targets Unused; kept for signature compatibility.
#' @return List with \code{estimate}, \code{assignment}, \code{cost},
#'   \code{matched}, \code{unmatched}, \code{l1_cost},
#'   \code{giou_cost}, \code{Q}, \code{G}, \code{method}.
#' @references Carion et al. (2020), End-to-End Object Detection with
#'   Transformers, ECCV 2020:213-229.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Detrbb(D, D)
Detrbb <- function(image, queries, n_objects = NULL, targets = NULL) {
  P <- .s03mat(image); TT <- .s03mat(queries)
  Q <- nrow(P); G <- nrow(TT)
  if (Q == 0L || G == 0L)
    stop("empty input: both boxes and targets are required")
  if (ncol(P) != 4L || ncol(TT) != 4L)
    stop("boxes must have four columns (cx, cy, w, h)")
  if (!is.null(n_objects) && as.integer(n_objects) != G)
    stop("n_objects disagrees with the number of target rows")
  if (G > Q) stop("more ground-truth boxes than queries")
  cor <- function(b) c(b[1] - b[3] / 2, b[2] - b[4] / 2,
                       b[1] + b[3] / 2, b[2] + b[4] / 2)
  l1 <- matrix(0, G, Q); giou <- matrix(0, G, Q)
  for (g in seq_len(G)) for (q in seq_len(Q)) {
    s <- 0
    for (k in 1:4) s <- s + abs(TT[g, k] - P[q, k])
    l1[g, q] <- s
    a <- cor(TT[g, ]); b <- cor(P[q, ])
    iw <- max(min(a[3], b[3]) - max(a[1], b[1]), 0)
    ih <- max(min(a[4], b[4]) - max(a[2], b[2]), 0)
    inter <- iw * ih
    ua <- (a[3] - a[1]) * (a[4] - a[2]) + (b[3] - b[1]) * (b[4] - b[2]) - inter
    iou <- if (ua > 0) inter / ua else 0
    cw <- max(a[3], b[3]) - min(a[1], b[1])
    ch <- max(a[4], b[4]) - min(a[2], b[2])
    ac <- cw * ch
    gi <- if (ac > 0) iou - (ac - ua) / ac else iou
    giou[g, q] <- 1 - gi
  }
  cost <- 5 * l1 + 2 * giou
  assign <- .detr_hungarian(cost)
  total <- 0; lt <- 0; gt <- 0
  for (g in seq_len(G)) {
    total <- total + cost[g, assign[g]]
    lt <- lt + l1[g, assign[g]]
    gt <- gt + giou[g, assign[g]]
  }
  matched <- sort(assign)
  unmatched <- setdiff(seq_len(Q), matched)
  .t1_result(estimate = total, assignment = assign - 1L, cost = total,
             matched = matched - 1L, unmatched = unmatched - 1L,
             l1_cost = lt, giou_cost = gt, Q = Q, G = G,
             method = "DETR set prediction with Hungarian matching")
}
