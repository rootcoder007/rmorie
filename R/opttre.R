# SPDX-License-Identifier: AGPL-3.0-or-later
#' Optimal treatment regime by exhaustive tree search
#'
#' Laber & Zhao's argument is that a regime is only useful if a
#' clinician can read it, and that the usual two-step approach --
#' regress the outcome, then take the argmax -- optimizes the wrong
#' thing: a small error in the outcome model where the two arms are
#' close flips the recommendation. Their tree instead maximizes the
#' ESTIMATED VALUE directly,
#' \code{V(d) = sum Y_i 1{A_i = d(W_i)} / pi_i / sum 1{A_i = d(W_i)} /
#' pi_i}, over axis-aligned splits. This implementation enumerates
#' every split of every covariate at every observed value and recurses.
#'
#' The candidate scan runs COVARIATE-MAJOR then observation, and an
#' improvement must be STRICT, so ties keep the first candidate in that
#' fixed order -- R scans a matrix column-major and Python row-major,
#' and an unpinned scan would return a different-but-equally-optimal
#' split per language.
#'
#' @param y Outcome, larger is better.
#' @param A Observed binary treatment, 0/1.
#' @param W Covariates.
#' @param pi Propensity \code{P(A = A_i | W_i)}, or \code{NULL} for the
#'   marginal randomization probability.
#' @param max_depth Maximum tree depth; 0 gives one constant rule.
#' @param min_leaf Minimum observations in a leaf.
#' @return List with \code{estimate}, \code{value},
#'   \code{value_all_treated}, \code{value_all_control}, \code{rule},
#'   \code{split_var}, \code{split_point}, \code{n_leaves},
#'   \code{depth}, \code{n}.
#' @references Laber, E. B. & Zhao, Y.-Q. (2015). Tree-based methods
#'   for individualized treatment regimes. Biometrika, 102(3), 501-514.
#'   doi:10.1093/biomet/asv028
#' @export
Opttre <- function(y, A, W, pi = NULL, max_depth = 2L, min_leaf = 1L) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("Opttre: y is empty")
  Av <- as.numeric(A)
  if (length(Av) != n) stop("Opttre: y and A have different lengths")
  if (!all(Av %in% c(0, 1))) stop("Opttre: A must be binary 0/1")
  Wm <- as.matrix(W)
  if (nrow(Wm) != n) stop("Opttre: W and y have different lengths")
  p <- ncol(Wm)
  md <- as.integer(max_depth)
  if (md < 0L) stop("Opttre: max_depth must be non-negative")
  ml <- as.integer(min_leaf)
  if (ml < 1L) stop("Opttre: min_leaf must be at least 1")
  if (is.null(pi)) {
    pt <- sum(Av) / n
    if (pt <= 0 || pt >= 1) stop("Opttre: both treatments must be observed")
    pv <- ifelse(Av == 1, pt, 1 - pt)
  } else {
    pv <- as.numeric(pi)
    if (length(pv) != n) stop("Opttre: pi and y have different lengths")
    if (any(pv <= 0 | pv > 1)) stop("Opttre: pi must lie in (0, 1]")
  }
  leaf_score <- function(idx, a) {
    s <- idx[Av[idx] == a]
    c(sum(yv[s] / pv[s]), sum(1 / pv[s]))
  }
  best_const <- function(idx) {
    q1 <- leaf_score(idx, 1)
    q0 <- leaf_score(idx, 0)
    v1 <- if (q1[2] > 0) q1[1] / q1[2] else -Inf
    v0 <- if (q0[2] > 0) q0[1] / q0[2] else -Inf
    if (v1 > v0) c(1, q1) else c(0, q0)
  }
  rule <- numeric(n)
  root_var <- -1L
  root_point <- NaN
  depth_used <- 0L
  build <- function(idx, depth, record_root) {
    bc <- best_const(idx)
    best_v <- NA_real_
    best_j <- NA_integer_
    best_thr <- NA_real_
    if (depth < md && length(idx) >= 2L * ml) {
      for (j in seq_len(p)) {
        for (i in idx) {
          thr <- Wm[i, j]
          left <- idx[Wm[idx, j] <= thr]
          right <- idx[Wm[idx, j] > thr]
          if (length(left) < ml || length(right) < ml) next
          bl <- best_const(left)
          br <- best_const(right)
          if (bl[3] <= 0 || br[3] <= 0) next
          v <- (bl[2] + br[2]) / (bl[3] + br[3])
          if (is.na(best_v) || v > best_v) {
            best_v <- v
            best_j <- j
            best_thr <- thr
          }
        }
      }
    }
    v_leaf <- if (bc[3] > 0) bc[2] / bc[3] else -Inf
    if (!is.na(best_v) && best_v > v_leaf) {
      thr <- best_thr
      j <- best_j
      left <- idx[Wm[idx, j] <= thr]
      right <- idx[Wm[idx, j] > thr]
      if (record_root) { root_var <<- j - 1L
      root_point <<- thr }
      if (depth + 1L > depth_used) depth_used <<- depth + 1L
      return(build(left, depth + 1L, FALSE) + build(right, depth + 1L, FALSE))
    }
    rule[idx] <<- bc[1]
    1L
  }
  n_leaves <- build(seq_len(n), 0L, TRUE)
  value <- function(rec) {
    m <- as.numeric(rec == Av)
    den <- sum(m / pv)
    if (den > 0) sum(yv * m / pv) / den else NaN
  }
  .t1_result(estimate = value(rule), value = value(rule),
             value_all_treated = value(rep(1, n)),
             value_all_control = value(rep(0, n)), rule = rule,
             split_var = root_var, split_point = root_point,
             n_leaves = n_leaves, depth = depth_used, n = n,
             method = "Value-search treatment regime tree (Laber & Zhao 2015)")
}
