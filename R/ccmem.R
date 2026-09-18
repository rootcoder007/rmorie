# SPDX-License-Identifier: AGPL-3.0-or-later

#' Cross-classified membership weight matrix
#'
#' Formula: W_ij = 1/n_j if i in cluster j else 0; rows sum to 1
#'
#' A unit belonging to several higher-level units at once -- pupils in
#' both a school and a neighbourhood -- contributes to each through a
#' membership weight, and those weights must sum to one per unit or the
#' random-effect variance is rescaled without anyone noticing.  With two
#' classifications and no explicit weights each contributes 1/2.
#'
#' @param y Response, length n.
#' @param cluster1 First classification label per unit.
#' @param cluster2 Second classification, or NULL.
#' @param weights Membership weight per classification, or NULL.
#' @return List with \code{estimate}, \code{W}, \code{row_sums},
#'   \code{levels1}, \code{levels2}, \code{n_units}, \code{n_levels},
#'   \code{method}.
#' @references Goldstein (1994), Sociological Methods & Research
#'   22(3):364-375; Browne, Goldstein & Rasbash (2001), Statistical
#'   Modelling 1(2):103-124.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ccmem(V, V)
Ccmem <- function(y, cluster1, cluster2 = NULL, weights = NULL) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("empty input: y has no observations")
  c1 <- cluster1
  if (length(c1) != n) stop("y and cluster1 must have the same length")
  cls <- list(c1)
  if (!is.null(cluster2)) {
    if (length(cluster2) != n)
      stop("y and cluster2 must have the same length")
    cls[[2]] <- cluster2
  }
  C <- length(cls)
  if (is.null(weights)) {
    wc <- rep(1 / C, C)
  } else {
    wc <- .s03vec(weights)
    if (length(wc) != C)
      stop("weights must have one entry per classification")
    s <- sum(wc)
    if (s <= 0) stop("classification weights must sum to a positive value")
    wc <- wc / s
  }
  levels <- lapply(cls, unique)
  cols <- sum(vapply(levels, length, 0L))
  W <- matrix(0, n, cols)
  off <- 0L
  for (k in seq_len(C)) {
    lv <- levels[[k]]
    for (j in seq_along(lv)) for (i in seq_len(n))
      if (cls[[k]][i] == lv[j]) W[i, off + j] <- wc[k]
    off <- off + length(lv)
  }
  rs <- numeric(n)
  for (i in seq_len(n)) rs[i] <- sum(W[i, ])
  .t1_result(estimate = sum(rs) / n, W = W, row_sums = rs,
             levels1 = length(levels[[1]]),
             levels2 = if (C > 1L) length(levels[[2]]) else 0,
             n_units = n, n_levels = cols,
             method = "cross-classified membership weight matrix")
}
