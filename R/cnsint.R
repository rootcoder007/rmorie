# SPDX-License-Identifier: AGPL-3.0-or-later

# Joint MAP for the Rasch model.  Difficulties are centred each cycle
# (the identification constraint), and a N(0, prior_var) penalty is
# carried on both sets of parameters because joint ML alone DIVERGES on
# perfectly separated response patterns.
#' Joint MAP for the Rasch model.  Difficulties are centred each cycle
#'
#' (the identification constraint), and a N(0, prior_var) penalty is
#' carried on both sets of parameters because joint ML alone DIVERGES on
#' perfectly separated response patterns.
#'
#' @param X A matrix; indexed by row and column.
#' @param iters Coerced to integer by the body, with \code{as.integer}.
#'   Defaults to \code{200}.
#' @param prior_var Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @return A list with \code{b}, \code{th}.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .rasch_jmle(X = X)
#' res
.rasch_jmle <- function(X, iters = 200, prior_var = 4) {
  n <- nrow(X)
  k <- ncol(X)
  b <- numeric(k)
  th <- numeric(n)
  for (it in seq_len(as.integer(iters))) {
    for (i in seq_len(n)) {
      num <- 0
      den <- 0
      for (j in seq_len(k)) {
        p <- .s03sigmoid(th[i] - b[j])
        num <- num + X[i, j] - p
        den <- den + p * (1 - p)
      }
      num <- num - th[i] / prior_var
      den <- den + 1 / prior_var
      if (den > 1e-12) th[i] <- th[i] + max(min(num / den, 1), -1)
    }
    for (j in seq_len(k)) {
      num <- 0
      den <- 0
      for (i in seq_len(n)) {
        p <- .s03sigmoid(th[i] - b[j])
        num <- num + p - X[i, j]
        den <- den + p * (1 - p)
      }
      num <- num - b[j] / prior_var
      den <- den + 1 / prior_var
      if (den > 1e-12) b[j] <- b[j] + max(min(num / den, 1), -1)
    }
    m <- sum(b) / k
    b <- b - m
  }
  list(b = b, th = th)
}

#' Concurrent calibration with anchor items
#'
#' Formula: jointly fit b_F, b_R on the combined sample with anchors
#'
#' Both groups are calibrated in ONE run, so the common anchor items put
#' every parameter on a single scale with no separate linking
#' transformation.  Any drift between the two groups' anchor
#' difficulties is therefore an estimate of anchor instability rather
#' than of scale.
#'
#' @param y An n x k matrix of 0/1 responses, both groups stacked.
#' @param item Ignored; the columns are the items.
#' @param group Group label per examinee, or NULL.
#' @param anchor Zero-based indices of the anchor items, or NULL.
#' @param iters Joint MAP cycles.
#' @return List with \code{estimate}, \code{b}, \code{b_focal},
#'   \code{b_reference}, \code{drift}, \code{theta_mean_focal},
#'   \code{theta_mean_reference}, \code{n}, \code{k}, \code{n_anchor},
#'   \code{method}.
#' @references Wingersky & Lord (1984), Applied Psychological
#'   Measurement 8(3):347-364; Kolen & Brennan (2014), Test Equating,
#'   Scaling, and Linking, 3rd ed., Springer, ch. 6.
#' @export
Cnsint <- function(y, item = NULL, group = NULL, anchor = NULL, iters = 200) {
  X <- .s03mat(y)
  n <- nrow(X)
  if (n == 0L) stop("empty input: y has no rows")
  k <- ncol(X)
  if (k < 2L) stop("need at least two items")
  if (any(!(X %in% c(0, 1)))) stop("responses must be 0/1")
  g <- if (is.null(group)) rep(0L, n) else group
  if (length(g) != n) stop("y and group must have the same length")
  anc <- if (is.null(anchor)) seq_len(k) else as.integer(anchor) + 1L
  if (any(anc < 1L | anc > k)) stop("anchor indices out of range")
  if (!length(anc)) stop("at least one anchor item is required")
  f0 <- .rasch_jmle(X, iters)
  b <- f0$b
  th <- f0$th
  keys <- unique(g)
  if (length(keys) == 1L) {
    bf <- b
    br <- b
    tf <- sum(th) / n
    tr <- tf
    drift <- rep(0, length(anc))
  } else {
    fi <- which(g == keys[1])
    ri <- which(g != keys[1])
    ff <- .rasch_jmle(X[fi, , drop = FALSE], iters)
    fr <- .rasch_jmle(X[ri, , drop = FALSE], iters)
    bf <- ff$b
    br <- fr$b
    tf <- sum(ff$th) / length(fi)
    tr <- sum(fr$th) / length(ri)
    drift <- bf[anc] - br[anc]
  }
  .t1_result(estimate = sum(abs(drift)) / length(drift), b = b,
             b_focal = bf, b_reference = br, drift = drift,
             theta_mean_focal = tf, theta_mean_reference = tr, n = n,
             k = k, n_anchor = length(anc),
             method = "concurrent calibration with anchor items")
}
