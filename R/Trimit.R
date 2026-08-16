# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weight trimming with the excess redistributed
#'
#' Plain truncation loses weighted mass and biases any population total
#' downwards.  Potter's redistribution step returns the removed mass to
#' the untrimmed units in proportion to their own weights, preserving the
#' total exactly.  Redistribution can lift a unit above the threshold, so
#' cap-and-redistribute is iterated until none exceeds it.
#'
#' Formula: w_i' = min(w_i, w_max), then sum_i (w_i - w_i') is spread over
#' the untrimmed units in proportion to w_i'; repeat.
#'
#' @param y Numeric observations, used to report the effect on the
#'   weighted mean.
#' @param weights Non-negative design weights of the same length.
#' @param threshold Maximum permitted weight; must be at least the mean
#'   weight or no feasible redistribution exists.
#' @return List with \code{estimate} (weighted mean after trimming),
#'   \code{mean_before}, \code{weights}, \code{n_trimmed},
#'   \code{iterations}, \code{sumw}, \code{max_weight},
#'   \code{deff_before}, \code{deff_after}, \code{n}, \code{method}.
#' @references Potter, F. J. (1990). A study of procedures to identify and
#'   trim extreme sampling weights. Proceedings of the Section on Survey
#'   Research Methods, American Statistical Association, 225-230.
#' @examples
#' Trimit(c(1, 2, 3, 4), c(1, 1, 1, 9), 4)
#' @export
Trimit <- function(y, weights, threshold) {
  yy <- .s03vec(y); w <- .s03vec(weights); n <- length(yy)
  if (n == 0L) stop("weight_trimming: y is empty")
  if (length(w) != n) stop("weight_trimming: y and weights differ in length")
  if (any(w < 0)) stop("weight_trimming: weights must be non-negative")
  thr <- as.numeric(threshold); tot <- sum(w)
  if (thr <= 0) stop("weight_trimming: threshold must be positive")
  if (thr * n < tot)
    stop("weight_trimming: threshold below the mean weight, no feasible redistribution")
  mu0 <- sum(w * yy) / tot
  cur <- w; it <- 0L
  for (k in seq_len(100L)) {
    over <- sum(cur[cur > thr] - thr)
    if (over <= 1e-15) break
    it <- k
    cur[cur > thr] <- thr
    free <- sum(cur[cur < thr])
    if (free <= 0) break
    idx <- cur < thr
    cur[idx] <- cur[idx] + over * cur[idx] / free
  }
  s1 <- sum(cur)
  list(estimate = as.numeric(sum(cur * yy) / s1), mean_before = as.numeric(mu0),
       weights = cur, n_trimmed = as.integer(sum(w > thr)),
       iterations = as.integer(it), sumw = as.numeric(s1),
       max_weight = as.numeric(max(cur)),
       deff_before = .trimit_deff(w), deff_after = .trimit_deff(cur),
       n = as.integer(n),
       method = "cap at w_max then redistribute the excess [Potter 1990]")
}

#' .trimit_deff
#'
#' Part of the Trimit implementation; see the file header for the source
#' it follows.
#'
#' @param w See Usage.
#' @return A numeric value.
#' @export
.trimit_deff <- function(w) {
  s <- sum(w)
  if (s <= 0) return(NaN)
  length(w) * sum(w * w) / (s * s)
}

# CANONICAL TEST
# r <- Trimit(c(1, 2, 3, 4), c(1, 1, 1, 9), 4)
# stopifnot(abs(r$sumw - 12) < 1e-12, r$max_weight <= 4 + 1e-12)
