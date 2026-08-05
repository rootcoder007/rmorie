# SPDX-License-Identifier: AGPL-3.0-or-later
#' Jackknife-after-bootstrap influence diagnostic
#'
#' Efron (1992), "Jackknife-after-bootstrap standard errors and influence
#' functions", Journal of the Royal Statistical Society Series B 54(1),
#' 83-111, and Davison and Hinkley (1997), Bootstrap Methods and their
#' Application, Section 3.10, which is the treatment consulted here.
#'
#' The resamples already drawn contain, for free, resamples of every
#' leave-one-out data set: the replicates whose index set never mentions
#' observation i are exactly a bootstrap sample of x with i removed.  So
#' theta_bar_(-i) = mean{t*_b : i not in the b-th index set} and
#' infl_i = theta_bar_(-i) - mean{t*_b}, with no refitting.  A large
#' |infl_i| says observation i moves the whole bootstrap distribution.
#'
#' The counting matters: an observation appearing in every resample has no
#' leave-i-out subset at all, and the honest answer there is a missing value,
#' not zero.  n_out reports the subset sizes so a diagnostic computed off two
#' replicates is not mistaken for a stable one.
#'
#' @param x the original sample of length n.
#' @param theta_b the B replicates.
#' @param B_idx list of B index vectors, 0-based, one per replicate.
#' @return list: infl_i, estimate, theta_minus, n_out, grand_mean,
#'   max_abs_influence, n, B, method.
#' @keywords internal
#' @examples
#' Btjkab(c(1, 2, 3), c(2, 2.5), list(c(0, 1), c(1, 2)))$infl_i
#' @export
Btjkab <- function(x, theta_b, B_idx) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n == 0L) stop("boot_jackknife_after_boot: x is empty")
  tb <- .s03vec(theta_b)
  B <- length(tb)
  if (B == 0L) stop("boot_jackknife_after_boot: no bootstrap replicates")
  idx <- lapply(B_idx, as.integer)
  if (length(idx) != B) {
    stop("boot_jackknife_after_boot: B_idx and theta_b have different lengths")
  }
  used <- matrix(FALSE, nrow = B, ncol = n)
  for (b in seq_len(B)) {
    for (i in idx[[b]]) {
      if (i < 0L || i >= n) stop("boot_jackknife_after_boot: an index is out of range")
      used[b, i + 1L] <- TRUE
    }
  }
  grand <- .s03mean(tb)
  infl <- numeric(n); tm <- numeric(n); nout <- integer(n)
  for (i in seq_len(n)) {
    s <- 0; cc <- 0L
    for (b in seq_len(B)) {
      if (!used[b, i]) {
        s <- s + tb[b]
        cc <- cc + 1L
      }
    }
    nout[i] <- cc
    if (cc == 0L) {
      tm[i] <- NA_real_
      infl[i] <- NA_real_
    } else {
      tm[i] <- s / cc
      infl[i] <- tm[i] - grand
    }
  }
  mx <- 0
  for (v in infl) if (!is.na(v) && abs(v) > mx) mx <- abs(v)
  list(infl_i = infl, estimate = mx, theta_minus = tm, n_out = nout, grand_mean = grand,
       max_abs_influence = mx, n = n, B = B,
       method = "Efron (1992) jackknife-after-bootstrap; Davison and Hinkley Sect. 3.10")
}
