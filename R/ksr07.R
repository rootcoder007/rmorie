# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric (multinomial) bootstrap of the empirical mean process.
#'
#' The bootstrap process is sqrt(n)(Phat_n - P_n), centred at the
#' EMPIRICAL measure. Resampling uses a pinned Lehmer generator with a
#' FIXED budget of B replicates, so the two language arms agree exactly.
#'
#' Formula: Phat_n f = n^-1 sum_i W_i f(X_i),
#'   (W_1..W_n) ~ Multinomial(n, 1/n); Ghat_n = sqrt(n) (Phat_n - P_n)
#'
#' @param x The sample.
#' @param B Number of bootstrap replicates (fixed budget).
#' @param seed Seed for the pinned generator.
#' @return List with \code{estimate}, \code{boot_mean}, \code{boot_sd},
#'   \code{process_sd}, \code{ci_lower}, \code{ci_upper}, \code{B},
#'   \code{n}.
#' @references Kosorok (2008), Introduction to Empirical Processes and
#'   Semiparametric Inference, Section 2.2.3 and Theorem 2.6. Fetched as
#'   the full text of the book.
#' @export
Bootemp <- function(x, B = 200, seed = 1) {
  x <- .t1_vec(x); n <- length(x); B <- as.integer(B)
  if (n < 2L) stop("the sample must have at least two observations")
  if (B < 2L) stop("B must be at least 2")
  Pn <- mean(x)
  g <- .t1_lcg(seed)
  stat <- numeric(B)
  for (b in seq_len(B)) {
    s <- 0
    for (i in seq_len(n)) {
      j <- as.integer(g$unif() * n)
      if (j >= n) j <- n - 1L
      s <- s + x[j + 1L]
    }
    stat[b] <- s / n
  }
  bm <- mean(stat); bsd <- stats::sd(stat)
  q <- sort(stat)
  lo <- q[max(1L, floor(0.025 * (B - 1)) + 1L)]
  hi <- q[min(B, ceiling(0.975 * (B - 1)) + 1L)]
  .t1_result(estimate = Pn, boot_mean = bm, boot_sd = bsd,
             process_sd = sqrt(n) * bsd, ci_lower = lo, ci_upper = hi,
             B = as.numeric(B), n = as.numeric(n),
             method = "Nonparametric bootstrap, Kosorok Section 2.2.3")
}

# NAMESPACE exported both of these names, but only the short function above
# was ever defined, so loading the namespace could not resolve them. Same
# alias pattern as ksr02.R.

#' @rdname Bootemp
#' @keywords internal
#' @export
morie_ksr07_kosorok_bootstrap_empirical <- Bootemp

#' @rdname Bootemp
#' @keywords internal
#' @export
morie_kosorok_bootstrap_empirical <- Bootemp
