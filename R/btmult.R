# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multinomial bootstrap resampling weights
#'
#' Efron, B. (1979), "Bootstrap methods: another look at the jackknife",
#' \emph{The Annals of Statistics} 7(1), 1-26, doi:10.1214/aos/1176344552,
#' p. 3, steps 1-3, read from the Project Euclid PDF rendered as page images:
#' construct F-hat putting mass 1/n at each of x_1 ... x_n; draw X* of size n
#' from F-hat with replacement (Eq. 2.4); approximate the distribution of
#' R(X, F) by that of R(X*, F-hat) (Eq. 2.5).  A draw of size n with
#' replacement from n points is a Multinomial(n; 1/n, ..., 1/n) count vector,
#' so the weight vector is that count vector divided by n; every row of W sums
#' to 1 and every entry is a multiple of 1/n.
#'
#' Determinism, which the parity check between the two language arms requires,
#' replaces the random draw with the van der Corput low-discrepancy sequence
#' already used elsewhere in this package: replication b, position i takes the
#' point vdc(b, p_i) where p_i is the i-th prime -- a Halton design, one base
#' per position.  A single stream shared by all positions is NOT usable here:
#' consecutive van der Corput points stratify so evenly that every replication
#' draws each index exactly once, every resample is a permutation of the data,
#' and the bootstrap variance collapses to zero.  rng shifts the starting prime
#' rather than being a seed.
#' With exhaustive the sequence is not used at all: all n^n index tuples are
#' enumerated, which is the complete bootstrap distribution rather than a
#' sample from it, and B is ignored.
#'
#' @param n Sample size.
#' @param B Number of replications when not enumerating.
#' @param rng Base of the van der Corput sequence, at least 2.
#' @param exhaustive Enumerate all n^n resamples instead (n <= 6).
#' @return list: estimate (mean weight, 1/n), W, counts, B, n, exhaustive,
#'   method.
#' @keywords internal
#' @examples
#' Btmult(3, exhaustive = TRUE)$B
#' @export
Btmult <- function(n, B = 200L, rng = 2L, exhaustive = FALSE) {
  n <- as.integer(n)
  if (n < 1L) stop("boot_multinomial_weights: n must be at least 1")
  B <- as.integer(B)
  if (!exhaustive && B < 1L) stop("boot_multinomial_weights: B must be at least 1")
  rng <- as.integer(rng)
  if (rng < 2L) stop("boot_multinomial_weights: rng must be a base of at least 2")
  cs <- .bt_counts(n, B, rng, isTRUE(exhaustive))
  W <- cs / n
  list(estimate = sum(W) / (nrow(W) * n), W = W, counts = cs,
       B = nrow(W), n = n, exhaustive = isTRUE(exhaustive),
       method = "Multinomial bootstrap weights")
}

# Resample count vectors: one row per bootstrap replication.
#' Resample count vectors: one row per bootstrap replication
#'
#' A step of the btmult implementation. Called by \code{Btcimed}, \code{Btmult}, \code{Btvarm}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param B A count; the body uses it as \code{seq_len(...)}.
#' @param rng Numeric; combined arithmetically in the body.
#' @param exhaustive A flag; the body branches on it.
#' @return The value of \code{out}, as built in the body.
#' @export
.bt_counts <- function(n, B, rng, exhaustive) {
  if (exhaustive) {
    if (n > 6L) {
      stop("boot_multinomial_weights: exhaustive enumeration is capped at n = 6")
    }
    rows <- matrix(0L, 1L, 0L)
    for (p in seq_len(n)) {
      nr <- nrow(rows)
      rows <- cbind(rows[rep(seq_len(nr), each = n), , drop = FALSE],
                    rep(seq_len(n), times = nr))
    }
    out <- matrix(0, nrow(rows), n)
    for (b in seq_len(nrow(rows))) {
      for (p in seq_len(n)) out[b, rows[b, p]] <- out[b, rows[b, p]] + 1
    }
    return(out)
  }
  ps <- .bt_primes(n + rng)
  out <- matrix(0, B, n)
  for (b in seq_len(B)) {
    for (i in seq_len(n)) {
      u <- .s03vdc(b - 1L, ps[rng - 2L + i])
      j <- as.integer(u * n) + 1L
      if (j > n) j <- n
      out[b, j] <- out[b, j] + 1
    }
  }
  out
}

#' .bt_primes
#'
#' A step of the btmult implementation. Called by \code{.bt_counts}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param m Passed to \code{<}.
#' @return The value of \code{ps}, as built in the body.
#' @export
.bt_primes <- function(m) {
  ps <- integer(0); c <- 2L
  while (length(ps) < m) {
    ok <- TRUE
    for (q in ps) {
      if (q * q > c) break
      if (c %% q == 0L) { ok <- FALSE; break }
    }
    if (ok) ps <- c(ps, c)
    c <- c + 1L
  }
  ps
}
