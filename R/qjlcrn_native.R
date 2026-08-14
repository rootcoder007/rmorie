# Johnson-Lindenstrauss projections with binary coins.
# Sources: Achlioptas, D. (2003) "Database-friendly random projections:
# Johnson-Lindenstrauss with binary coins", Journal of Computer and
# System Sciences 66(4), 671-687, doi:10.1016/S0022-0000(03)00025-4
# (Theorem 1.1 for the dimension bound k0, the two entry distributions,
# the scaling E = AR/sqrt(k) and the 1 - n^-beta success probability;
# Sec. 1.1 for the observation that the projection reduces to additions
# and subtractions and that the sparse distribution touches a third of
# the attributes; Lemma 5.1 for the moment argument the theorem rests
# on); Johnson, W. B. & Lindenstrauss, J. (1984) "Extensions of
# Lipschitz mappings into a Hilbert space", in Conference in Modern
# Analysis and Probability, Contemporary Mathematics 26, 189-206,
# doi:10.1090/conm/026/737400 (the original lemma).
#
# Native implementation mirroring Python morie.fn.qjlcrn exactly: the
# same dimension bound k0 in closed form, the same +-1 (Rademacher) or
# sqrt(3)*(+1,0,-1) entry distributions drawn from the shared
# SplitMix64 generator one cell at a time in row-major order, the same
# scaling E = AR / sqrt(k) with the same left-to-right accumulation,
# the same pairwise squared-distance check skipping coincident pairs,
# and the same validation conditions so both arms refuse the same
# inputs.

DISTRIBUTIONS <- c("rademacher", "sparse")

#' Johnson-Lindenstrauss target dimension
#'
#' Theorem 1.1 from Achlioptas (2003): for \code{n} points and
#' \code{epsilon}, \code{beta} > 0,
#' \code{k0 = (4 + 2 beta) log n / (epsilon^2/2 - epsilon^3/3)}.
#' Any \code{k >= k0} achieves every pairwise squared distance within
#' \code{1 +/- epsilon} with probability at least
#' \code{1 - n^-beta}.
#'
#' @param n Number of points, integer \code{>= 2}.
#' @param epsilon Distortion tolerance, in \code{(0, 1)}.
#' @param beta Confidence parameter, positive; the failure
#'   probability is \code{n^-beta}.
#' @return A list with \code{k0}, \code{k} (the smallest integer
#'   \code{>= k0}), \code{n}, \code{epsilon}, \code{beta},
#'   \code{failure_probability}, \code{note}.
#' @references Achlioptas, D. (2003). Database-friendly random
#'   projections. Journal of Computer and System Sciences, 66(4),
#'   671-687, Theorem 1.1.
#' @export
target_dimension <- function(n, epsilon, beta = 1.0) {
  n <- as.integer(n)
  e <- as.numeric(epsilon); b <- as.numeric(beta)
  if (n < 2L) stop("qjlcrn: need at least two points")
  if (!(e > 0 && e < 1))
    stop(sprintf("qjlcrn: epsilon must lie in (0, 1), got %s",
                 format(e)))
  if (b <= 0)
    stop(sprintf("qjlcrn: beta must be positive, got %s", format(b)))
  denom <- e * e / 2 - e ^ 3 / 3
  k0 <- (4 + 2 * b) * log(as.numeric(n)) / denom
  list(k0 = k0, k = as.integer(ceiling(k0)), n = n, epsilon = e,
       beta = b,
       failure_probability = as.numeric(n) ^ (-b),
       note = "log is natural, as in the paper")
}

#' Entry-distribution moments
#'
#' Both Achlioptas distributions have mean \code{0} and variance
#' \code{1} exactly, which is what Theorem 1.1 needs and what this
#' function verifies in closed form rather than by simulation.
#'
#' @param distribution One of \code{"rademacher"}, \code{"sparse"}.
#' @return A list with \code{mean}, \code{variance},
#'   \code{fourth_moment}, \code{support}, \code{density},
#'   \code{note}.
#' @references Achlioptas, D. (2003). Database-friendly random
#'   projections. Journal of Computer and System Sciences, 66(4),
#'   671-687, Lemma 5.1.
#' @export
moments <- function(distribution = "rademacher") {
  if (!(distribution %in% DISTRIBUTIONS))
    stop(sprintf("qjlcrn: distribution must be one of %s, got %s",
                 paste(DISTRIBUTIONS, collapse = ", "), distribution))
  s <- sqrt(3)
  if (distribution == "rademacher") {
    support <- list(c(1.0, 0.5), c(-1.0, 0.5))
  } else {
    support <- list(c(s, 1 / 6), c(0.0, 2 / 3), c(-s, 1 / 6))
  }
  m1 <- sum(vapply(support, function(v) v[1] * v[2], numeric(1)))
  m2 <- sum(vapply(support, function(v) v[1] ^ 2 * v[2], numeric(1)))
  m4 <- sum(vapply(support, function(v) v[1] ^ 4 * v[2], numeric(1)))
  list(mean = m1, variance = m2, fourth_moment = m4,
       support = support,
       density = if (distribution == "rademacher") 1.0 else 1 / 3,
       note = "the sparse distribution is zero two-thirds of the time, so only a third of the attributes are touched per output coordinate")
}

#' Random projection matrix with binary coins
#'
#' A \code{d}-by-\code{k} matrix whose entries are drawn from the
#' chosen Achlioptas distribution using the shared SplitMix64
#' generator, one uniform per cell in row-major order, so the R arm
#' and the Python arm consume the same stream in the same order.
#'
#' @param d Source dimension, positive integer.
#' @param k Target dimension, positive integer.
#' @param distribution One of \code{"rademacher"}, \code{"sparse"}.
#' @param seed Seed for the shared generator.
#' @return A length-\code{d} list whose elements are length-\code{k}
#'   numeric vectors; together they form the matrix \code{R}.
#' @references Achlioptas, D. (2003). Database-friendly random
#'   projections. Journal of Computer and System Sciences, 66(4),
#'   671-687, Theorem 1.1.
#' @export
projection_matrix <- function(d, k, distribution = "rademacher",
                              seed = 0L) {
  if (!(distribution %in% DISTRIBUTIONS))
    stop(sprintf("qjlcrn: distribution must be one of %s, got %s",
                 paste(DISTRIBUTIONS, collapse = ", "), distribution))
  d <- as.integer(d); k <- as.integer(k)
  if (d < 1L || k < 1L) stop("qjlcrn: dimensions must be positive")
  e <- .ghc_rng(seed)
  s <- sqrt(3)
  R <- vector("list", d)
  for (t in seq_len(d)) {
    row <- numeric(k)
    for (j in seq_len(k)) {
      u <- .ghc_unif(e, 1L, 0, 1)
      if (distribution == "rademacher") {
        row[j] <- if (u < 0.5) 1.0 else -1.0
      } else {
        row[j] <- if (u < 1 / 6) s else if (u > 5 / 6) -s else 0.0
      }
    }
    R[[t]] <- row
  }
  R
}

#' Random projection with binary coins
#'
#' Draws a \code{d}-by-\code{k} matrix \code{R} from the chosen
#' Achlioptas distribution and returns the embedding
#' \code{E = A R / sqrt(k)}, the scaling that preserves the expected
#' squared norm (Achlioptas 2003, Theorem 1.1). As
#' \code{k} grows past the Theorem 1.1 bound \code{k0} the realised
#' pairwise squared distances land in \code{1 +/- epsilon} with
#' probability at least \code{1 - n^-beta} -- this is what
#' \code{distortion} checks on a given \code{E}.
#'
#' @param A List of points, each a numeric vector; all rows must
#'   share the same length.
#' @param k Target dimension, positive integer.
#' @param distribution One of \code{"rademacher"}, \code{"sparse"}.
#' @param seed Seed for the shared generator.
#' @return A list with \code{estimate}, \code{embedding} \code{E},
#'   \code{matrix} \code{R}, \code{k}, \code{d}, \code{n},
#'   \code{distribution}, \code{nonzero_fraction}, \code{norm},
#'   \code{method}.
#' @references Achlioptas, D. (2003). Database-friendly random
#'   projections. Journal of Computer and System Sciences, 66(4),
#'   671-687, Theorem 1.1 and Sec. 1.1. Johnson, W. B. &
#'   Lindenstrauss, J. (1984). Extensions of Lipschitz mappings
#'   into a Hilbert space. Contemporary Mathematics, 26, 189-206.
#' @export
morie_qjlcrn <- function(A, k, distribution = "rademacher",
                         seed = 0L) {
  n <- length(A)
  if (n == 0L) stop("qjlcrn: no points supplied")
  d <- length(A[[1L]])
  if (any(vapply(A, function(row) length(row) != d, logical(1))))
    stop(sprintf("qjlcrn: every point needs %d coordinates", d))
  R <- projection_matrix(d, as.integer(k), distribution, seed)
  kk <- as.integer(k)
  scale <- 1.0 / sqrt(as.numeric(k))
  E <- lapply(seq_len(n), function(i)
    vapply(seq_len(kk), function(j) {
      s <- 0.0
      for (t in seq_len(d)) s <- s + A[[i]][t] * R[[t]][j]
      scale * s
    }, numeric(1)))
  nz <- sum(vapply(R, function(row)
    sum(vapply(row, function(v) v != 0.0, logical(1))), integer(1)))
  list(estimate = as.numeric(k), embedding = E, matrix = R,
       k = kk, d = d, n = n, distribution = distribution,
       nonzero_fraction = nz / (d * kk),
       norm = "l2 -- Johnson-Lindenstrauss says nothing about l1",
       method = "random projection with binary coins; Achlioptas (2003) Theorem 1.1")
}

#' Realised pairwise distortion
#'
#' Measures how well \code{E} preserves the pairwise squared
#' distances of \code{A}, over every pair, rather than reporting a
#' probabilistic promise. Pairs whose original squared distance is
#' zero (coincident points) are skipped, mirroring the Python arm.
#'
#' @param A Original points, list of equal-length numeric vectors.
#' @param E Embedded points, one row per point of \code{A}.
#' @return A list with \code{worst_distortion}, \code{min_ratio},
#'   \code{max_ratio}, \code{mean_ratio}, \code{n_pairs}.
#' @references Achlioptas, D. (2003). Theorem 1.1. Johnson, W. B.
#'   & Lindenstrauss, J. (1984). Contemporary Mathematics, 26,
#'   189-206.
#' @export
distortion <- function(A, E) {
  if (length(A) != length(E))
    stop("qjlcrn: the embedding must have one row per point")
  n <- length(A)
  dA <- if (n > 0L) length(A[[1L]]) else 0L
  dE <- if (n > 0L) length(E[[1L]]) else 0L
  worst <- 0.0
  ratios <- numeric(0)
  if (n >= 2L) {
    for (i in seq_len(n - 1L)) {
      Ai <- A[[i]]; Ei <- E[[i]]
      for (j in (i + 1L):n) {
        d0 <- sum((Ai[seq_len(dA)] - A[[j]][seq_len(dA)]) ^ 2)
        d1 <- sum((Ei[seq_len(dE)] - E[[j]][seq_len(dE)]) ^ 2)
        if (d0 <= 0) next
        r <- d1 / d0
        ratios <- c(ratios, r)
        if (abs(r - 1) > worst) worst <- abs(r - 1)
      }
    }
  }
  if (length(ratios) == 0L)
    stop("qjlcrn: every pair of points coincides")
  list(worst_distortion = worst,
       min_ratio = min(ratios), max_ratio = max(ratios),
       mean_ratio = sum(ratios) / length(ratios),
       n_pairs = length(ratios))
}

#' One-line method cheatsheet
#'
#' Mirrors Python \code{morie.fn.qjlcrn.cheatsheet}: a single
#' character string summarising the dimension bound, the two entry
#' distributions and the \code{l2}-only scope.
#'
#' @return A character string.
#' @references Achlioptas, D. (2003). Database-friendly random
#'   projections. Journal of Computer and System Sciences, 66(4),
#'   671-687.
#' @export
cheatsheet <- function() {
  paste("qjlcrn: k0 = (4 + 2 beta) log n / (eps^2/2 - eps^3/3), R with entries +-1 (or sqrt(3) times {+1,0,-1} at 1/6, 2/3, 1/6), E = AR/sqrt(k). Both distributions have mean 0 and variance 1, so no Gaussians and no multiplications are needed; the sparse one touches a third of the attributes. Every pairwise squared distance is then within 1 +- eps with probability 1 - n^-beta. This is an l2 statement only.")
}

# compact alias per ledger/NAMING.md
johnson_lindenstrauss <- morie_qjlcrn
