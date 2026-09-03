# PolarQuant: quantization by polar transformation.
# Source: Han, I., Kacham, P., Karbasi, A., Mirrokni, V. and Zandieh,
# A. (2025), PolarQuant: quantizing KV caches with polar
# transformation, arXiv:2502.02617.  Definition 1 gives the recursive
# polar decomposition used here: coordinate pairs are replaced by a
# radius and an angle, level 1 angles lying in [0, 2 pi) and higher
# levels in [0, pi/2] because the radii are non-negative.  Angles are
# quantized on a uniform grid and dequantized to bin midpoints; the
# radius is kept exactly, which is what bounds the relative error.
#
# ATTRIBUTION NOTE: the generated stub this replaces credited
# "PolarQuant (Tang 2024)".  No such paper exists.  Both arms cite
# Han et al. (2025) instead, against which this is implemented.
#
# Native implementation mirroring Python morie.fn.polqnt exactly:
# same pair order, same bin index floor(t K / W) clamped to [0, K-1],
# same midpoint dequantization, same bit accounting.

#' .mor_pq_decompose
#'
#' A step of the polqnt_native implementation. Called by \code{morie_polqnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A list with \code{levels}, \code{radius}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .mor_pq_decompose(x = x)
#' res
.mor_pq_decompose <- function(x) {
  levels <- list()
  r <- x
  while (length(r) > 1L) {
    first <- length(levels) == 0L
    idx <- seq(1L, length(r), by = 2L)
    a <- r[idx]
    b <- r[idx + 1L]
    rad <- sqrt(a * a + b * b)
    t <- atan2(b, a)
    if (first) t[t < 0] <- t[t < 0] + 2 * pi
    levels[[length(levels) + 1L]] <- t
    r <- rad
  }
  list(levels = levels, radius = r[1L])
}

#' .mor_pq_reconstruct
#'
#' A step of the polqnt_native implementation. Called by \code{morie_polqnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param levels A vector; its length is taken and its elements indexed.
#' @param radius See Usage.
#' @return The value of \code{r}, as built in the body.
#' @export
.mor_pq_reconstruct <- function(levels, radius) {
  r <- radius
  for (ell in seq(length(levels), 1L)) {
    ang <- levels[[ell]]
    nxt <- numeric(2L * length(ang))
    nxt[seq(1L, length(nxt), by = 2L)] <- r * cos(ang)
    nxt[seq(2L, length(nxt), by = 2L)] <- r * sin(ang)
    r <- nxt
  }
  r
}

#' PolarQuant polar-transformation quantization
#'
#' Recursively replaces coordinate pairs by (radius, angle), quantizes
#' only the angles on uniform grids and keeps the final radius exactly
#' (Han et al. 2025, Definition 1).  Level-1 angles span
#' \eqn{[0, 2\pi)} and are given \code{bits_first} bits; deeper levels
#' span \eqn{\[0, \pi/2\]} and get \code{bits_rest}.
#'
#' @param x Vector whose length is a power of two, at least 2.
#' @param bits_first Bits per level-1 angle.
#' @param bits_rest Bits per deeper angle.
#' @param quantize \code{TRUE} (default) quantizes the angles;
#'   \code{FALSE} reconstructs from the exact angles, which isolates
#'   the transformation from the quantization and must be lossless.
#'   Both routes are available.
#' @return A list with \code{reconstruction}, \code{estimate},
#'   \code{radius}, \code{codes}, \code{mse}, \code{relative_l2},
#'   \code{bits_per_coord}, \code{n}, \code{method}.
#' @references Han, I., Kacham, P., Karbasi, A., Mirrokni, V. and
#'   Zandieh, A. (2025). PolarQuant. arXiv:2502.02617.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_polqnt(V)
morie_polqnt <- function(x, bits_first = 4L, bits_rest = 2L,
                         quantize = TRUE) {
  v <- as.numeric(x)
  d <- length(v)
  if (d < 2L || bitwAnd(d, d - 1L) != 0L)
    stop("length of x must be a power of two, at least 2")
  b1 <- as.integer(bits_first)
  br <- as.integer(bits_rest)
  if (b1 < 1L || br < 1L) stop("bit widths must be at least 1")
  dec <- .mor_pq_decompose(v)
  levels <- dec$levels
  codes <- numeric(0)
  if (isTRUE(quantize)) {
    qlevels <- vector("list", length(levels))
    for (ell in seq_along(levels)) {
      W <- if (ell == 1L) 2 * pi else 0.5 * pi
      nb <- if (ell == 1L) b1 else br
      K <- bitwShiftL(1L, nb)
      t <- levels[[ell]]
      q <- as.integer(t * K / W)
      q[q > K - 1L] <- K - 1L
      q[q < 0L] <- 0L
      codes <- c(codes, as.numeric(q))
      qlevels[[ell]] <- (q + 0.5) * W / K
    }
    rec <- .mor_pq_reconstruct(qlevels, dec$radius)
  } else {
    rec <- .mor_pq_reconstruct(levels, dec$radius)
  }
  err2 <- sum((rec - v)^2)
  nrm2 <- sum(v * v)
  nlev <- length(levels)
  nbits <- (d %/% 2L) * b1
  if (nlev > 1L)
    nbits <- nbits + sum(vapply(seq.int(2L, nlev), function(ell)
      bitwShiftR(d, ell) * br, numeric(1)))
  list(reconstruction = rec, estimate = rec, radius = dec$radius,
       codes = codes, mse = err2 / d,
       relative_l2 = if (nrm2 > 0) sqrt(err2 / nrm2) else 0,
       bits_per_coord = nbits / d, n = as.numeric(d),
       method = "PolarQuant polar-transformation quantization")
}
