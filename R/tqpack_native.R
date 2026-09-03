#' Bit-packing of quantiser indices
#'
#' Packs an array of `b`-bit codebook indices into a dense byte string,
#' and unpacks it again. This is the storage half of a quantiser: choosing
#' 4-bit codewords saves nothing if each is then stored in a 64-bit float.
#'
#' The layout is **big-endian bit order within a big-endian byte stream**:
#' index 0 occupies the most significant `b` bits of byte 0, the next
#' index continues immediately after it, crossing byte boundaries without
#' padding. Only the final byte is zero-padded on the right. Fixing the
#' convention explicitly matters -- a reader that assumes the opposite bit
#' order recovers plausible-looking indices that are silently wrong, and
#' no checksum in the format would catch it.
#'
#' Round-tripping is exact by construction, for every width and every
#' length, and that is the property the anchors check exhaustively rather
#' than on a sample.

#' .tqpack_pack_indices
#'
#' A step of the tqpack_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param indices Optional; may be \code{NULL}. A list; the body checks with \code{is.list}.
#' @param bits Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{result}, as built in the body.
#' @export
.tqpack_pack_indices <- function(indices, bits) {
  b <- as.integer(bits)
  if (is.na(b) || b < 1L || b > 32L) {
    stop(sprintf("pack_indices: bits must lie in 1..32, got %r", bits))
  }

  if (is.null(indices)) {
    idx_vec <- numeric(0)
  } else if (is.list(indices)) {
    idx_vec <- as.numeric(unlist(indices))
  } else {
    idx_vec <- as.numeric(indices)
  }

  limit <- 2 ^ b - 1
  n <- length(idx_vec)

  vals <- numeric(n)
  for (i in seq_len(n)) {
    v <- idx_vec[i]
    if (is.na(v)) {
      stop("pack_indices: index is NA")
    }
    if (v != floor(v)) {
      stop(sprintf("pack_indices: index %r is not an integer", v))
    }
    if (v < 0 || v > limit) {
      stop(sprintf("pack_indices: index %d does not fit in %d bits (max %d)",
                   v, b, limit))
    }
    vals[i] <- v
  }

  n_bytes <- (n * b + 7L) %/% 8L
  out <- integer(n_bytes)
  out_idx <- 0L
  acc <- 0
  nbits <- 0L
  two_to_b <- 2 ^ b

  for (iv in vals) {
    acc <- acc * two_to_b + iv
    nbits <- nbits + b
    while (nbits >= 8L) {
      nbits <- nbits - 8L
      pow2 <- 2 ^ nbits
      out_idx <- out_idx + 1L
      out[out_idx] <- as.integer(acc %/% pow2)
      acc <- acc %% pow2
    }
  }

  if (nbits > 0L) {
    out_idx <- out_idx + 1L
    out[out_idx] <- as.integer(acc * 2 ^ (8L - nbits))
  }

  out <- out[seq_len(out_idx)]

  compression <- if (out_idx > 0L) (64.0 * n / (8.0 * out_idx)) else 0.0

  result <- list(
    estimate = as.list(out),
    bytes = as.list(out),
    n_bytes = out_idx,
    n_indices = n,
    bits = b,
    bits_used = n * b,
    padding_bits = out_idx * 8L - n * b,
    compression_vs_float64 = compression,
    method = "Big-endian bit packing of fixed-width indices"
  )

  return(result)
}

#' .tqpack_unpack_indices
#'
#' A step of the tqpack_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param data Optional; may be \code{NULL}. A list; the body checks with \code{is.list}.
#' @param bits Coerced to integer by the body, with \code{as.integer}.
#' @param count Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{result}, as built in the body.
#' @export
.tqpack_unpack_indices <- function(data, bits, count) {
  b <- as.integer(bits)
  if (is.na(b) || b < 1L || b > 32L) {
    stop(sprintf("unpack_indices: bits must lie in 1..32, got %r", bits))
  }
  n <- as.integer(count)
  if (is.na(n) || n < 0L) {
    stop("unpack_indices: count must be non-negative")
  }

  if (is.null(data)) {
    by <- numeric(0)
  } else if (is.list(data)) {
    by <- as.numeric(unlist(data))
  } else {
    by <- as.numeric(data)
  }

  by_int <- integer(length(by))
  for (i in seq_along(by)) {
    v <- by[i]
    if (is.na(v)) {
      stop("unpack_indices: data contains NA")
    }
    by_int[i] <- as.integer(v %% 256)
  }
  by <- by_int

  need <- (n * b + 7L) %/% 8L
  if (length(by) < need) {
    stop(sprintf("unpack_indices: %d bytes cannot hold %d indices of %d bits (need %d)",
                 length(by), n, b, need))
  }

  out <- numeric(n)
  out_idx <- 0L
  acc <- 0
  nbits <- 0L
  pos <- 1L

  while (out_idx < n) {
    while (nbits < b) {
      acc <- acc * 256 + by[pos]
      pos <- pos + 1L
      nbits <- nbits + 8L
    }
    nbits <- nbits - b
    pow2 <- 2 ^ nbits
    out_idx <- out_idx + 1L
    out[out_idx] <- acc %/% pow2
    acc <- acc %% pow2
  }

  result <- list(
    estimate = as.list(out),
    indices = as.list(out),
    n_indices = n,
    bits = b,
    method = "Big-endian bit unpacking of fixed-width indices"
  )

  return(result)
}

#' .tqpack_cheatsheet
#'
#' A step of the tqpack_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .tqpack_cheatsheet()
#' res
.tqpack_cheatsheet <- function() {
  return("tqpack: pack b-bit indices big-endian, index 0 in the top b bits of byte 0, crossing byte boundaries; tail padded on the right; n_bytes = ceil(n*b/8); round-trip is exact.")
}

# Public API
morie_tqpack <- .tqpack_pack_indices
morie_tqpack_pack_indices <- .tqpack_pack_indices
morie_tqpack_unpack_indices <- .tqpack_unpack_indices
morie_tqpack_cheatsheet <- .tqpack_cheatsheet
tqpack <- .tqpack_pack_indices
turboquant_bit_pack_indices <- .tqpack_pack_indices
