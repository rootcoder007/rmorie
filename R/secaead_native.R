# morie.fn -- function file (rootcoder007/morie)
# AEAD_CHACHA20_POLY1305: encrypt and authenticate, in that order.
#
# A stream cipher gives confidentiality and nothing else: flip a
# ciphertext bit and exactly the corresponding plaintext bit flips,
# undetected. AEAD closes that by pairing the cipher with a one-time
# authenticator, and RFC 8439's construction is specific about how.
#
# ChaCha20 builds a 64-byte keystream block from a 16-word state --
# four constants, eight key words, a block counter, three nonce words
# -- by twenty rounds of the quarter-round, alternating column and
# diagonal rounds, then ADDING the original state back. That final
# addition is what makes the block function non-invertible.
#
# Poly1305 is a one-time authenticator over F_(2^130 - 5), and its key
# must be CLAMPED: r[3], r[7], r[11], r[15] keep only their low four
# bits and r[4], r[8], r[12] lose their low two.
#
# The one-time key is derived per message: ChaCha20 block 0 with the
# same key and nonce produces it, and the message is then encrypted
# starting at counter 1. Reusing a Poly1305 key across two messages
# lets an attacker solve for r and forge at will.
#
# The MAC input is padded and length-tagged: AAD, zero-padded to a
# 16-byte boundary; ciphertext, likewise; then the two lengths as
# 64-bit little-endian integers.
#
# Decryption verifies before it returns anything, with a
# constant-time comparison, and returns nothing at all on failure.
#
# References
# ----------
# Nir, Y. & Langley, A. (2018) "ChaCha20 and Poly1305 for IETF
# Protocols", RFC 8439, doi:10.17487/RFC8439. Secs. 2.1-2.3, 2.5,
# 2.6, 2.8, 2.8.2.
#
# Bernstein, D. J. (2008) "ChaCha, a variant of Salsa20", Workshop
# Record of SASC 2008.
#
# Bernstein, D. J. (2005) "The Poly1305-AES message-authentication
# code", FSE 2005, LNCS 3557, 32-49, doi:10.1007/11502760_3.

.secaead_MASK32 <- 4294967296          # 2^32
.secaead_CONST <- c(0x61707865, 0x3320646e, 0x79622d32, 0x6b206574)
.secaead_B26 <- 67108864               # 2^26

#' Return an integer vector of byte values (0..255)
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
.secaead_as_bytes <- function(x) {
  # Return an integer vector of byte values (0..255).
  if (is.raw(x)) {
    return(as.integer(x))
  }
  if (is.character(x)) {
    return(as.integer(charToRaw(paste(x, collapse=""))))
  }
  if (is.null(x)) {
    return(integer(0))
  }
  as.integer(x) %% 256L
}

#' .secaead_hexlify
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param bs See Usage.
#' @return A character value.
#' @export
.secaead_hexlify <- function(bs) {
  paste(sprintf("%02x", .secaead_as_bytes(bs)), collapse="")
}

#' .secaead_constant_time_equal
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A logical value.
#' @export
.secaead_constant_time_equal <- function(a, b) {
  x <- .secaead_as_bytes(a)
  y <- .secaead_as_bytes(b)
  if (length(x) != length(y)) {
    # still compare over the max length so the branch does not leak,
    # but unequal lengths always fail
    m <- max(length(x), length(y), 1L)
    diff <- bitwXor(length(x), length(y))
    for (i in seq_len(m)) {
      xi <- if (length(x) > 0L) x[((i - 1L) %% length(x)) + 1L] else 0L
      yi <- if (length(y) > 0L) y[((i - 1L) %% length(y)) + 1L] else 0L
      diff <- bitwOr(diff, bitwXor(xi, yi))
    }
    return(diff == 0L)
  }
  diff <- 0L
  for (i in seq_along(x)) {
    diff <- bitwOr(diff, bitwXor(x[i], y[i]))
  }
  diff == 0L
}

# ------------------------------------------------------------- ChaCha20
# 32-bit words are held as doubles in [0, 2^32) to sidestep R's signed
# 32-bit integer range in bitwXor.

#' .secaead_xor32
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.secaead_xor32 <- function(a, b) {
  ah <- a %/% 65536
  al <- a %% 65536
  bh <- b %/% 65536
  bl <- b %% 65536
  bitwXor(ah, bh) * 65536 + bitwXor(al, bl)
}

#' Left-rotate a 32-bit word by n (n in {7, 8, 12, 16}); x*2^n stays
#'
#' below 2^48 for these n, well within double precision
#'
#' @param x See Usage.
#' @param n See Usage.
#' @return A numeric value.
#' @export
.secaead_rotl <- function(x, n) {
  # left-rotate a 32-bit word by n (n in {7, 8, 12, 16}); x*2^n stays
  # below 2^48 for these n, well within double precision
  ((x * (2 ^ n)) %% .secaead_MASK32) + (x %/% (2 ^ (32 - n)))
}

#' .secaead_qr
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param s See Usage.
#' @param a See Usage.
#' @param b See Usage.
#' @param c See Usage.
#' @param d See Usage.
#' @return The value of \code{s}, as built in the body.
#' @export
.secaead_qr <- function(s, a, b, c, d) {
  s[a] <- (s[a] + s[b]) %% .secaead_MASK32
  s[d] <- .secaead_rotl(.secaead_xor32(s[d], s[a]), 16)
  s[c] <- (s[c] + s[d]) %% .secaead_MASK32
  s[b] <- .secaead_rotl(.secaead_xor32(s[b], s[c]), 12)
  s[a] <- (s[a] + s[b]) %% .secaead_MASK32
  s[d] <- .secaead_rotl(.secaead_xor32(s[d], s[a]), 8)
  s[c] <- (s[c] + s[d]) %% .secaead_MASK32
  s[b] <- .secaead_rotl(.secaead_xor32(s[b], s[c]), 7)
  s
}

#' B is an integer byte vector whose length is a multiple of 4
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param b See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.secaead_words_le <- function(b) {
  # b is an integer byte vector whose length is a multiple of 4
  n <- length(b)
  out <- numeric(n %/% 4L)
  for (i in seq_len(n %/% 4L)) {
    j <- (i - 1L) * 4L
    out[i] <- b[j + 1L] + b[j + 2L] * 256 + b[j + 3L] * 65536 +
      b[j + 4L] * 16777216
  }
  out
}

#' .secaead_le_bytes
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param words See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.secaead_le_bytes <- function(words) {
  out <- integer(length(words) * 4L)
  for (i in seq_along(words)) {
    w <- words[i]
    j <- (i - 1L) * 4L
    out[j + 1L] <- as.integer(w %% 256)
    out[j + 2L] <- as.integer((w %/% 256) %% 256)
    out[j + 3L] <- as.integer((w %/% 65536) %% 256)
    out[j + 4L] <- as.integer((w %/% 16777216) %% 256)
  }
  out
}

#' One 64-byte keystream block. The permuted state is ADDED to the
#'
#' original, which is what stops the block function being invertible.
#'
#' @param key See Usage.
#' @param counter See Usage.
#' @param nonce See Usage.
#' @param rounds Defaults to \code{20}.
#' @return The value of \code{.secaead_le_bytes}.
#' @export
morie_secaead_chacha20_block <- function(key, counter, nonce, rounds=20) {
  # One 64-byte keystream block. The permuted state is ADDED to the
  # original, which is what stops the block function being invertible.
  k <- .secaead_as_bytes(key)
  n <- .secaead_as_bytes(nonce)
  if (length(k) != 32L) {
    stop(sprintf("secaead: the key must be 32 bytes, got %d", length(k)))
  }
  if (length(n) != 12L) {
    stop(sprintf("secaead: the nonce must be 12 bytes, got %d", length(n)))
  }
  state <- c(.secaead_CONST, .secaead_words_le(k),
             as.numeric(counter) %% .secaead_MASK32,
             .secaead_words_le(n))
  work <- state
  for (r in seq_len(as.integer(rounds) %/% 2L)) {
    work <- .secaead_qr(work, 1, 5, 9, 13)
    work <- .secaead_qr(work, 2, 6, 10, 14)
    work <- .secaead_qr(work, 3, 7, 11, 15)
    work <- .secaead_qr(work, 4, 8, 12, 16)
    work <- .secaead_qr(work, 1, 6, 11, 16)
    work <- .secaead_qr(work, 2, 7, 12, 13)
    work <- .secaead_qr(work, 3, 8, 9, 14)
    work <- .secaead_qr(work, 4, 5, 10, 15)
  }
  .secaead_le_bytes((work + state) %% .secaead_MASK32)
}

#' XOR the data with the keystream from counter onward
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param key See Usage.
#' @param counter See Usage.
#' @param nonce See Usage.
#' @param data See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_secaead_chacha20 <- function(key, counter, nonce, data) {
  # XOR the data with the keystream from counter onward.
  d <- .secaead_as_bytes(data)
  out <- integer(length(d))
  nblk <- (length(d) + 63L) %/% 64L
  for (bi in seq_len(nblk)) {
    ks <- morie_secaead_chacha20_block(key,
                                       as.numeric(counter) + (bi - 1L),
                                       nonce)
    lo <- (bi - 1L) * 64L + 1L
    hi <- min(bi * 64L, length(d))
    idx <- lo:hi
    out[idx] <- bitwXor(d[idx], ks[seq_along(idx)])
  }
  out
}

# ------------------------------------------------------------- Poly1305
# Field arithmetic mod 2^130 - 5 in base 2^26 limbs (LSB first), stored
# as doubles. Multiplication carries after each partial product so no
# intermediate exceeds 2^53.

#' Bs: integer bytes LSB first -> base 2^26 limbs
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param bs See Usage.
#' @return The value of \code{limbs}, as built in the body.
#' @export
.secaead_limbs_from_bytes <- function(bs) {
  # bs: integer bytes LSB first -> base 2^26 limbs
  limbs <- numeric(0)
  cur <- 0
  bits <- 0
  for (byte in bs) {
    cur <- cur + byte * (2 ^ bits)
    bits <- bits + 8
    while (bits >= 26) {
      limbs <- c(limbs, cur %% .secaead_B26)
      cur <- cur %/% .secaead_B26
      bits <- bits - 26
    }
  }
  if (cur > 0 || length(limbs) == 0L) {
    limbs <- c(limbs, cur)
  }
  limbs
}

#' .secaead_bytes_from_limbs
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param limbs See Usage.
#' @param nbytes See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.secaead_bytes_from_limbs <- function(limbs, nbytes) {
  out <- integer(nbytes)
  cur <- 0
  bits <- 0
  pos <- 1L
  li <- 1L
  while (pos <= nbytes) {
    if (bits < 8 && li <= length(limbs)) {
      cur <- cur + limbs[li] * (2 ^ bits)
      bits <- bits + 26
      li <- li + 1L
    }
    if (bits < 8 && li > length(limbs)) {
      # nothing left to feed; emit remaining low bits
      out[pos] <- as.integer(cur %% 256)
      cur <- cur %/% 256
      bits <- max(0, bits - 8)
      pos <- pos + 1L
      next
    }
    out[pos] <- as.integer(cur %% 256)
    cur <- cur %/% 256
    bits <- bits - 8
    pos <- pos + 1L
  }
  out
}

#' Normalize base 2^26 in place; returns limbs each < 2^26 plus a
#'
#' possible extra high limb
#'
#' @param limbs See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.secaead_p_carry <- function(limbs) {
  # normalize base 2^26 in place; returns limbs each < 2^26 plus a
  # possible extra high limb
  out <- limbs
  carry <- 0
  for (i in seq_along(out)) {
    v <- out[i] + carry
    carry <- floor(v / .secaead_B26)
    out[i] <- v - carry * .secaead_B26
  }
  while (carry > 0) {
    out <- c(out, carry %% .secaead_B26)
    carry <- floor(carry / .secaead_B26)
  }
  out
}

#' .secaead_p_add
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return The value of \code{.secaead_p_carry}.
#' @export
.secaead_p_add <- function(a, b) {
  n <- max(length(a), length(b))
  a <- c(a, rep(0, n - length(a)))
  b <- c(b, rep(0, n - length(b)))
  .secaead_p_carry(a + b)
}

#' Fold limbs at index >= 6 (weight >= 2^130) back with factor 5,
#'
#' since 2^130 == 5 (mod 2^130 - 5), then carry-normalize; repeat
#'
#' @param res See Usage.
#' @return The value of \code{[}.
#' @export
.secaead_p_reduce <- function(res) {
  # fold limbs at index >= 6 (weight >= 2^130) back with factor 5,
  # since 2^130 == 5 (mod 2^130 - 5), then carry-normalize; repeat
  repeat {
    res <- .secaead_p_carry(res)
    if (length(res) <= 5L) {
      break
    }
    # fold each limb of weight >= 2^130 back into limb-5-lower with
    # factor 5 (2^130 == 5 mod 2^130 - 5), highest first, in place
    for (t in seq.int(length(res), 6L)) {
      res[t - 5L] <- res[t - 5L] + 5 * res[t]
      res[t] <- 0
    }
    res <- res[1:5]
  }
  res <- c(res, rep(0, max(0L, 5L - length(res))))
  res[1:5]
}

#' Reduce to the canonical residue in [0, 2^130 - 5): one conditional
#'
#' subtraction of P suffices because acc < 2^130 < 2P
#'
#' @param acc See Usage.
#' @return The value of \code{acc}, as built in the body.
#' @export
.secaead_p_final <- function(acc) {
  # reduce to the canonical residue in [0, 2^130 - 5): one conditional
  # subtraction of P suffices because acc < 2^130 < 2P
  acc <- .secaead_p_reduce(acc)
  plimbs <- c(.secaead_B26 - 5, rep(.secaead_B26 - 1, 4))
  g <- numeric(5)
  borrow <- 0
  for (i in seq_len(5)) {
    v <- acc[i] - plimbs[i] - borrow
    if (v < 0) {
      v <- v + .secaead_B26
      borrow <- 1
    } else {
      borrow <- 0
    }
    g[i] <- v
  }
  if (borrow == 0) acc <- g  # acc >= P, so use acc - P
  acc
}

#' .secaead_p_mulmod
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return The value of \code{.secaead_p_reduce}.
#' @export
.secaead_p_mulmod <- function(a, b) {
  la <- length(a)
  lb <- length(b)
  res <- numeric(la + lb)
  for (i in seq_len(la)) {
    ai <- a[i]
    if (ai == 0) {
      next
    }
    for (j in seq_len(lb)) {
      kk <- i + j - 1L
      res[kk] <- res[kk] + ai * b[j]
      carry <- floor(res[kk] / .secaead_B26)
      res[kk] <- res[kk] - carry * .secaead_B26
      res[kk + 1L] <- res[kk + 1L] + carry
    }
  }
  .secaead_p_reduce(res)
}

#' K: 32 integer bytes; clamp the low 16 (r) per RFC 8439 Sec 2.5
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param k See Usage.
#' @return The value of \code{r}, as built in the body.
#' @export
.secaead_clamp_key <- function(k) {
  # k: 32 integer bytes; clamp the low 16 (r) per RFC 8439 Sec 2.5
  r <- k[1:16]
  r[4] <- bitwAnd(r[4], 0x0f)
  r[8] <- bitwAnd(r[8], 0x0f)
  r[12] <- bitwAnd(r[12], 0x0f)
  r[16] <- bitwAnd(r[16], 0x0f)
  r[5] <- bitwAnd(r[5], 0xfc)
  r[9] <- bitwAnd(r[9], 0xfc)
  r[13] <- bitwAnd(r[13], 0xfc)
  r
}

#' The one-time authenticator over 2^130 - 5. key is 32 bytes: the
#'
#' low 16 become r (clamped) and the high 16 become s.
#'
#' @param message See Usage.
#' @param key See Usage.
#' @return The value of \code{.secaead_bytes_from_limbs}.
#' @export
morie_secaead_poly1305_mac <- function(message, key) {
  # The one-time authenticator over 2^130 - 5. key is 32 bytes: the
  # low 16 become r (clamped) and the high 16 become s.
  k <- .secaead_as_bytes(key)
  if (length(k) != 32L) {
    stop(sprintf("secaead: the Poly1305 key must be 32 bytes, got %d",
                 length(k)))
  }
  r_bytes <- .secaead_clamp_key(k)
  r <- .secaead_limbs_from_bytes(r_bytes)
  s <- .secaead_limbs_from_bytes(k[17:32])
  m <- .secaead_as_bytes(message)
  acc <- 0
  i <- 1L
  while (i <= length(m)) {
    blk <- m[i:min(i + 15L, length(m))]
    n <- .secaead_limbs_from_bytes(c(blk, 1L))  # append the high bit
    acc <- .secaead_p_add(if (length(acc) == 1L && acc[1] == 0) 0 else acc,
                          n)
    acc <- .secaead_p_mulmod(acc, r)
    i <- i + 16L
  }
  acc <- .secaead_p_final(acc)
  acc <- .secaead_p_add(acc, s)
  # final result mod 2^128: take the low 16 bytes
  .secaead_bytes_from_limbs(acc, 16L)
}

#' Block 0 gives the one-time key; the message starts at 1
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param key See Usage.
#' @param nonce See Usage.
#' @return The value of \code{[}.
#' @export
morie_secaead_poly1305_key_gen <- function(key, nonce) {
  # Block 0 gives the one-time key; the message starts at 1.
  morie_secaead_chacha20_block(key, 0, nonce)[1:32]
}

#' .secaead_pad16
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param b See Usage.
#' @return The value of \code{rep}.
#' @export
.secaead_pad16 <- function(b) {
  rep(0L, (16L - length(b) %% 16L) %% 16L)
}

#' .secaead_len8
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param n See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.secaead_len8 <- function(n) {
  out <- integer(8)
  v <- n
  for (i in seq_len(8)) {
    out[i] <- as.integer(v %% 256)
    v <- v %/% 256
  }
  out
}

#' .secaead_mac_data
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param aad See Usage.
#' @param ciphertext See Usage.
#' @return A vector, from \code{c}.
#' @export
.secaead_mac_data <- function(aad, ciphertext) {
  a <- .secaead_as_bytes(aad)
  c <- .secaead_as_bytes(ciphertext)
  c(a, .secaead_pad16(a), c, .secaead_pad16(c),
    .secaead_len8(length(a)), .secaead_len8(length(c)))
}

#' Encrypt from counter 1, then authenticate AAD and ciphertext
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param key See Usage.
#' @param nonce See Usage.
#' @param plaintext See Usage.
#' @param aad Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{ciphertext}, \code{ciphertext_hex}, \code{tag}, \code{tag_hex}, \code{onetime_key}, \code{aad_len}, \code{ct_len}, \code{method}, \code{note}.
#' @export
morie_secaead_aead_encrypt <- function(key, nonce, plaintext, aad=NULL) {
  # Encrypt from counter 1, then authenticate AAD and ciphertext.
  otk <- morie_secaead_poly1305_key_gen(key, nonce)
  ct <- morie_secaead_chacha20(key, 1, nonce, plaintext)
  tag <- morie_secaead_poly1305_mac(.secaead_mac_data(aad, ct), otk)
  list(
    estimate=.secaead_hexlify(ct), ciphertext=ct,
    ciphertext_hex=.secaead_hexlify(ct), tag=tag,
    tag_hex=.secaead_hexlify(tag), onetime_key=otk,
    aad_len=length(.secaead_as_bytes(aad)), ct_len=length(ct),
    method=paste0("AEAD_CHACHA20_POLY1305; Nir & Langley (2018) ",
                  "RFC 8439"),
    note=paste0("the length block is what keeps the AAD and ",
                "ciphertext boundary unambiguous")
  )
}

#' morie_secaead_aead_decrypt
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @param key See Usage.
#' @param nonce See Usage.
#' @param ciphertext See Usage.
#' @param tag See Usage.
#' @param aad Defaults to \code{NULL}.
#' @return A list with \code{valid}, \code{plaintext}, \code{expected_tag}.
#' @export
morie_secaead_aead_decrypt <- function(key, nonce, ciphertext, tag,
                                       aad=NULL) {
  # Verify FIRST, in constant time, and return nothing on failure.
  otk <- morie_secaead_poly1305_key_gen(key, nonce)
  want <- morie_secaead_poly1305_mac(.secaead_mac_data(aad, ciphertext),
                                     otk)
  if (!.secaead_constant_time_equal(want, tag)) {
    return(list(valid=FALSE, plaintext=NULL,
                note=paste0("tag mismatch: nothing is returned, because ",
                            "a caller given the plaintext anyway will ",
                            "use it")))
  }
  pt <- morie_secaead_chacha20(key, 1, nonce, ciphertext)
  list(valid=TRUE, plaintext=pt, expected_tag=want)
}

#' morie_secaead_cheatsheet
#'
#' Part of the secaead_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_secaead_cheatsheet <- function() {
  paste0(
    "secaead: a stream cipher alone lets an attacker flip a ",
    "plaintext bit by flipping a ciphertext bit, undetected. ",
    "ChaCha20 builds a 64-byte block from a 16-word state in ",
    "20 rounds and ADDS the original state back -- that ",
    "addition is what makes it non-invertible. Poly1305 is a ",
    "ONE-TIME authenticator mod 2^130 - 5 whose key must be ",
    "CLAMPED, and whose key is derived from ChaCha20 block 0 ",
    "with the message encrypted from block 1, because reusing ",
    "it across messages lets an attacker solve for r. The MAC ",
    "input is AAD, pad16, ciphertext, pad16, then both lengths ",
    "as 64-bit LE -- without the lengths the field boundary is ",
    "ambiguous. Decryption verifies BEFORE returning anything."
  )
}

# compact alias per ledger/NAMING.md
morie_secaead_chacha20poly1305 <- morie_secaead_aead_encrypt
# public names resolved by fn/_lazy_map.json
morie_secaead_aead_chacha20poly1305 <- morie_secaead_aead_encrypt

#' @export
morie_secaead <- morie_secaead_aead_encrypt
