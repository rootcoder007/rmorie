# AEAD_CHACHA20_POLY1305 against the RFC 8439 test vectors (Secs. 2.3.2,
# 2.4.2, 2.5.2, 2.6.2, 2.8.2), with Poly1305 also recomputed in exact
# big-integer arithmetic from its definition.

se_hex <- function(s) {
  s <- gsub("[^0-9a-f]", "", tolower(s))
  strtoi(substring(s, seq(1, nchar(s), 2), seq(2, nchar(s), 2)), 16L)
}
se_key <- 0:31
se_key80 <- 0x80:0x9f

test_that("the ChaCha20 block function matches RFC 8439 Sec. 2.3.2", {
  blk <- morie_secaead_chacha20_block(se_key, 1, se_hex("000000090000004a00000000"))
  ref <- se_hex("10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4e
                 d2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e")
  expect_identical(blk, ref)
  expect_error(morie_secaead_chacha20_block(1:5, 0, 1:12), "32 bytes")
  expect_error(morie_secaead_chacha20_block(se_key, 0, 1:8), "12 bytes")
})

test_that("ChaCha20 encryption matches RFC 8439 Sec. 2.4.2 and is an involution", {
  pt <- paste0("Ladies and Gentlemen of the class of '99: If I could offer you ",
               "only one tip for the future, sunscreen would be it.")
  nonce <- se_hex("000000000000004a00000000")
  ct <- morie_secaead_chacha20(se_key, 1, nonce, pt)
  ref_head <- se_hex("6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0b")
  expect_identical(ct[1:32], ref_head)
  expect_length(ct, nchar(pt))
  expect_identical(morie_secaead_chacha20(se_key, 1, nonce, ct),
                   as.integer(charToRaw(pt)))
  # block i of the stream is the block function at counter + i
  expect_identical(bitwXor(ct[65:114], as.integer(charToRaw(pt))[65:114]),
                   morie_secaead_chacha20_block(se_key, 2, nonce)[1:50])
})

se_poly_ref <- function(msg, key) {
  r <- key[1:16]
  r[c(4, 8, 12, 16)] <- bitwAnd(r[c(4, 8, 12, 16)], 15L)
  r[c(5, 9, 13)] <- bitwAnd(r[c(5, 9, 13)], 252L)
  le <- function(b) {
    v <- gmp::as.bigz(0)
    for (i in rev(seq_along(b))) v <- v * 256 + b[i]
    v
  }
  p <- gmp::as.bigz(2)^130 - 5
  rr <- le(r)
  acc <- gmp::as.bigz(0)
  for (i in seq(1, length(msg), by = 16)) {
    blk <- msg[i:min(i + 15, length(msg))]
    acc <- ((acc + le(c(blk, 1L))) * rr) %% p
  }
  tag <- (acc + le(key[17:32])) %% (gmp::as.bigz(2)^128)
  out <- integer(16)
  for (i in 1:16) {
    out[i] <- as.integer(tag %% 256)
    tag <- tag %/% 256
  }
  out
}

test_that("Poly1305 matches RFC 8439 Sec. 2.5.2 and the big-integer definition", {
  key <- se_hex("85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b")
  msg <- "Cryptographic Forum Research Group"
  tag <- morie_secaead_poly1305_mac(msg, key)
  expect_identical(tag, se_hex("a8061dc1305136c6c22b8baf0c0127a9"))
  skip_if_not_installed("gmp")
  expect_identical(tag, se_poly_ref(as.integer(charToRaw(msg)), key))
  set.seed(8)
  for (len in c(1, 15, 16, 17, 47, 64)) {
    m <- sample(0:255, len, replace = TRUE)
    k <- sample(0:255, 32, replace = TRUE)
    expect_identical(morie_secaead_poly1305_mac(m, k), se_poly_ref(m, k))
  }
  expect_error(morie_secaead_poly1305_mac(msg, 1:16), "32 bytes")
})

test_that("the one-time key is ChaCha20 block 0 (RFC 8439 Sec. 2.6.2)", {
  nonce <- se_hex("000000000001020304050607")
  otk <- morie_secaead_poly1305_key_gen(se_key80, nonce)
  expect_identical(otk, se_hex("8ad5a08b905f81cc815040274ab29471a833b637e3fd0da508dbb8e2fdd1a646"))
  expect_identical(otk, morie_secaead_chacha20_block(se_key80, 0, nonce)[1:32])
})

test_that("AEAD encrypt/decrypt match RFC 8439 Sec. 2.8.2 and reject tampering", {
  pt <- paste0("Ladies and Gentlemen of the class of '99: If I could offer you ",
               "only one tip for the future, sunscreen would be it.")
  aad <- se_hex("50515253c0c1c2c3c4c5c6c7")
  nonce <- se_hex("070000004041424344454647")
  r <- morie_secaead_aead_encrypt(se_key80, nonce, pt, aad)
  expect_identical(r$tag, se_hex("1ae10b594f09e26a7e902ecbd0600691"))
  expect_identical(r$ciphertext[1:16], se_hex("d31a8d34648e60db7b86afbc53ef7ec2"))
  expect_identical(r$ciphertext, morie_secaead_chacha20(se_key80, 1, nonce, pt))
  # the MAC input: aad | pad16 | ct | pad16 | len(aad) | len(ct), 64-bit LE
  md <- c(aad, rep(0L, 4), r$ciphertext, rep(0L, (16 - length(r$ciphertext) %% 16) %% 16),
          12L, rep(0L, 7), length(r$ciphertext), rep(0L, 7))
  expect_identical(r$tag, morie_secaead_poly1305_mac(md, r$onetime_key))
  d <- morie_secaead_aead_decrypt(se_key80, nonce, r$ciphertext, r$tag, aad)
  expect_true(d$valid)
  expect_identical(d$plaintext, as.integer(charToRaw(pt)))
  bad <- r$ciphertext
  bad[5] <- bitwXor(bad[5], 1L)
  expect_false(morie_secaead_aead_decrypt(se_key80, nonce, bad, r$tag, aad)$valid)
  expect_null(morie_secaead_aead_decrypt(se_key80, nonce, bad, r$tag, aad)$plaintext)
  expect_false(morie_secaead_aead_decrypt(se_key80, nonce, r$ciphertext, r$tag)$valid)
  expect_false(morie_secaead_aead_decrypt(se_key80, nonce, r$ciphertext, r$tag[1:15], aad)$valid)
  expect_identical(morie_secaead(se_key80, nonce, pt, aad)$tag_hex, r$tag_hex)
  expect_match(morie_secaead_cheatsheet(), "CLAMPED", fixed = TRUE)
})
