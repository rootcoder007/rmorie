# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3JJJ1: libsodium-backed symmetric crypto.

test_that("libsodium availability flag is exposed", {
  expect_type(morie_crypto_sodium_available(), "logical")
  expect_length(morie_crypto_sodium_available(), 1L)
})

test_that("libsodium version string is non-empty when available", {
  if (!morie_crypto_sodium_available()) skip("no libsodium in this build")
  v <- morie_crypto_sodium_version()
  expect_type(v, "character")
  expect_true(nzchar(v))
  # Should match semver-ish.
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+")
})

test_that("morie_crypto_random_bytes returns the requested length", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  for (n in c(1L, 16L, 32L, 1024L)) {
    r <- morie_crypto_random_bytes(n)
    expect_type(r, "raw")
    expect_equal(length(r), n)
  }
})

test_that("ChaCha20-Poly1305 round-trips through libsodium", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  set.seed(42)
  k <- morie_crypto_random_bytes(32)
  n <- morie_crypto_random_bytes(12)
  pt <- charToRaw("the quick brown fox jumps over the lazy dog")
  aad <- charToRaw("hdr:v1")
  enc <- morie_crypto_chacha20_poly1305_encrypt(k, n, pt, aad)
  expect_equal(length(enc$ct), length(pt))
  expect_equal(length(enc$tag), 16L)
  dec <- morie_crypto_chacha20_poly1305_decrypt(k, n, c(enc$ct, enc$tag), aad)
  expect_equal(dec, pt)
})

test_that("ChaCha20-Poly1305 detects tag forgery", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  k <- morie_crypto_random_bytes(32)
  n <- morie_crypto_random_bytes(12)
  pt <- charToRaw("secret")
  enc <- morie_crypto_chacha20_poly1305_encrypt(k, n, pt)
  bad <- c(enc$ct, enc$tag)
  bad[length(bad)] <- as.raw(0L)  # flip the last tag byte
  expect_error(
    morie_crypto_chacha20_poly1305_decrypt(k, n, bad),
    regexp = "decrypt failed")
})

test_that("ChaCha20-Poly1305 detects key mismatch", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  k1 <- morie_crypto_random_bytes(32)
  k2 <- morie_crypto_random_bytes(32)
  n <- morie_crypto_random_bytes(12)
  enc <- morie_crypto_chacha20_poly1305_encrypt(k1, n, charToRaw("x"))
  expect_error(
    morie_crypto_chacha20_poly1305_decrypt(k2, n, c(enc$ct, enc$tag)))
})

test_that("ChaCha20-Poly1305 enforces key + nonce sizes", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  expect_error(
    morie_crypto_chacha20_poly1305_encrypt(
      raw(31), raw(12), charToRaw("x")),
    regexp = "key must be 32 bytes")
  expect_error(
    morie_crypto_chacha20_poly1305_encrypt(
      raw(32), raw(13), charToRaw("x")),
    regexp = "nonce must be 12 bytes")
})

test_that("HKDF-SHA256 matches the RFC 5869 §A.1 test vector", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  # RFC 5869 Test Case 1 (SHA-256, basic test).
  ikm  <- as.raw(c(rep(0x0b, 22)))
  salt <- as.raw(c(0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,
                    0x08,0x09,0x0a,0x0b,0x0c))
  info <- as.raw(c(0xf0,0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,
                    0xf8,0xf9))
  expected <- as.raw(c(
    0x3c,0xb2,0x5f,0x25,0xfa,0xac,0xd5,0x7a,0x90,0x43,
    0x4f,0x64,0xd0,0x36,0x2f,0x2a,0x2d,0x2d,0x0a,0x90,
    0xcf,0x1a,0x5a,0x4c,0x5d,0xb0,0x2d,0x56,0xec,0xc4,
    0xc5,0xbf,0x34,0x00,0x72,0x08,0xd5,0xb8,0x87,0x18,
    0x58,0x65))
  okm <- morie_crypto_hkdf_sha256(ikm, length = 42L,
                                     salt = salt, info = info)
  expect_equal(okm, expected)
})

test_that("HKDF-SHA256 matches the Python morie default (zero-salt 32B output)", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  ikm <- charToRaw("morie-test-ikm")
  out <- morie_crypto_hkdf_sha256(ikm)
  expect_type(out, "raw")
  expect_equal(length(out), 32L)
  # Determinism: two calls give the same output.
  out2 <- morie_crypto_hkdf_sha256(ikm)
  expect_equal(out, out2)
})

test_that("HKDF-SHA256 length clamping: rejects > 255 * 32", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  expect_error(
    morie_crypto_hkdf_sha256(charToRaw("x"), length = 255L * 32L + 1L),
    regexp = "length must be in")
})

test_that("HKDF-SHA256 short-length truncation works", {
  if (!morie_crypto_sodium_available()) skip("no libsodium")
  out16 <- morie_crypto_hkdf_sha256(charToRaw("x"), length = 16L)
  expect_equal(length(out16), 16L)
  out64 <- morie_crypto_hkdf_sha256(charToRaw("x"), length = 64L)
  expect_equal(length(out64), 64L)
  # First 16 bytes of the 64-byte expansion match the 16-byte output.
  expect_equal(out64[1:16], out16)
})
