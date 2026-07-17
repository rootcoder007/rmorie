# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/crypto_hybrid.R -- HKDF + not-yet-implemented stubs.

set.seed(1)

test_that("hkdf_sha256 returns the requested length raw vector", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  out <- morie_crypto_hkdf_sha256("seed", len = 32L, salt = "salt", info = "ctx")
  expect_true(is.raw(out))
  expect_equal(length(out), 32L)
})

test_that("hkdf_sha256 is deterministic on identical inputs", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  a <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx")
  b <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx")
  expect_identical(a, b)
})

test_that("hkdf_sha256 changes with different info or salt", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  base <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx")
  diff_info <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx2")
  diff_salt <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt2", info = "ctx")
  expect_false(identical(base, diff_info))
  expect_false(identical(base, diff_salt))
})

test_that("hkdf_sha256 supports raw inputs", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  out <- morie_crypto_hkdf_sha256(charToRaw("hi"), len = 8L,
                                  salt = charToRaw("salt"), info = charToRaw("ctx"))
  expect_equal(length(out), 8L)
})

test_that("hkdf_sha256 rejects bad lengths", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  expect_error(morie_crypto_hkdf_sha256("x", len = 0L), "length")
  expect_error(morie_crypto_hkdf_sha256("x", len = 1e6L), "length")
})

test_that("hkdf_sha256 defaults salt to zeros and runs", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  out <- morie_crypto_hkdf_sha256("x")
  expect_equal(length(out), 32L)
})

test_that("hybrid keygen/encrypt/decrypt round-trips", {
  skip_if_not(morie_crypto_liboqs_available(), "no liboqs")
  kp <- morie_crypto_hybrid_keygen()
  expect_equal(length(kp$pk), 1184L)
  expect_equal(length(kp$sk), 2400L)
  pt <- charToRaw("the quick brown fox jumps over the lazy dog")
  ct <- morie_crypto_hybrid_encrypt(pt, kp$pk)
  expect_true(is.raw(ct))
  # container: 4 + 1088 + 12 + 32 + 16 + 12 + len(pt) + 16
  expect_equal(length(ct), 1180L + length(pt))
  expect_identical(morie_crypto_hybrid_decrypt(ct, kp$sk), pt)
  # string input accepted
  ct2 <- morie_crypto_hybrid_encrypt("hi", kp$pk)
  expect_identical(rawToChar(morie_crypto_hybrid_decrypt(ct2, kp$sk)), "hi")
})

test_that("hybrid decrypt rejects tampering and wrong keys", {
  skip_if_not(morie_crypto_liboqs_available(), "no liboqs")
  kp <- morie_crypto_hybrid_keygen()
  ct <- morie_crypto_hybrid_encrypt(charToRaw("payload"), kp$pk)
  bad <- ct
  bad[length(bad) - 1L] <- xor(bad[length(bad) - 1L], as.raw(1L))
  expect_error(morie_crypto_hybrid_decrypt(bad, kp$sk))
  kp2 <- morie_crypto_hybrid_keygen()
  expect_error(morie_crypto_hybrid_decrypt(ct, kp2$sk))
})

test_that("hybrid encrypt/decrypt validate inputs", {
  expect_error(morie_crypto_hybrid_encrypt(list(), as.raw(1:4)), "raw")
  expect_error(morie_crypto_hybrid_decrypt("notraw", as.raw(1:4)), "raw")
  expect_error(morie_crypto_hybrid_decrypt(as.raw(1:2), as.raw(1:4)), "short")
  expect_error(morie_crypto_hybrid_decrypt(raw(2000), raw(100)), "short")
})

test_that(".morie_wrapping_key produces 32 bytes", {
  skip_if_not_installed("sodium")
  skip_if_not(morie_crypto_sodium_available(), "no libsodium")
  set.seed(1)
  out <- rmorie:::.morie_wrapping_key(as.raw(1:8), as.raw(9:16))
  expect_equal(length(out), 32L)
})