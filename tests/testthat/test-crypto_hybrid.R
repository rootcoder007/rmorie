# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/crypto_hybrid.R -- HKDF + not-yet-implemented stubs.

set.seed(1)

test_that("hkdf_sha256 returns the requested length raw vector", {
  set.seed(1)
  out <- morie_crypto_hkdf_sha256("seed", len = 32L, salt = "salt", info = "ctx")
  expect_true(is.raw(out))
  expect_equal(length(out), 32L)
})

test_that("hkdf_sha256 is deterministic on identical inputs", {
  set.seed(1)
  a <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx")
  b <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx")
  expect_identical(a, b)
})

test_that("hkdf_sha256 changes with different info or salt", {
  set.seed(1)
  base <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx")
  diff_info <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt", info = "ctx2")
  diff_salt <- morie_crypto_hkdf_sha256("ikm", len = 16L, salt = "salt2", info = "ctx")
  expect_false(identical(base, diff_info))
  expect_false(identical(base, diff_salt))
})

test_that("hkdf_sha256 supports raw inputs", {
  set.seed(1)
  out <- morie_crypto_hkdf_sha256(charToRaw("hi"), len = 8L,
                                  salt = charToRaw("salt"), info = charToRaw("ctx"))
  expect_equal(length(out), 8L)
})

test_that("hkdf_sha256 rejects bad lengths", {
  set.seed(1)
  expect_error(morie_crypto_hkdf_sha256("x", len = 0L), "length")
  expect_error(morie_crypto_hkdf_sha256("x", len = 1e6L), "length")
})

test_that("hkdf_sha256 defaults salt to zeros and runs", {
  set.seed(1)
  out <- morie_crypto_hkdf_sha256("x")
  expect_equal(length(out), 32L)
})

test_that("hybrid_keygen surfaces not-implemented", {
  set.seed(1)
  expect_error(morie_crypto_hybrid_keygen(), "not implemented")
})

test_that("hybrid_encrypt validates inputs then surfaces not-implemented", {
  set.seed(1)
  expect_error(morie_crypto_hybrid_encrypt("hi", "notraw"), "raw")
  expect_error(morie_crypto_hybrid_encrypt("hi", as.raw(1:4)), "not implemented")
})

test_that("hybrid_decrypt validates inputs then surfaces not-implemented", {
  set.seed(1)
  expect_error(morie_crypto_hybrid_decrypt("notraw", as.raw(1:4)), "raw")
  expect_error(morie_crypto_hybrid_decrypt(as.raw(1:4), as.raw(1:4)), "not implemented")
})

test_that(".morie_wrapping_key produces 32 bytes", {
  set.seed(1)
  out <- morie:::.morie_wrapping_key(as.raw(1:8), as.raw(9:16))
  expect_equal(length(out), 32L)
})