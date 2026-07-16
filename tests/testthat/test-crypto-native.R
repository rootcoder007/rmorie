# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 22 — native SHA-256 / HMAC / PBKDF2 against published vectors.
#' @srrstats {G5.4} NIST FIPS 180-4, RFC 4231 and RFC 6070/7914-style
#'   published test vectors pin the implementations bit-for-bit.

test_that("SHA-256 matches NIST FIPS 180-4 vectors", {
  expect_equal(rmorie:::.rmorie_sha256_hex_impl("abc"),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  expect_equal(rmorie:::.rmorie_sha256_hex_impl(""),
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  expect_equal(rmorie:::.rmorie_sha256_hex_impl(
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  # long input exercises multi-block + length padding
  expect_equal(rmorie:::.rmorie_sha256_hex_impl(
    paste(rep("a", 1e6), collapse = "")),
    "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
})

test_that("HMAC-SHA256 matches RFC 4231 vectors", {
  # Test case 1
  key <- as.raw(rep(0x0b, 20))
  out <- rmorie:::.rmorie_hmac_sha256_impl(key, "Hi There")
  expect_equal(paste(format(out), collapse = ""),
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
  # Test case 2 (key shorter than block)
  out2 <- rmorie:::.rmorie_hmac_sha256_impl("Jefe",
    "what do ya want for nothing?")
  expect_equal(paste(format(out2), collapse = ""),
    "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
  # Test case 6 (key longer than block: 131 bytes of 0xaa)
  key6 <- as.raw(rep(0xaa, 131))
  out6 <- rmorie:::.rmorie_hmac_sha256_impl(key6,
    "Test Using Larger Than Block-Size Key - Hash Key First")
  expect_equal(paste(format(out6), collapse = ""),
    "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54")
})

test_that("PBKDF2-HMAC-SHA256 matches published vectors", {
  # RFC 7914 (scrypt) appendix uses PBKDF2-SHA256 vectors:
  out <- rmorie:::.rmorie_pbkdf2_sha256_impl("passwd", "salt", 1L, 64L)
  expect_equal(substr(paste(format(out), collapse = ""), 1, 32),
               "55ac046e56e3089fec1691c22544b605")
  out2 <- rmorie:::.rmorie_pbkdf2_sha256_impl("Password", "NaCl",
                                              80000L, 64L)
  expect_equal(substr(paste(format(out2), collapse = ""), 1, 32),
               "4ddcd8f60b98be21830cee5ef22701f9")
  expect_error(rmorie:::.rmorie_pbkdf2_sha256_impl("p", "s", 0L, 32L),
               "iterations")
})

test_that("native HKDF path keeps hybrid wrap Python-compatible", {
  # RFC 5869 test case 1 through the R-level HKDF helper
  ikm <- as.raw(rep(0x0b, 22))
  salt <- as.raw(0:12)
  info <- as.raw(c(0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
                   0xf8, 0xf9))
  okm <- rmorie:::.morie_hkdf_sha256(ikm, len = 42L, salt = salt,
                                     info = info)
  expect_equal(paste(format(okm), collapse = ""),
    paste0("3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf",
           "34007208d5b887185865"))
})
