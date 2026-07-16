# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 22 cross-validation: native hashes vs digest / openssl.
library(testthat)
library(rmorie)

test_that("native SHA-256 hex equals digest::digest on random strings", {
  skip_if_not_installed("digest")
  set.seed(220)
  for (i in 1:20) {
    s <- paste(sample(c(letters, LETTERS, 0:9), sample(1:200, 1),
                      replace = TRUE), collapse = "")
    expect_equal(rmorie:::.rmorie_sha256_hex_impl(s),
                 digest::digest(s, algo = "sha256", serialize = FALSE))
  }
})

test_that("native HMAC equals openssl::sha256(key=) on random inputs", {
  skip_if_not_installed("openssl")
  set.seed(221)
  for (i in 1:10) {
    key <- as.raw(sample(0:255, sample(1:100, 1), replace = TRUE))
    msg <- as.raw(sample(0:255, sample(1:500, 1), replace = TRUE))
    mine <- rmorie:::.rmorie_hmac_sha256_impl(key, msg)
    ref <- as.raw(openssl::sha256(msg, key = key))
    expect_identical(mine, ref)
  }
})
