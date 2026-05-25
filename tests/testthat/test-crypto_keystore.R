# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/crypto_keystore.R -- create/store/load roundtrip.

set.seed(1)

.tmp_keystore <- function() {
  td <- file.path(tempdir(), paste0("morie-ks-", as.integer(Sys.time())))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  file.path(td, "keystore.json")
}

test_that("hex<->raw roundtrip works", {
  set.seed(1)
  expect_equal(length(morie:::.morie_hex_to_raw("")), 0L)
  r <- as.raw(c(0x00, 0x10, 0xff))
  h <- morie:::.morie_raw_to_hex(r)
  expect_equal(nchar(h), 6L)
  expect_identical(morie:::.morie_hex_to_raw(h), r)
})

test_that("hex_to_raw rejects bad input", {
  set.seed(1)
  expect_error(morie:::.morie_hex_to_raw("abc"), "odd length")
  expect_error(morie:::.morie_hex_to_raw(c("aa", "bb")), "single")
})

test_that("raw_to_hex rejects non-raw", {
  set.seed(1)
  expect_error(morie:::.morie_raw_to_hex("abc"), "raw")
})

test_that("resolve_path expands ~", {
  set.seed(1)
  out <- morie:::.morie_resolve_path("~/foo")
  expect_type(out, "character")
  expect_false(startsWith(out, "~"))
})

test_that("read_store errors on missing keystore", {
  set.seed(1)
  expect_error(morie:::.morie_read_store(tempfile()), "not found")
})

test_that("create -> store -> load roundtrip recovers sk", {
  set.seed(1)
  path <- .tmp_keystore()
  on.exit(unlink(path, force = TRUE))

  pwd <- "correct horse battery staple"
  morie_crypto_keystore_create(pwd, path = path)
  expect_true(file.exists(path))

  pk <- as.raw(sample(0:255, 32, replace = TRUE))
  sk <- as.raw(sample(0:255, 64, replace = TRUE))
  morie_crypto_keystore_store("alice", pk = pk, sk = sk, password = pwd, path = path)

  out <- morie_crypto_keystore_load("alice", password = pwd, path = path)
  expect_true(is.raw(out$pk))
  expect_true(is.raw(out$sk))
  expect_identical(out$pk, pk)
  expect_identical(out$sk, sk)
})

test_that("keystore_list returns stored key names", {
  set.seed(1)
  path <- .tmp_keystore()
  on.exit(unlink(path, force = TRUE))
  pwd <- "pw"
  morie_crypto_keystore_create(pwd, path = path)
  morie_crypto_keystore_store("k1", as.raw(1:4), as.raw(5:8), pwd, path = path)
  morie_crypto_keystore_store("k2", as.raw(1:4), as.raw(5:8), pwd, path = path)
  out <- morie_crypto_keystore_list(pwd, path = path)
  expect_setequal(out, c("k1", "k2"))
})

test_that("create rejects existing keystore", {
  set.seed(1)
  path <- .tmp_keystore()
  on.exit(unlink(path, force = TRUE))
  morie_crypto_keystore_create("pw", path = path)
  expect_error(morie_crypto_keystore_create("pw", path = path), "already exists")
})

test_that("store rejects non-raw pk/sk + non-string name", {
  set.seed(1)
  path <- .tmp_keystore()
  on.exit(unlink(path, force = TRUE))
  morie_crypto_keystore_create("pw", path = path)
  expect_error(morie_crypto_keystore_store("x", "notraw", as.raw(1:4), "pw", path = path), "raw")
  expect_error(morie_crypto_keystore_store(c("a", "b"), as.raw(1:4), as.raw(1:4), "pw", path = path), "single")
})

test_that("load errors when key not present", {
  set.seed(1)
  path <- .tmp_keystore()
  on.exit(unlink(path, force = TRUE))
  morie_crypto_keystore_create("pw", path = path)
  expect_error(morie_crypto_keystore_load("absent", password = "pw", path = path), "not found")
})

test_that("load with wrong password fails clean", {
  set.seed(1)
  path <- .tmp_keystore()
  on.exit(unlink(path, force = TRUE))
  morie_crypto_keystore_create("right", path = path)
  morie_crypto_keystore_store("k1", as.raw(1:4), as.raw(5:8), "right", path = path)
  expect_error(morie_crypto_keystore_load("k1", password = "wrong", path = path), "decrypt")
})

test_that("derive_key requires raw salt + single-string password", {
  set.seed(1)
  expect_error(morie:::.morie_derive_key("pw", "notraw"), "raw")
  expect_error(morie:::.morie_derive_key(c("a", "b"), as.raw(1:4)), "single")
})