# The 1.4.0 hybrid container: the wrapping key comes from the ML-KEM shared
# secret (1.3.x derived it from the ciphertext and the public key alone, so
# anyone with the file and the public key could open it), and the container
# and key files are the same bytes as morie's.

fixture <- function(f) testthat::test_path("fixtures", "crypto", f)
read_all <- function(f) readBin(f, "raw", file.info(f)$size)

test_that("the 1.4.0 container carries its marker and its key needs the KEM secret", {
  kp <- morie_crypto_hybrid_keygen()
  ct <- morie_crypto_hybrid_encrypt(charToRaw("payload"), kp$pk)
  expect_identical(ct[1:9], c(charToRaw("MORIEHYB"), as.raw(2L)))
  expect_identical(morie_crypto_hybrid_container_version(ct), 2L)
  expect_identical(morie_crypto_hybrid_container_version(ct[-(1:9)]), 1L)
  expect_identical(morie_crypto_hybrid_container_version("not raw"), 1L)
  # the 1.3.x attack: the public-data derivation does not open a 1.4.0 file
  body <- ct[-(1:9)]
  n <- readBin(body[1:4], "integer", size = 4L, endian = "big")
  kem <- body[5:(4 + n)]
  o <- 4L + n
  forged <- .morie_legacy_wrapping_key(kem, kp$pk)
  expect_error(morie_crypto_chacha20_poly1305_decrypt(forged, body[(o + 1L):(o + 12L)],
                                                      body[(o + 13L):(o + 60L)]))
  # the right key is the shared secret's
  ss <- morie_crypto_mlkem768_decaps(kp$sk, kem)
  wk <- .morie_wrapping_key(ss, kem, kp$pk)
  expect_length(morie_crypto_chacha20_poly1305_decrypt(wk, body[(o + 1L):(o + 12L)],
                                                       body[(o + 13L):(o + 60L)]), 32L)
})

test_that("a 1.3.x container still opens, with a warning to encrypt it again", {
  kp <- morie_crypto_hybrid_keygen()
  e <- morie_crypto_mlkem768_encaps(kp$pk)
  wk <- .morie_legacy_wrapping_key(e$ct, kp$pk)
  sym <- as.raw(1:32)
  wn <- as.raw(rep(7L, 12L))
  pn <- as.raw(rep(9L, 12L))
  w <- morie_crypto_chacha20_poly1305_encrypt(wk, wn, sym)
  p <- morie_crypto_chacha20_poly1305_encrypt(sym, pn, charToRaw("old file"))
  old <- c(writeBin(length(e$ct), raw(), size = 4L, endian = "big"), e$ct, wn, w$ct, w$tag, pn, p$ct, p$tag)
  expect_warning(pt <- morie_crypto_hybrid_decrypt(old, kp$sk), "encrypted by rmorie / morie 1.3.x")
  expect_identical(rawToChar(pt), "old file")
})

test_that("rmorie opens a container and key file written by morie (Python) 1.4.0", {
  sk <- read_all(fixture("py.moriesk"))
  expect_identical(sk[1:8], c(charToRaw("MORIESK"), as.raw(2L)))
  ct <- read_all(fixture("py_to_r.morieenc"))
  expect_identical(rawToChar(morie_crypto_hybrid_decrypt(ct, sk[-(1:8)])), "Python to R")
  # and encrypts to morie's public key file: decapsulation with morie's secret key opens it
  pk <- read_all(fixture("py.moriepk"))[-(1:8)]
  back <- morie_crypto_hybrid_encrypt(charToRaw("R to Python"), pk)
  expect_identical(rawToChar(morie_crypto_hybrid_decrypt(back, sk[-(1:8)])), "R to Python")
})

test_that("crypto CLI: marked key files, an owner-only secret key, no silent overwrite", {
  d <- withr::local_tempdir()
  f <- file.path(d, "toy.csv")
  writeLines(c("secret,value", "alpha,42"), f)
  run <- function(...) {
    rest <- c(...)
    flag <- function(name) {
      i <- match(name, rest)
      if (is.na(i) || i == length(rest)) NULL else rest[[i + 1L]]
    }
    txt <- character()
    st <- .cli_crypto(rest, flag, function(s) txt <<- c(txt, s))
    list(status = st, out = paste(txt, collapse = ""))
  }
  keys <- file.path(d, "keys")
  expect_equal(run("keygen", "--name", "k1", "--output", keys)$status, 0L)
  pkf <- file.path(keys, "k1.moriepk")
  skf <- file.path(keys, "k1.moriesk")
  expect_identical(read_all(pkf)[1:8], c(charToRaw("MORIEPK"), as.raw(2L)))
  expect_identical(read_all(skf)[1:8], c(charToRaw("MORIESK"), as.raw(2L)))
  if (.Platform$OS.type == "unix") expect_identical(format(file.mode(skf)), "600")
  r <- run("keygen", "--name", "k1", "--output", keys)
  expect_equal(r$status, 1L)
  expect_match(r$out, "already exists")
  expect_equal(run("keygen", "--name", ".bad", "--output", keys)$status, 2L)
  expect_equal(run("encrypt", f, "--to", pkf)$status, 0L)
  enc <- paste0(f, ".morieenc")
  expect_identical(morie_crypto_hybrid_container_version(read_all(enc)), 2L)
  expect_equal(run("encrypt", f, "--to", pkf)$status, 1L)  # the .morieenc exists
  expect_equal(run("encrypt", f, "--to", pkf, "--force")$status, 0L)
  r <- run("decrypt", enc, "--key", skf, "--out", file.path(d, "back.csv"))
  expect_equal(r$status, 0L)
  expect_identical(readLines(file.path(d, "back.csv")), c("secret,value", "alpha,42"))
  expect_equal(run("decrypt", enc, "--key", skf, "--out", file.path(d, "back.csv"))$status, 1L)
  # the wrong kind of key, a missing key file, a markerless (1.3.x) public key
  expect_match(run("encrypt", f, "--to", skf, "--out", file.path(d, "x"))$out, "is a secret key")
  expect_match(run("decrypt", enc, "--key", pkf, "--out", file.path(d, "x"))$out, "is a public key")
  expect_match(run("decrypt", enc, "--key", file.path(d, "none.moriesk"))$out, "no such key file")
  expect_match(run("encrypt", f, "--to", file.path(d, "none.moriepk"))$out, "no such public key file")
  old_pk <- file.path(d, "old.moriepk")
  writeBin(read_all(pkf)[-(1:8)], old_pk)
  expect_match(run("encrypt", f, "--to", old_pk, "--out", file.path(d, "y"))$out, "not FIPS 203")
  old_sk <- file.path(d, "old.moriesk")
  writeBin(read_all(skf)[-(1:8)], old_sk)
  expect_match(run("decrypt", enc, "--key", old_sk, "--out", file.path(d, "z"))$out, "did not make")
  # a wrong secret key: the decryption fails cleanly
  expect_equal(run("keygen", "--name", "k2", "--output", keys)$status, 0L)
  r <- run("decrypt", enc, "--key", file.path(keys, "k2.moriesk"), "--out", file.path(d, "w"))
  expect_equal(r$status, 1L)
  expect_match(r$out, "decrypt failed")
  # a morie (Python) key file and container through the CLI
  r <- run("decrypt", fixture("py_to_r.morieenc"), "--key", fixture("py.moriesk"), "--out", file.path(d, "py.txt"))
  expect_equal(r$status, 0L)
  expect_identical(rawToChar(read_all(file.path(d, "py.txt"))), "Python to R")
  expect_equal(run("bogus")$status, 2L)
})
