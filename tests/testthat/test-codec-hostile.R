# SPDX-License-Identifier: AGPL-3.0-or-later
# Malformed input for the hand-written decoders: every input either decodes or raises an R
# error, never crashes, hangs or reads out of bounds. The sanitizer job runs this file
# under ASAN + UBSAN, which turns any out-of-bounds read into a failure.

snappy_dec <- function(x) .Call(C_rmd_snappy_decompress, x)
gzip_dec <- function(x, n) .Call(C_rmd_gzip_decompress, x, n)
flip <- function(y, mask) {
  j <- sample.int(length(y), 1L)
  y[j] <- xor(y[j], as.raw(mask))
  y
}

test_that("snappy rejects truncated, flipped and random inputs with an R error", {
  plain <- charToRaw(strrep("the quick brown fox jumps over the lazy dog ", 40))
  good <- .Call(C_rmd_snappy_compress, plain)
  expect_identical(snappy_dec(good), plain)
  rng <- function(n) as.raw(sample.int(256L, n, replace = TRUE) - 1L)
  outcomes <- character()
  for (i in seq_len(1500L)) {
    x <- switch(i %% 3L + 1L,
      good[seq_len(sample.int(length(good), 1L))],             # truncation
      flip(good, 1L),                                          # bit flip
      rng(sample.int(64L, 1L))                                  # noise
    )
    r <- tryCatch(
      {
        snappy_dec(x)
        "ok"
      },
      error = function(e) "error"
    )
    outcomes <- c(outcomes, r)
  }
  expect_true(all(outcomes %in% c("ok", "error")))
  expect_true(any(outcomes == "error"))
  expect_error(snappy_dec(as.raw(c(0xff, 0xff, 0xff, 0xff, 0x0f))), "expected")
  expect_error(snappy_dec(raw(0)), "length header")
  # a header claiming terabytes: the allocation itself aborted CRAN gcc-ASAN (2026-10-22)
  terabytes <- as.raw(c(0x88, 0x96, 0xbd, 0xf7, 0xd0, 0x05, 0x01, 0x02))
  expect_error(snappy_dec(terabytes), "length header")
  four_gib <- as.raw(c(0xff, 0xff, 0xff, 0xff, 0x0f, 0x00, 0x41))
  expect_error(snappy_dec(four_gib), "can hold")
  expect_error(snappy_dec(as.raw(c(0x80, 0x80, 0x04, 0x00, 0x41))), "can hold")
})

test_that("gzip refuses a stated size its input cannot hold, before allocating", {
  gz <- .Call(C_rmd_gzip_compress, charToRaw("abc"))
  expect_identical(.Call(C_rmd_gzip_decompress, gz, 3), charToRaw("abc"))
  expect_error(.Call(C_rmd_gzip_decompress, gz, 2^31), "can hold")
  expect_error(.Call(C_rmd_gzip_decompress, gz, 2^40), "can hold")
  expect_error(.Call(C_rmd_gzip_decompress, gz, -1), "bad uncompressed size")
})

test_that("gzip rejects truncated, flipped and random inputs with an R error", {
  plain <- charToRaw(strrep("rmoriedata ", 300))
  good <- .Call(C_rmd_gzip_compress, plain)
  expect_identical(gzip_dec(good, length(plain)), plain)
  for (i in seq_len(600L)) {
    x <- if (i %% 2L) good[seq_len(sample.int(length(good), 1L))] else flip(good, 4L)
    r <- tryCatch(
      {
        gzip_dec(x, length(plain))
        "ok"
      },
      error = function(e) "error"
    )
    expect_true(r %in% c("ok", "error"))
  }
  # a size the stream does not deliver is an error, not a padded result
  expect_error(gzip_dec(good, length(plain) + 10L))
})

test_that("the page CRC helper is zlib's crc32", {
  expect_identical(.Call(C_rmd_crc32, charToRaw("123456789")), 3421780262)
  expect_identical(.Call(C_rmd_crc32, raw(0)), 0)
})
