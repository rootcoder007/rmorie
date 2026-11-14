# SPDX-License-Identifier: AGPL-3.0-or-later
# The Parquet page codecs (src/snappy.c, src/gzip.c; src/zstd.c reads only, in
# test-zstd.R): every codec round-trips,
# rmoriedata's GZIP store reads, and real compression happens.

test_that("snappy compresses, round-trips, and handles runs and incompressible bytes", {
  comp <- .pq_snappy_compress
  dec <- .pq_snappy_decompress
  set.seed(1)
  text <- charToRaw(paste(rep("the quick brown fox jumps over the lazy dog. ", 2000),
                          collapse = ""))
  cases <- list(raw(0), as.raw(7), as.raw(rep(65, 70000)),
                as.raw(sample(0:255, 50000, TRUE)), text)
  for (x in cases) expect_identical(dec(comp(x)), x)
  expect_lt(length(comp(text)), length(text) / 10)
  expect_lt(length(comp(as.raw(rep(65, 70000)))), 5000)
  # a literal-only stream (what the writer produced before) still reads
  lit <- c(as.raw(c(5L, bitwShiftL(4L, 2L))), charToRaw("hello"))
  expect_identical(rawToChar(dec(lit)), "hello")
  expect_error(dec(as.raw(c(10L, 2L, 1L, 0L))), "copy offset|bad")
})

test_that("every codec writes a file that reads back identically", {
  df <- data.frame(a = c(1.5, NA, 3), b = c("x", "y", NA), c = c(TRUE, FALSE, TRUE),
                   d = c(1L, 2L, 3L), stringsAsFactors = FALSE)
  for (cmp in list("gzip", "snappy", NULL)) {
    f <- tempfile(fileext = ".parquet")
    morie_write_parquet(df, f, compression = cmp)
    expect_equal(morie_read_parquet(f), df, ignore_attr = TRUE)
  }
  # the writer does not write ZSTD; the reader reads it (test-zstd.R)
  expect_error(morie_write_parquet(df, tempfile(), compression = "zstd"),
               "gzip")
})

test_that("rmoriedata's shipped store reads with this reader as its CSV copy", {
  skip_if_not_installed("rmoriedata")
  p <- rmoriedata::mdpath("siu_directors_reports")
  d <- morie_read_parquet(p)
  expect_equal(d, rmoriedata::mdload("siu_directors_reports"), ignore_attr = TRUE)
  # the SIU table's pages hold ~10.6 MB of text; compressed it is ~2 MB
  expect_lt(file.size(p), 3e6)
})
