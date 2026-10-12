# R/aaa_parquet.R: every column type the writer infers round-trips through the reader, with
# and without Snappy; the Snappy codec on literal and repeated runs; the reader's refusals.

.pqt <- function(name) get(name, envir = environment(morie_read_parquet))

.pqt_frame <- function() {
  data.frame(
    flag = c(TRUE, NA, FALSE, TRUE),
    count = c(1L, NA, -7L, 2147483647L),
    value = c(0.5, NA, -1e300, Inf),
    label = c("plain", NA, "", "café — 中"),
    day = as.Date(c("2024-01-02", NA, "1970-01-01", "1969-12-31")),
    when = as.POSIXct(c("2024-01-02 03:04:05.25", NA, "1970-01-01 00:00:00", "2000-02-29 12:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE)
}

test_that("every inferred column type round-trips, Snappy or not", {
  df <- .pqt_frame()
  for (comp in list("snappy", NULL)) {
    p <- withr::local_tempfile(fileext = ".parquet")
    morie_write_parquet(df, p, compression = comp)
    back <- morie_read_parquet(p)
    expect_equal(names(back), names(df))
    expect_equal(back$flag, df$flag)
    expect_equal(back$count, df$count)
    expect_equal(back$value, df$value)
    expect_equal(back$label, df$label)
    expect_equal(back$day, df$day)
    expect_s3_class(back$when, "POSIXct")
    expect_equal(as.numeric(back$when), as.numeric(df$when))
    expect_equal(attr(back$when, "tzone"), "UTC")
    expect_equal(nrow(back), 4L)
    sel <- morie_read_parquet(p, columns = c("label", "count"))
    expect_equal(names(sel), c("label", "count"))
    expect_equal(sel$count, df$count)
    expect_error(morie_read_parquet(p, columns = c("count", "nope", "zz")), "no such column\\(s\\) in .*: nope, zz")
  }
  expect_error(morie_read_parquet(file.path(tempdir(), "missing.parquet")), "no such file")
  expect_error(morie_write_parquet(df, tempfile(), compression = "gzip"), "compression must be \"snappy\" or NULL; got .gzip.")
  # a frame of one row and one of many rows, all NA
  one <- data.frame(x = 3.5)
  p1 <- withr::local_tempfile(fileext = ".parquet")
  morie_write_parquet(one, p1)
  expect_equal(morie_read_parquet(p1), one)
  nas <- data.frame(x = rep(NA_integer_, 300), y = rep(NA_character_, 300), stringsAsFactors = FALSE)
  p2 <- withr::local_tempfile(fileext = ".parquet")
  morie_write_parquet(nas, p2)
  expect_equal(morie_read_parquet(p2), nas)
  long <- data.frame(i = seq_len(5000), s = sprintf("row-%05d", seq_len(5000)), d = sin(seq_len(5000)),
                     stringsAsFactors = FALSE)
  p3 <- withr::local_tempfile(fileext = ".parquet")
  morie_write_parquet(long, p3)
  expect_equal(morie_read_parquet(p3), long)
})

test_that("the Snappy codec restores literal bytes and repeated runs", {
  comp <- .pqt(".pq_snappy_compress")
  decomp <- .pqt(".pq_snappy_decompress")
  set.seed(5)
  literal <- as.raw(sample(0:255, 3000, TRUE))
  expect_equal(decomp(comp(literal)), literal)
  runs <- as.raw(rep(c(1, 2, 3, 4), 2000))
  z <- comp(runs)
  expect_equal(decomp(z), runs)
  mixed <- c(literal[1:100], runs[1:900], literal[101:400], as.raw(rep(7, 70000)))
  expect_equal(decomp(comp(mixed)), mixed)
  expect_equal(decomp(comp(raw(0))), raw(0))
  expect_equal(decomp(comp(as.raw(9))), as.raw(9))
  bw <- .pqt(".pq_bit_width")
  expect_equal(vapply(c(0, 1, 2, 3, 4, 7, 8, 255, 256, 1023), bw, numeric(1)), c(0, 1, 2, 2, 3, 3, 4, 8, 9, 10))
})
