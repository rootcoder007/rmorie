# SPDX-License-Identifier: AGPL-3.0-or-later
# Non-ASCII text must round-trip through the Parquet codec byte for byte
# in every locale, including C, where enc2utf8() on an unmarked string
# would replace each byte with its "<c3><a9>" display form.

utf8_bytes <- as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9, 0x20, 0xe4, 0xb8, 0xad,
                       0xe6, 0x96, 0x87)) # "café 中文"

test_that("the writer keeps UTF-8 bytes whether or not the string is marked", {
  marked <- rawToChar(utf8_bytes)
  Encoding(marked) <- "UTF-8"
  unmarked <- rawToChar(utf8_bytes) # Encoding "unknown", as read.csv gives
  f <- tempfile(fileext = ".parquet")
  morie_write_parquet(data.frame(a = c(marked, unmarked, NA),
                                              stringsAsFactors = FALSE), f)
  back <- morie_read_parquet(f)$a
  expect_identical(charToRaw(back[1]), utf8_bytes)
  expect_identical(charToRaw(back[2]), utf8_bytes)
  expect_true(is.na(back[3]))
  expect_identical(Encoding(back[1:2]), c("UTF-8", "UTF-8"))
  expect_true(all(validUTF8(back[1:2])))
  expect_identical(nchar(back[1]), 7L)
  expect_true(all(validUTF8(back[1:2])))
  expect_identical(nchar(back[1]), 7L)
})

test_that("rmoriedata's SIU table carries the same bytes from CSV and Parquet", {
  skip_if_not_installed("rmoriedata")
  a <- rmoriedata::mdload("siu_directors_reports")
  b <- morie_read_parquet(rmoriedata::mdpath("siu_directors_reports"))
  hit <- NULL
  for (col in names(a)[vapply(a, is.character, NA)]) {
    i <- which(vapply(a[[col]], function(z) any(charToRaw(z) > 0x7f), TRUE))
    if (length(i)) {
      hit <- c(col, i[1])
      break
    }
  }
  expect_false(is.null(hit)) # the corpus has non-ASCII cells; the scan must find one
  expect_identical(charToRaw(a[[hit[1]]][as.integer(hit[2])]),
                   charToRaw(b[[hit[1]]][as.integer(hit[2])]))
})

test_that("unmarked non-UTF-8 bytes are refused by name; marked latin1 is transcoded", {
  latin1 <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9))) # "caf\xe9", Encoding unknown
  f <- tempfile(fileext = ".parquet")
  expect_error(
    morie_write_parquet(data.frame(a = latin1, stringsAsFactors = FALSE), f),
    "column .a. holds a string that is not valid UTF-8")
  marked <- latin1
  Encoding(marked) <- "latin1"
  morie_write_parquet(data.frame(a = marked, stringsAsFactors = FALSE), f)
  expect_identical(charToRaw(morie_read_parquet(f)$a), utf8_bytes[1:5])
})

test_that("a data page v2 is reported as such before any decompression", {
  f <- tempfile(fileext = ".parquet")
  morie_write_parquet(data.frame(a = c("x", "y", "z"),
                                              stringsAsFactors = FALSE), f)
  raw <- readBin(f, "raw", file.size(f))
  # the first page header follows the 4-byte magic: field 1 (i32, page
  # type) is 0x15 then the zigzag value; DATA_PAGE = 0, DATA_PAGE_V2 = 3
  skip_if_not(identical(raw[5:6], as.raw(c(0x15, 0x00))),
              "writer page-header layout changed; fixture needs updating")
  raw[6] <- as.raw(0x06)
  writeBin(raw, f)
  expect_error(morie_read_parquet(f), "data page v2")
})

test_that("duplicate column names are refused on write and kept apart on read", {
  f <- tempfile(fileext = ".parquet")
  dup <- setNames(data.frame(1:3, 4:6), c("x", "x"))
  expect_error(morie_write_parquet(dup, f), "duplicate column names: .x.")
  # a file that really holds two columns named x: write x and y
  # uncompressed, then rename y to x in the footer (the only 0x79 bytes)
  morie_write_parquet(data.frame(x = 1:3, y = 4:6), f,
                                   compression = NULL)
  raw <- readBin(f, "raw", file.size(f))
  skip_if_not(sum(raw == as.raw(0x79)) == 2L, "fixture layout changed")
  raw[raw == as.raw(0x79)] <- as.raw(0x78)
  writeBin(raw, f)
  expect_warning(d <- morie_read_parquet(f), "duplicate column names")
  expect_identical(names(d), c("x", "x.1"))
  expect_identical(d$x, 1:3)
  expect_identical(d$x.1, 4:6)
})
