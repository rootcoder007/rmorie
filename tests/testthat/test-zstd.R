# SPDX-License-Identifier: AGPL-3.0-or-later
# The Zstandard decoder (src/zstd.c). fixtures/zstd/*.zst were made with the zstd
# command-line tool by rmoriedata's dev/zsfix.sh (this file and the fixtures are
# rmoriedata's), which wrote index.csv with each one's size and
# the SHA-256 of what `zstd -d` gives; the *.parquet files are one table written
# with ZSTD by nanoparquet, arrow and DuckDB (rmoriedata's dev/zspq.R). Which block,
# literals and sequence types each vector exercises is checked by rmoriedata's
# dev/fuzz/zscover.sh.

zdec <- function(x, n) .Call(C_rmd_zstd_decompress, x, n)
zsha <- function(x) rmoriebricklayer::csha256(x)
zidx <- function() {
  utils::read.csv(test_path("fixtures", "zstd", "index.csv"), stringsAsFactors = FALSE)
}
zfix <- function(f) {
  p <- test_path("fixtures", "zstd", f)
  readBin(p, "raw", file.size(p))
}
zsize <- function(f) {
  idx <- zidx()
  idx$size[idx$file == f]
}
hx <- function(...) as.raw(c(...))
le3 <- function(h) hx(h %% 256L, h %/% 256L %% 256L, h %/% 65536L)
magic <- hx(0x28, 0xb5, 0x2f, 0xfd)
# one raw block "hello" in a single-segment frame with a 1-byte content size of 5
hello <- c(magic, hx(0x20, 0x05, 0x29, 0x00, 0x00), charToRaw("hello"))
# a frame with a 1 KiB window and no content size, around the blocks given
frame1k <- function(...) c(magic, hx(0x00, 0x00), ...)
rawblk <- function(x) c(le3(length(x) * 8L), x)
cblock <- function(body) c(le3(length(body) * 8L + 5L), body)
# literals "abc", then one sequence whose three codes are RLE: literal length 3,
# offset code `ofc` with extra bits in `bits`, match length 3
seq1 <- function(ofc, bits) {
  cblock(c(hx(0x18), charToRaw("abc"), hx(0x01, 0x54, 0x03, ofc, 0x00), bits))
}
# Huffman literals, one stream, 4 symbols: two direct weights in `w`, stream `s`
huf <- function(w, s) cblock(hx(0x42, 0xc0, 0x00, 0x81, w, s, 0x00))
# two raw 1 KiB blocks, then `last`: offsets up to 2051 reach back into them
a1k <- as.raw(rep(0:255, 4L))
far <- function(last) frame1k(rawblk(a1k), rawblk(rev(a1k)), last)

test_that("every zstd CLI vector decompresses to its recorded bytes", {
  idx <- zidx()
  expect_gte(nrow(idx), 40L)
  for (k in seq_len(nrow(idx))) {
    out <- zdec(zfix(idx$file[k]), idx$size[k])
    expect_identical(length(out), as.integer(idx$size[k]), info = idx$file[k])
    expect_identical(zsha(out), idx$sha256[k], info = idx$file[k])
  }
  expect_identical(zdec(zfix("empty.zst"), 0), raw(0))
  expect_identical(rawToChar(zdec(zfix("concat.zst"), 25)),
                   "hello hello hello world\na")
})

test_that("hand-built frames decode as RFC 8878 says", {
  expect_identical(rawToChar(zdec(hello, 5)), "hello")
  # a dictionary id of 0 names no dictionary
  expect_identical(rawToChar(zdec(c(magic, hx(0x21, 0x00, 0x05), hello[7:14]), 5)),
                   "hello")
  # an 8 MiB window is accepted whatever the output size
  expect_identical(rawToChar(zdec(c(magic, hx(0x00, 0x68), hello[7:14]), 5)), "hello")
  # offset 3 after the literals "abc": the match copies them
  expect_identical(rawToChar(zdec(frame1k(seq1(2, hx(0x06))), 6)), "abcabc")
  # Huffman weights 1, 1 and an implied 2: symbol 2 has the 1-bit code
  expect_identical(zdec(frame1k(huf(0x11, 0x1f)), 4), as.raw(rep(2L, 4L)))
  # a match 1000 bytes back, inside the 1 KiB window
  head <- c(a1k, rev(a1k), charToRaw("abc"))
  expect_identical(zdec(far(seq1(9, hx(0xeb, 0x03))), 2054),
                   c(head, head[length(head) - 1000L + 1:3]))
})

test_that("every prefix of a frame is refused in words", {
  frames <- list(hello = hello, short = zfix("short.zst"),
                 rle = zfix("rlemodes.zst"), skip = zfix("skip.zst"))
  sizes <- c(hello = 5, short = zsize("short.zst"), rle = 6, skip = zsize("skip.zst"))
  for (nm in names(frames)) {
    f <- frames[[nm]]
    expect_identical(length(zdec(f, sizes[[nm]])), as.integer(sizes[[nm]]))
    for (k in seq_along(f) - 1L) {
      expect_error(zdec(f[seq_len(k)], sizes[[nm]]), "^zstd: ", info = paste(nm, k))
    }
  }
  expect_error(zdec(raw(0), 0), "holds no frame")
  expect_error(zdec(hello[1:9], 5), "truncated")
})

test_that("damaged and unsupported frames are refused in words", {
  bad <- hello
  bad[2] <- as.raw(0xb6)
  expect_error(zdec(bad, 5), "bad magic number")
  bad <- hello
  bad[5] <- as.raw(0x28)
  expect_error(zdec(bad, 5), "reserved bit")
  bad <- hello
  bad[7] <- as.raw(0x2f)
  expect_error(zdec(bad, 5), "reserved block type")
  expect_error(zdec(c(magic, hx(0x21, 0x07, 0x05), hello[7:14]), 5), "dictionary")
  expect_error(zdec(c(magic, hx(0x22, 0x01, 0x00, 0x05), hello[7:14]), 5), "dictionary")
  # windows: 2^41 bytes, and 16 MiB for 5 bytes of output
  expect_error(zdec(c(magic, hx(0x00, 0xf8), hello[7:14]), 5), "window")
  expect_error(zdec(c(magic, hx(0x00, 0x70), hello[7:14]), 5), "window")
  # the stated size, and the frame's own content size
  expect_error(zdec(hello, 4), "content size")
  expect_error(zdec(hello, 6), "stated size")
  expect_error(zdec(c(magic, hx(0x00, 0x68), hello[7:14]), 4), "stated size")
  bad <- hello
  bad[6] <- as.raw(6)
  expect_error(zdec(bad, 6), "content size does not match")
  # a 6-byte raw block where the window (the content size, 5) allows 5
  expect_error(zdec(c(magic, hx(0x20, 0x05, 0x31, 0x00, 0x00), charToRaw("hello!")), 5),
               "larger than its maximum")
  short <- zfix("short.zst")
  bad <- short
  bad[length(bad)] <- xor(bad[length(bad)], as.raw(1))
  expect_error(zdec(bad, zsize("short.zst")), "checksum mismatch")
  # garbage after the last frame
  expect_error(zdec(c(hello, charToRaw("junk")), 5), "bad magic number")
  expect_error(zdec(c(hello, hx(1, 2)), 5), "bytes after the last frame are not a frame")
  for (k in 1:3) {
    expect_error(zdec(c(hello, charToRaw("junk")[seq_len(k)]), 5), "after the last frame",
                 info = k)
    # the start of another frame's magic number is a frame cut short
    expect_error(zdec(c(hello, magic[seq_len(k)]), 5), "truncated", info = k)
    skm <- hx(0x5a, 0x2a, 0x4d)[seq_len(k)]
    expect_error(zdec(c(hello, skm), 5), "truncated", info = k)
  }
  expect_error(zdec(hx(1, 2), 5), "truncated")
  # the audit's frames: trailing data after a frame (1 byte), and a compressed block
  # whose sequence count of 0 is written in the 2-byte form (0x80 0x00) with 4 junk bytes
  # after it, which libzstd refuses and which once decoded as "hello"
  trail <- hx(0x28, 0xb5, 0x2f, 0xfd, 0x20, 0x01, 0x09, 0x00, 0x00, 0x41, 0x00)
  expect_error(zdec(trail, 1), "after the last frame")
  nseq0 <- hx(0x28, 0xb5, 0x2f, 0xfd, 0x00, 0x00, 0x65, 0x00, 0x00, 0x28, 0x68, 0x65,
              0x6c, 0x6c, 0x6f, 0x80, 0x00, 0x4a, 0x55, 0x4e, 0x4b)
  expect_error(zdec(nseq0, 5), "sequences section")
  # the same block with the count in the 2-byte form and nothing after it decodes
  ok <- nseq0[1:17]
  ok[7] <- as.raw(0x45)
  expect_identical(rawToChar(zdec(ok, 5)), "hello")
})

test_that("corrupt Huffman, FSE and sequence data is refused in words", {
  # weights 3 and 1 leave 3/8 of the code space, not a power of two
  expect_error(zdec(frame1k(huf(0x31, 0x1f)), 4), "Huffman table")
  # weights past the 11-bit limit
  expect_error(zdec(frame1k(huf(0xc1, 0x1f)), 4), "Huffman table")
  # FSE-coded weights with an accuracy log of 20 (6 is the most)
  expect_error(zdec(frame1k(cblock(hx(0x42, 0xc0, 0x00, 0x01, 0x0f, 0x01, 0x00))), 4),
               "Huffman table")
  # a stream 3 bits long for 4 symbols, and one with bits left over
  expect_error(zdec(frame1k(huf(0x11, 0x0f)), 4), "Huffman-coded")
  expect_error(zdec(frame1k(huf(0x11, 0x3f)), 4), "Huffman-coded")
  # literal-length table with accuracy log 20 (9 is the most), offsets with 9 (8)
  expect_error(zdec(frame1k(cblock(hx(0x00, 0x01, 0x80, 0x0f, 0x01))), 3), "FSE table")
  expect_error(zdec(frame1k(cblock(hx(0x00, 0x01, 0x20, 0x04, 0x01))), 3), "FSE table")
  # repeat tables and treeless literals with nothing to repeat
  expect_error(zdec(frame1k(cblock(hx(0x00, 0x01, 0xfc, 0x01))), 3), "no earlier table")
  expect_error(zdec(frame1k(cblock(hx(0x43, 0x40, 0x00, 0x01, 0x00))), 4),
               "no earlier table")
  # reserved bits of the symbol compression modes
  expect_error(zdec(frame1k(cblock(hx(0x00, 0x01, 0x55, 0x00, 0x00, 0x00, 0x01))), 3),
               "reserved bit")
  # an RLE code past the largest literal-length code (35)
  expect_error(zdec(frame1k(cblock(hx(0x00, 0x01, 0x54, 0x24, 0x00, 0x00, 0x01))), 3),
               "sequences section")
  # sequences whose bitstream has bits left over
  expect_error(zdec(frame1k(seq1(2, hx(0x1a))), 6), "sequences section")
  # offset 10 with 3 bytes of output, and 1500 with a 1 KiB window
  expect_error(zdec(frame1k(seq1(3, hx(0x0d))), 6), "before the start")
  expect_error(zdec(far(seq1(10, hx(0xdf, 0x05))), 2054), "beyond the window")
})

test_that("the wrapper checks its arguments before allocating", {
  expect_error(zdec("x", 1), "expected a raw vector")
  expect_error(zdec(hello, -1), "bad uncompressed size")
  expect_error(zdec(hello, NA), "bad uncompressed size")
  expect_error(zdec(hello, 2.5), "bad uncompressed size")
  expect_error(zdec(hello, 2^40), "can hold")
})

test_that("mutated frames decode or are refused, never crash", {
  set.seed(8878)
  idx <- zidx()
  pick <- c("short.zst", "mid-l3.zst", "mid-l19.zst", "sym4.zst", "phrases.zst",
            "digits.zst", "rlemodes.zst", "nseq3.zst", "letters.zst", "skip.zst")
  outcomes <- character()
  for (i in seq_len(2000L)) {
    f <- pick[(i - 1L) %% length(pick) + 1L]
    x <- zfix(f)
    j <- sample.int(length(x), sample.int(4L, 1L), replace = TRUE)
    x[j] <- as.raw(sample.int(256L, length(j), replace = TRUE) - 1L)
    if (i %% 5L == 0L) x <- x[seq_len(sample.int(length(x), 1L))]
    r <- tryCatch(
      {
        zdec(x, idx$size[idx$file == f])
        "ok"
      },
      error = function(e) {
        if (startsWith(conditionMessage(e), "zstd: ")) "error" else "other"
      }
    )
    outcomes <- c(outcomes, r)
  }
  expect_true(all(outcomes %in% c("ok", "error")))
  expect_true(any(outcomes == "error"))
})

test_that("ZSTD Parquet files from other writers read like the same table in snappy", {
  i <- seq_len(400L)
  df <- data.frame(
    id = i,
    x = ifelse(i %% 11L == 0L, NA, round(sin(i) * 1000, 3)),
    grp = ifelse(i %% 13L == 0L, NA, c("north", "south", "east", "west")[i %% 4L + 1L]),
    flag = i %% 3L == 0L,
    big = as.numeric(i) * 1e6,
    stringsAsFactors = FALSE
  )
  snappy <- tempfile(fileext = ".parquet")
  morie_write_parquet(df, snappy, compression = "snappy")
  want <- morie_read_parquet(snappy)
  for (w in c("nanoparquet", "arrow", "duckdb")) {
    p <- test_path("fixtures", "zstd", paste0(w, ".parquet"))
    bytes <- readBin(p, "raw", file.size(p))
    expect_gt(length(grepRaw(magic, bytes, fixed = TRUE, all = TRUE)), 0L)
    got <- morie_read_parquet(p)
    expect_equal(got, want, ignore_attr = TRUE, info = w)
    expect_equal(got, df, ignore_attr = TRUE, info = w)
  }
  # a damaged ZSTD page is a parquet: error that names the codec
  p <- test_path("fixtures", "zstd", "duckdb.parquet")
  bytes <- readBin(p, "raw", file.size(p))
  at <- grepRaw(magic, bytes, fixed = TRUE)
  bytes[at + 4L] <- as.raw(bitwOr(as.integer(bytes[at + 4L]), 8L))
  bad <- tempfile(fileext = ".parquet")
  writeBin(bytes, bad)
  expect_error(morie_read_parquet(bad), "^parquet: zstd: a reserved bit")
})
