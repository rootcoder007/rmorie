# Anchors for bit-packing of quantiser indices.
#
# The file's own header makes the case for these tests: the layout is
# big-endian bit order within a big-endian byte stream, and "a reader that
# assumes the opposite bit order recovers plausible-looking indices that
# are silently wrong, and no checksum in the format would catch it". So
# the byte layout is asserted against hand-computed values, not just
# round-tripped. The file sat at 1.0% coverage with no test naming either
# of its functions.

bytes_of <- function(p) as.integer(unlist(p$bytes))

test_that("the byte layout is big-endian bit order, hand-computed", {
  # 4-bit indices 1, 2, 3: 0001 0010 0011 then four bits of padding,
  # so 0x12 0x30
  expect_identical(bytes_of(.tqpack_pack_indices(c(1, 2, 3), 4)),
                   c(0x12L, 0x30L))
  # 3-bit indices 5, 3, 1: 101 011 001 -> 10101100 1 + padding -> 0xAC 0x80
  expect_identical(bytes_of(.tqpack_pack_indices(c(5, 3, 1), 3)),
                   c(0xACL, 0x80L))
  # 1-bit indices 1,0,1,1,0,0,0,1 fill exactly one byte: 10110001 = 0xB1
  expect_identical(bytes_of(.tqpack_pack_indices(c(1, 0, 1, 1, 0, 0, 0, 1), 1)),
                   0xB1L)
  # 8-bit indices are the bytes themselves
  expect_identical(bytes_of(.tqpack_pack_indices(c(0, 127, 255), 8)),
                   c(0L, 127L, 255L))
  # a single 2-bit index sits in the top of the byte: 11 000000 = 0xC0
  expect_identical(bytes_of(.tqpack_pack_indices(3, 2)), 0xC0L)
})

test_that("unpacking inverts packing at every width", {
  set.seed(1)
  for (b in 1:16) {
    for (n in c(1, 7, 8, 9, 37)) {
      idx <- sample.int(2^b, n, replace = TRUE) - 1L
      p <- .tqpack_pack_indices(idx, b)
      u <- .tqpack_unpack_indices(p$bytes, b, n)
      expect_identical(as.integer(unlist(u$indices)), as.integer(idx))
    }
  }
})

test_that("the packed size is the ceiling of the bits required", {
  for (b in c(1, 3, 4, 5, 8, 12, 16)) {
    for (n in c(1, 2, 7, 8, 37, 100)) {
      p <- .tqpack_pack_indices(rep(0, n), b)
      expect_identical(as.integer(p$n_bytes), as.integer(ceiling(n * b / 8)))
      expect_identical(as.integer(p$bits_used), as.integer(n * b))
      expect_identical(as.integer(p$n_indices), as.integer(n))
      expect_identical(as.integer(p$bits), as.integer(b))
      # the padding is what is left of the final byte, so under eight
      expect_identical(as.integer(p$padding_bits),
                       as.integer(8 * p$n_bytes - n * b))
      expect_true(p$padding_bits >= 0 && p$padding_bits < 8)
    }
  }
})

test_that("only the final byte is padded, and it is padded with zeros", {
  # indices that leave five bits spare: three 1-bit indices in one byte
  p <- .tqpack_pack_indices(c(1, 1, 1), 1)
  expect_identical(as.integer(p$padding_bits), 5L)
  # 111 followed by five zeros is 0xE0
  expect_identical(bytes_of(p), 0xE0L)
  # a stream that fills its bytes exactly has no padding
  q <- .tqpack_pack_indices(rep(1, 8), 1)
  expect_identical(as.integer(q$padding_bits), 0L)
  expect_identical(bytes_of(q), 0xFFL)
})

test_that("the compression figure is the ratio actually achieved", {
  # eight bytes per index as a double, against the packed size, which
  # includes the padding rather than pretending 64/bits
  for (spec in list(c(3, 4), c(37, 5), c(100, 12))) {
    n <- spec[1]; b <- spec[2]
    p <- .tqpack_pack_indices(rep(0, n), b)
    expect_equal(as.numeric(p$compression_vs_float64),
                 (8 * n) / as.numeric(p$n_bytes), tolerance = 1e-12)
  }
  # narrower codes compress harder
  wide <- .tqpack_pack_indices(rep(0, 64), 16)
  narrow <- .tqpack_pack_indices(rep(0, 64), 2)
  expect_gt(as.numeric(narrow$compression_vs_float64),
            as.numeric(wide$compression_vs_float64))
})

test_that("the widest index for a width is representable", {
  for (b in c(1, 2, 3, 4, 7, 8, 12, 16)) {
    top <- 2^b - 1
    p <- .tqpack_pack_indices(c(0, top), b)
    u <- .tqpack_unpack_indices(p$bytes, b, 2L)
    expect_identical(as.integer(unlist(u$indices)), c(0L, as.integer(top)))
    # and one more than that does not fit
    expect_error(.tqpack_pack_indices(2^b, b), "does not fit")
  }
})

test_that("an index or a width outside the format is refused", {
  expect_error(.tqpack_pack_indices(16, 4), "does not fit in 4 bits")
  expect_error(.tqpack_pack_indices(-1, 4), "does not fit")
  expect_error(.tqpack_pack_indices(1, 0), "bits must lie in 1\\.\\.32")
  expect_error(.tqpack_pack_indices(1, 33), "bits must lie in 1\\.\\.32")
  # and a byte string too short for the indices asked of it
  p <- .tqpack_pack_indices(c(1, 2, 3), 4)
  expect_error(.tqpack_unpack_indices(p$bytes, 4, 100), "cannot hold")
})

test_that("an empty index vector packs to nothing", {
  p <- .tqpack_pack_indices(integer(0), 4)
  expect_identical(as.integer(p$n_bytes), 0L)
  expect_identical(as.integer(p$n_indices), 0L)
  expect_identical(as.integer(p$bits_used), 0L)
  u <- .tqpack_unpack_indices(p$bytes, 4, 0L)
  expect_length(unlist(u$indices), 0L)
})

test_that("reading with the wrong width does not silently agree", {
  # the failure the header warns about: a reader using a different width
  # gets plausible numbers back, so the convention has to be pinned by
  # tests rather than trusted
  idx <- c(1, 2, 3, 4, 5, 6)
  p <- .tqpack_pack_indices(idx, 4)
  wrong <- .tqpack_unpack_indices(p$bytes, 3, 6L)
  expect_false(identical(as.integer(unlist(wrong$indices)), as.integer(idx)))
  # while the right width recovers them exactly
  right <- .tqpack_unpack_indices(p$bytes, 4, 6L)
  expect_identical(as.integer(unlist(right$indices)), as.integer(idx))
})
