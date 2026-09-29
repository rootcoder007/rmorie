# Argon2 (RFC 9106): the Sec. 5 test vectors for all three variants, the
# prehash and variable-length hash against openssl BLAKE2b, and the
# compression function G checked end to end by rebuilding a one-lane,
# one-pass Argon2d from prehash, H' and G and matching the native tag.

sa_le32 <- function(n) writeBin(as.integer(n), raw(), size = 4L, endian = "little")

test_that("Argon2 reproduces the RFC 9106 test vectors", {
  pw <- as.raw(rep(1, 32))
  salt <- as.raw(rep(2, 16))
  k <- as.raw(rep(3, 8))
  ad <- as.raw(rep(4, 12))
  ref <- c(argon2d = "512b391b6f1162975371d30919734294f868e3be3984f3c1a13a4db9fabe4acb",
           argon2i = "c814d9d1dc7f37aa13f0d77f2494bda1c8de6b016dd388d29952a4c4672b6ce8",
           argon2id = "0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659")
  for (v in names(ref)) {
    r <- morie_secarg_argon2(pw, salt, 32, 3, 4, 32, v, secret = k, associated = ad)
    expect_identical(r$tag_hex, ref[[v]])
  }
  expect_identical(morie_secarg(pw, salt, 32, 3, 4, 32, "argon2id", k, ad)$memory_used_kib, 32L)
})

test_that("the prehash and H' are the specified BLAKE2b constructions", {
  skip_if_not_installed("openssl")
  pw <- charToRaw("password")
  salt <- charToRaw("somesalt")
  h0 <- morie_secarg_prehash(pw, salt, 1, 32, 8, 1, "argon2d")
  buf <- c(sa_le32(1), sa_le32(32), sa_le32(8), sa_le32(1), sa_le32(0x13), sa_le32(0),
           sa_le32(8), pw, sa_le32(8), salt, sa_le32(0), sa_le32(0))
  expect_identical(h0, as.raw(openssl::blake2b(buf)))
  expect_identical(morie_secarg_variable_hash(pw, 64), as.raw(openssl::blake2b(c(sa_le32(64), pw))))
  # longer outputs chain 64-byte BLAKE2b and keep 32 bytes per link
  v1 <- as.raw(openssl::blake2b(c(sa_le32(128), pw)))
  v2 <- as.raw(openssl::blake2b(v1))
  v3 <- as.raw(openssl::blake2b(v2))
  expect_identical(morie_secarg_variable_hash(pw, 128), c(v1[1:32], v2[1:32], v3))
  expect_error(morie_secarg_prehash(pw, as.raw(1:4), 1, 32, 8, 1), "at least 8 bytes")
  expect_error(morie_secarg_prehash(pw, salt, 1, 32, 8, 1, "argon3"), "variant must be")
  expect_error(morie_secarg_variable_hash(pw, 0), "positive")
})

test_that("G rebuilds a one-lane, one-pass Argon2d that matches the native tag", {
  pw <- charToRaw("correct horse")
  salt <- charToRaw("saltsalt12")
  h0 <- morie_secarg_prehash(pw, salt, 1, 32, 8, 1, "argon2d")
  B <- vector("list", 8)
  B[[1]] <- morie_secarg_variable_hash(c(h0, sa_le32(0), sa_le32(0)), 1024)
  B[[2]] <- morie_secarg_variable_hash(c(h0, sa_le32(1), sa_le32(0)), 1024)
  for (j in 2:7) {
    prev <- B[[j]]
    lo <- as.integer(prev[1]) + 256 * as.integer(prev[2])
    hi <- as.integer(prev[3]) + 256 * as.integer(prev[4])
    # x = floor(J1^2 / 2^32) without leaving exact double arithmetic
    x <- hi * hi + (2 * hi * lo * 65536 + lo * lo) %/% 2^32
    W <- j - 1
    y <- (W * x) %/% 2^32
    ref <- W - 1 - y
    B[[j + 1]] <- morie_secarg_compress(prev, B[[ref + 1]])
  }
  tag <- morie_secarg_variable_hash(B[[8]], 32)
  nat <- morie_secarg_argon2(pw, salt, memory = 8, passes = 1, parallelism = 1, tag_length = 32,
                             variant = "argon2d")
  expect_identical(tag, nat$tag)
  # both input forms are accepted: raw blocks give raw, numeric words give numeric
  w <- as.numeric(1:128)
  z <- rep(0, 128)
  rb <- function(v) as.raw(as.vector(sapply(v, function(t) c(t %% 256, t %/% 256, rep(0, 6)))))
  expect_identical(rb(1:128), rb(w))
  gr <- morie_secarg_compress(rb(1:128), rb(rep(0, 128)))
  gn <- morie_secarg_compress(w, z)
  expect_length(gr, 1024L)
  expect_length(gn, 128L)
  expect_error(morie_secarg_compress(raw(10), raw(10)), "1024 bytes")
  expect_error(morie_secarg_compress(rep(-1, 128), z), "integers in")
})

test_that("parameter advice follows RFC 9106 Sec. 4", {
  a <- morie_secarg_parameter_advice()
  expect_identical(c(a$memory, a$passes, a$parallelism), c(2L * 1024L * 1024L, 1L, 4L))
  expect_equal(a$memory_gib, 2)
  b <- morie_secarg_parameter_advice("second")
  expect_identical(c(b$memory, b$passes), c(65536L, 3L))
  expect_error(morie_secarg_parameter_advice("third"), "first' or 'second")
})
