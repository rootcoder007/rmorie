# SPDX-License-Identifier: AGPL-3.0-or-later
# The native hash family against the published test vectors, and against
# the digest package whenever it is installed (Suggests only).

test_that("hash kernels reproduce the published test vectors", {
  h <- function(s, algo) morie_digest(s, algo = algo, serialize = FALSE)
  expect_identical(h("", "md5"), "d41d8cd98f00b204e9800998ecf8427e")
  expect_identical(h("abc", "md5"), "900150983cd24fb0d6963f7d28e17f72")
  expect_identical(h("abc", "sha1"), "a9993e364706816aba3e25717850c26c9cd0d89d")
  expect_identical(h("abc", "sha224"), "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7")
  expect_identical(h("abc", "sha256"), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  expect_identical(h("abc", "sha384"), paste0("cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed",
                                             "8086072ba1e7cc2358baeca134c825a7"))
  expect_identical(h("abc", "sha512"), paste0("ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a",
                                             "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"))
  expect_identical(h("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "sha256"),
                   "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  expect_identical(h("123456789", "crc32"), "cbf43926")
  expect_identical(h("123456789", "crc32c"), "e3069283")
  expect_identical(h("", "xxhash32"), "02cc5d05")
  expect_identical(h("", "xxhash64"), "ef46db3751d8e999")
  expect_identical(h("Nobody inspects the spammish repetition", "xxhash32"), "e2293b2f")
  expect_identical(h("", "murmur32"), "00000000")
  expect_identical(morie_digest("", algo = "murmur32", serialize = FALSE, seed = 1), "514e28b7")
  expect_identical(h("The quick brown fox jumps over the lazy dog", "murmur32"), "2e4ff723")
  expect_identical(morie_hmac("key", "The quick brown fox jumps over the lazy dog", algo = "md5"),
                   "80070713463e7749b90c2dc24911e275")
  expect_identical(morie_hmac("key", "The quick brown fox jumps over the lazy dog", algo = "sha256"),
                   "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8")
  expect_identical(morie_hmac("key", "The quick brown fox jumps over the lazy dog", algo = "sha1"),
                   "de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9")
  # FIPS-197 C.1: AES-128 on 00112233445566778899aabbccddeeff with key 000102..0f
  key <- as.raw(0:15)
  pt <- as.raw(c(0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff))
  aes <- morie_aes(key, mode = "ECB")
  ct <- aes$encrypt(pt)
  expect_identical(paste(sprintf("%02x", as.integer(ct)), collapse = ""), "69c4e0d86a7b0430d8cdb78070b4c55a")
  expect_identical(aes$decrypt(ct, raw = TRUE), pt)
  aes256 <- morie_aes(as.raw(0:31), mode = "ECB")
  expect_identical(paste(sprintf("%02x", as.integer(aes256$encrypt(pt))), collapse = ""), "8ea2b7ca516745bfeafc49904b496089")
})

test_that("morie_digest matches digest::digest across algorithms and inputs", {
  skip_if_not_installed("digest")
  algos <- c("md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64", "murmur32", "crc32c")
  set.seed(7)
  objects <- list(1:10, "a string", c(a = 1.5, b = NA), mtcars[1:5, ], list(x = 1, y = list(z = "q")),
                  NULL, TRUE, as.raw(1:100), factor(c("u", "v")), as.Date("2024-01-01"),
                  matrix(runif(20), 4), function(x) x + 1)
  strings <- c("", "a", "abc", strrep("x", 63), strrep("y", 64), strrep("z", 65), strrep("w", 1000), "é中")
  raws <- list(raw(0), as.raw(0:255), as.raw(sample(0:255, 10000, TRUE)))
  for (algo in algos) {
    for (o in objects) expect_identical(morie_digest(o, algo = algo), digest::digest(o, algo = algo), label = algo)
    for (s in strings) expect_identical(morie_digest(s, algo = algo, serialize = FALSE),
                                        digest::digest(s, algo = algo, serialize = FALSE), label = paste(algo, nchar(s)))
    for (r in raws) {
      expect_identical(morie_digest(r, algo = algo, serialize = FALSE), digest::digest(r, algo = algo, serialize = FALSE), label = algo)
      expect_identical(morie_digest(r, algo = algo, serialize = FALSE, raw = TRUE),
                       digest::digest(r, algo = algo, serialize = FALSE, raw = TRUE), label = paste(algo, "raw"))
    }
    expect_identical(morie_digest(strings[7], algo = algo, serialize = FALSE, length = 100),
                     digest::digest(strings[7], algo = algo, serialize = FALSE, length = 100), label = paste(algo, "length"))
    expect_identical(morie_digest(strings[7], algo = algo, serialize = FALSE, skip = 10),
                     digest::digest(strings[7], algo = algo, serialize = FALSE, skip = 10), label = paste(algo, "skip"))
    expect_identical(morie_digest(mtcars, algo = algo, ascii = TRUE), digest::digest(mtcars, algo = algo, ascii = TRUE), label = paste(algo, "ascii"))
  }
  for (algo in c("xxhash32", "xxhash64", "murmur32")) for (seed in c(1, 42, 2^31 - 1))
    expect_identical(morie_digest("seeded input", algo = algo, serialize = FALSE, seed = seed),
                     digest::digest("seeded input", algo = algo, serialize = FALSE, seed = seed), label = paste(algo, seed))
  p <- tempfile()
  writeBin(as.raw(sample(0:255, 70000, TRUE)), p)
  for (algo in c("md5", "sha1", "sha256", "sha512", "crc32", "xxhash64"))
    expect_identical(morie_digest(file = p, algo = algo), digest::digest(file = p, algo = algo), label = paste(algo, "file"))
  expect_identical(morie_digest(p, algo = "sha256", file = TRUE, skip = 100, length = 5000),
                   digest::digest(p, algo = "sha256", file = TRUE, skip = 100, length = 5000))
  expect_error(morie_digest("x", algo = "blake3"), "not implemented natively")
  expect_identical(morie_digest("x", algo = "nope", errormode = "silent"), NULL)
})

test_that("morie_hmac, morie_digest2int, morie_sha1 and morie_aes match digest", {
  skip_if_not_installed("digest")
  for (algo in c("md5", "sha1", "sha256", "sha512", "crc32")) {
    expect_identical(morie_hmac("key", "message", algo = algo), digest::hmac("key", "message", algo = algo), label = algo)
    expect_identical(morie_hmac(as.raw(1:100), "message", algo = algo), digest::hmac(as.raw(1:100), "message", algo = algo), label = paste(algo, "longkey"))
    expect_identical(morie_hmac("k", charToRaw("m"), algo = algo, raw = TRUE), digest::hmac("k", charToRaw("m"), algo = algo, raw = TRUE), label = paste(algo, "raw"))
  }
  expect_identical(morie_digest2int(c("abc", "", "é", "a longer string")), digest::digest2int(c("abc", "", "é", "a longer string")))
  expect_identical(morie_digest2int("abc", seed = 99L), digest::digest2int("abc", seed = 99L))
  sha1_cases <- list(c(1.1, 2.2, pi, 0, -1e-9, NA, Inf), 1:5, "chr", c(TRUE, NA), factor(c("a", "b")),
                     matrix(c(1.5, 2, 3, 4.25), 2), matrix(1:4, 2), data.frame(a = 1:3, b = c(0.1, 0.2, 0.3), c = letters[1:3]),
                     list(a = 1, b = "x", c = list(d = 2.5)), as.Date("2024-05-06"),
                     as.POSIXct("2024-05-06 07:08:09.25", tz = "UTC"), c(1 + 2i, 3 - 4i), array(1:8, c(2, 2, 2)), NULL,
                     y ~ x + z, function(a, b = 2) a + b, quote(f(x)), as.name("sym"), as.raw(1:5))
  for (x in sha1_cases) {
    expect_identical(morie_sha1(x), digest::sha1(x), label = class(x)[1])
    expect_identical(morie_sha1(x, digits = 6L, zapsmall = 3L, algo = "sha256"), digest::sha1(x, digits = 6L, zapsmall = 3L, algo = "sha256"),
                     label = paste(class(x)[1], "sha256"))
  }
  key <- as.raw(1:16)
  iv <- as.raw(17:32)
  msg <- charToRaw(strrep("The quick brown fox jumps over the lazy dog", 3))
  msg32 <- msg[1:32]
  for (mode in c("ECB", "CBC", "CFB", "CTR")) {
    text <- if (mode %in% c("ECB", "CBC")) msg32 else msg
    ours <- morie_aes(key, mode = mode, IV = iv)
    theirs <- digest::AES(key, mode = mode, IV = iv)
    ct <- ours$encrypt(text)
    expect_identical(ct, theirs$encrypt(text), label = mode)
    ours2 <- morie_aes(key, mode = mode, IV = iv)
    expect_identical(ours2$decrypt(ct, raw = TRUE), text, label = paste(mode, "decrypt"))
  }
  cbc <- morie_aes(key, mode = "CBC", IV = iv, padding = TRUE)
  ct <- cbc$encrypt(msg)
  cbc2 <- digest::AES(key, mode = "CBC", IV = iv, padding = TRUE)
  expect_identical(ct, cbc2$encrypt(msg))
  expect_identical(morie_aes(key, mode = "CBC", IV = iv, padding = TRUE)$decrypt(ct, raw = TRUE), msg)
  for (k in list(as.raw(1:24), as.raw(1:32))) {
    a <- morie_aes(k, "ECB")
    b <- digest::AES(k, "ECB")
    expect_identical(a$encrypt(msg32), b$encrypt(msg32), label = length(k))
  }
})
