# SPDX-License-Identifier: AGPL-3.0-or-later
# The native symmetric layer (src/morie_crypto_native_sym.cpp): published
# vectors, agreement with libsodium, the wrappers without libsodium, and key
# stores written by morie's Python arm and by rmorie 1.3.x (sodium package).

.h <- function(s) rmorie:::.morie_hex_to_raw(s)
.rfc8439 <- list(
  key = "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f",
  nonce = "070000004041424344454647", aad = "50515253c0c1c2c3c4c5c6c7",
  pt = paste0("Ladies and Gentlemen of the class of '99: If I could offer you only one tip ",
              "for the future, sunscreen would be it."),
  out = paste0("d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca967",
               "1282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee32809",
               "1b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116",
               "1ae10b594f09e26a7e902ecbd0600691")
)

test_that("scrypt reproduces RFC 7914 section 12", {
  expect_identical(
    rmorie:::.morie_scrypt(raw(0), raw(0), N = 16L, r = 1L, p = 1L, size = 64L),
    .h(paste0("77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442",
              "fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906"))
  )
  expect_identical(
    rmorie:::.morie_scrypt(charToRaw("pleaseletmein"), charToRaw("SodiumChloride"),
                           N = 16384L, r = 8L, p = 1L, size = 64L),
    .h(paste0("7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2",
              "d5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887"))
  )
  expect_error(rmorie:::.morie_scrypt_romix_impl(raw(128), 12L, 1L, 1L), "power of 2")
  expect_error(rmorie:::.morie_scrypt_romix_impl(raw(128), 16L, 0L, 1L), "r must be")
  expect_error(rmorie:::.morie_scrypt_romix_impl(raw(100), 16L, 1L, 1L), "B must be 128 bytes")
})

test_that("ChaCha20-Poly1305 reproduces RFC 8439 section 2.8.2 and refuses a changed message", {
  v <- .rfc8439
  key <- .h(v$key)
  nonce <- .h(v$nonce)
  aad <- .h(v$aad)
  pt <- charToRaw(v$pt)
  out <- rmorie:::.morie_aead_seal_impl(key, nonce, pt, aad)
  expect_identical(out, .h(v$out))
  expect_identical(rmorie:::.morie_aead_open_impl(key, nonce, out, aad), pt)
  bad <- out
  bad[1] <- xor(bad[1], as.raw(1))
  expect_null(rmorie:::.morie_aead_open_impl(key, nonce, bad, aad))
  expect_null(rmorie:::.morie_aead_open_impl(key, nonce, out, raw(0)))
  empty <- rmorie:::.morie_aead_seal_impl(key, nonce, raw(0), raw(0))
  expect_length(empty, 16L)
  expect_identical(rmorie:::.morie_aead_open_impl(key, nonce, empty, raw(0)), raw(0))
  expect_error(rmorie:::.morie_aead_seal_impl(key[-1], nonce, pt, aad), "key must be 32 bytes")
  expect_error(rmorie:::.morie_aead_open_impl(key, nonce[-1], out, aad), "nonce must be 12 bytes")
  expect_error(rmorie:::.morie_aead_open_impl(key, nonce, out[1:3], aad), "too short")
})

test_that("the native layer gives libsodium's bytes", {
  skip_if_not(morie_crypto_sodium_available(), "rmorie was built without libsodium")
  set.seed(7)
  for (len in c(0L, 1L, 63L, 64L, 65L, 300L)) for (la in c(0L, 5L, 16L, 33L)) {
    k <- as.raw(sample(0:255, 32L, TRUE))
    n <- as.raw(sample(0:255, 12L, TRUE))
    m <- as.raw(sample(0:255, len, TRUE))
    a <- as.raw(sample(0:255, la, TRUE))
    e <- morie_crypto_chacha20_poly1305_encrypt(k, n, m, a)
    expect_identical(rmorie:::.morie_aead_seal_impl(k, n, m, a), c(e$ct, e$tag))
    expect_identical(rmorie:::.morie_aead_open_impl(k, n, c(e$ct, e$tag), a), m)
  }
  ikm <- as.raw(1:40)
  expect_identical(morie_crypto_hkdf_sha256(ikm, 70L, charToRaw("s"), charToRaw("i")),
                   rmorie:::.morie_hkdf_sha256(ikm, 70L, charToRaw("s"), charToRaw("i")))
  expect_identical(morie_crypto_hkdf_sha256(ikm, 32L), rmorie:::.morie_hkdf_sha256(ikm, 32L, raw(0)))
})

test_that("the wrappers and the hybrid envelope run on the native layer without libsodium", {
  local_mocked_bindings(morie_crypto_sodium_available = function() FALSE)
  k <- as.raw(1:32)
  n <- as.raw(1:12)
  e <- morie_crypto_chacha20_poly1305_encrypt(k, n, charToRaw("hi"), charToRaw("a"))
  expect_identical(c(e$ct, e$tag), rmorie:::.morie_aead_seal_impl(k, n, charToRaw("hi"), charToRaw("a")))
  expect_identical(morie_crypto_chacha20_poly1305_decrypt(k, n, c(e$ct, e$tag), charToRaw("a")), charToRaw("hi"))
  expect_error(morie_crypto_chacha20_poly1305_decrypt(k, n, c(e$ct, e$tag)), "bad tag")
  # RFC 5869 A.1 and A.3 (empty salt and info)
  ikm <- as.raw(rep(0x0b, 22L))
  expect_identical(
    morie_crypto_hkdf_sha256(ikm, 42L, .h("000102030405060708090a0b0c"), .h("f0f1f2f3f4f5f6f7f8f9")),
    .h("3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865")
  )
  expect_identical(
    morie_crypto_hkdf_sha256(ikm, 42L),
    .h("8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8")
  )
  r <- morie_crypto_random_bytes(48L)
  expect_length(r, 48L)
  expect_false(identical(r, morie_crypto_random_bytes(48L)))
  expect_error(morie_crypto_random_bytes(0L), "positive")
  kp <- morie_crypto_hybrid_keygen()
  ct <- morie_crypto_hybrid_encrypt("native all the way", kp$pk)
  expect_identical(rawToChar(morie_crypto_hybrid_decrypt(ct, kp$sk)), "native all the way")
})

test_that("a key store written by morie's Python arm opens, and one written here has its format", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(paste0(
    '{"salt": "e1353a75af1aa7046d3be96411168896", "keys": {"alice": {"v": 2, ',
    '"pk": "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f", ',
    '"sk_nonce": "48b1a7bf50a6de3c62869fae", "sk_ct": "5092b1a3e5c822c68fe77facefcc02ad1c3525da',
    'ff5ffdb39df9fa66d555c28e26dbd36c258c8186d20a7cfbde97b0b63168d6568a9e0279a8cb7b9ff4e0b90a", ',
    '"sk_tag": "fe3c47f2c49ae4e344be3b7b1146c2e9"}}}'
  ), path)
  pw <- "päss word"
  out <- morie_crypto_keystore_load("alice", pw, path = path)
  expect_identical(out$pk, as.raw(0:31))
  expect_identical(out$sk, as.raw(100:163))
  expect_error(morie_crypto_keystore_load("alice", "pass word", path = path), "wrong password")
  # ... and one written here is Python's format: 16-byte salt, 12-byte nonce, separate tag
  morie_crypto_keystore_store("carol", as.raw(1:3), as.raw(9:1), pw, path = path)
  st <- rmorie:::.morie_from_json(path, simplifyVector = FALSE)
  expect_identical(names(st$keys$carol), c("v", "pk", "sk_nonce", "sk_ct", "sk_tag"))
  expect_equal(st$keys$carol$v, 2)
  expect_identical(nchar(c(st$salt, st$keys$carol$sk_nonce, st$keys$carol$sk_tag)), c(32L, 24L, 32L))
  expect_identical(morie_crypto_keystore_load("carol", pw, path = path)$sk, as.raw(9:1))
  path2 <- tempfile(fileext = ".json")
  on.exit(unlink(path2), add = TRUE)
  morie_crypto_keystore_create(pw, path = path2)
  expect_identical(nchar(rmorie:::.morie_from_json(path2, simplifyVector = FALSE)$salt), 32L)
})

test_that("a key store written by rmorie 1.3.x through the sodium package still opens", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  legacy <- paste0(
    '{"salt": "ff9779c469cafe6d81d3c7df9b3855e28e2734b5278ffe0e811d02b475492ae8", "keys": {"bob": ',
    '{"pk": "0a0b", "sk_nonce": "b20dbf668da6a3a357a22f1a6a1324a0ed2a8e0b62a19259", "sk_ct": ',
    '"868a9be5f8779806e16729873a1b6a459ada3795a1cbeaa8f001d0e08c457bcbac48726669dfaa0337f79e05',
    '6037c9033db7a8ad08aa270c"}}}'
  )
  writeLines(legacy, path)
  out <- morie_crypto_keystore_load("bob", "old pw", path = path)
  expect_identical(out$pk, as.raw(c(10, 11)))
  expect_identical(out$sk, as.raw(1:40))
  expect_error(morie_crypto_keystore_load("bob", "new pw", path = path), "wrong password")
  # a box too short to hold a tag, a nonce of the wrong length: refused, not misread
  expect_null(rmorie:::.morie_secretbox_open_impl(as.raw(1:32), as.raw(1:24), as.raw(1:5)))
  expect_error(rmorie:::.morie_secretbox_open_impl(as.raw(1:32), as.raw(1:12), as.raw(1:20)), "nonce must be 24")
  writeLines(sub('"sk_ct"', '"sk_tag": "00", "sk_ct"', legacy, fixed = TRUE), path)
  expect_error(morie_crypto_keystore_load("bob", "old pw", path = path), "corrupt entry")
})

test_that("the native secretbox opens what the sodium package sealed", {
  skip_if_not_installed("sodium")
  set.seed(3)
  for (len in c(0L, 1L, 31L, 32L, 33L, 200L)) {
    key <- sodium::random(32L)
    nonce <- sodium::random(24L)
    m <- as.raw(sample(0:255, len, TRUE))
    box <- as.raw(sodium::data_encrypt(m, key, nonce = nonce))
    expect_identical(rmorie:::.morie_secretbox_open_impl(key, nonce, box), m)
    box[length(box)] <- xor(box[length(box)], as.raw(1))
    expect_null(rmorie:::.morie_secretbox_open_impl(key, nonce, box))
  }
  expect_identical(
    rmorie:::.morie_scrypt(charToRaw("pw"), as.raw(1:32)),
    sodium::scrypt(charToRaw("pw"), salt = as.raw(1:32), size = 32L)
  )
})
