# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3JJJ2: liboqs-backed post-quantum crypto tests.

test_that("liboqs availability flag is exposed", {
  expect_type(morie_crypto_liboqs_available(), "logical")
  expect_length(morie_crypto_liboqs_available(), 1L)
})

test_that("liboqs version string is non-empty when available", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  v <- morie_crypto_liboqs_version()
  expect_type(v, "character")
  expect_true(nzchar(v))
  expect_match(v, "^[0-9]+\\.[0-9]+")
})

# ========================================== ML-KEM-768

test_that("ML-KEM-768 keypair has FIPS 203 sizes", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kp <- morie_crypto_mlkem768_keygen()
  expect_named(kp, c("pk", "sk"))
  expect_equal(length(kp$pk), 1184L)
  expect_equal(length(kp$sk), 2400L)
  expect_type(kp$pk, "raw")
  expect_type(kp$sk, "raw")
})

test_that("ML-KEM-768 encaps/decaps round-trip yields identical shared secret", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kp <- morie_crypto_mlkem768_keygen()
  e <- morie_crypto_mlkem768_encaps(kp$pk)
  expect_named(e, c("ct", "shared_secret"))
  expect_equal(length(e$ct), 1088L)
  expect_equal(length(e$shared_secret), 32L)
  ss <- morie_crypto_mlkem768_decaps(kp$sk, e$ct)
  expect_equal(ss, e$shared_secret)
})

test_that("ML-KEM-768 decaps with wrong sk fails (implicit-reject silent)", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  # ML-KEM uses Fujisaki-Okamoto implicit rejection: decaps with a
  # wrong sk returns a DIFFERENT shared secret (no error). The
  # contract is that wrong-key decap MUST NOT equal the encap secret.
  kpA <- morie_crypto_mlkem768_keygen()
  kpB <- morie_crypto_mlkem768_keygen()
  e <- morie_crypto_mlkem768_encaps(kpA$pk)
  ssA <- morie_crypto_mlkem768_decaps(kpA$sk, e$ct)
  ssB <- morie_crypto_mlkem768_decaps(kpB$sk, e$ct)
  expect_equal(ssA, e$shared_secret)
  expect_false(identical(ssB, e$shared_secret))
})

test_that("ML-KEM-768 enforces pk/sk/ct sizes", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  expect_error(morie_crypto_mlkem768_encaps(raw(1183)),
                regexp = "pk must be 1184 bytes")
  expect_error(morie_crypto_mlkem768_decaps(raw(2399), raw(1088)),
                regexp = "size mismatch")
  expect_error(morie_crypto_mlkem768_decaps(raw(2400), raw(1087)),
                regexp = "size mismatch")
})

test_that("Two ML-KEM-768 keygens produce different keys (CSPRNG sanity)", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  k1 <- morie_crypto_mlkem768_keygen()
  k2 <- morie_crypto_mlkem768_keygen()
  expect_false(identical(k1$pk, k2$pk))
  expect_false(identical(k1$sk, k2$sk))
})

# ========================================== ML-DSA-65

test_that("ML-DSA-65 keypair has FIPS 204 sizes", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kp <- morie_crypto_mldsa65_keygen()
  expect_named(kp, c("pk", "sk"))
  expect_equal(length(kp$pk), 1952L)
  expect_equal(length(kp$sk), 4032L)
})

test_that("ML-DSA-65 sign + verify round-trip succeeds", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kp <- morie_crypto_mldsa65_keygen()
  msg <- charToRaw("morie research-stack signed payload v1")
  sig <- morie_crypto_mldsa65_sign(kp$sk, msg)
  expect_type(sig, "raw")
  # ML-DSA-65 signatures vary in length but stay <= 3309 bytes.
  expect_true(length(sig) > 0L && length(sig) <= 3309L)
  expect_true(morie_crypto_mldsa65_verify(kp$pk, msg, sig))
})

test_that("ML-DSA-65 verify rejects modified message", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kp <- morie_crypto_mldsa65_keygen()
  msg <- charToRaw("original")
  tampered <- charToRaw("oriGinal")
  sig <- morie_crypto_mldsa65_sign(kp$sk, msg)
  expect_true(morie_crypto_mldsa65_verify(kp$pk, msg, sig))
  expect_false(morie_crypto_mldsa65_verify(kp$pk, tampered, sig))
})

test_that("ML-DSA-65 verify rejects wrong pk", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kpA <- morie_crypto_mldsa65_keygen()
  kpB <- morie_crypto_mldsa65_keygen()
  msg <- charToRaw("payload")
  sig <- morie_crypto_mldsa65_sign(kpA$sk, msg)
  expect_true(morie_crypto_mldsa65_verify(kpA$pk, msg, sig))
  expect_false(morie_crypto_mldsa65_verify(kpB$pk, msg, sig))
})

test_that("ML-DSA-65 verify rejects modified signature", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  kp <- morie_crypto_mldsa65_keygen()
  sig <- morie_crypto_mldsa65_sign(kp$sk, charToRaw("x"))
  bad <- sig
  bad[1] <- as.raw(bitwXor(as.integer(bad[1]), 0x01L))
  expect_false(morie_crypto_mldsa65_verify(kp$pk, charToRaw("x"), bad))
})

test_that("ML-DSA-65 enforces pk/sk sizes", {
  if (!morie_crypto_liboqs_available()) skip("no liboqs")
  expect_error(morie_crypto_mldsa65_sign(raw(4031), charToRaw("x")),
                regexp = "sk must be 4032 bytes")
  expect_error(morie_crypto_mldsa65_verify(raw(1951), charToRaw("x"),
                                              raw(100)),
                regexp = "pk must be 1952 bytes")
})
