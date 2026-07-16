# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 22 extension — PQC families: hash-based + code-based (+ the
# always-available native Lamport OTS and CSPRNG).

test_that("CSPRNG returns n distinct-looking bytes", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(),
                              error = function(e) FALSE)),
              "libsodium not available")
  a <- morie_crypto_random_bytes(64L)
  b <- morie_crypto_random_bytes(64L)
  expect_length(a, 64L)
  expect_false(identical(a, b))
  expect_error(morie_crypto_random_bytes(0L), "positive")
})

test_that("Lamport OTS: sign/verify roundtrip; tamper + reuse rejected", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(),
                              error = function(e) FALSE)),
              "libsodium not available (CSPRNG)")
  kp <- morie_crypto_lamport_keygen()
  sig <- morie_crypto_lamport_sign(kp, "the die is cast")
  expect_true(morie_crypto_lamport_verify(kp$pk, "the die is cast", sig))
  expect_false(morie_crypto_lamport_verify(kp$pk, "the die is cast!",
                                           sig))
  # tampered reveal fails
  sig2 <- sig
  sig2$reveal[[7]] <- as.raw(rep(0, 32))
  expect_false(morie_crypto_lamport_verify(kp$pk, "the die is cast",
                                           sig2))
  # one-time enforcement
  expect_error(morie_crypto_lamport_sign(kp, "second message"),
               "already signed")
})

test_that("PQC inventory reports all three families", {
  inv <- morie_crypto_pqc_inventory()
  expect_setequal(unique(inv$family),
                  c("lattice", "hash-based", "code-based"))
  sodium_ok <- isTRUE(tryCatch(morie_crypto_sodium_available(),
                               error = function(e) FALSE))
  expect_identical(
    inv$available[inv$primitive == "Lamport OTS (native SHA-256)"],
    sodium_ok)
})

test_that("SLH-DSA + HQC roundtrip when liboqs provides them", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_liboqs_available(),
                              error = function(e) FALSE)),
              "liboqs not available")
  kp <- tryCatch(morie_crypto_slhdsa_keygen(), error = function(e) NULL)
  if (!is.null(kp)) {
    sig <- morie_crypto_slhdsa_sign(kp$sk, "post-quantum")
    expect_true(morie_crypto_slhdsa_verify(kp$pk, "post-quantum", sig))
    expect_false(morie_crypto_slhdsa_verify(kp$pk, "tampered", sig))
  } else {
    skip("this liboqs build lacks SLH-DSA/SPHINCS+")
  }
})

test_that("HQC-128 KEM shared secrets agree", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_liboqs_available(),
                              error = function(e) FALSE)),
              "liboqs not available")
  kp <- tryCatch(morie_crypto_hqc_keygen(), error = function(e) NULL)
  if (is.null(kp)) skip("this liboqs build lacks HQC")
  enc <- morie_crypto_hqc_encaps(kp$pk)
  ss <- morie_crypto_hqc_decaps(kp$sk, enc$ct)
  expect_identical(ss, enc$shared_secret)
})
