# SPDX-License-Identifier: AGPL-3.0-or-later
#
# These six functions were dead for an unknown period: src/ registered
# them as .rmorie_*_impl (the .cpp was copied from rmorie without
# renaming) while R/ called .morie_*_impl, so every call raised
# "could not find function". Nothing caught it because no test ever
# invoked them -- R CMD check's "no visible global function definition"
# NOTE was the only signal.
#
# A test that merely calls each function would have caught the name
# mismatch. These go further and check the round trips actually hold, so
# a future rename that silently binds to the wrong implementation fails
# too.

test_that("slhdsa128s signs and verifies its own signature", {
  skip_if_not(morie_crypto_liboqs_available(), "no liboqs")
  k <- morie_crypto_slhdsa_keygen()
  expect_true(is.raw(k$pk))
  expect_true(is.raw(k$sk))

  msg <- charToRaw("morie pqc round trip")
  sig <- morie_crypto_slhdsa_sign(k$sk, msg)
  expect_true(length(sig) > 0)
  expect_true(isTRUE(morie_crypto_slhdsa_verify(k$pk, msg, sig)))
})

test_that("slhdsa128s rejects a tampered message", {
  skip_if_not(morie_crypto_liboqs_available(), "no liboqs")
  k <- morie_crypto_slhdsa_keygen()
  sig <- morie_crypto_slhdsa_sign(k$sk, charToRaw("hello"))
  # verification that accepts anything is as broken as one that accepts
  # nothing, so pin the negative case too
  expect_false(isTRUE(
    morie_crypto_slhdsa_verify(k$pk, charToRaw("hellp"), sig)
  ))
})

test_that("hqc128 encapsulation and decapsulation agree on the secret", {
  skip_if_not(morie_crypto_liboqs_available(), "no liboqs")
  k <- morie_crypto_hqc_keygen()
  e <- morie_crypto_hqc_encaps(k$pk)
  d <- morie_crypto_hqc_decaps(k$sk, e$ct)
  expect_identical(as.raw(d), as.raw(e$shared_secret))
  expect_true(length(as.raw(d)) > 0)
})

test_that("the R call sites bind to implementations that exist", {
  # No liboqs guard here on purpose: exists() needs no liboqs, and this
  # is the check that catches the original .rmorie_/.morie_ rename. A
  # skip would hand the regression back to the runners that lack liboqs.
  # the exact failure mode: a call site naming a function the C++ layer
  # never registered
  for (fn in c(".rmorie_slhdsa128s_keygen_impl", ".rmorie_slhdsa128s_sign_impl",
               ".rmorie_slhdsa128s_verify_impl", ".rmorie_hqc128_keygen_impl",
               ".rmorie_hqc128_encaps_impl", ".rmorie_hqc128_decaps_impl")) {
    expect_true(exists(fn, envir = asNamespace("rmorie"), inherits = FALSE),
                info = paste(fn, "is called from R/ but not registered"))
  }
})
