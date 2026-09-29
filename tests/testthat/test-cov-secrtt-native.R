# Coverage for envelope encryption (DEK per record, KEK above it). The
# AEAD is pinned to the RFC 8439 section 2.8.2 test vector, the DEK
# derivation is recomputed as RFC 5869 HKDF over openssl's HMAC-SHA256,
# and the wrap / rotate / shred operations are checked by their
# round-trip and authentication properties.

.hx <- function(s) as.raw(strtoi(substring(s, seq(1, nchar(s), 2), seq(2, nchar(s), 2)), 16L))
.hkdf <- function(ikm, salt, info, len) {
  if (is.null(salt)) salt <- as.raw(rep(0, 32))
  prk <- as.raw(openssl::sha256(ikm, key = salt))
  out <- raw(0)
  t <- raw(0)
  i <- 1L
  while (length(out) < len) {
    t <- as.raw(openssl::sha256(c(t, info, as.raw(i)), key = prk))
    out <- c(out, t)
    i <- i + 1L
  }
  out[seq_len(len)]
}
.kek1 <- as.raw(1:32)
.kek2 <- as.raw(33:64)
.non <- function(k) as.raw(c(rep(0, 11), k))

test_that("the AEAD under the envelope reproduces RFC 8439 section 2.8.2", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)), "no libsodium")
  key <- as.raw(0x80:0x9f)
  nonce <- .hx("070000004041424344454647")
  aad <- .hx("50515253c0c1c2c3c4c5c6c7")
  pt <- charToRaw("Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it.")
  ct <- .hx(paste0("d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b",
                   "1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc",
                   "3ff4def08e4b7a9de576d26586cec64b6116"))
  s <- morie_secrtt_seal_record(pt, key, nonce, aad)
  expect_identical(as.raw(s$ciphertext), ct)
  expect_identical(as.raw(s$tag), .hx("1ae10b594f09e26a7e902ecbd0600691"))
  expect_identical(morie_secrtt_open_record(s, key), pt)
  s2 <- seal_record(pt, key, nonce, aad)
  expect_identical(as.raw(s2$ciphertext), ct)
  expect_identical(as.raw(open_record(s2, key)), pt)
  bad <- s
  bad$tag <- as.raw(bad$tag)
  bad$tag[1] <- as.raw(bitwXor(as.integer(bad$tag[1]), 1L))
  expect_error(morie_secrtt_open_record(bad, key), "failed authentication")
  bad2 <- s
  bad2$aad <- raw(0)
  expect_error(morie_secrtt_open_record(bad2, key), "failed authentication")
})

test_that("per-record DEKs are HKDF-SHA256 over 'dek:' || record id", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)), "no libsodium")
  skip_if_not_installed("openssl")
  seed <- charToRaw("master-seed-for-tests-0123456789")
  rid <- charToRaw("rec-7")
  d <- morie_secrtt_generate_dek(seed, rid)
  expect_identical(as.raw(d$dek), .hkdf(seed, NULL, c(charToRaw("dek:"), rid), 32))
  salt <- as.raw(5:20)
  ds <- morie_secrtt_generate_dek(seed, rid, salt = salt)
  expect_identical(as.raw(ds$dek), .hkdf(seed, salt, c(charToRaw("dek:"), rid), 32))
  expect_false(identical(d$dek, morie_secrtt_generate_dek(seed, charToRaw("rec-8"))$dek))
  expect_identical(morie_secrtt(seed, rid)$dek_hex, d$dek_hex)
})

test_that("wrapping binds the KEK id and unwrapping authenticates it", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)), "no libsodium")
  dek <- as.raw(100:131)
  w <- morie_secrtt_wrap_dek(dek, .kek1, .non(1), kek_id = "kek-A", aad = charToRaw("tenant"))
  e <- morie_secaead_aead_encrypt(.kek1, .non(1), dek, c(charToRaw("tenant"), charToRaw("kek-A")))
  expect_identical(as.raw(w$wrapped), as.raw(e$ciphertext))
  expect_identical(as.raw(w$tag), as.raw(e$tag))
  u <- morie_secrtt_unwrap_dek(w, .kek1, audit_log = list())
  expect_identical(as.raw(u$dek), dek)
  expect_length(u$audit_log, 1L)
  expect_true(u$audit_log[[1]]$ok)
  w2 <- w
  w2$kek_id <- "kek-B"
  expect_error(morie_secrtt_unwrap_dek(w2, .kek1), "failed authentication")
  expect_error(morie_secrtt_unwrap_dek(w, .kek2), "failed authentication")
  expect_error(morie_secrtt_wrap_dek(dek[-1], .kek1, .non(1)), "32 bytes")
  rw <- wrap_dek(dek, .kek1, .non(1), kek_id = "kek-A", aad = charToRaw("tenant"))
  expect_identical(as.raw(rw$wrapped), as.raw(w$wrapped))
  expect_identical(as.raw(unwrap_dek(rw, .kek1)$dek), dek)
  expect_error(wrap_dek(dek[1:5], .kek1, .non(1)), "32 bytes")
})

test_that("KEK rotation re-wraps every DEK and rewrites no record", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)), "no libsodium")
  deks <- list(as.raw(1:32), as.raw(40:71), as.raw(200:231))
  wr <- lapply(seq_along(deks), function(i) morie_secrtt_wrap_dek(deks[[i]], .kek1, .non(i)))
  r <- morie_secrtt_rotate_kek(wr, .kek1, .kek2, lapply(4:6, .non), audit_log = list())
  expect_identical(r$n, 3L)
  expect_identical(r$records_reencrypted, 0L)
  expect_length(r$audit_log, 3L)
  for (i in 1:3) {
    expect_identical(r$wrapped[[i]]$kek_id, "kek-2")
    expect_identical(as.raw(morie_secrtt_unwrap_dek(r$wrapped[[i]], .kek2)$dek), deks[[i]])
    expect_error(morie_secrtt_unwrap_dek(r$wrapped[[i]], .kek1), "failed authentication")
  }
  expect_error(morie_secrtt_rotate_kek(wr, .kek1, .kek2, list(.non(9))), "nonce must never be reused")
  wa <- lapply(seq_along(deks), function(i) wrap_dek(deks[[i]], .kek1, .non(i), aad = charToRaw("t1")))
  rr <- rotate_kek(wa, .kek1, .kek2, lapply(4:6, .non))
  expect_identical(as.raw(unwrap_dek(rr$wrapped[[2]], .kek2)$dek), deks[[2]])
  ra <- morie_secrtt_rotate_kek(lapply(seq_along(deks), function(i) morie_secrtt_wrap_dek(deks[[i]], .kek1, .non(i), aad = charToRaw("t1"))), .kek1, .kek2, lapply(4:6, .non))
  expect_identical(as.raw(morie_secrtt_unwrap_dek(ra$wrapped[[3]], .kek2)$dek), deks[[3]])
  expect_error(rotate_kek(wr, .kek1, .kek2, list()), "nonce must never be reused")
  sh <- morie_secrtt_crypto_shred("kek-2", c(r$wrapped[1:2], wr[3]))
  expect_identical(sh$indices, c(0L, 1L))
  expect_identical(sh$still_recoverable, 2L)
  expect_false(sh$complete)
  expect_true(morie_secrtt_crypto_shred("kek-2", r$wrapped)$complete)
  cs <- crypto_shred("kek-2", c(r$wrapped[1:2], wr[3]))
  expect_identical(cs[c("records_shredded", "indices", "still_recoverable", "complete")], sh[c("records_shredded", "indices", "still_recoverable", "complete")])
})

test_that("DEK rotation re-seals the record under the new key", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)), "no libsodium")
  pt <- charToRaw("patient 42: blood pressure 120/80")
  d1 <- as.raw(1:32)
  d2 <- as.raw(32:1)
  s <- morie_secrtt_seal_record(pt, d1, .non(1), charToRaw("hdr"))
  r <- morie_secrtt_rotate_dek(s, d1, d2, .non(2))
  expect_identical(r$records_reencrypted, 1L)
  expect_identical(morie_secrtt_open_record(r$sealed, d2), pt)
  expect_error(morie_secrtt_open_record(r$sealed, d1), "failed authentication")
  expect_identical(r$sealed$aad, charToRaw("hdr"))
  rr <- rotate_dek(seal_record(pt, d1, .non(1)), d1, d2, .non(3))
  expect_identical(as.raw(open_record(rr$sealed, d2)), pt)
})

test_that("rotation cost compares single-key and envelope rewrite volumes", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)), "no libsodium")
  c1 <- morie_secrtt_rotation_cost(1000, 4096)
  expect_equal(c(c1$single_key_bytes, c1$envelope_kek_bytes, c1$ratio), c(4096000, 32000, 128))
  expect_identical(c1$records_touched_envelope, 0L)
  c2 <- rotation_cost(10, 64, dek_bytes = 16)
  expect_equal(c2$ratio, 4)
  expect_error(morie_secrtt_rotation_cost(0, 10), "must be positive")
  expect_error(rotation_cost(5, 0), "must be positive")
})
