# HKDF-SHA256 (RFC 5869): test case 1 of the RFC, and extract/expand
# recomputed with openssl's HMAC-SHA256.

kd_hex <- function(s) as.raw(strtoi(substring(s, seq(1, nchar(s), 2), seq(2, nchar(s), 2)), 16L))
kd_hmac <- function(k, m) as.raw(openssl::sha256(m, key = k))
kd_ikm <- as.raw(rep(0x0b, 22))
kd_salt <- as.raw(0:12)
kd_info <- as.raw(0xf0:0xf9)

test_that("HKDF reproduces RFC 5869 test case 1", {
  prk <- "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"
  okm <- paste0("3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf",
                "34007208d5b887185865")
  r <- morie_seckdf_hkdf(kd_ikm, kd_salt, kd_info, 42)
  expect_identical(r$prk_hex, prk)
  expect_identical(r$okm_hex, okm)
  expect_identical(r$blocks, 2L)
  h <- hkdf(kd_ikm, kd_salt, kd_info, 42)
  expect_identical(tolower(h$okm_hex), okm)
  expect_identical(tolower(h$prk_hex), prk)
})

test_that("extract is HMAC(salt, IKM) and expand chains T(i) = HMAC(PRK, T(i-1) | info | i)", {
  skip_if_not_installed("openssl")
  e <- morie_seckdf_extract(kd_ikm, kd_salt)
  expect_identical(e$prk, kd_hmac(kd_salt, kd_ikm))
  expect_identical(morie_seckdf_extract(kd_ikm)$prk, kd_hmac(raw(32), kd_ikm))
  expect_identical(extract(kd_ikm)$prk, kd_hmac(raw(32), kd_ikm))
  t1 <- kd_hmac(e$prk, c(kd_info, as.raw(1)))
  t2 <- kd_hmac(e$prk, c(t1, kd_info, as.raw(2)))
  t3 <- kd_hmac(e$prk, c(t2, kd_info, as.raw(3)))
  x <- morie_seckdf_expand(e$prk, kd_info, 70)
  expect_identical(x$okm, c(t1, t2, t3)[1:70])
  expect_identical(x$blocks, 3L)
  expect_identical(expand(e$prk, kd_info, 70)$okm, x$okm)
  expect_error(morie_seckdf_expand(e$prk, kd_info, 0), "positive")
  expect_error(morie_seckdf_expand(e$prk, kd_info, 255 * 32 + 1), "exceeds 255")
  expect_error(morie_seckdf_expand(as.raw(1:10), kd_info), "shorter than the hash length")
  expect_error(expand(as.raw(1:10), kd_info), "shorter than the hash length")
  sk <- morie_seckdf_hkdf(e$prk, info = kd_info, length = 16, skip_extract = TRUE)
  expect_identical(sk$okm, t1[1:16])
  expect_true(sk$extract_skipped)
  expect_identical(hkdf(e$prk, info = kd_info, length = 16, skip_extract = TRUE)$okm, t1[1:16])
  expect_identical(morie_seckdf(kd_ikm, kd_salt)$prk, e$prk)
})

test_that("context keys share one PRK and differ by info", {
  skip_if_not_installed("openssl")
  ctx <- c("enc", "mac", "iv")
  r <- morie_seckdf_derive_context_keys(kd_ikm, ctx, kd_salt, length = 16)
  prk <- kd_hmac(kd_salt, kd_ikm)
  for (c in ctx) expect_identical(r$keys[[c]], kd_hmac(prk, c(charToRaw(c), as.raw(1)))[1:16])
  expect_true(r$all_distinct)
  r2 <- derive_context_keys(kd_ikm, ctx, kd_salt, length = 16)
  expect_identical(r2$keys, r$keys)
})
