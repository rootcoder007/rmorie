# Coverage tests for R/dits16_native.R (Peebles and Xie 2023): patch
# tokens, Gflop accounting, adaLN-Zero modulation, the DiT block and the
# scaling comparison.

test_that("patch grid and Gflops", {
  p <- patch_grid(32, 4)
  expect_equal(c(p$tokens, p$grid), c(64L, 8L))
  expect_identical(morie_dits16, patch_grid)
  expect_error(patch_grid(32, 0), "must be positive")
  expect_error(patch_grid(30, 4), "does not divide")
  g <- gflops(64, 12, 384)
  attn <- 4 * 64 * 384^2 + 2 * 64^2 * 384
  mlp <- 8 * 64 * 384^2
  expect_equal(g$gflops, 12 * (attn + mlp) / 1e9)
  expect_equal(g$attention_share, attn / (attn + mlp))
  expect_error(gflops(0, 1, 1), "must be positive")
})

test_that("adaLN-Zero normalises, scales, shifts and gates", {
  h <- c(0.5, -1, 2, 0.3)
  cc <- c(1, -0.5)
  Ws <- matrix(1:8 / 10, 4)
  Wb <- matrix(-(1:8) / 20, 4)
  Wa <- matrix(0, 4, 2)
  r <- adaln_zero(cc, h, Ws, Wb, Wa)
  nrm <- (h - mean(h)) / sqrt(mean((h - mean(h))^2) + 1e-6)
  expect_equal(r$modulated, nrm * (1 + as.numeric(Ws %*% cc)) + as.numeric(Wb %*% cc), tolerance = 1e-12)
  expect_true(r$identity_at_init)
  expect_false(adaln_zero(cc, h, Ws, Wb, Ws)$identity_at_init)
  expect_error(adaln_zero(cc, h, Ws[1:3, ], Wb, Wa), "3 rows for 4 channels")
})

test_that("the DiT block is the identity at initialisation and gated after", {
  h <- c(0.5, -1, 2, 0.3)
  cc <- c(1, -0.5)
  Ws <- matrix(1:8 / 10, 4)
  Wb <- matrix(-(1:8) / 20, 4)
  Z <- matrix(0, 4, 2)
  att <- function(x) rev(x)
  mlp <- function(x) tanh(x)
  z <- dit_block(h, cc, att, mlp, Ws, Wb, Z, Ws, Wb, Z)
  expect_equal(z$output, h)
  expect_true(z$identity_at_init)
  Wa <- matrix(c(0.2, 0.1, -0.3, 0.4, 0, 0.5, 0.1, -0.2), 4)
  b <- dit_block(h, cc, att, mlp, Ws, Wb, Wa, Wb, Ws, Wa)
  a1 <- adaln_zero(cc, h, Ws, Wb, Wa)
  h1 <- h + a1$gate * att(a1$modulated)
  a2 <- adaln_zero(cc, h1, Wb, Ws, Wa)
  expect_equal(b$output, h1 + a2$gate * mlp(a2$modulated), tolerance = 1e-12)
  expect_false(b$identity_at_init)
})

test_that("scaling comparison ranks configurations by Gflops", {
  cf <- list(list("XL/2", 32, 2, 28, 1152), list("B/4", 32, 4, 12, 768), list("S/8", 32, 8, 12, 384))
  r <- scaling_comparison(cf)
  expect_equal(vapply(r$ranked, `[[`, "", "name"), c("S/8", "B/4", "XL/2"))
  expect_equal(r$ranked[[3]]$tokens, 256L)
  expect_equal(r$ranked[[3]]$gflops, gflops(256, 28, 1152)$gflops)
  expect_equal(r$ranked[[1]]$parameters, 12 * 18 * 384^2)
})
