# VQ-GAN decoder side (Esser, Rombach & Ommer 2021): exact index lookup,
# the adaptive adversarial weight, patch-wise discrimination and the
# sliding-window cover of a latent grid.

vq_cb <- rbind(c(0, 1), c(2, -1), c(0.5, 0.5))

test_that("decoding indices is an exact 0-based codebook lookup", {
  d <- morie_vqgdec_decode_indices(c(2, 0, 2), vq_cb)
  expect_identical(d$codes, list(vq_cb[3, ], vq_cb[1, ], vq_cb[3, ]))
  expect_identical(d$n, 3L)
  expect_identical(morie_vqgdec_decode_indices(0, list(c(1, 2), c(3, 4)))$codes, list(c(1, 2)))
  expect_error(morie_vqgdec_decode_indices(3, vq_cb), "index 3 is outside a codebook of 3")
  # nearest-code quantisation followed by lookup is the identity on codes
  q <- apply(vq_cb, 1, function(z) which.min(colSums((t(vq_cb) - z)^2)) - 1L)
  expect_identical(do.call(rbind, morie_vqgdec_decode_indices(q, vq_cb)$codes), vq_cb)
})

test_that("the adaptive weight is |g_rec| / (|g_gan| + delta), clipped", {
  a <- morie_vqgdec_adaptive_weight(-0.3, 0.05, delta = 1e-3)
  expect_equal(a$lambda, 0.3 / 0.051, tolerance = 1e-15)
  expect_false(a$clipped)
  b <- morie_vqgdec_adaptive_weight(2, 0, delta = 1e-6, clip = 100)
  expect_identical(b$lambda, 100)
  expect_equal(b$raw, 2e6, tolerance = 1e-12)
  expect_true(b$clipped)
  expect_error(morie_vqgdec_adaptive_weight(1, 1, delta = 0), "delta must be positive")
})

test_that("the patch discriminator scores each p x p block", {
  img <- matrix(1:24, 4, 6)
  r <- morie_vqgdec_patch_discriminator(img, patch = 2)
  ref <- outer(1:2, 1:3, Vectorize(function(i, j) mean(img[2 * i - (1:0), 2 * j - (1:0)])))
  expect_equal(do.call(rbind, r$scores), ref, tolerance = 1e-15)
  expect_identical(r$n_patches, 6L)
  expect_equal(r$mean, mean(ref), tolerance = 1e-15)
  mx <- morie_vqgdec_patch_discriminator(img, 2, scorer = max)
  expect_equal(mx$scores[[2]], c(8, 16, 24))
  expect_error(morie_vqgdec_patch_discriminator(img, 4), "does not tile")
})

test_that("sliding windows are the product of the row and column starts", {
  starts <- function(n, w, s) sort(unique(c(seq(0, n - w, by = s), n - w)))
  for (cfg in list(c(10, 7, 4, 2), c(8, 8, 4, 4), c(9, 6, 3, 2), c(5, 12, 5, 3))) {
    r <- morie_vqgdec_sliding_windows(cfg[1], cfg[2], cfg[3], cfg[4])
    g <- expand.grid(j = starts(cfg[2], cfg[3], cfg[4]), i = starts(cfg[1], cfg[3], cfg[4]))
    expect_identical(do.call(rbind, r$windows), unname(cbind(as.integer(g$i), as.integer(g$j))))
    expect_true(r$covers_everything)
    expect_equal(r$context, cfg[3] * cfg[3])
  }
  expect_identical(morie_vqgdec_sliding_windows(4, 4, 2)$n_windows, 4L)
  expect_error(morie_vqgdec_sliding_windows(4, 4, 5), "must fit")
})

test_that("decode looks up the codes, applies the generator and the weight", {
  r <- morie_vqgdec_decode(c(1, 2), vq_cb, generator = function(z) do.call(rbind, z) * 2,
                           grad_rec = 0.4, grad_gan = 0.1)
  expect_equal(r$image, rbind(vq_cb[2, ], vq_cb[3, ]) * 2)
  expect_equal(r$adaptive_lambda, 0.4 / (0.1 + 1e-6), tolerance = 1e-15)
  plain <- morie_vqgdec(c(0), vq_cb)
  expect_identical(plain$image, list(vq_cb[1, ]))
  expect_null(plain$adaptive_lambda)
  expect_match(morie_vqgdec_cheatsheet(), "PATCH-BASED", fixed = TRUE)
})
