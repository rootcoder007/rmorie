# SAM mask decoder (Kirillov et al. 2023): two-way attention, nearest
# upsampling, the dynamic mask head, focal (Lin et al. 2017) and dice
# (Milletari et al. 2016) losses, recomputed with matrix algebra.

sd_attend <- function(Q, K, V) {
  S <- Q %*% t(K) / sqrt(ncol(Q))
  W <- exp(S - apply(S, 1, max))
  W <- W / rowSums(W)
  list(out = W %*% V, W = W)
}
sd_P <- rbind(c(0.1, 0.2, -0.3), c(0.4, -0.1, 0.2))
sd_I <- rbind(c(0.5, 0.1, 0), c(0.2, 0.2, 0.3), c(-0.3, 0.4, 0.1), c(0.1, 0.1, -0.2),
              c(0, -0.5, 0.2), c(0.3, 0.3, 0.3))

test_that("the two-way block updates prompts and image in both directions", {
  r <- two_way_block(sd_P, sd_I)
  P1 <- sd_P + sd_attend(sd_P, sd_P, sd_P)$out
  a <- sd_attend(P1, sd_I, sd_I)
  P2 <- P1 + a$out
  b <- sd_attend(sd_I, P2, P2)
  expect_equal(r$prompt_tokens, P2, tolerance = 1e-14)
  expect_equal(r$image_tokens, sd_I + b$out, tolerance = 1e-14)
  expect_equal(r$prompt_to_image, a$W, tolerance = 1e-14)
  expect_equal(r$image_to_prompt, b$W, tolerance = 1e-14)
  expect_error(two_way_block(sd_P, sd_I[, 1:2]), "dimensional")
})

test_that("upsampling repeats each cell f times in both directions", {
  G <- matrix(1:6, 2)
  expect_equal(upsample(G, 2), G[c(1, 1, 2, 2), c(1, 1, 2, 2, 3, 3)])
  expect_equal(upsample(list(c(1, 2), c(3, 4)), 2),
               list(c(1, 1, 2, 2), c(1, 1, 2, 2), c(3, 3, 4, 4), c(3, 3, 4, 4)))
  cells <- list(list(c(1, 0), c(0, 1)), list(c(2, 2), c(3, 3)))
  up <- upsample(cells, 2)
  expect_length(up, 4L)
  expect_identical(up[[3]][[4]], c(3, 3))
  expect_identical(up[[2]][[1]], c(1, 0))
  expect_error(upsample(G, 0), ">= 1")
})

test_that("the dynamic head scores every location with the prompt's weights", {
  w <- c(0.5, -1, 2)
  cells <- lapply(1:2, function(i) lapply(1:3, function(j) sd_I[(i - 1) * 3 + j, ]))
  h <- dynamic_mask_head(w, cells)
  ref <- matrix(as.numeric(sd_I %*% w), 2, byrow = TRUE)
  expect_equal(h$logits, ref, tolerance = 1e-15)
  expect_equal(h$probability, stats::plogis(ref), tolerance = 1e-15)
  sq <- sd_I[1:4, ]
  hm <- dynamic_mask_head(w, sq, mlp = function(v) 2 * v)
  expect_equal(hm$logits, matrix(as.numeric(sq %*% (2 * w)), 2, byrow = TRUE), tolerance = 1e-15)
  expect_error(dynamic_mask_head(1:2, cells), "2-wide")
})

test_that("focal and dice losses follow their definitions", {
  p <- c(0.9, 0.2, 0.6, 0.05)
  y <- c(1, 0, 0, 1)
  pt <- ifelse(y == 1, p, 1 - p)
  at <- ifelse(y == 1, 0.25, 0.75)
  f <- focal_loss(p, y)
  expect_equal(f$loss, mean(-at * (1 - pt)^2 * log(pt)), tolerance = 1e-15)
  expect_equal(f$modulating, (1 - pt)^2, tolerance = 1e-15)
  # gamma = 0, alpha = 0.5 is half the binary cross-entropy
  expect_equal(focal_loss(p, y, gamma = 0, alpha = 0.5)$loss, 0.5 * mean(-log(pt)), tolerance = 1e-15)
  d <- dice_loss(p, y)
  expect_equal(d$dice, 2 * sum(p * y) / (sum(p) + sum(y)), tolerance = 1e-15)
  expect_equal(d$loss, 1 - d$dice, tolerance = 1e-15)
  expect_identical(dice_loss(c(0, 0), c(0, 0))$dice, 1)
  expect_error(focal_loss(p, y[-1]), "differ in size")
  expect_error(dice_loss(p, y[-1]), "differ in size")
})

test_that("decode_mask runs the blocks, upsamples and applies the head", {
  r <- decode_mask(sd_P, sd_I, c(2, 3), n_blocks = 2, upsample_factor = 2, output_index = 1)
  P <- sd_P
  I <- sd_I
  for (b in 1:2) {
    t2 <- two_way_block(P, I)
    P <- t2$prompt_tokens
    I <- t2$image_tokens
  }
  L <- matrix(as.numeric(I %*% P[2, ]), 2, byrow = TRUE)
  Lup <- L[c(1, 1, 2, 2), rep(1:3, each = 2)]
  expect_equal(r$logits, Lup, tolerance = 1e-14)
  expect_equal(r$mask, stats::plogis(Lup), tolerance = 1e-14)
  expect_identical(r$shape, c(4L, 6L))
  expect_equal(morie_samdec(sd_P, sd_I, c(2, 3))$mask, sam_mask_decoder(sd_P, sd_I, c(2, 3))$mask)
  expect_error(decode_mask(sd_P, sd_I, c(2, 2)), "do not fill")
})
