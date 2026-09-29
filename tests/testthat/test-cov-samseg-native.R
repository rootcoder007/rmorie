# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/samseg_native.R (Segment Anything, Kirillov et al.
# 2023): sparse prompts as positional encodings plus a per-type
# embedding, dense mask prompts summed into the image embedding, the
# amortised cost of one image encoding, and the promptable interface.

.ss_pe <- function(x, y, dim = 8) {
  f <- 2^(0:(dim / 2 - 1)) * pi
  as.numeric(rbind(sin(f * x), cos(f * y)))
}

test_that("encode_point_prompt adds the foreground / background embedding", {
  pts <- list(c(0.2, 0.7), c(0.5, 0.5))
  r <- encode_point_prompt(pts, c(1, 0))
  expect_equal(r$tokens[[1]], .ss_pe(0.2, 0.7), tolerance = 1e-12)
  expect_equal(r$tokens[[2]], .ss_pe(0.5, 0.5), tolerance = 1e-12)
  expect_equal(r$n_prompts, 2L)
  expect_true(r$sparse)
  te <- list(foreground = rep(1, 8), background = rep(-1, 8))
  t2 <- encode_point_prompt(pts, c(1, 0), type_embeddings = te)
  expect_equal(t2$tokens[[1]], .ss_pe(0.2, 0.7) + 1, tolerance = 1e-12)
  expect_equal(t2$tokens[[2]], .ss_pe(0.5, 0.5) - 1, tolerance = 1e-12)
  # a background click at the same place is a different token
  same <- encode_point_prompt(list(c(0.2, 0.7), c(0.2, 0.7)), c(1, 0), type_embeddings = te)
  expect_false(isTRUE(all.equal(same$tokens[[1]], same$tokens[[2]])))
  expect_length(encode_point_prompt(pts[1], 1, dim = 4)$tokens[[1]], 4L)
  expect_error(encode_point_prompt(pts, 1), "2 points but 1 labels")
  expect_error(encode_point_prompt(pts, c(1, 2)), "must be 1 \\(foreground\\) or 0")
  expect_error(encode_point_prompt(pts, c(1, 0), type_embeddings = list(foreground = 1)),
               "wrong width")
})

test_that("encode_box_prompt encodes the two corners", {
  b <- encode_box_prompt(c(0.1, 0.2, 0.8, 0.9))
  expect_equal(b$tokens[[1]], .ss_pe(0.1, 0.2), tolerance = 1e-12)
  expect_equal(b$tokens[[2]], .ss_pe(0.8, 0.9), tolerance = 1e-12)
  expect_equal(b$n_prompts, 2L)
  te <- list(box_tl = rep(2, 8), box_br = rep(3, 8))
  t2 <- encode_box_prompt(c(0.1, 0.2, 0.8, 0.9), type_embeddings = te)
  expect_equal(t2$tokens[[1]], .ss_pe(0.1, 0.2) + 2, tolerance = 1e-12)
  expect_equal(t2$tokens[[2]], .ss_pe(0.8, 0.9) + 3, tolerance = 1e-12)
  expect_error(encode_box_prompt(c(0.8, 0.2, 0.1, 0.9)), "empty or inverted")
  expect_error(encode_box_prompt(c(0.1, 0.9, 0.8, 0.9)), "empty or inverted")
})

test_that("encode_mask_prompt sums the dense prompt into the image embedding", {
  E <- matrix(1:6, 2, 3)
  M <- matrix(c(0, 1, 0, 1, 0, 1), 2, 3)
  r <- encode_mask_prompt(M, E, weight = 2)
  expect_equal(r$embedding, E + 2 * M)
  expect_false(r$sparse)
  expect_equal(encode_mask_prompt(list(c(0, 0, 0), c(1, 1, 1)), E)$embedding,
               E + rbind(rep(0, 3), rep(1, 3)))
  expect_error(encode_mask_prompt(matrix(0, 3, 3), E), "3x3 but the image embedding is 2x3")
})

test_that("amortised_cost charges the encoder once", {
  r <- amortised_cost(500, 50, 10)
  expect_equal(r$total_ms, 500 + 10 * 50)
  expect_equal(r$per_prompt_ms, 100)
  expect_equal(r$naive_ms, 10 * 550)
  expect_equal(r$speedup, 5500 / 1000)
  expect_true(r$interactive)
  expect_false(amortised_cost(500, 150, 3)$interactive)
  expect_equal(amortised_cost(500, 50, 1)$speedup, 1)
  expect_error(amortised_cost(500, 50, 0), "at least one prompt")
  expect_error(amortised_cost(0, 50, 2), "timings must be positive")
})

test_that("promptable_segment returns the decoder's masks and needs at least one", {
  dec <- function(emb, tok, multi) if (multi) list("a", "b", "c") else list("a")
  for (fn in list(promptable_segment, segmentanything, sam_segment, samsegment, morie_samseg)) {
    r <- fn(matrix(0, 2, 2), list(rep(0, 8)), dec)
    expect_equal(r$n_masks, 3L)
    expect_identical(r$estimate, "a")
    expect_true(r$multimask)
  }
  one <- promptable_segment(matrix(0, 2, 2), list(), dec, multimask = FALSE)
  expect_equal(one$n_masks, 1L)
  expect_false(one$multimask)
  expect_error(promptable_segment(matrix(0, 2, 2), list(), function(e, t, m) list()),
               "requires a valid mask for ANY prompt")
})
