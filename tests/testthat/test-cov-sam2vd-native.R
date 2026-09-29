# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sam2vd_native.R (SAM 2 streaming memory, Ravi et al.
# 2024). Two FIFO queues, sinusoidal temporal offsets on recent
# memories only, scaled dot-product cross-attention to memories and
# object pointers, and an empty memory at frame 0 are all recomputed.

test_that("memory_bank and push_memory keep two FIFO queues and pointers", {
  b <- morie_sam2vd_memory_bank(2, 1)
  for (t in 0:3) b <- morie_sam2vd_push_memory(b, t, c(t, t), object_pointer = c(1, t))
  expect_equal(vapply(b$recent, function(e) e$frame, 0L), 2:3)
  expect_length(b$pointers, 3L)
  expect_equal(b$pointers[[1]]$frame, 1L)
  b <- morie_sam2vd_push_memory(b, 9, c(9, 9), prompted = TRUE)
  b <- morie_sam2vd_push_memory(b, 10, c(10, 10), prompted = TRUE)
  expect_equal(vapply(b$prompted, function(e) e$frame, 0L), 10L)
  expect_equal(vapply(b$recent, function(e) e$frame, 0L), 2:3)
  expect_error(morie_sam2vd_memory_bank(0, 1), ">= 1")
})

test_that("temporal_embedding adds sin(scale * d * i) to recent memories only", {
  e <- list(frame = 2L, features = c(1, 2, 3), prompted = FALSE)
  r <- morie_sam2vd_temporal_embedding(e, 5, scale = 0.2)
  expect_equal(r$features, c(1, 2, 3) + sin(0.2 * 3 * (1:3)), tolerance = 1e-12)
  expect_equal(r$distance, 3L)
  r2 <- morie_sam2vd_temporal_embedding(e, 5, dim = 2)
  expect_equal(r2$features, c(1 + sin(0.3), 2 + sin(0.6), 3), tolerance = 1e-12)
  p <- morie_sam2vd_temporal_embedding(list(frame = 0L, features = 1:2, prompted = TRUE), 9)
  expect_false(p$embedded)
  expect_equal(p$features, 1:2)
})

test_that("memory_attention adds softmax-weighted memories", {
  b <- morie_sam2vd_memory_bank(3, 1)
  b <- morie_sam2vd_push_memory(b, 0, c(1, 0), prompted = TRUE)
  b <- morie_sam2vd_push_memory(b, 1, c(0, 1), object_pointer = c(0.5, 0.5))
  b <- morie_sam2vd_push_memory(b, 2, c(1, 1), object_pointer = c(1, 2, 3))
  x <- c(0.3, -0.2)
  mem <- list(c(1, 0), c(0, 1) + sin(0.1 * 2 * (1:2)), c(1, 1) + sin(0.1 * (1:2)), c(0.5, 0.5))
  M <- do.call(rbind, mem)
  s <- as.numeric(M %*% x) / sqrt(2)
  w <- exp(s - max(s)) / sum(exp(s - max(s)))
  r <- morie_sam2vd_memory_attention(x, b, 3)
  expect_equal(r$weights, w, tolerance = 1e-12)
  expect_equal(r$features, x + as.numeric(t(M) %*% w), tolerance = 1e-12)
  expect_equal(r$n_memories, 4L)
  np <- morie_sam2vd_memory_attention(x, b, 3, include_pointers = FALSE)
  expect_equal(np$n_memories, 3L)
  y1 <- r$features
  s2 <- as.numeric(M %*% y1) / sqrt(2)
  w2 <- exp(s2 - max(s2)) / sum(exp(s2 - max(s2)))
  expect_equal(morie_sam2vd_memory_attention(x, b, 3, n_blocks = 2)$features,
               y1 + as.numeric(t(M) %*% w2), tolerance = 1e-12)
  e0 <- morie_sam2vd_memory_attention(x, morie_sam2vd_memory_bank(), 0)
  expect_false(e0$attended)
  expect_equal(e0$features, x)
  expect_error(morie_sam2vd_memory_attention(c(1, 2, 3), b, 3), "width 2 but the frame has 3")
})

test_that("propagate: frame 0 is plain SAM, later frames attend to memory", {
  frames <- list(c(1, 0), c(0.5, 0.5), c(0, 1))
  enc <- function(f) f * 2
  dec <- function(feat, prompt) sum(feat) + if (is.null(prompt)) 0 else prompt
  for (fn in list(morie_sam2vd_propagate, morie_sam2vd, morie_sam2vd_sam2video, morie_sam2vd_sam2_video_propagation)) {
    r <- fn(frames, enc, dec, prompts = list("1" = 100))
    expect_true(r$first_frame_is_sam)
    expect_equal(r$masks[[1]], 2)
    expect_equal(unlist(r$conditioned), c(FALSE, TRUE, TRUE))
    b <- morie_sam2vd_memory_bank()
    b <- morie_sam2vd_push_memory(b, 0, c(2, 0), object_pointer = c(2, 0))
    a1 <- morie_sam2vd_memory_attention(c(1, 1), b, 1)
    expect_equal(r$masks[[2]], sum(a1$features) + 100, tolerance = 1e-12)
  }
})

test_that("morie_sam2vd_cheatsheet names the two queues", {
  expect_match(morie_sam2vd_cheatsheet(), "TWO FIFO queues", fixed = TRUE)
})
