# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/llavx_native.R (LLaVA visual instruction tuning, Liu,
# Li, Wu & Lee 2023): the symbolic image, the three prompt kinds, the
# single projection matrix (one token per patch) and the two training
# stages.

.lv_caps <- c("a dog on a sofa", "indoor scene")
.lv_boxes <- list(list("dog", 0.1, 0.2, 0.35, 0.4), list("sofa", 0, 0.5, 1, 0.5))

test_that("symbolic_representation lists captions then boxes", {
  s <- symbolic_representation(.lv_caps, .lv_boxes)
  expect_identical(s$text, paste(c(.lv_caps,
                                   "dog: [0.100, 0.200, 0.350, 0.400]",
                                   "sofa: [0.000, 0.500, 1.000, 0.500]"), collapse = "\n"))
  expect_equal(c(s$n_captions, s$n_boxes), c(2L, 2L))
  expect_identical(symbolic_representation(.lv_caps, list())$text, paste(.lv_caps, collapse = "\n"))
  expect_error(symbolic_representation(character(0), list()), "no symbolic representation")
})

test_that("instruction_prompt appends the kind's request", {
  s <- symbolic_representation(.lv_caps, .lv_boxes)
  for (k in c("conversation", "detailed_description", "complex_reasoning")) {
    p <- instruction_prompt(s, k)
    expect_identical(p$kind, k)
    expect_true(startsWith(p$prompt, paste0(s$text, "\n\n")))
    expect_gt(nchar(p$prompt), nchar(s$text) + 2)
  }
  expect_match(instruction_prompt(s)$prompt, "as if you can see it", fixed = TRUE)
  expect_match(instruction_prompt(s, "detailed_description")$prompt, "in detail", fixed = TRUE)
  expect_match(instruction_prompt(s, "complex_reasoning")$prompt, "step-by-step", fixed = TRUE)
  expect_error(instruction_prompt(s, "caption"), "kind must be one of")
})

test_that("project_patches applies one matrix per patch and build_sequence concatenates", {
  P <- list(c(1, 0, 2), c(-1, 0.5, 0))
  W <- list(c(1, 2, 0.5), c(0, -1, 1))
  b <- c(0.25, -0.5)
  out <- project_patches(P, W, b)
  Wm <- do.call(rbind, W)
  expect_length(out, 2L)
  for (i in 1:2) expect_equal(out[[i]], as.numeric(Wm %*% P[[i]]) + b, tolerance = 1e-12)
  expect_equal(project_patches(P, W)[[1]], as.numeric(Wm %*% P[[1]]), tolerance = 1e-12)
  expect_error(project_patches(P, list(c(1, 2), c(0, 1))), "expects 2 features but got 3")
  txt <- list(c(1, 1), c(0, -1), c(2, 0))
  for (fn in list(build_sequence, visualinstruction, llava_visual_chat)) {
    sq <- fn(out, txt)
    expect_equal(sq$sequence, c(out, txt))
    expect_equal(c(sq$n_visual, sq$n_text), c(2L, 3L))
    expect_identical(sq$estimate, sq$sequence)
  }
  expect_equal(build_sequence(list(), txt)$n_visual, 0L)
  expect_error(build_sequence(out, list(c(1, 2, 3))), "2-dimensional but text embeddings are 3")
})

test_that("training_stage freezes the encoder, and the LM only in stage 1", {
  s1 <- training_stage(1)
  expect_equal(unlist(s1$trainable), "projection")
  expect_setequal(unlist(s1$frozen), c("vision_encoder", "language_model"))
  expect_identical(s1$data, "image-caption pairs")
  s2 <- training_stage(2)
  expect_setequal(unlist(s2$trainable), c("projection", "language_model"))
  expect_equal(unlist(s2$frozen), "vision_encoder")
  expect_error(training_stage(3), "stage must be 1 or 2")
})

test_that("morie_llavx dispatches every op", {
  s <- symbolic_representation(.lv_caps, .lv_boxes)
  expect_equal(morie_llavx("symbolic_representation", .lv_caps, .lv_boxes)$text, s$text)
  expect_equal(morie_llavx("instruction_prompt", s, "conversation")$prompt, instruction_prompt(s)$prompt)
  expect_equal(morie_llavx("project_patches", list(c(1, 2)), list(c(1, 1)))[[1]], 3)
  expect_equal(morie_llavx("build_sequence", list(c(1, 2)), list(c(0, 1)))$n_visual, 1L)
  expect_equal(morie_llavx("visualinstruction", list(c(1, 2)), list())$n_text, 0L)
  expect_equal(morie_llavx("llava_visual_chat", list(), list(c(1, 2)))$n_visual, 0L)
  expect_equal(morie_llavx("training_stage", 2)$stage, 2L)
  expect_match(morie_llavx("cheatsheet")$cheatsheet, "LANGUAGE-ONLY", fixed = TRUE)
  expect_error(morie_llavx("caption"), "unknown op")
  expect_error(morie_llavx(), "op must be one of")
})
