# LayoutLMv3 (Huang et al. 2022): 0..1000 layout boxes, segment-level
# boxes, block masking and word-patch alignment, recomputed directly.

test_that("boxes are normalised to the 0..scale grid and clamped", {
  for (f in list(normalise_bbox, ocrwit_normalise_bbox)) {
    expect_identical(f(c(10, 20, 300, 410), 600, 800),
                     as.integer(round(c(10 / 600, 20 / 800, 300 / 600, 410 / 800) * 1000)))
    expect_identical(f(c(0, 0, 700, 50), 600, 800, scale = 100), c(0L, 0L, 100L, 6L))
    expect_error(f(c(5, 5, 1, 9), 10, 10), "inverted")
    expect_error(f(c(1, 1, 2, 2), 0, 10), "positive")
  }
  expect_error(normalise_bbox(c(1, 2, 3), 10, 10), "four coordinates")
})

test_that("segment boxes are the union of their words' boxes", {
  boxes <- list(c(10, 10, 50, 30), c(60, 12, 90, 28), c(10, 100, 40, 120))
  seg <- c("a", "a", "b")
  nb <- lapply(boxes, normalise_bbox, 200, 200)
  refa <- c(min(nb[[1]][1], nb[[2]][1]), min(nb[[1]][2], nb[[2]][2]),
            max(nb[[1]][3], nb[[2]][3]), max(nb[[1]][4], nb[[2]][4]))
  for (f in list(segment_layout_boxes, ocrwit_segment_layout_boxes)) {
    r <- f(boxes, seg, 200, 200)
    expect_equal(r$segment_boxes$a, refa)
    expect_equal(r$segment_boxes$b, nb[[3]])
    expect_equal(r$per_token[[2]], refa)
    expect_identical(r$n_segments, 2L)
    expect_error(f(boxes, c("a", "b"), 200, 200), "segment ids")
  }
  m <- do.call(rbind, boxes)
  expect_equal(segment_layout_boxes(m, seg, 200, 200)$segment_boxes$a, refa)
})

test_that("mask units draws block starts from the shared stream", {
  for (f in list(mask_units, ocrwit_mask_units)) {
    r <- f(20, rate = 0.3, seed = 5, block = 2)
    # replay: starts floor(u n) until round(n r) units are covered
    e <- .ghc_rng(5)
    m <- integer(0)
    while (length(m) < 6) {
      s <- as.integer(.ghc_unif(e, 1L) * 20) %% 20
      m <- unique(c(m, seq.int(s, min(20L, s + 2L) - 1L)))
    }
    expect_identical(r$masked, sort(m))
    expect_identical(r$kept, setdiff(0:19, sort(m)))
    expect_equal(r$rate, length(m) / 20)
    expect_error(f(0), "nothing to mask")
    expect_error(f(5, rate = 1), "\\(0,1\\)")
  }
})

test_that("patch_of_box covers every grid cell the box overlaps", {
  # page 140 x 140, grid 14: cells are 10 x 10
  for (f in list(patch_of_box, ocrwit_patch_of_box)) {
    expect_identical(f(c(15, 5, 38, 9), 140, 140), c(1L, 2L, 3L))
    r <- f(c(15, 15, 25, 32), 140, 140)
    ref <- as.integer(sort(as.vector(outer(1:3, 1:2, function(rr, cc) rr * 14 + cc))))
    expect_identical(r, ref)
    # a zero-width box still covers its own cell
    expect_identical(f(c(45, 45, 45, 45), 140, 140), 4L * 14L + 4L)
    expect_error(f(c(5, 5, 1, 9), 10, 10), "inverted")
  }
})

test_that("word-patch alignment labels unmasked words by their patches", {
  boxes <- list(c(0, 0, 9, 9), c(15, 5, 38, 9), c(100, 100, 130, 130), c(50, 50, 55, 55))
  masked <- c(2, 150)
  r <- word_patch_alignment(boxes, masked, 140, 140, masked_text = 3)
  expect_identical(names(r$labels), c("0", "1", "2"))
  ref <- vapply(boxes[1:3], function(b) as.integer(any(patch_of_box(b, 140, 140) %in% masked)),
                integer(1))
  expect_identical(unname(unlist(r$labels)), ref)
  expect_equal(r$positive_rate, mean(ref))
  expect_identical(r$n_examples, 3L)
  for (f in list(morie_ocrwit, layoutlmv3, ocr_wit_layout, ocrwit_word_patch_alignment)) {
    expect_identical(f(boxes, masked, 140, 140, masked_text = 3)$labels, r$labels)
  }
  expect_error(word_patch_alignment(boxes[1], masked, 140, 140, masked_text = 0), "every text token")
  expect_match(ocrwit_cheatsheet(), "WORD-PATCH", fixed = TRUE)
})
