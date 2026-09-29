# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/rouge_native.R (Lin 2004). ROUGE-N clipped n-gram
# counts, ROUGE-L from the LCS and ROUGE-W with f(k) = k^alpha are
# worked by hand, including Lin's Sec. 4 example where two references
# share an LCS of 4 but only one has it consecutive.

test_that("lcs_length is the longest common subsequence", {
  expect_equal(morie_lcs_length(strsplit("ABCBDAB", "")[[1]], strsplit("BDCABA", "")[[1]]), 4L)
  expect_equal(morie_lcs_length(character(0), "a"), 0L)
  expect_equal(morie_lcs_length(c("x", "y"), c("y", "x")), 1L)
})

test_that("rouge_n clips n-gram matches and keeps the best reference", {
  cand <- "the cat sat on the mat"
  ref <- "the cat is on the mat today"
  r1 <- morie_rouge_n(cand, ref, 1)
  # unigram matches: the x2, cat, on, mat = 5 of 6 candidate, 7 reference
  expect_equal(r1$matches, 5L)
  expect_equal(c(r1$precision, r1$recall), c(5 / 6, 5 / 7))
  expect_equal(r1$f1, 2 * (5 / 6) * (5 / 7) / (5 / 6 + 5 / 7), tolerance = 1e-12)
  expect_equal(r1$estimate, r1$recall)
  r2 <- morie_rouge_n(cand, ref, 2)
  # bigrams in common: "the cat", "on the", "the mat"
  expect_equal(r2$matches, 3L)
  expect_equal(c(r2$n_candidate, r2$n_reference), c(5L, 6L))
  # clipping: "the the the" against "the cat" matches once
  expect_equal(morie_rouge_n("the the the", "the cat", 1)$matches, 1L)
  best <- morie_rouge_n(cand, list("a dog ran", "the cat sat"), 1)
  expect_equal(best$matches, 3L)
  fb <- morie_rouge_n(cand, ref, 1, beta = 2)
  expect_equal(fb$f1, 5 * (5 / 6) * (5 / 7) / (5 / 7 + 4 * 5 / 6), tolerance = 1e-12)
  expect_equal(morie_rouge_n("", ref)$f1, 0)
  expect_error(morie_rouge_n(cand, ref, 0), "at least 1")
})

test_that("rouge_l uses the LCS over candidate and reference lengths", {
  r <- morie_rouge_l("police killed the gunman", "police kill the gunman")
  expect_equal(r$lcs, 3L)
  expect_equal(c(r$precision, r$recall), c(0.75, 0.75))
  expect_equal(r$estimate, 0.75)
  r2 <- morie_rouge_l("the gunman kill police", c("police kill the gunman", "the gunman police"))
  expect_equal(r2$lcs, 3L)
  expect_equal(r2$n_reference, 3L)
})

test_that("rouge_w rewards consecutive matches (Lin 2004 Sec. 4)", {
  X <- c("A", "B", "C", "D", "E", "F", "G")
  Y1 <- c("A", "B", "C", "D", "H", "I", "K")
  Y2 <- c("A", "H", "B", "K", "C", "I", "D")
  w1 <- morie_rouge_w(Y1, list(X), alpha = 2)
  w2 <- morie_rouge_w(Y2, list(X), alpha = 2)
  expect_equal(w1$wlcs, 16)
  expect_equal(w2$wlcs, 4)
  expect_equal(w1$recall, sqrt(16 / 49), tolerance = 1e-12)
  expect_equal(w2$precision, sqrt(4 / 49), tolerance = 1e-12)
  expect_gt(w1$f1, w2$f1)
  expect_equal(morie_rouge_l(Y1, list(X))$lcs, morie_rouge_l(Y2, list(X))$lcs)
  expect_error(morie_rouge_w(Y1, list(X), alpha = 0.5), "at least 1")
})

test_that("morie_rouge dispatches on variant", {
  c1 <- "the cat sat"
  r1 <- "the cat ran"
  expect_equal(morie_rouge(c1, r1), morie_rouge_l(c1, r1))
  expect_equal(morie_rouge(c1, r1, "w", alpha = 1.5), morie_rouge_w(c1, r1, 1.5))
  expect_equal(morie_rouge(c1, r1, "N", n = 2), morie_rouge_n(c1, r1, 2))
  expect_error(morie_rouge(c1, r1, "S"), "N, L or W")
  expect_match(morie_rouge_cheatsheet(), "f(k)=k^alpha", fixed = TRUE)
})
