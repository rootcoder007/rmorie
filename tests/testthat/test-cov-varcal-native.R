# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/varcal_native.R (DeepVariant pipeline, Poplin et al.
# 2016). A ten-base reference with six reads, three carrying G at
# 0-based position 4 (reference A): the pileup, the candidate, the
# encoded image cells and the fallback genotype posterior
# (scores (1 - 2f)+, 1 - |2f - 1|, (2f - 1)+ times the prior) are
# derived by hand.

.vc_ref <- "ACGTACGTAC"
.vc_alt <- "ACGTGCGTAC"
.vc_reads <- list(
  list(pos = 0, seq = .vc_alt, bq = rep(30, 10), reverse = TRUE),
  list(pos = 0, seq = .vc_alt),
  list(pos = 2, seq = substr(.vc_alt, 3, 8), bq = c(30, 30, 5, 30, 30, 30)),
  list(pos = 1, seq = substr(.vc_ref, 2, 9), mq = 20),
  list(pos = 0, seq = .vc_ref),
  list(pos = 3, seq = substr(.vc_ref, 4, 10)),
  list(pos = 1, seq = substr(.vc_alt, 2, 7)))

test_that("pileup_column gathers the bases, qualities and strands at a position", {
  col <- varcal_pileup_column(.vc_reads, 4, .vc_ref)
  expect_identical(col$reference, "A")
  expect_equal(col$depth, 7L)
  expect_identical(vapply(col$observations, function(o) o$base, ""), c("G", "G", "G", "A", "A", "A", "G"))
  expect_equal(vapply(col$observations, function(o) o$bq, 1L), c(30L, 30L, 5L, 30L, 30L, 30L, 30L))
  expect_equal(col$observations[[4]]$mq, 20L)
  expect_true(col$observations[[1]]$reverse)
  expect_identical(varcal_pileup_column(.vc_reads, 12, .vc_ref)$reference, "N")
})

test_that("find_candidates keeps alternate bases above count and fraction", {
  cands <- varcal_find_candidates(.vc_reads, .vc_ref)
  expect_length(cands, 1L)
  cd <- cands[[1]]
  # the bq 5 G is dropped by min_bq = 10: 3 G of 6 kept reads
  expect_equal(cd$position, 4)
  expect_identical(cd$alternate, "G")
  expect_equal(cd$alt_count, 3)
  expect_equal(cd$depth, 6L)
  expect_equal(cd$alt_fraction, 0.5)
  expect_equal(varcal_find_candidates(.vc_reads, .vc_ref, min_bq = 0)[[1]]$alt_count, 4)
  expect_length(varcal_find_candidates(.vc_reads, .vc_ref, min_alt_count = 5), 0L)
  expect_length(varcal_find_candidates(.vc_reads, .vc_ref, min_alt_fraction = 0.6), 0L)
  expect_error(varcal_find_candidates(.vc_reads, .vc_ref, min_alt_fraction = 2), "min_alt_fraction")
  expect_error(varcal_find_candidates(.vc_reads, .vc_ref, min_alt_count = 0), "min_alt_count")
})

test_that("encode_pileup codes base, quality, strand and reference match", {
  cd <- varcal_find_candidates(.vc_reads, .vc_ref)[[1]]
  img <- varcal_encode_pileup(.vc_reads, .vc_ref, cd, width = 5)
  expect_equal(img$centre, 4L)
  expect_equal(img$n_reads, 7L)
  # columns 2..6; reference row: C G T A C? positions 2,3,4,5,6 = G T A C G
  expect_equal(vapply(img$reference_row, function(x) x[1], 0), c(0.75, 1, 0.25, 0.5, 0.75))
  # read 1 at the centre: G, bq 30 -> 0.5, reverse strand -> 0, mismatch -> 0
  expect_equal(img$read_rows[[1]][[3]], c(0.75, 0.5, 0, 0))
  # read 4 (starts at 1) at the centre: A, default bq 30, forward, match
  expect_equal(img$read_rows[[4]][[3]], c(0.25, 0.5, 1, 1))
  # read 7 covers 1..6: column 7 is outside the read; read 6 starts at 3
  expect_equal(img$read_rows[[6]][[1]], c(0, 0, 0, 0))
  edge <- varcal_encode_pileup(.vc_reads, .vc_ref, list(position = 1), width = 5)
  expect_equal(edge$reference_row[[1]], c(0, 1, 1, 1))
  expect_equal(varcal_encode_pileup(.vc_reads, .vc_ref, cd, width = 5, height = 2)$n_reads, 2L)
  expect_error(varcal_encode_pileup(.vc_reads, .vc_ref, cd, width = 4), "odd")
  expect_error(varcal_encode_pileup(.vc_reads, .vc_ref, cd, channels = "rgb"), "channels must be one of")
})

test_that("genotype_posterior multiplies the scores by the prior", {
  cd <- varcal_find_candidates(.vc_reads, .vc_ref)[[1]]
  img <- varcal_encode_pileup(.vc_reads, .vc_ref, cd, width = 5)
  f <- 4 / 7
  sc <- c(max(1 - 2 * f, 0), 1 - abs(2 * f - 1), max(2 * f - 1, 0))
  pr <- c(0.9985, 0.001, 0.0005)
  g <- varcal_genotype_posterior(img)
  expect_equal(g$scores, sc, tolerance = 1e-12)
  expect_equal(unlist(g$posterior), setNames(sc * pr / sum(sc * pr), varcal_GENOTYPES), tolerance = 1e-12)
  k <- which.max(sc * pr)
  expect_identical(g$call, varcal_GENOTYPES[k])
  expect_equal(g$quality, -10 * log10(1 - (sc * pr / sum(sc * pr))[k]), tolerance = 1e-12)
  u <- varcal_genotype_posterior(img, scorer = function(im) c(1, 2, 7), prior = c(1, 1, 1) / 3)
  expect_equal(unlist(u$posterior), setNames(c(1, 2, 7) / 10, varcal_GENOTYPES), tolerance = 1e-12)
  expect_identical(u$call, "hom_alt")
  z <- varcal_genotype_posterior(img, scorer = function(im) c(0, 0, 0))
  expect_equal(unname(unlist(z$posterior)), pr)
  expect_error(varcal_genotype_posterior(img, prior = c(0.5, 0.5)), "three probabilities")
  expect_error(varcal_genotype_posterior(img, scorer = function(im) c(-1, 1, 1)), "non-negative")
})

test_that("morie_varcal calls, and evaluate scores against a truth set", {
  for (fn in list(morie_varcal, deep_variant_call)) {
    r <- fn(.vc_reads, .vc_ref, scorer = function(im) c(0, 1, 0), min_quality = 0)
    expect_equal(r$n_candidates, 1L)
    expect_identical(r$calls[[1]]$call, "het")
    expect_true(r$calls[[1]]$passes)
    expect_equal(r$estimate, 1)
    r2 <- fn(.vc_reads, .vc_ref, min_alt_count = 5)
    expect_equal(r2$n_candidates, 0L)
  }
  called <- list(list(position = 4, alternate = "G"), list(position = 7, alternate = "A"))
  truth <- list(list(position = 4, alternate = "G"), list(position = 9, alternate = "T"),
                list(position = 4, alternate = "G"))
  cands <- c(called, list(list(position = 9, alternate = "T"), list(position = 1, alternate = "A")))
  e <- varcal_evaluate(called, truth, cands)
  expect_equal(c(e$true_positives, e$called, e$truth), c(1, 2, 2))
  expect_equal(c(e$ppv, e$sensitivity), c(0.5, 0.5))
  expect_equal(c(e$candidate_ppv, e$candidate_sensitivity), c(0.5, 1))
  expect_equal(e$ppv_gain, 0)
  expect_equal(e$sensitivity_loss, 0.5)
  expect_equal(varcal_evaluate(list(), list())$ppv, 0)
})

test_that("varcal constants and cheatsheet", {
  expect_identical(varcal_GENOTYPES, c("hom_ref", "het", "hom_alt"))
  expect_identical(varcal_CHANNEL_SETS, "base_quality_strand")
  expect_match(varcal_cheatsheet(), "8.1% PPV", fixed = TRUE)
})
