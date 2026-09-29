# MetaBAT 2 pieces (Kang et al. 2019): canonical k-mer frequencies,
# abundance correlation, length weights, the composite distance, greedy
# binning and purity/completeness, recomputed directly.

mb_kmers <- function(s, k, canonical = TRUE) {
  ch <- strsplit(s, "")[[1]]
  comp <- c(A = "T", C = "G", G = "C", T = "A")
  km <- vapply(seq_len(length(ch) - k + 1), function(i) paste(ch[i:(i + k - 1)], collapse = ""), "")
  km <- km[!grepl("[^ACGT]", km)]
  if (canonical) {
    rc <- vapply(km, function(m) paste(rev(comp[strsplit(m, "")[[1]]]), collapse = ""), "")
    km <- ifelse(km < rc, km, rc)
  }
  tb <- table(km)
  tb / sum(tb)
}

test_that("k-mer frequencies count canonical k-mers and skip ambiguous ones", {
  s <- "ACGTTGCANACCGT"
  for (can in c(TRUE, FALSE)) {
    r <- tetranucleotide_frequency(s, kk = 2, canonical = can)
    ref <- mb_kmers(s, 2, can)
    expect_identical(r$kmers, names(ref))
    expect_equal(unname(r$vector), as.numeric(ref), tolerance = 1e-15)
    expect_identical(r$n_kmers, 11L)
  }
  expect_equal(sum(tetranucleotide_frequency("ACGTACGGTACCA")$vector), 1, tolerance = 1e-15)
  expect_error(tetranucleotide_frequency("ACG", 4), "shorter than k")
  expect_error(tetranucleotide_frequency("NNNNN", 4), "no valid k-mers")
  expect_error(tetranucleotide_frequency("ACGT", 0), "at least 1")
})

test_that("abundance correlation and length weights", {
  a <- c(1, 3, 2, 8)
  b <- c(2, 5, 3, 9)
  expect_equal(abundance_correlation(a, b)$correlation, stats::cor(a, b), tolerance = 1e-14)
  expect_identical(abundance_correlation(c(1, 1), c(2, 3))$correlation, 0)
  expect_error(abundance_correlation(1, 2), "at least 2 samples")
  expect_error(abundance_correlation(1:3, 1:2), "differ in length")
  expect_equal(length_weight(10000)$weight, log(4) / log(40), tolerance = 1e-15)
  expect_identical(length_weight(1e6)$weight, 1)
  expect_true(length_weight(1000)$below_minimum)
  expect_error(length_weight(0), "positive")
})

test_that("the composite distance mixes composition and abundance", {
  ta <- c(0.1, 0.3, 0.6)
  tb <- c(0.2, 0.2, 0.6)
  ca <- c(1, 4, 2)
  cb <- c(2, 3, 5)
  d <- composite_distance(ta, tb, ca, cb, 5000, 20000, w_abundance = 0.3)
  dt <- sqrt(sum((ta - tb)^2))
  da <- 1 - stats::cor(ca, cb)
  expect_equal(d$distance, 0.7 * dt + 0.3 * da, tolerance = 1e-14)
  expect_equal(d$confidence, log(2) / log(40), tolerance = 1e-15)
  one <- composite_distance(ta, tb, 1, 2)
  expect_false(one$abundance_usable)
  expect_equal(one$distance, dt, tolerance = 1e-15)
  expect_error(composite_distance(ta, tb[-1]), "differ in length")
})

test_that("binning groups close contigs from the longest down", {
  tnf <- list(c(0.1, 0.9), c(0.12, 0.88), c(0.8, 0.2), c(0.82, 0.18), c(0.5, 0.5))
  L <- c(3e5, 1e5, 2e5, 2.5e5, 5e4)
  r <- bin_contigs(tnf, lengths = L, threshold = 0.1, min_bin_size = 2e5)
  expect_identical(r$bins, list(c(1L, 2L), c(4L, 3L)))
  expect_identical(r$unbinned, 5L)
  expect_identical(metabat2(tnf, lengths = L, threshold = 0.1, min_bin_size = 2e5)$bins, r$bins)
  pc <- purity_completeness(list(c(1, 2, 5), c(3, 4)), c("x", "x", "y", "y", "y"))
  expect_equal(pc$per_bin[[1]]$purity, 2 / 3)
  expect_equal(pc$per_bin[[1]]$completeness, 1)
  expect_equal(pc$per_bin[[2]]$completeness, 2 / 3)
  expect_equal(pc$mean_purity, (2 / 3 + 1) / 2, tolerance = 1e-15)
  expect_equal(morie_metabd("length_weight", 10000)$weight, log(4) / log(40), tolerance = 1e-15)
  expect_match(morie_metabd("cheatsheet")$cheatsheet, "ADAPTIVE", fixed = TRUE)
  expect_error(morie_metabd("nope"), "unknown op")
})
