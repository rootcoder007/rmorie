# Coverage for long-read polishing: global Needleman-Wunsch alignment
# (score against an independent DP, and the returned alignment scored
# column by column), run-length encoding round trips, pileup counts, the
# majority column call with min_depth / min_frac protection, majority
# insertions, and the progressive consensus.

.nw <- function(a, b, ma = 1, mi = -1, g = -2) {
  a <- strsplit(a, "")[[1]]
  b <- strsplit(b, "")[[1]]
  S <- outer(0:length(a), 0:length(b), function(i, j) g * (i + j) * (i == 0 | j == 0))
  for (i in seq_along(a)) for (j in seq_along(b))
    S[i + 1, j + 1] <- max(S[i, j] + ifelse(a[i] == b[j], ma, mi), S[i, j + 1] + g, S[i + 1, j] + g)
  S[length(a) + 1, length(b) + 1]
}
.colscore <- function(x, y, ma = 1, mi = -1, g = -2) {
  x <- strsplit(x, "")[[1]]
  y <- strsplit(y, "")[[1]]
  sum(ifelse(x == "-" | y == "-", g, ifelse(x == y, ma, mi)))
}

test_that("global alignment attains the Needleman-Wunsch optimum", {
  for (p in list(c("GATTACA", "GCATGCT"), c("ACGT", "AGT"), c("AAAA", "AAAAAA"), c("", "ACG"))) {
    al <- morie_longrd_align(p[1], p[2])
    expect_equal(al$score, .nw(p[1], p[2]))
    expect_identical(gsub("-", "", al$a), p[1])
    expect_identical(gsub("-", "", al$b), p[2])
    if (nzchar(al$a)) expect_equal(.colscore(al$a, al$b), al$score)
  }
  expect_equal(morie_longrd_align("ACGT", "ACCT", match = 2, mismatch = -3, gap = -1)$score, .nw("ACGT", "ACCT", 2, -3, -1))
})

test_that("run-length encoding round-trips", {
  r <- morie_longrd_rle("AAACGGT")
  expect_identical(vapply(r, `[[`, "", 1), c("A", "C", "G", "T"))
  expect_identical(vapply(r, `[[`, 1L, 2), c(3L, 1L, 2L, 1L))
  expect_identical(morie_longrd_unrle(r), "AAACGGT")
  expect_identical(morie_longrd_rle(""), list())
  expect_identical(morie_longrd_unrle(list()), "")
})

test_that("pileup counts bases, deletions and insertions per draft column", {
  pu <- morie_longrd_pileup("ACGT", c("ACGT", "AGT", "ACGGT"))
  cnt <- vapply(pu$cols, function(x) x[c("A", "C", "G", "T", "-")], integer(5))
  expect_identical(unname(cnt[, 2]), c(0L, 2L, 0L, 0L, 1L))
  expect_equal(unname(colSums(cnt)), rep(3, 4))
  ins <- unlist(pu$ins)
  expect_identical(sum(ins), 1L)
  expect_identical(names(ins), "G")
})

test_that("polishing calls column majorities and majority insertions", {
  draft <- "ACGTTA"
  reads <- c("ACCTTA", "ACCTTA", "ACCTTA", "ACGTTA")
  r <- morie_longrd(draft, reads)
  expect_identical(r$polished, "ACCTTA")
  expect_identical(r$n_changed, 1L)
  expect_equal(r$support[3], 0.75)
  expect_identical(r$depth, rep(4L, 6))
  expect_false(r$identical)
  # too shallow: every column is protected and the draft survives
  s <- morie_longrd(draft, reads[1:2])
  expect_identical(s$polished, draft)
  expect_identical(s$n_protected, 6L)
  ins <- morie_longrd("ACGT", rep("ACGGT", 3), min_depth = 2)
  expect_identical(ins$polished, "ACGGT")
  expect_identical(ins$n_polished_runs, 4L)
  del <- morie_longrd("ACCGT", rep("ACGT", 3))
  expect_identical(del$polished, "ACGT")
  expect_identical(del$n_changed, 3L)
  expect_error(morie_longrd("", reads), "empty draft")
  expect_error(morie_longrd(draft, character(0)), "needs reads")
  expect_error(morie_longrd(draft, reads, method = "racon"), "pileup or poa")
  expect_match(morie_longrd_cheatsheet(), "Needleman-Wunsch")
})

test_that("progressive consensus merges reads along their alignments", {
  expect_identical(morie_longrd_poa("ACGT"), "ACGT")
  # sorted: ACGGT then ACGT; the aligned gap takes the read's base
  expect_identical(morie_longrd_poa(c("ACGT", "ACGGT")), "ACGGT")
  al <- morie_longrd_align("ACGT", "TCGA")
  x <- strsplit(al$a, "")[[1]]
  y <- strsplit(al$b, "")[[1]]
  expect_identical(morie_longrd_poa(c("TCGA", "ACGT")), paste(ifelse(x == "-", y, x), collapse = ""))
  expect_identical(morie_longrd("ACGT", c("ACGT", "ACGGT"), method = "poa")$polished, "ACGGT")
  expect_error(morie_longrd_poa(character(0)), "at least one read")
})
