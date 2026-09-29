# Coverage for the DELLY structural-variant caller. Insert statistics are
# recomputed with stats::median / stats::mad / stats::sd, pair signatures
# and reference rewrites from their definitions, and the split-read path
# is checked end to end on a simulated 100 bp deletion whose breakpoints
# are known.

.pr <- function(p1, p2, s1 = "+", s2 = "-", c1 = "1", c2 = "1", len = 50L) {
  list(chrom1 = c1, pos1 = p1, strand1 = s1, chrom2 = c2, pos2 = p2, strand2 = s2, read_length = len)
}
.rc <- function(s) paste(rev(chartr("ACGT", "TGCA", strsplit(s, "")[[1]])), collapse = "")

.genome <- function() {
  set.seed(42)
  b <- sample(c("A", "C", "G", "T"), 600, replace = TRUE)
  # distinct flanks so the junction has exactly one leftmost placement
  b[250] <- "A"
  b[350] <- "C"
  b[251] <- "G"
  b[351] <- "T"
  paste(b, collapse = "")
}

test_that("insert-size statistics: median, 1.4826 MAD or SD, majority orientation", {
  ins <- c(300, 310, 295, 305, 290, 1200, 298)
  ps <- lapply(seq_along(ins), function(i) .pr(100 * i, 100 * i + ins[i] - 50))
  ps <- c(ps, list(.pr(40, 500, "-", "+")))
  st <- morie_sv_dl_insert_size_stats(ps)
  expect_identical(st$orientation, c("+", "-"))
  expect_equal(st$median, stats::median(ins))
  expect_equal(st$sd, stats::mad(ins), tolerance = 1e-12)
  expect_identical(st$n, 7L)
  sd2 <- morie_sv_dl_insert_size_stats(ps, spread = "sd")
  expect_equal(sd2$sd, stats::sd(ins), tolerance = 1e-12)
  expect_error(morie_sv_dl_insert_size_stats(ps, spread = "iqr"), "spread must be")
  expect_error(morie_sv_dl_insert_size_stats(list(.pr(1, 2, c2 = "2"))), "no same-chromosome pairs")
  expect_error(morie_sv_dl_insert_size_stats(ps, orientation = c("+", "x")), "pair of strands")
})

test_that("pair signatures follow Figure 2 and the reference rewrites Figure 4", {
  cl <- function(p) morie_sv_dl_classify_pair(p, median = 300, sd = 10)
  expect_null(cl(.pr(100, 350)))
  expect_identical(cl(.pr(100, 400)), c("DEL", ""))
  expect_identical(cl(.pr(100, 350, "-", "+")), c("DUP", ""))
  expect_identical(cl(.pr(100, 350, "+", "+")), c("INV", "left"))
  expect_identical(cl(.pr(100, 350, "-", "-")), c("INV", "right"))
  expect_identical(cl(.pr(100, 50, "+", "+", "2", "1")), c("TRA", "1"))
  expect_identical(cl(.pr(100, 50, "-", "+", "1", "2")), c("TRA", "3"))
  # mate order is normalised: the left-most alignment becomes mate 1
  expect_identical(cl(.pr(400, 100, "-", "+")), c("DEL", ""))
  expect_error(cl(list(chrom1 = "1", pos1 = 1)), "needs chrom1")
  ref <- "AACCGGTTAC"
  expect_identical(morie_sv_dl_deletion_type_reference(ref, "DEL"), ref)
  expect_identical(morie_sv_dl_deletion_type_reference(ref, "DUP"), "GTTACAACCG")
  expect_identical(morie_sv_dl_deletion_type_reference(ref, "INV"), paste0("AACCG", .rc("GTTAC")))
  expect_identical(morie_sv_dl_deletion_type_reference(tolower(ref), "TRA"), paste0(.rc("GTTAC"), "AACCG"))
  expect_error(morie_sv_dl_deletion_type_reference(ref, "CNV"), "sv_type must be one of")
})

test_that("Gotoh prefix/suffix vectors and the optimal split recover a deletion", {
  g <- .genome()
  seg <- substr(g, 101, 160)
  f <- morie_sv_dl_gotoh_score_vectors(substr(seg, 11, 40), seg)
  # an exact substring scores one per base; once a prefix (suffix) is long
  # enough to be unique it ends (starts) where it was cut from
  expect_equal(f$f, as.numeric(1:30))
  expect_equal(f$f_at[12:30], 10L + 12:30)
  expect_equal(f$r, as.numeric(30:1))
  expect_equal(f$r_at[1:19], 10L + 0:18)
  cons <- paste0(substr(seg, 1, 20), substr(seg, 31, 60))
  gv <- morie_sv_dl_gotoh_score_vectors(cons, seg)
  sp <- morie_sv_dl_optimal_split(gv$f, gv$r)
  expect_equal(sp$score, max(outer(gv$f, gv$r, "+")[upper.tri(diag(50))]))
  expect_identical(gv$r_at[sp$j] - gv$f_at[sp$i], 10L)
  expect_error(morie_sv_dl_optimal_split(1:3, 1:2), "same length")
  expect_error(morie_sv_dl_optimal_split(1, 1), "too short")
  expect_error(morie_sv_dl_gotoh_score_vectors("", seg), "non-empty")
})

test_that("k-mer diagonals, maximal clique and majority consensus", {
  g <- .genome()
  ref <- substr(g, 201, 320)
  read <- paste0(substr(ref, 6, 30), substr(ref, 51, 75))
  d <- morie_sv_dl_kmer_diagonals(read, ref, k = 7)
  expect_identical(nrow(d), 2L)
  expect_equal(d[, 1], c(5, 25))
  hits <- vapply(c(5, 25), function(dg) sum(vapply(0:43, function(o) substr(read, o + 1, o + 7) == substr(ref, o + dg + 1, o + dg + 7), TRUE)), 1)
  expect_equal(d[, 2], hits)
  expect_gte(min(hits), 19)
  expect_null(morie_sv_dl_kmer_diagonals("ACG", ref, k = 7))
  expect_null(morie_sv_dl_kmer_diagonals(substr(ref, 1, 40), ref, k = 7))
  expect_error(morie_sv_dl_kmer_diagonals(read, ref, k = 0), "at least 1")
  E <- rbind(c(1, 1, 2), c(2, 2, 3), c(3, 1, 3), c(0.5, 3, 4), c(4, 4, 5))
  # seed at the 0.5 edge (3, 4); 2, 1 and 5 each touch the clique by one
  # edge but none is adjacent to both members, so the clique stays {3, 4}
  expect_identical(morie_sv_dl_maximal_clique(1:5, E), c(3L, 4L))
  expect_identical(morie_sv_dl_maximal_clique(1:3, E), 1:3)
  expect_identical(morie_sv_dl_maximal_clique(c(1L, 5L), E), integer(0))
  cs <- morie_sv_dl_split_read_consensus(c("ACGTA", "CGTTA", "ACGAA"), c(0, 1, 0))
  expect_identical(cs$consensus, "ACGTAA")
  expect_identical(cs$start, 0L)
  expect_identical(morie_sv_dl_split_read_consensus(c("AC", "GT"), c(0, 5))$consensus, "AC")
  expect_error(morie_sv_dl_split_read_consensus(c("AC", "GT"), 0), "one start per read")
})

test_that("paired-end clustering calls one DEL and split reads refine it to the base", {
  g <- .genome()
  sample <- paste0(substr(g, 1, 250), substr(g, 351, 600))
  pairs <- list()
  for (k in 0:14) pairs[[length(pairs) + 1]] <- .pr(20 + 10 * k, 20 + 10 * k + 150 + (k %% 5) - 2)
  for (k in 0:14) pairs[[length(pairs) + 1]] <- .pr(360 + 10 * k, 360 + 10 * k + 150 + (k %% 3) - 1)
  del <- c(150, 160, 170, 175, 185)
  for (x in del) pairs[[length(pairs) + 1]] <- .pr(x, x + 250)
  st <- morie_sv_dl_insert_size_stats(pairs)
  gr <- morie_sv_dl_build_sv_graph(lapply(del, function(x) .pr(x, x + 250)), st$median, st$sd, c("DEL", ""))
  expect_equal(gr$sizes, rep(300 - st$median, 5))
  expect_identical(nrow(gr$edges), 10L)
  expect_true(all(gr$edges[, 1] == 0))
  pe <- morie_sv_dl_paired_end_calls(pairs)
  expect_length(pe, 1L)
  expect_identical(pe[[1]]$type, "DEL")
  expect_identical(pe[[1]]$support, 5L)
  expect_identical(pe[[1]]$start, as.integer(max(del) + 50))
  expect_identical(pe[[1]]$end, as.integer(min(del + 250)))
  expect_equal(pe[[1]]$size, 300 - st$median)
  # each split read has at least 10 bases (4 seven-mers) on both sides of
  # the junction at sample position 250
  splits <- vapply(c(230, 233, 236, 238, 240), function(s) substr(sample, s + 1, s + 30), "")
  sv <- morie_sv_dl_structural_variant(pairs, reference = g, split_reads = splits, k = 7)
  expect_identical(sv$n_calls, 1L)
  expect_identical(sv$n_precise, 1L)
  cl <- sv$calls[[1]]
  expect_true(cl$precise)
  expect_identical(c(cl$start, cl$end), c(250L, 350L))
  expect_equal(cl$size, 100)
  expect_identical(cl$microhomology, 0L)
  rb <- morie_sv_dl_refine_breakpoint(pe[[1]], substr(g, 36, 600), splits, k = 7)
  expect_identical(c(rb$start, rb$end, rb$size), c(215L, 315L, 100L))
  expect_identical(rb$split_support, 5L)
  expect_identical(rb$kmer_offset, 100L)
  expect_null(morie_sv_dl_refine_breakpoint(pe[[1]], substr(g, 36, 600), splits, k = 7, min_split_support = 6))
  far <- pe[[1]]
  far$size <- 300
  expect_null(morie_sv_dl_refine_breakpoint(far, substr(g, 36, 600), splits, k = 7))
  expect_identical(morie_sv_dl(pairs)$n_precise, 0L)
  expect_error(morie_sv_dl_paired_end_calls(list()), "no read pairs")
  expect_error(morie_sv_dl_paired_end_calls(pairs, min_support = 0), "min_support")
  expect_match(morie_sv_dl_cheatsheet(), "DELLY")
})
