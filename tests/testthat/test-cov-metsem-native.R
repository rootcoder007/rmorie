# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/metsem_native.R (de Bruijn metagenome assembly).
# A 24-base reference cut into overlapping reads must reassemble
# exactly; the k-mer graph, the unitig walk, N50 and the relative tip
# and bubble filters are recomputed from their definitions.

.ms_ref <- "ACGTTGCAATCCGGATTACGAGCT"
.ms_reads <- vapply(seq(1, 17, by = 4), function(i) substr(.ms_ref, i, i + 7), "")

test_that("kmers slides a window of width k", {
  expect_identical(morie_metsem_kmers("ACGTA", 3), c("ACG", "CGT", "GTA"))
  expect_identical(morie_metsem_kmers("AC", 3), character(0))
  expect_error(morie_metsem_kmers("ACGT", 1), "k of at least two")
})

test_that("graph counts k-mer multiplicities and indexes both ends", {
  g <- morie_metsem_graph(c("ACGTA", "CGTAC", "AC"), 3)
  expect_equal(unlist(g$edges[c("ACG", "CGT", "GTA", "TAC")]), c(ACG = 1, CGT = 2, GTA = 2, TAC = 1))
  expect_equal(g$used, 2L)
  expect_equal(g$short, 1L)
  expect_identical(g$out[["CG"]], "CGT")
  expect_identical(g$inc[["GT"]], "CGT")
  expect_identical(g$nodes, sort(unique(c(substr(names(g$edges), 1, 2), substr(names(g$edges), 2, 3)))))
})

test_that("unitigs are maximal non-branching paths; n50 is the half-mass length", {
  g <- morie_metsem_graph(.ms_ref, 5)
  u <- morie_metsem_unitigs(g)
  expect_length(u, 1L)
  expect_identical(u[[1]]$seq, .ms_ref)
  expect_equal(u[[1]]$coverage, 1)
  expect_equal(u[[1]]$n_edges, nchar(.ms_ref) - 4L)
  # a branch splits the walk: two reads sharing a 4-mer prefix
  gb <- morie_metsem_graph(c("ACGTAA", "ACGTCC"), 5)
  ub <- morie_metsem_unitigs(gb)
  # node ACGT branches, so each branch is its own unitig of two 5-mers
  expect_length(ub, 2L)
  expect_setequal(vapply(ub, function(x) x$seq, ""), c("ACGTAA", "ACGTCC"))
  expect_equal(vapply(ub, function(x) x$n_edges, 0L), c(2L, 2L))
  expect_equal(morie_metsem_n50(c(10, 8, 6, 2)), 8L)
  expect_equal(morie_metsem_n50(c(5, 5)), 5L)
  expect_equal(morie_metsem_n50(integer(0)), 0L)
  expect_length(morie_metsem_unitigs(morie_metsem_graph("AC", 5)), 0L)
})

test_that("morie_metsem reassembles the reference and drops a low-coverage tip", {
  r <- morie_metsem(.ms_reads, 5)
  expect_identical(r$contigs, .ms_ref)
  expect_equal(r$total_length, nchar(.ms_ref))
  expect_equal(r$n50, nchar(.ms_ref))
  expect_equal(r$n_reads_used, length(.ms_reads))
  expect_equal(r$n_kmers, r$n_kmers_initial)
  expect_equal(r$n_tips_removed, 0L)
  # one erroneous read adds a short branch seen once against coverage 5
  bad <- c(rep(.ms_reads, 5), paste0(substr(.ms_ref, 1, 6), "TTTT"))
  rb <- morie_metsem(bad, 5, tip_length = 12, tip_ratio = 0.5)
  expect_identical(rb$contigs, .ms_ref)
  expect_gt(rb$n_tips_removed + rb$n_bubbles_removed, 0L)
  expect_lt(rb$n_kmers, rb$n_kmers_initial)
  # a bubble: two paths between the same pair of nodes, one rare
  ml <- morie_metsem(.ms_reads, 5, min_length = 100)
  expect_equal(ml$n_contigs, 0L)
  expect_equal(ml$n_short, 1L)
  expect_identical(ml$short_contigs, .ms_ref)
  expect_equal(ml$n50, 0L)
  expect_error(morie_metsem(character(0), 5), "needs reads")
  expect_error(morie_metsem(.ms_reads, 1), "k of at least two")
})

test_that("morie_metsem_cheatsheet names the relative filters", {
  expect_match(morie_metsem_cheatsheet(), "RELATIVE", fixed = TRUE)
})
