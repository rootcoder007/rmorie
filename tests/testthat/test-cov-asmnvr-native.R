# Coverage for Eulerian-path assembly (Pevzner, Tang & Waterman 2001):
# the de Bruijn graph of (k-1)-mers (set and count multiplicity), the
# Hierholzer path (every edge used once, consecutive vertices overlapping
# by k-2), exact reconstruction of a repeat-free genome from its reads,
# branching / non-Eulerian graphs, and the argument checks.

test_that("the de Bruijn graph links k-mer prefixes to suffixes", {
  g <- de_bruijn_graph(c("ACGTA", "CGTAC"), 3)
  expect_identical(sort(ls(g$edges)), c("AC", "CG", "GT", "TA"))
  expect_identical(g$edges[["TA"]], "AC")
  expect_identical(c(g$outdeg[["CG"]], g$indeg[["CG"]]), c(1L, 1L))
  gc <- de_bruijn_graph(c("ACGTA", "CGTAC"), 3, multiplicity = "count")
  expect_identical(gc$edges[["CG"]], c("GT", "GT"))
  expect_error(de_bruijn_graph("ACG", 1), "k must be >= 2")
  expect_error(de_bruijn_graph("AC", 3), "no read is at least k = 3")
  expect_error(de_bruijn_graph("ACG", 2, "bag"), "'set' or 'count'")
})

test_that("a repeat-free genome is reconstructed from overlapping reads", {
  genome <- "ATGGCGTGCAATCCGTA"
  reads <- substring(genome, seq(1, 11, 2), seq(1, 11, 2) + 6)
  a <- morie_asmnvr(reads, k = 5)
  expect_identical(a$sequence, genome)
  expect_true(a$unambiguous)
  expect_identical(a$n_kmers, nchar(genome) - 4L)
  p <- a$path
  expect_true(all(substring(p[-length(p)], 2) == substring(p[-1], 1, 3)))
  g <- de_bruijn_graph(reads, 5)
  ep <- eulerian_path(g$edges, g$indeg, g$outdeg)
  expect_identical(ep, p)
  expect_identical(a$contigs, genome)
})

test_that("branching and non-Eulerian graphs are reported, not resolved", {
  b <- morie_asmnvr(c("AAGTC", "AAGCC"), k = 3)
  expect_null(b$sequence)
  expect_false(b$unambiguous)
  expect_true("AG" %in% b$branching)
  cyc <- morie_asmnvr("ACGACGA", k = 3)
  expect_true(cyc$length_is_lower_bound)
  expect_lte(nchar(cyc$sequence), 7L)
  expect_error(morie_asmnvr(character(0)), "non-empty")
})
