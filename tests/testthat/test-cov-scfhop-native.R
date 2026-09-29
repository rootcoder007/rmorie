# Scaffold hopping (Schneider et al. 1999; Bemis & Murcko 1996): CATS
# pharmacophore types and pair counts worked out by hand on small
# molecules, the Murcko framework, and the WL scaffold signature.

sc_pairs <- c("AA", "AD", "AL", "AN", "AP", "DD", "DL", "DN", "DP", "LL", "LN", "LP",
              "NN", "NP", "PP")

test_that("pharmacophore types follow the stated atom rules", {
  expect_identical(morie_scfhop_types("CCO"), list("L", character(0), c("A", "D")))
  expect_identical(morie_scfhop_types("CCN")[[3]], c("A", "D", "P"))
  # an amide nitrogen is not a positive ionisable centre
  expect_identical(morie_scfhop_types("CC(=O)N")[[4]], c("A", "D"))
  # both carboxyl oxygens are negative ionisable
  ac <- morie_scfhop_types("CC(=O)O")
  expect_true("N" %in% ac[[3]] && "N" %in% ac[[4]])
  expect_identical(morie_scfhop_types("CCl")[[2]], "L")
})

test_that("CATS counts type pairs by topological distance with three scalings", {
  # ethanol: L on C1, A and D on O, two bonds apart
  v <- morie_scfhop_cats("CCO", maxdist = 2, scaling = "none")
  ref <- matrix(0, 15, 3, dimnames = list(sc_pairs, 0:2))
  ref["LL", "0"] <- 1
  ref["AL", "2"] <- 1
  ref["DL", "2"] <- 1
  ref["AA", "0"] <- 1
  ref["DD", "0"] <- 1
  ref["AD", "0"] <- 2
  expect_equal(v, as.numeric(t(ref)))
  expect_equal(morie_scfhop_cats("CCO", 2, "count"), as.numeric(t(ref)) / sum(ref))
  # type scaling divides each pair block by the counts of its two types
  have <- c(A = 1, D = 1, L = 1, N = 0, P = 0)
  s <- vapply(sc_pairs, function(p) have[[substr(p, 1, 1)]] + have[[substr(p, 2, 2)]], 1)
  sc <- ref / ifelse(s > 0, s, 1)
  expect_equal(morie_scfhop_cats("CCO", 2), as.numeric(t(sc)))
  expect_length(morie_scfhop_cats("CCO"), 15L * 10L)
  expect_error(morie_scfhop_cats("CCO", scaling = "z"), "type, count or none")
  expect_error(morie_scfhop_cats("CCO", maxdist = -1), "below zero")
})

test_that("similarity metrics: min/max Tanimoto, 1/(1+euclid), cosine", {
  a <- c(1, 0, 2, 0.5)
  b <- c(0.5, 1, 2, 0)
  expect_equal(morie_scfhop_similarity(a, b), sum(pmin(a, b)) / sum(pmax(a, b)), tolerance = 1e-15)
  expect_equal(morie_scfhop_similarity(a, b, "euclidean"), 1 / (1 + sqrt(sum((a - b)^2))),
               tolerance = 1e-15)
  expect_equal(morie_scfhop_similarity(a, b, "cosine"),
               sum(a * b) / sqrt(sum(a^2) * sum(b^2)), tolerance = 1e-15)
  expect_identical(morie_scfhop_similarity(c(0, 0), c(0, 0)), 0)
  expect_error(morie_scfhop_similarity(1:2, 1:3), "different lengths")
  expect_error(morie_scfhop_similarity(a, b, "l1"), "tanimoto, euclidean or cosine")
})

test_that("the Murcko framework strips side chains and keeps ring linkers", {
  expect_identical(morie_scfhop_murcko("c1ccccc1CCO")$atoms, 0:5)
  expect_length(morie_scfhop_murcko("c1ccccc1CCO")$bonds, 6L)
  expect_identical(morie_scfhop_murcko("c1ccccc1CCc1ccccc1")$atoms, 0:13)
  expect_length(morie_scfhop_murcko("CCCCO")$atoms, 0L)
  expect_identical(morie_scfhop_signature("CCCCO"), numeric(0))
})

test_that("the WL signature separates scaffolds, not side chains", {
  expect_identical(morie_scfhop_signature("c1ccccc1O"), morie_scfhop_signature("c1ccccc1CN"))
  expect_false(identical(morie_scfhop_signature("c1ccccc1O"), morie_scfhop_signature("c1ccncc1O")))
  expect_length(morie_scfhop_signature("c1ccccc1O"), 6L)
})

test_that("morie_scfhop ranks by CATS similarity and flags scaffold changes", {
  db <- c("c1ccccc1CCN", "c1ccncc1CCN", "CCCCN", "c1ccccc1CCO")
  r <- morie_scfhop("c1ccccc1CCN", db, threshold = 0.2)
  lead <- morie_scfhop_cats("c1ccccc1CCN")
  sim <- vapply(db, function(s) morie_scfhop_similarity(lead, morie_scfhop_cats(s)), 1)
  expect_equal(r$similarity, unname(sort(sim, decreasing = TRUE)), tolerance = 1e-15)
  expect_identical(r$order, order(-sim, 1:4) - 1L)
  differs <- vapply(r$ranked, `[[`, TRUE, "scaffold_differs")
  names(differs) <- vapply(r$ranked, `[[`, "", "smiles")
  expect_false(differs[["c1ccccc1CCN"]])
  expect_false(differs[["c1ccccc1CCO"]])
  expect_true(differs[["c1ccncc1CCN"]])
  expect_true(differs[["CCCCN"]])
  expect_identical(r$n_hops, sum(differs & unname(sim[names(differs)]) >= 0.2))
  expect_identical(r$lead_scaffold_size, 6L)
  expect_match(morie_scfhop_cheatsheet(), "Bemis-Murcko", fixed = TRUE)
})
