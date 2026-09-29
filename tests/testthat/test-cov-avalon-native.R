# Coverage tests for R/avalon_native.R: FNV-1a hashing (published test
# vectors), SMILES parsing, implicit hydrogens, ring perception, feature
# strings, folding and Tanimoto similarity.

test_that("FNV-1a 32-bit matches the published test vectors", {
  expect_equal(morie_avalon_fnv(""), 2166136261)
  expect_equal(morie_avalon_fnv("a"), 3826002220)
  expect_equal(morie_avalon_fnv("foobar"), 3214735720)
  expect_error(morie_avalon_fnv("€"), "ASCII")
})

test_that("SMILES parsing: atoms, bonds, branches, rings, bracket atoms", {
  g <- morie_avalon_parse("CC(=O)O")
  expect_equal(g$el, c("C", "C", "O", "O"))
  expect_equal(do.call(rbind, g$bonds), rbind(c(0, 1, 1), c(1, 2, 2), c(1, 3, 1)))
  expect_equal(morie_avalon_h(g$el, g$arom, g$chg, g$hexp, g$bonds), c(3L, 0L, 0L, 1L))
  b <- morie_avalon_parse("c1ccccc1")
  expect_equal(b$arom, rep(1L, 6))
  expect_equal(b$closures, 6L)
  expect_true(all(vapply(b$bonds, `[`, 0, 3) == 4))
  expect_equal(morie_avalon_h(b$el, b$arom, b$chg, b$hexp, b$bonds), rep(1L, 6))
  q <- morie_avalon_parse("[NH4+].[O-]C(Cl)=O")
  expect_equal(q$chg, c(1L, -1L, 0L, 0L, 0L))
  expect_equal(q$hexp[1], 4L)
  expect_length(q$bonds, 3L)
  expect_equal(length(morie_avalon_parse("C%12CC%12")$closures), 1L)
  expect_error(morie_avalon_parse("C1CC"), "never matched")
  expect_error(morie_avalon_parse("C(C"), "never closed")
  expect_error(morie_avalon_parse("CX"), "unsupported SMILES character")
  expect_error(morie_avalon_parse(""), "empty SMILES")
})

test_that("ring perception finds the smallest ring through each closure", {
  g <- morie_avalon_parse("C1CC1C2CCC2")
  r <- morie_avalon_rings(length(g$el), g$bonds, g$closures)
  expect_equal(sort(lengths(r$rings)), c(3L, 4L))
  expect_equal(r$inring, rep(1L, 7))
  a <- morie_avalon_parse("CCC1CC1")
  expect_equal(morie_avalon_rings(5, a$bonds, a$closures)$inring, c(0L, 0L, 1L, 1L, 1L))
})

test_that("feature strings for ethanol", {
  f <- morie_avalon_features("CCO")
  atoms <- c("A|C|0|0|1|0|3", "A|C|0|0|2|0|2", "A|O|0|0|1|0|1")
  expect_true(all(atoms %in% f))
  expect_true(all(c("B|C|1|C", "B|C|1|O", "D|C|O|2", "D|C|C|1", "D|C|O|1") %in% f))
  expect_true("P|C|1|C|1|O" %in% f)
  expect_equal(f, sort(unique(f), method = "radix"))
  expect_equal(morie_avalon_features("CCO", classes = "atom"), sort(atoms, method = "radix"))
  expect_error(morie_avalon_features("CCO", classes = "torsion"), "unknown feature class")
})

test_that("folded fingerprint and Tanimoto similarity", {
  fp <- morie_avalon("CCO", n_bits = 64)
  idx <- unique(vapply(fp$features, function(s) morie_avalon_fnv(s) %% 64, 0))
  expect_equal(sort(fp$on), sort(idx))
  expect_equal(fp$n_collisions, fp$n_features - length(idx))
  expect_equal(fp$density, length(idx) / 64)
  expect_equal(fp$n_hydrogens, 6L)
  expect_equal(morie_avalon("c1ccccc1")$n_rings, 1L)
  a <- c(1, 0, 1, 1, 0)
  b <- c(1, 1, 0, 1, 0)
  expect_equal(morie_avalon_tanimoto(a, b), 2 / 4)
  expect_equal(morie_avalon_tanimoto(fp$bits, fp$bits), 1)
  expect_equal(morie_avalon_tanimoto(c(0, 0), c(0, 0)), 0)
  expect_error(morie_avalon_tanimoto(a, b[-1]), "different widths")
  expect_error(morie_avalon("CCO", n_bits = 0), "at least one bit")
  expect_match(morie_avalon_cheatsheet(), "FNV-1a")
})
