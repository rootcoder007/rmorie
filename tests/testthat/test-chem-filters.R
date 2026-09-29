.aspirin <- "CC(=O)Oc1ccccc1C(=O)O"
.cf_2p32t <- 4294967296

.combine_t <- function(seed, v) {
  add <- (v %% .cf_2p32t + 2654435769 + (seed * 64) %% .cf_2p32t + seed %/% 4) %% .cf_2p32t
  bitwXor(seed %/% 65536, add %/% 65536) * 65536 + bitwXor(seed %% 65536, add %% 65536)
}

test_that("MACCS keys match the RDKit reference values", {
  # the values in the doctest of RDKit's Chem/MACCSkeys.py
  expect_equal(MaccsFingerprint("CNO")$on_bits,
               c(24, 68, 69, 71, 93, 94, 102, 124, 131, 139, 151, 158, 160, 161, 164))
  expect_equal(MaccsFingerprint("CCC")$on_bits, c(74, 114, 149, 155, 160))
})

test_that("MACCS keys follow their definitions", {
  r <- MaccsFingerprint(.aspirin)
  on <- r$on_bits
  expect_equal(r$bits[1], 0)
  for (key in c(163, 162, 164, 154, 157, 159, 146, 139)) expect_true(key %in% on)
  expect_false(145 %in% on)
  expect_false(125 %in% on)
  expect_false(166 %in% on)
  for (key in c(42, 103, 88)) expect_false(key %in% on)
  two <- MaccsFingerprint("c1ccc2ccccc2c1")$on_bits
  expect_true(all(c(125, 145) %in% two))
  salt <- MaccsFingerprint("CC(=O)[O-].[Na+]")$on_bits
  expect_true(all(c(166, 49, 35) %in% salt))
})

test_that("properties recompute from the published contributions", {
  p <- MolecularProperties(.aspirin)
  expect_equal(p$MW, 9 * 12.011 + 4 * 15.999 + 8 * 1.008, tolerance = 1e-12)
  expect_equal(c(p$heavy_atoms, p$charge), c(13, 0))
  # Ertl TPSA: ester -O- 9.23, two carbonyl =O 17.07 each, acid -OH 20.23
  expect_equal(p$TPSA, 9.23 + 2 * 17.07 + 20.23, tolerance = 1e-12)
  expect_equal(c(p$HBD, p$HBA, p$Rot), c(1, 3, 2))
  anion <- MolecularProperties("CC(=O)[O-]")
  expect_equal(anion$charge, -1)
  expect_equal(anion$TPSA, 17.07 + 23.06, tolerance = 1e-12)
  diol <- MolecularProperties("OCCO")
  expect_equal(c(diol$HBD, diol$HBA), c(2, 2))
  expect_equal(diol$TPSA, 2 * 20.23, tolerance = 1e-12)
})

test_that("Morgan identifiers recompute from the hash", {
  r <- MorganEnvironments("CC", radius = 1)
  seed <- 0
  # atomic number, total degree, total H, charge, mass shift (no ring flag)
  for (cc in c(6, 4, 3, 0, 0)) seed <- .combine_t(seed, cc)
  expect_equal(r$counts[[as.character(seed)]], 2)
  layer1 <- .combine_t(.combine_t(0, seed), .combine_t(.combine_t(0, 1), seed))
  expect_equal(r$counts[[as.character(layer1)]], 1)
  expect_equal(sort(as.numeric(r$counts)), c(1, 2))
  benzene <- MorganEnvironments("c1ccccc1")
  expect_equal(sort(as.numeric(benzene$counts)), c(6, 6, 6))
  expect_true(all(vapply(benzene$environments, function(e) e[3] <= 2, TRUE)))
  expect_equal(sum(benzene$counts), length(benzene$environments))
})

test_that("the SA score recomputes from its parts", {
  me <- MorganEnvironments("C1CC2CCC1CC2")
  fs <- as.list(rep(0, length(me$counts)))
  names(fs) <- names(me$counts)
  r <- SaScore("C1CC2CCC1CC2", fs)
  n <- 8
  expect_equal(c(r$n_bridgehead, r$n_spiro), c(2, 0))
  expect_false(r$macrocycle)
  expect_equal(r$fragment_score, 0)
  expect_equal(r$complexity, -(n^1.005 - n) - log10(3), tolerance = 1e-12)
  raw <- r$fragment_score + r$complexity + r$symmetry
  expect_equal(r$score, 11 - (raw + 5) / 6.5 * 9, tolerance = 1e-12)
  spiro <- SaScore("C1CCC2(CC1)CCCC2")
  expect_equal(c(spiro$n_spiro, spiro$fragment_score), c(1, -4))
  expect_true(SaScore("C1CCCCCCCCC1")$macrocycle)
  expect_equal(SaScore("N[C@@H](Cc1ccccc1)C(=O)O")$n_stereo, 1)
  hard <- SaScore("CC12CCC3C(CCC4=CC(=O)CCC34C)C1CCC2O")
  expect_gte(hard$score, 1)
  expect_lte(hard$score, 10)
  expect_gt(hard$score, SaScore("CCO")$score)
})

test_that("REOS alerts and property ranges", {
  r <- ReosFilter(.aspirin)
  expect_true(r$passed)
  expect_equal(r$filter, "OK")
  expect_length(r$alerts, 0)
  expect_length(r$violations, 0)
  halide <- ReosFilter("CCCCCCCCCCCCBr", rule_sets = "Glaxo")
  expect_false(halide$passed)
  expect_equal(halide$alerts[[1]], c("Glaxo", "R1 Reactive alkyl halides"))
  expect_equal(halide$filter, "R1 Reactive alkyl halides > 0")
  big <- ReosFilter("CCCCCCCCCCCCCCCCCCCCCCCC", rule_sets = character(0))
  expect_setequal(big$violations, c("LogP", "Rot"))
  expect_equal(ReosFilter(.aspirin, mw = c(0, 100))$violations, "MW")
  both <- ReosFilter("Oc1ccccc1O", rule_sets = c("PAINS", "Dundee"))
  expect_gt(length(both$alerts), 0)
  expect_true(all(vapply(both$alerts, function(a) a[1] %in% c("PAINS", "Dundee"), TRUE)))
  expect_equal(both$properties$MW, MolecularProperties("Oc1ccccc1O")$MW)
  expect_error(ReosFilter(.aspirin, rule_sets = "Nonesuch"), "unknown rule set")
})
