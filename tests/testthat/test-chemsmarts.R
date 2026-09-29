tab <- function(t) .cs_crippen[match(t, .cs_crippen$type), ]

test_that("ClogpEstimate reproduces the table sum for ethanol", {
  types <- c("C1", "C3", "O2", rep("H1", 5), "H2")
  r <- ClogpEstimate("CCO")
  expect_equal(sort(r$types), sort(types))
  expect_equal(r$logp, sum(tab(types)$logp), tolerance = 1e-12)
  expect_equal(r$mr, sum(tab(types)$mr), tolerance = 1e-12)
  expect_equal(ClogpEstimate("C1=CC=CC=C1O")$logp, ClogpEstimate("c1ccccc1O")$logp, tolerance = 1e-12)
})

test_that("SmartsMatch anchors", {
  expect_equal(SmartsMatch("CC(=O)Oc1ccccc1C(=O)O", "[CX3](=O)[OX2H1]")$anchors, 11)
  expect_equal(SmartsMatch("CC(=O)Nc1ccccc1", "[#6;R]!@[#7;!H0]")$anchors, 5)
  expect_false(SmartsMatch("c1ccccc1", "C")$matched)
  expect_error(SmartsMatch("CCO", "[C"))
})

test_that("PainsFilter flags known interference motifs", {
  expect_equal(PainsFilter("Oc1ccc(CC)cc1O")$names, "catechol_A(92)")
  expect_true(.cs_pains$name[250] %in% PainsFilter("Cc1ccc(Nc2ccc(N)cn2)cc1")$names)
  expect_false(PainsFilter("CC(=O)Oc1ccccc1C(=O)O")$flagged)
})
