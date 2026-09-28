test_that("molecular descriptors recompute", {
  expect_equal(SmilesMolecularWeight("CC(=O)Oc1ccccc1C(=O)O"), 9 * 12.011 + 8 * 1.008 + 4 * 15.999, tolerance = 1e-12)
  expect_equal(SmilesTpsa("Cn1cnc2c1c(=O)n(C)c(=O)n2C")$tpsa, 3 * 4.93 + 12.89 + 2 * 17.07, tolerance = 1e-12)
  expect_identical(SmilesHbd("OCC(O)CO"), 3L)
  expect_identical(SmilesRotatableBonds("CC(=O)NC1=CC=C(O)C=C1"), 1L)
})
