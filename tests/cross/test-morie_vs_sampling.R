test_that("SampfordDesign joint probabilities equal sampling::UPsampfordpi2", {
  skip_if_not_installed("sampling")
  for (pik in list(c(0.1, 0.25, 0.35, 0.5, 0.8, 0.3, 0.7), c(0.2, 0.4, 0.6, 0.8), seq(0.05, 0.95, length.out = 10))) {
    pik <- pik * round(sum(pik)) / sum(pik)
    expect_lt(max(abs(SampfordDesign(pik)$joint - sampling::UPsampfordpi2(pik))), 1e-12)
  }
})
