.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("EValue")

test_that("Evalu equals EValue::evalues.RR", {
  for (v in list(c(2.2, 1.3, 3.7), c(0.5, 0.3, 0.8), c(1.8, 0.9, 3.6), c(4, 2.5, 7))) {
    ref <- suppressMessages(EValue::evalues.RR(v[1], lo = v[2], hi = v[3]))
    r <- Evalu(v[1], ci_lower = v[2], ci_upper = v[3])
    expect_equal(r$evalue, unname(ref["E-values", "point"]), tolerance = 1e-12)
    lim <- ref["E-values", c("lower", "upper")]
    expect_equal(r$evalue_ci, unname(lim[!is.na(lim)][1]), tolerance = 1e-12)
  }
})
