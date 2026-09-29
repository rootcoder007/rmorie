.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("pwr")

test_that("PowerAnova and PowerPropTest(cohen_h) equal the pwr package", {
  expect_equal(PowerAnova(n = 15, k = 4, f = 0.3), pwr::pwr.anova.test(k = 4, n = 15, f = 0.3)$power, tolerance = 1e-10)
  expect_equal(PowerPropTest(n = 50, p1 = 0.3, p2 = 0.5, method = "cohen_h"),
               pwr::pwr.2p.test(h = pwr::ES.h(0.3, 0.5), n = 50)$power, tolerance = 1e-12)
  expect_equal(PowerTTest(n = 20, delta = 0.7), pwr::pwr.t.test(n = 20, d = 0.7)$power, tolerance = 1e-10)
  expect_equal(CalculateInteractionPower(120, effect_size = 0.25),
               pwr::pwr.f2.test(u = 1, v = 118, f2 = 0.25^2)$power, tolerance = 1e-10)
})
