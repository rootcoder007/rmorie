test_that("PwrRTest equals pwr::pwr.r.test", {
  skip_if_not_installed("pwr")
  for (alt in c("two.sided", "greater", "less")) {
    for (n in c(12, 40, 150)) {
      r <- if (alt == "less") -0.27 else 0.27
      expect_equal(PwrRTest(n = n, r = r, alternative = alt)$power,
                   pwr::pwr.r.test(n = n, r = r, alternative = alt)$power, tolerance = 1e-12)
    }
  }
  expect_equal(PwrRTest(r = 0.3, power = 0.8)$n, pwr::pwr.r.test(r = 0.3, power = 0.8)$n, tolerance = 1e-4)
  expect_equal(PwrRTest(n = 100, power = 0.9)$r, pwr::pwr.r.test(n = 100, power = 0.9)$r, tolerance = 1e-4)
})
