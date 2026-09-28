skip_if_not_installed("REAT")

test_that("regional indices match REAT exactly", {
  E <- rbind(c(12, 30, 58, 7), c(50, 21, 33, 9), c(5, 8, 90, 14))
  lq <- LocationQuotient(E)
  for (r in 1:3) for (i in 1:4) {
    expect_equal(lq[r, i], REAT::locq(E[r, i], sum(E[, i]), sum(E[r, ]), sum(E)), tolerance = 1e-13)
  }
  expect_equal(KrugmanIndex(E[1, ], colSums(E))$K, REAT::krugman.spec(E[1, ], colSums(E)), tolerance = 1e-13)
  expect_equal(KrugmanIndex(E[, 2], rowSums(E))$K, REAT::krugman.conc(E[, 2], rowSums(E)), tolerance = 1e-13)
  expect_equal(LqGini(E[2, ], colSums(E)), REAT::gini.spec(E[2, ], colSums(E)), tolerance = 1e-13)
  expect_equal(HerfindahlIndex(E[3, ])$H, REAT::herf(E[3, ]), tolerance = 1e-13)
  expect_equal(HerfindahlIndex(E[3, ])$normalized, REAT::herf(E[3, ], coefnorm = TRUE), tolerance = 1e-13)
  emp <- c(10, 25, 7, 40, 18, 3, 30)
  reg <- c("a", "b", "a", "c", "d", "b", "d")
  eg <- EllisonGlaeser(emp, reg, c(120, 90, 200, 150))
  ref <- REAT::ellison.a(emp, c(120, 90, 200, 150)[match(unique(reg), c("a", "b", "c", "d"))], reg,
                         print.results = FALSE)
  expect_equal(c(eg$gamma, eg$G, eg$z, eg$H), unname(ref[1, c(1, 2, 3, 5)]), tolerance = 1e-12)
})
