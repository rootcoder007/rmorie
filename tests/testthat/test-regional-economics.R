test_that("regional indices match their definitions", {
  E <- rbind(c(10, 30, 60), c(50, 20, 30), c(5, 5, 90))
  lq <- LocationQuotient(E)
  expect_equal(lq[2, 1], (50 / 100) / (65 / 300))
  expect_equal(colSums(lq * rowSums(E)) / 300, rep(1, 3))
  expect_equal(unlist(KrugmanIndex(c(10, 30, 60), c(30, 30, 40))), c(K = 0.4, hoover = 0.2))
  expect_equal(round(LqGini(c(10, 30, 60), c(30, 30, 40)), 5), 0.27451)
  h <- HerfindahlIndex(c(50, 30, 20), k = 2)
  expect_equal(c(h$H, h$normalized, h$CR), c(0.38, 0.07, 0.8))
  expect_error(KrugmanIndex(1:2, 1:3), "equal length")
})

test_that("Ellison-Glaeser and Duranton-Overman", {
  r <- EllisonGlaeser(c(10, 20, 30, 40), c("a", "a", "b", "c"), c(100, 100, 100))
  expect_equal(round(c(r$gamma, r$G, r$H), 6), c(-0.414286, 0.006667, 0.3))
  cg <- CoagglomerationIndex(c(10, 20, 30, 40, 25, 25), c(1, 1, 1, 2, 2, 2), c("a", "b", "a", "a", "c", "b"),
                             c(100, 100, 100))
  expect_equal(round(cg$gamma_c, 6), 0.083333)
  k <- DurantonOverman(rbind(c(0, 0), c(1, 0), c(0, 1)), c(0, 1), bandwidth = 0.5)
  expect_equal(round(k$K, 6), c(0.153718, 0.720813))
  sites <- cbind(0:24 %% 5, 0:24 %/% 5)
  b <- DurantonOverman(rbind(c(0, 0), c(3, 0), c(0, 4), c(1, 1)), c(1, 3), sites = sites, n_sim = 40, seed = 3)
  expect_true(all(b$lower <= b$upper))
})
