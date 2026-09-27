test_that("Tukey-Kramer, Scheffe and Johansen match references", {
  g1 <- c(2.1, 3.4, 1.9, 5.6, 4.4, 3.3, 2.8, 6.1, 3.9, 4.2, 2.2, 5.0, 40)
  g2 <- c(3.3, 4.1, 2.7, 6.8, 5.9, 4.4, 3.6, 7.2, 4.8, 5.3, 3.1, 6.6, 4.0, 9.5, 5.5)
  g3 <- c(5.1, 6.3, 4.8, 7.7, 6.9, 5.5, 6.1, 8.4, 5.9, 7.0, 4.6)
  g4 <- c(1.2, 2.8, 3.3, 2.1, 4.4, 3.9, 2.5, 3.0, 1.8, 2.6)
  th <- TukeyHSD(aov(v ~ f, data.frame(v = c(g1, g2, g3), f = factor(rep(1:3, c(13, 15, 11))))))$f
  r <- TukeyKramer(list(g1, g2, g3))
  expect_equal(r$comparisons$diff, -unname(th[, "diff"]), tolerance = 1e-12)
  expect_equal(r$comparisons$p_adj, unname(th[, "p adj"]), tolerance = 1e-9)
  expect_equal(ScheffeCI(list(g1, g2, g3), c(1, -0.5, -0.5))$S, 5.226295022118386, tolerance = 1e-12)
  j <- JohanQ(list(g1, g2, g3, g4), rbind(c(1, 1, -1, -1)))
  expect_equal(c(j$statistic, j$crit, j$p_value), c(0.003459911679137071, 4.2118872408293, 0.9535559358142094), tolerance = 1e-9)
  expect_equal(QTukey(0.95, 3, 20)$q, 3.577934581525569, tolerance = 1e-12)
})
