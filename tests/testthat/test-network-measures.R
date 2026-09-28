.ns_E <- rbind(c(0, 1), c(0, 2), c(0, 4), c(0, 10), c(1, 2), c(1, 4), c(1, 7), c(2, 3), c(2, 7), c(2, 10),
    c(2, 12), c(3, 4), c(3, 7), c(3, 8), c(3, 10), c(3, 11), c(3, 12), c(3, 13), c(4, 5), c(4, 11), c(4, 12),
    c(5, 6), c(6, 7), c(6, 8), c(6, 13), c(7, 8), c(8, 9), c(8, 12), c(8, 13), c(9, 10), c(10, 11), c(11,
    12), c(12, 13))

test_that("Centralities and NetworkSummary equal igraph 2.3", {
  c <- Centralities(14, .ns_E)
  expect_equal(c$closeness, c(0.04, 0.041666666666666664, 0.047619047619047616, 0.05555555555555555,
      0.047619047619047616, 0.037037037037037035, 0.04, 0.047619047619047616, 0.047619047619047616,
      0.037037037037037035, 0.043478260869565216, 0.043478260869565216, 0.05, 0.041666666666666664),
      tolerance = 1e-12)
  expect_equal(c$betweenness, c(2.1499999999999995, 1.9083333333333334, 5.760714285714284,
      11.686904761904762, 11.402380952380954, 1.4999999999999998, 4.676190476190476, 6.445238095238095,
      8.609523809523811, 0.825, 6.383333333333334, 1.2, 5.085714285714285, 1.3666666666666667),
      tolerance = 1e-12)
  expect_equal(c$eigenvector, c(0.4728574066597655, 0.4809569646238407, 0.7544424403056289,
      0.9999999999999999, 0.6735861265464871, 0.202720743585162, 0.39325342162047877, 0.6302013821908234,
      0.6878492044729186, 0.24081720468726928, 0.5794770360283162, 0.581442694214562, 0.8068410692792957,
      0.5487667703769896), tolerance = 1e-12)
  expect_equal(c$pagerank, c(0.06178761856974354, 0.061525775173256754, 0.08674185657181396,
      0.11227679928504421, 0.08977890287800859, 0.03724681912583612, 0.0650063788415648, 0.07455595837750398,
      0.08989941409429947, 0.03644203408672737, 0.07642351789656408, 0.06058192538496862, 0.0863121444459754,
      0.061420855268693134), tolerance = 1e-12)
  s <- NetworkSummary(14, .ns_E)
  expect_equal(s$transitivity, 0.40714285714285714, tolerance = 1e-12)
  expect_equal(s$assortativity, -0.039689034369883934, tolerance = 1e-12)
  expect_equal(s$efficiency, 0.661172161172161, tolerance = 1e-12)
  expect_equal(c(s$diameter, s$radius), c(3, 2))
  expect_equal(ModularityScore(14, .ns_E, ifelse(1:14 <= 7, 0, 1)), 0.014692378328741929, tolerance = 1e-12)
  expect_equal(unlist(NetworkConnectivity(14, .ns_E)), c(edge = 2, vertex = 2))
})

test_that("small graphs by hand", {
  path <- rbind(c(0, 1), c(1, 2), c(2, 3))
  expect_equal(Centralities(4, path)$betweenness, c(0, 2, 2, 0))
  expect_equal(Centralities(3, rbind(c(0, 1), c(1, 2)))$information, c(1, 1.5, 1))
  expect_equal(NetworkSummary(3, rbind(c(0, 1), c(1, 0), c(1, 2)), TRUE)$reciprocity, 2 / 3)
  E0 <- (1 + 0.5 + 1 / 3 + 1 + 0.5 + 1) / 6
  expect_equal(NodeVulnerability(4, path)$vulnerability[1], (E0 - 2.5 / 6) / E0)
  expect_equal(NetworkConnectivity(5, cbind(0:4, c(1:4, 0)))$vertex, 2)
})
