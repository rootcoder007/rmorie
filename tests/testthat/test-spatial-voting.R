test_that("VoterUtility matches the Python arm and closed forms", {
  X <- rbind(c(0.2, -0.4), c(1.0, 0.5))
  Z <- rbind(c(0.5, 0.5), c(-0.3, 0.1), c(1.2, -1.0))
  expect_equal(VoterUtility(X, Z)$utility[1, ], c(-0.9, -0.5, -1.36), tolerance = 1e-12)
  expect_equal(VoterUtility(X, Z, "gaussian")$utility[1, ], c(0.6376281516217733, 0.7788007830714049, 0.5066169923655897), tolerance = 1e-12)
  expect_equal(VoterUtility(X, Z, "angular")$utility[1, 3], sum(X[1, ] * Z[3, ]) / sqrt(sum(X[1, ]^2) * sum(Z[3, ]^2)), tolerance = 1e-15)
  expect_equal(VoterUtility(X, Z, "rm", region = 1, beta = 2)$utility[1, 3], sum(X[1, ] * Z[3, ]) - 2 * (sqrt(sum(Z[3, ]^2)) - 1), tolerance = 1e-15)
  expect_equal(VoterUtility(X, Z, "discount", status_quo = c(0.1, 0), discount = 0.4)$utility[1, 3],
               -sum((X[1, ] - (c(0.1, 0) + 0.4 * (Z[3, ] - c(0.1, 0))))^2), tolerance = 1e-15)
  expect_identical(VoterUtility(X, Z)$choice, c(2L, 1L))
})

test_that("VoteProbability links, rules and the uncertain ideal point", {
  X <- rbind(c(0.2, -0.4), c(1.0, 0.5))
  Z <- rbind(c(0.5, 0.5), c(-0.3, 0.1), c(1.2, -1.0))
  U <- VoterUtility(X, Z[1:2, ])$utility
  expect_equal(VoteProbability(X, Z[1:2, ], link = "logistic", scale = 0.5)$probability[, 1], plogis((U[, 1] - U[, 2]) / 0.5),
               tolerance = 1e-15)
  expect_equal(VoteProbability(X, Z[1:2, ], link = "student_t", df = 4)$probability[, 1], pt(U[, 1] - U[, 2], 4), tolerance = 1e-15)
  P <- VoteProbability(X, Z, rule = "multinomial", scale = 0.8)$probability
  expect_equal(rowSums(P), c(1, 1), tolerance = 1e-15)
  p <- VoteProbability(rbind(c(0.3, -0.2)), rbind(c(0.5, 0.5), c(-0.3, 0.1)), ideal_cov = rbind(c(0.2, 0.05), c(0.05, 0.1)),
                       scale = 0.7)$probability[1, 1]
  expect_equal(p, 0.47081833627233527, tolerance = 1e-12)
})
