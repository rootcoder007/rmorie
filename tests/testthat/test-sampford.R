test_that("SampfordDesign gives Sampford's joint inclusion probabilities", {
  J <- SampfordDesign(c(0.2, 0.4, 0.6, 0.8))$joint
  expect_lt(max(abs(J[1, ] - c(0.2, 0.0277227722772277, 0.0534653465346534, 0.1188118811881188))), 1e-14)
  pik <- c(0.1, 0.25, 0.35, 0.5, 0.8, 0.3, 0.7)
  J <- SampfordDesign(pik)$joint
  expect_lt(max(abs(rowSums(J) - diag(J) - 2 * pik)), 1e-14)
})

test_that("SampfordDesign draws the Python arm's samples", {
  pik <- c(0.1, 0.25, 0.35, 0.5, 0.8, 0.3, 0.7)
  S <- t(vapply(0:199, function(s) SampfordDesign(pik, seed = s)$sample, integer(7)))
  expect_true(all(rowSums(S) == 3))
  expect_identical(as.numeric(colSums(S)), c(28, 48, 80, 110, 160, 48, 126))
  r <- SampfordDesign(c(0, 1, 0.5, 0.5, 0.6, 0.4))
  expect_identical(r$sample[1:2], c(0L, 1L))
  expect_identical(r$joint[2, ], c(0, 1, 0.5, 0.5, 0.6, 0.4))
  expect_lt(max(abs(rowSums(r$joint) - diag(r$joint) - 2 * c(0, 1, 0.5, 0.5, 0.6, 0.4))), 1e-14)
})
