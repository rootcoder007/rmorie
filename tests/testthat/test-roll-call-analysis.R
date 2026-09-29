# Tests for RollCallAnalysis: cohesion, party influence and logistic ideal points.

n <- 20
m <- 30
x <- sin((0:19) * 1.3) * 1.5 + ifelse((0:19) %% 2 == 1, 0.8, -0.8)
V <- matrix(0, n, m)
for (i in 0:19) for (j in 0:29) V[i + 1, j + 1] <- as.numeric(sin(j * 0.7) + (0.5 + ((0.3 * j) %% 5)) * x[i + 1] + 0.3 * cos(i * j) > 0)
V[3, 4] <- NA
party <- ifelse((0:19) %% 2 == 1, "L", "R")

test_that("RollcallCohesion Rice index and participation", {
  r <- RollcallCohesion(V, party)
  L <- V[party == "L", ]
  expect_equal(r$rice$L, abs(colSums(L == 1) - colSums(L == 0)) / colSums(!is.na(L)))
  expect_equal(r$participation[3], (m - 1) / m)
})

test_that("PartyInfluence and LogitIdealPoints", {
  pd <- as.numeric((0:19) %% 2 == 1)
  p <- PartyInfluence(V, pd, lopsided = 0.7)
  j <- p$close_votes[1] + 1
  ok <- !is.na(V[, j])
  expect_equal(p$party_effect[1], unname(coef(lm(V[ok, j] ~ p$ideal_points[ok] + pd[ok]))[3]), tolerance = 1e-9)
  l <- LogitIdealPoints(V, prior_sd = 3)
  expect_equal(sum(l$ideal_points^2) / n, 1, tolerance = 1e-9)
  expect_gt(abs(cor(l$ideal_points, x)), 0.9)
})
