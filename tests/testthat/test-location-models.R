pts <- function(n, off) {
  u <- .morie_random_uniform(4000, seed = 3, stream = 0)[off + seq_len(2 * n)]
  matrix(10 * u, ncol = 2, byrow = TRUE)
}

test_that("LinearAssignment is optimal (lpSolve::lp.assign) and handles rectangles", {
  u <- .morie_random_uniform(4000, seed = 3, stream = 0)
  for (t in 0:5) {
    C <- matrix(round(20 * u[50 * t + 1:30], 3), 5, 6, byrow = TRUE)
    r <- LinearAssignment(C)
    br <- min(apply(t(utils::combn(6, 5)), 1, function(cols) {
      pm <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(pm(v[-i]), function(z) c(v[i], z))))
      min(vapply(pm(cols), function(p) sum(C[cbind(1:5, p)]), 0))
    }))
    expect_equal(r$total, br, tolerance = 1e-12)
    expect_equal(LinearAssignment(t(C))$total, br, tolerance = 1e-12)
  }
  expect_identical(LinearAssignment(rbind(c(4, 1, 3), c(2, 0, 5), c(3, 2, 2)))$total, 5)
})

test_that("TravellingSalesman exact equals the shortest tour", {
  P <- pts(7, 700)
  D <- as.matrix(stats::dist(P))
  pm <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(pm(v[-i]), function(z) c(v[i], z))))
  br <- min(vapply(pm(2:7), function(p) {
    tr <- c(1, p, 1)
    sum(D[cbind(tr[-8], tr[-1])])
  }, 0))
  r <- TravellingSalesman(P)
  expect_equal(r$length, br, tolerance = 1e-12)
  h <- TravellingSalesman(P, method = "heuristic")
  expect_identical(sort(h$tour), 1:7)
  expect_gte(h$length, br - 1e-12)
  expect_identical(TravellingSalesman(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))$length, 4)
})

test_that("location models match enumeration and the Python arm", {
  P <- pts(9, 1000)
  u <- .morie_random_uniform(4000, seed = 3, stream = 0)
  w <- 1 + round(4 * u[1500 + 1:9])
  D <- as.matrix(stats::dist(P))
  for (p in 1:3) {
    cm <- utils::combn(9, p)
    expect_equal(PMedian(p, P, weights = w)$objective,
                 min(apply(cm, 2, function(S) sum(w * apply(D[, S, drop = FALSE], 1, min)))), tolerance = 1e-12)
    expect_equal(PCenter(p, P)$radius, min(apply(cm, 2, function(S) max(apply(D[, S, drop = FALSE], 1, min)))), tolerance = 1e-12)
    expect_equal(MaximalCovering(p, 3, P, weights = w)$covered_weight,
                 max(apply(cm, 2, function(S) sum(w[apply(D[, S, drop = FALSE] <= 3, 1, any)]))))
  }
  expect_identical(SetCoveringLocation(1, cbind(0:4, 0))$sites, c(1L, 4L))
  expect_identical(as.integer(FacilityLocation(c(1, 100, 1), rbind(c(0, 0), c(1, 0), c(10, 0)))$sites), c(1L, 3L))
})

test_that("TransportationProblem matches lpSolve::lp.transport", {
  r <- TransportationProblem(rbind(c(4, 6), c(5, 3)), c(10, 10), c(8, 12))
  expect_identical(r$total_cost, 74)
  C <- rbind(c(4, 6, 9), c(5, 3, 8))
  r <- TransportationProblem(C, c(15, 10), c(8, 12, 4))
  expect_identical(r$total_cost, 110)
})

test_that("routing, flow capture, compactness and opening", {
  expect_identical(VehicleRoutingSavings(c(0, 1, 1, 1), 2, rbind(c(0, 0), c(1, 0), c(2, 0), c(0, 5)))$routes, list(c(2L, 3L), 4L))
  paths <- list(c(1, 2, 3), c(4, 2, 5), c(6, 7), c(3, 7, 8), c(8, 9))
  expect_identical(FlowCapturingLocation(2, paths, c(3, 2, 4, 1, 5))$captured_volume, 11)
  sq <- PolygonCompactness(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))
  expect_equal(sq$polsby_popper, pi / 4, tolerance = 1e-15)
  expect_equal(sq$reock, 2 / pi, tolerance = 1e-15)
  L <- PolygonCompactness(rbind(c(0, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 2), c(0, 2)))
  expect_equal(L$convex_hull, 3 / 3.5, tolerance = 1e-15)
  expect_equal(L$reock, 3 / (2 * pi), tolerance = 1e-12)
  img <- matrix(0, 5, 5)
  img[2:3, 2:3] <- 1
  img[4, 4] <- 1
  o <- MorphologicalOpening(img, matrix(1, 2, 2), c(1, 1))$opened
  expect_identical(o[4, ], rep(0, 5))
  expect_identical(o[2:3, 2:3], matrix(1, 2, 2))
  g <- matrix(round(9 * .morie_random_uniform(42, seed = 5, stream = 0)), 6, 7)
  og <- MorphologicalOpening(g)$opened
  expect_true(all(og <= g))
  expect_identical(MorphologicalOpening(og)$opened, og)
})

test_that("heuristic paths match the Python arm", {
  P <- pts(14, 2000)
  expect_identical(TravellingSalesman(P, method = "heuristic")$tour, c(0L, 1L, 7L, 11L, 5L, 10L, 4L, 9L, 12L, 3L, 8L, 6L, 13L, 2L) + 1L)
  u <- .morie_random_uniform(4000, seed = 3, stream = 0)
  uf <- list(c(0, 1, 5, 7, 8), c(0, 1, 4, 6, 7, 8), c(0, 1, 3, 4, 6, 8))
  pm <- list(c(6, 7, 8), c(1, 6, 8), c(2, 4, 5))
  for (k in 0:2) {
    P <- pts(9, 1000 + 30 * k)
    w <- 1 + round(4 * u[1500 + 10 * k + 1:9])
    f <- round(2 + 6 * u[1800 + 10 * k + 1:9], 3)
    expect_equal(FacilityLocation(f, P, weights = w, max_enum = 1)$sites, uf[[k + 1]] + 1)
    expect_equal(PMedian(3, P, weights = w, max_enum = 1)$sites, pm[[k + 1]] + 1)
  }
})
