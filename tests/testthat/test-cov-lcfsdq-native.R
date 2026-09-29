# Coverage for the nearest-neighbour cluster query: k-th nearest
# neighbours under three metrics (against dist()), the Clark-Evans ratio
# with the CSR and Donnelly moments, the buffer edge correction, local
# means within the NN radius, and the G function against its CSR curve.

.pts <- function() {
  set.seed(7)
  cbind(stats::runif(25, 0, 10), stats::runif(25, 0, 10))
}

test_that("k-th nearest neighbours under each metric", {
  P <- .pts()
  for (m in c("euclidean", "manhattan", "maximum")) {
    D <- as.matrix(stats::dist(P, method = m))
    diag(D) <- Inf
    nn <- morie_lcfsdq_nn(P, k = 2, metric = if (m == "maximum") "chebyshev" else m)
    expect_equal(nn$dist, unname(apply(D, 1, function(r) sort(r)[2])), tolerance = 1e-12)
    expect_identical(nn$index, unname(as.integer(apply(D, 1, function(r) order(r)[2]) - 1L)))
  }
  expect_error(morie_lcfsdq_nn(P, k = 25), "1..n-1")
})

test_that("Clark-Evans ratio with CSR and Donnelly moments", {
  d <- c(0.8, 1.1, 0.5, 1.4, 0.9, 1.0)
  ce <- morie_lcfsdq_clark_evans(d, 6, 20, 18)
  lam <- 6 / 20
  e <- 0.5 / sqrt(lam)
  se <- sqrt((4 - pi) / (4 * pi * lam * 6))
  expect_equal(c(ce$R, ce$se), c(mean(d) / e, se), tolerance = 1e-12)
  expect_equal(ce$p, 2 * stats::pnorm(-abs((mean(d) - e) / se)), tolerance = 1e-12)
  dn <- morie_lcfsdq_clark_evans(d, 6, 20, 18, edge = "donnelly")
  ed <- 0.5 * sqrt(20 / 6) + (0.0514 + 0.041 / sqrt(6)) * 18 / 6
  vd <- 0.0703 * 20 / 36 + 0.037 * 18 * sqrt(20 / 6^5)
  expect_equal(c(dn$expected, dn$se), c(ed, sqrt(vd)), tolerance = 1e-12)
  expect_error(morie_lcfsdq_clark_evans(d, 6, 0, 1), "zero area")
  expect_error(morie_lcfsdq_clark_evans(d, 6, 1, 1, edge = "torus"), "edge must be one of")
})

test_that("the query: radius, local means, flags, buffer and the G function", {
  P <- .pts()
  x <- P[, 1] + P[, 2]
  r <- morie_lcfsdq(x, P, sd_multiplier = 0.5)
  D <- as.matrix(stats::dist(P))
  diag(D) <- Inf
  nd <- apply(D, 1, min)
  rad <- mean(nd) + 0.5 * stats::sd(nd)
  expect_equal(r$radius, rad, tolerance = 1e-12)
  i <- 4
  mem <- which(D[i, ] <= rad)
  if (length(mem)) {
    expect_equal(r$local_mean[i], mean(x[mem]), tolerance = 1e-12)
    expect_equal(r$local_z[i], (mean(x[mem]) - mean(x)) / (stats::sd(x) / sqrt(length(mem))), tolerance = 1e-12)
  }
  expect_identical(r$clustered, as.integer(which(nd < mean(nd) - 0.5 * stats::sd(nd)) - 1))
  A <- diff(range(P[, 1])) * diff(range(P[, 2]))
  expect_equal(r$area, A, tolerance = 1e-12)
  expect_equal(r$G, vapply(r$grid, function(q) mean(nd <= q), 1))
  expect_equal(r$G_csr, 1 - exp(-25 / A * pi * r$grid^2), tolerance = 1e-12)
  b <- morie_lcfsdq(x, P, edge = "buffer", sd_multiplier = 0)
  bb <- c(range(P[, 1]), range(P[, 2]))
  rb <- mean(nd)
  keep <- which(P[, 1] - bb[1] >= rb & bb[2] - P[, 1] >= rb & P[, 2] - bb[3] >= rb & bb[4] - P[, 2] >= rb)
  expect_identical(b$clark_evans$n_kept, length(keep))
  expect_equal(b$se, sqrt((4 - pi) / (4 * pi * (25 / A) * length(keep))), tolerance = 1e-12)
  expect_error(morie_lcfsdq(x, P, edge = "buffer", sd_multiplier = 20), "fewer than three points")
  expect_error(morie_lcfsdq(x[-1], P), "same length")
  expect_error(morie_lcfsdq(x[1:2], P[1:2, ]), "at least three")
  expect_error(morie_lcfsdq(x, P, metric = "cosine"), "metric must be one of")
  expect_match(morie_lcfsdq_cheatsheet(), "chebyshev")
})
