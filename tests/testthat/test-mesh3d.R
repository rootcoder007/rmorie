# Tests for Mesh3D: 3-D Delaunay and Voronoi, Ruppert refinement.

i <- 0:29
P <- cbind(sin(i * 1.3 + 0.2) * 2, cos(i * 2.7) * 1.5, sin(i * 0.37) + 0.1 * i)

test_that("Delaunay3D tetrahedra have empty circumspheres", {
  r <- Delaunay3D(P)
  for (k in seq_len(nrow(r$tetrahedra))) {
    cc <- r$circumcenters[k, ]
    d2 <- colSums((t(P) - cc)^2)
    r2 <- d2[r$tetrahedra[k, 1]]
    expect_true(all(abs(d2[r$tetrahedra[k, ]] - r2) <= 1e-9 * r2))
    expect_true(all(d2 >= r2 * (1 - 1e-9)))
  }
})

test_that("Voronoi3D recovers the unit cell of a jittered lattice", {
  lat <- as.matrix(expand.grid(-1:1, -1:1, -1:1))
  lat <- lat + 1e-7 * sin(outer(7 * (0:26), 0:2, "+"))
  w <- Voronoi3D(lat)
  expect_equal(w$bounded, seq_len(27) == 14)
  expect_equal(w$volumes[14], 1, tolerance = 1e-5)
})

test_that("RuppertRefine reaches the target angle and keeps the input points", {
  j <- 0:8
  Q <- cbind(4 * ((j * 0.618) %% 1), 2 * ((j * 0.414 + 0.3) %% 1))
  r <- RuppertRefine(Q, min_angle = 20)
  expect_gte(r$min_angle, 20)
  expect_equal(r$points[1:9, ], Q)
  ar <- apply(r$triangles, 1, function(t) {
    a <- r$points[t[1], ]
    b <- r$points[t[2], ]
    cc <- r$points[t[3], ]
    abs((b[1] - a[1]) * (cc[2] - a[2]) - (b[2] - a[2]) * (cc[1] - a[1])) / 2
  })
  h <- r$points[r$segments[, 1], ]
  ha <- abs(sum(h[, 1] * h[c(nrow(h), seq_len(nrow(h) - 1)), 2] - h[c(nrow(h), seq_len(nrow(h) - 1)), 1] * h[, 2])) / 2
  expect_equal(sum(ar), ha, tolerance = 1e-9)
})
