# Cross test: 3-D Delaunay tetrahedralisation and hull volume against geometry (Qhull).

test_that("Delaunay3D equals geometry::delaunayn and fills the convex hull", {
  skip_if_not_installed("geometry")
  i <- 0:29
  P <- cbind(sin(i * 1.3 + 0.2) * 2, cos(i * 2.7) * 1.5, sin(i * 0.37) + 0.1 * i)
  got <- Delaunay3D(P)$tetrahedra
  ref <- t(apply(geometry::delaunayn(P, options = "Qt Qbb Qc"), 1, sort))
  ref <- ref[do.call(order, as.data.frame(ref)), , drop = FALSE]
  expect_equal(unname(got), unname(ref))
  tv <- sum(apply(got, 1, function(s) {
    abs(det(t(P[s[-1], ]) - P[s[1], ])) / 6
  }))
  expect_equal(tv, geometry::convhulln(P, options = "FA")$vol, tolerance = 1e-12)
})
