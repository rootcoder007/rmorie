test_that("VoronoiCells equals deldir tiles, edges and neighbours", {
  skip_if_not_installed("deldir")
  U <- .morie_random_uniform(200, seed = 35, stream = 0)
  P <- cbind(3 * U[1:100], U[101:200])
  d <- deldir::deldir(P[, 1], P[, 2], rw = c(0, 3, 0, 1), digits = 15)
  r <- VoronoiCells(P, c(0, 3, 0, 1))
  expect_lt(max(abs(r$areas - d$summary$dir.area)), 1e-12)
  s <- d$dirsgs
  s <- s[sqrt((s$x1 - s$x2)^2 + (s$y1 - s$y2)^2) > 1e-9, ]
  expect_identical(nrow(r$edges), nrow(s))
  expect_setequal(paste(r$edges[, 1], r$edges[, 2]), paste(pmin(s$ind1, s$ind2), pmax(s$ind1, s$ind2)))
})

test_that("DelaunayTriangulation equals deldir triangles", {
  skip_if_not_installed("deldir")
  U <- .morie_random_uniform(200, seed = 36, stream = 0)
  P <- cbind(3 * U[1:100], U[101:200])
  d <- deldir::deldir(P[, 1], P[, 2], rw = c(0, 3, 0, 1), digits = 15)
  ref <- t(vapply(deldir::triang.list(d), function(t) sort(t$ptNum), numeric(3)))
  r <- DelaunayTriangulation(P)
  expect_setequal(apply(t(apply(r$triangles, 1, sort)), 1, paste, collapse = "-"), apply(ref, 1, paste, collapse = "-"))
})
