test_that("CoxProcess replays the Philox inversion and matches Python", {
  field <- rbind(c(2, 5, 0.5), c(1, 8, 3))
  u <- .morie_random_uniform(4096, seed = 11, stream = 1000)
  pos <- 1
  pts <- matrix(0, 0, 2)
  for (iy in 1:2) {
    for (ix in 1:3) {
      m <- field[iy, ix]
      v <- u[pos]
      pos <- pos + 1
      k <- 0
      p <- exp(-m)
      F <- p
      while (v > F && p > 0) {
        k <- k + 1
        p <- p * m / k
        F <- F + p
      }
      for (t in seq_len(k)) {
        pts <- rbind(pts, c(ix - 1 + u[pos], iy + u[pos + 1]))
        pos <- pos + 2
      }
    }
  }
  r <- CoxProcess(field, c(0, 3, 1, 3), seed = 11)
  expect_equal(r$points, pts, tolerance = 1e-14)
  expect_equal(r$expected_count, 19.5)
  # Python doctest of morie.fn.sgcox.cox_process
  r2 <- CoxProcess(rbind(c(2, 5), c(1, 8)), c(0, 2, 0, 2), seed = 3)
  expect_equal(r2$n_points, 15)
  expect_equal(r2$points[1, ], c(0.05866488174069673, 0.4574055118719116), tolerance = 1e-14)
})
