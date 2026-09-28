test_that("HexagonalGridSample equals sp's hexagonal grid on a rectangle", {
  skip_if_not_installed("sp")
  for (cs in c(0.7, 1.3, 2.1)) {
    bb <- rbind(c(-1, 11.5), c(2, 9))
    ref <- sp:::hexGrid(bb, cellsize = cs, offset = c(0.25, 0.4))
    sq <- rbind(c(-1, 2), c(11.5, 2), c(11.5, 9), c(-1, 9))
    ours <- HexagonalGridSample(sq, cs, offset = c(0.25, 0.4))
    inb <- ref$x >= -1 & ref$x <= 11.5 & ref$y >= 2 & ref$y <= 9
    expect_equal(unname(ours), unname(as.matrix(ref[inb, ])), tolerance = 1e-12)
  }
})
