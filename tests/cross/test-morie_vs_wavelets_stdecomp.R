# Cross tests: Haar DWT and MRA against the wavelets package.

test_that("HaarDwt and HaarMra equal wavelets::dwt and wavelets::mra", {
  skip_if_not_installed("wavelets")
  x <- sin((0:63) * 0.37) * 2 + 0.01 * (0:63)^1.5 + cos((0:63) * 1.9)
  ref <- wavelets::dwt(x, filter = "haar", n.levels = 4, boundary = "periodic")
  got <- HaarDwt(x, 4)
  for (j in 1:4) {
    expect_equal(got$details[[j]], as.numeric(ref@W[[j]]), tolerance = 1e-13)
    expect_equal(got$smooths[[j]], as.numeric(ref@V[[j]]), tolerance = 1e-13)
  }
  m <- wavelets::mra(x, filter = "haar", n.levels = 4, method = "dwt", boundary = "periodic")
  gm <- HaarMra(x, 4)
  for (j in 1:4) expect_equal(gm$details[[j]], as.numeric(m@D[[j]]), tolerance = 1e-12)
  expect_equal(gm$smooth, as.numeric(m@S[[4]]), tolerance = 1e-12)
})
