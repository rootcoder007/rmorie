test_that("patch_of_box returns every grid cell the box overlaps", {
  # 280 x 140 page, 14 x 14 grid: patches are 20 x 10 units
  cells <- function(b) {
    cs <- seq.int(b[1] %/% 20, max(ceiling(b[3] / 20) - 1, b[1] %/% 20))
    rs <- seq.int(b[2] %/% 10, max(ceiling(b[4] / 10) - 1, b[2] %/% 10))
    sort(as.integer(as.vector(outer(rs, cs, function(r, c) r * 14 + c))))
  }
  for (b in list(c(12, 3, 18, 8), c(35, 12, 65, 18), c(100, 50, 100, 50),
                 c(270, 131, 280, 140))) {
    expect_equal(patch_of_box(b, 280, 140, 14), cells(b))
  }
  # right half of the first patch stays in patch 0
  expect_equal(patch_of_box(c(12, 3, 18, 8), 280, 140, 14), 0L)
})
