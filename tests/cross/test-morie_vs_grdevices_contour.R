test_that("IsoLines reproduces grDevices::contourLines vertices and lengths", {
  x <- seq(-2, 2, length.out = 23)
  y <- seq(-1.5, 2.5, length.out = 19)
  z <- outer(x, y, function(a, b) exp(-((a - 0.3)^2 + (b - 0.4)^2)) + 0.4 * exp(-((a + 1)^2 + (b - 1.2)^2) / 0.5))
  for (lev in c(0.15, 0.3, 0.55, 0.8)) {
    ref <- grDevices::contourLines(x, y, z, levels = lev)
    mine <- IsoLines(x, y, z, lev)
    len <- function(l) sum(sqrt(diff(l$x)^2 + diff(l$y)^2))
    expect_equal(mine$length, sum(vapply(ref, len, 0)), tolerance = 1e-12)
    key <- function(ls) sort(unlist(lapply(ls, function(l) sprintf("%.10f %.10f", l$x, l$y))))
    expect_equal(unique(key(mine$lines)), unique(key(ref)))
  }
})
