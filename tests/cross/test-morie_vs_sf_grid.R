test_that("square grid centres equal sf::st_make_grid", {
  skip_if_not_installed("sf")
  for (cfg in list(c(0.3, -1.2, 4.1, 2, 0.7), c(10, 20, 15.5, 23, 1.25))) {
    b <- sf::st_bbox(c(xmin = cfg[1], ymin = cfg[2], xmax = cfg[3], ymax = cfg[4]))
    ref <- sf::st_coordinates(sf::st_make_grid(sf::st_as_sfc(b), cellsize = cfg[5], what = "centers"))
    expect_equal(RegularGrid(cfg[1:4], cfg[5]), unname(ref), tolerance = 1e-12)
  }
})
