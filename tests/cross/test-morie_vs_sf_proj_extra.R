test_that("Robinson, Web Mercator, inverses and rotated pole equal PROJ via sf", {
  skip_if_not_installed("sf")
  ll <- rbind(c(10, 50), c(-120, -30), c(179, 89.5), c(33.3, -71.2), c(-75.6, 12.4), c(140, -5))
  wgs <- "+proj=longlat +datum=WGS84"
  rob <- sf::sf_project(wgs, "+proj=robin +datum=WGS84", ll)
  for (i in seq_len(nrow(ll))) expect_equal(RobinsonProject(ll[i, 1], ll[i, 2]), rob[i, ], tolerance = 1e-12)
  inv <- sf::sf_project("+proj=robin +datum=WGS84", wgs, rob)
  # PROJ 9.8's Newton inversion stops about 1e-5 degrees short; ours inverts the table to rounding
  for (i in seq_len(nrow(ll))) expect_equal(MapUnproject(rob[i, 1], rob[i, 2], "robin"), inv[i, ], tolerance = 1e-6)
  wm <- sf::sf_project(wgs, "EPSG:3857", ll[-3, ])
  for (i in seq_len(nrow(wm))) expect_equal(WebMercator(ll[-3, ][i, 1], ll[-3, ][i, 2]), wm[i, ], tolerance = 1e-12)
  mer <- sf::sf_project(wgs, "+proj=merc +datum=WGS84", ll[-3, ])
  for (i in seq_len(nrow(mer))) expect_equal(MapUnproject(mer[i, 1], mer[i, 2], "merc"), ll[-3, ][i, ], tolerance = 1e-12)
  for (pr in c("moll", "sinu")) {
    xy <- sf::sf_project(wgs, paste0("+proj=", pr, " +R=6371000"), ll)
    for (i in seq_len(nrow(ll))) expect_equal(MapUnproject(xy[i, 1], xy[i, 2], pr), ll[i, ], tolerance = 1e-10)
  }
  ob <- "+proj=ob_tran +o_proj=longlat +o_lat_p=39.25 +o_lon_p=0 +lon_0=18 +datum=WGS84"
  rot <- sf::sf_project(wgs, ob, ll)
  for (i in seq_len(nrow(ll))) expect_equal(RotatedPole(ll[i, 1], ll[i, 2], -162, 39.25), rot[i, ], tolerance = 1e-10)
})
